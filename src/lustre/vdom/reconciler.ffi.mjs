// IMPORTS ---------------------------------------------------------------------

import { escape } from "../../../houdini/houdini.mjs";

import {
  element_kind,
  fragment_kind,
  map_kind,
  memo_kind,
  raw_container_kind,
  text_kind,
} from "./vnode.mjs";

import {
  always_kind,
  attribute_kind,
  event_kind,
  never_kind,
  property_kind,
} from "./vattr.mjs";

import {
  insert_kind,
  move_kind,
  remove_kind,
  replace_kind,
  replace_raw_content_kind,
  replace_text_kind,
  update_kind,
} from "./patch.mjs";

import { separator_element, separator_subtree } from "./path.mjs";

import { iterate } from "../internals/list.ffi.mjs";

import { NAMESPACE_HTML } from "../internals/constants.ffi.mjs";

import {
  Result$Error,
  Result$isOk,
  Result$Ok,
  Result$Ok$0,
} from "../../gleam.mjs";

//

const setTimeout = globalThis.setTimeout;
const clearTimeout = globalThis.clearTimeout;

// Helpers to convert between Gleam Result and nullable for insert_before /
// move_before reference nodes and get_attribute return values.
const wrapRef = (ref) => ref != null ? Result$Ok(ref) : Result$Error(undefined);

// METADATA / STATEFUL TREE ----------------------------------------------------

// We store some additional data for every node that we create.
// We store that "metadata" using a symbol on each DOM node.

const meta = Symbol("lustre");

class MetadataNode {
  constructor(kind, parent, node, key) {
    this.kind = kind;

    // store the key of the element to be able to reconstruct the path
    // once an event gets dispatched.
    this.key = key;

    // parent and children point to the _metadata_ nodes.
    this.parent = parent;
    this.children = [];

    // a reference back to the "real" DOM node.
    this.node = node;

    // in "debug" mode or after virtualisation, fragments also have an "end" marker.
    // we need to move and modify that end marker with the fragment if it exists.
    this.endNode = null;

    // data for the event handlers and attached throttlers and debouncers.
    this.handlers = new Map();
    this.throttles = new Map();
    this.debouncers = new Map();
  }

  get isVirtual() {
    return this.kind === fragment_kind || this.kind === map_kind;
  }

  get parentNode() {
    return this.isVirtual ? this.node.parentNode : this.node;
  }
}

// A node is a Lustre node if it has this metadata.
export const isLustreNode = (node) => node[meta] instanceof MetadataNode;

//
export const insertMetadataChild = (kind, parent, node, index, key) => {
  const child = new MetadataNode(kind, parent, node, key);

  node[meta] = child;
  parent?.children.splice(index, 0, child);

  return child;
};

const getPath = (node) => {
  let path = "";

  for (let current = node[meta]; current.parent; current = current.parent) {
    // Map nodes use a different separator to mark isolated event subtrees.
    // This allows the cache to split paths and look up handlers in the correct
    // subtree, keeping event handlers stable when parent Map nodes update.
    const separator = current.parent && current.parent.kind === map_kind
      ? separator_subtree
      : separator_element;

    if (current.key) {
      path = `${separator}${current.key}${path}`;
    } else {
      const index = current.parent.children.indexOf(current);
      path = `${separator}${index}${path}`;
    }
  }

  // remove the leading separator.
  return path.slice(1);
};

// RECONCILER ------------------------------------------------------------------

export class Reconciler {
  #root = null;

  #decodeEvent;
  #dispatch;
  #platform;

  #debug = false;

  constructor(root, decodeEvent, dispatch, platform, { debug = false } = {}) {
    this.#root = root;
    this.#decodeEvent = decodeEvent;
    this.#dispatch = dispatch;
    this.#platform = platform;
    this.#debug = debug;
  }

  mount(vdom) {
    insertMetadataChild(element_kind, null, this.#root, 0, null);
    this.#insertChild(this.#root, null, this.#root[meta], 0, vdom);
  }

  push(patch, memos = null) {
    this.#memos = memos;
    this.#stack.push({ node: this.#root[meta], patch: patch });
    this.#reconcile();
  }

  // PATCHING ------------------------------------------------------------------

  #memos;
  #stack = [];

  #reconcile() {
    const stack = this.#stack;

    while (stack.length) {
      const { node, patch } = stack.pop();
      const { children: childNodes } = node;
      const { changes, removed, children: childPatches } = patch;

      iterate(changes, (change) => this.#patch(node, change));

      if (removed) {
        this.#removeChildren(node, childNodes.length - removed, removed);
      }

      iterate(childPatches, (childPatch) => {
        const child = childNodes[childPatch.index | 0];
        this.#stack.push({ node: child, patch: childPatch });
      });
    }
  }

  #patch(node, change) {
    switch (change.kind) {
      case replace_text_kind:
        this.#replaceText(node, change);
        break;

      case replace_raw_content_kind:
        this.#replaceRawContent(node, change);
        break;

      case update_kind:
        this.#update(node, change);
        break;

      case move_kind:
        this.#move(node, change);
        break;

      case remove_kind:
        this.#remove(node, change);
        break;

      case replace_kind:
        this.#replace(node, change);
        break;

      case insert_kind:
        this.#insert(node, change);
        break;
    }
  }

  // CHANGES -------------------------------------------------------------------

  #insert(parent, { children, before }) {
    const fragment = this.#platform.create_fragment();
    const beforeEl = this.#getReference(parent, before);

    this.#insertChildren(fragment, null, parent, before | 0, children);

    this.#platform.insert_before(
      parent.parentNode,
      fragment,
      wrapRef(beforeEl),
    );
  }

  #replace(parent, { index, with: child }) {
    this.#removeChildren(parent, index | 0, 1);
    const beforeEl = this.#getReference(parent, index);
    this.#insertChild(parent.parentNode, beforeEl, parent, index | 0, child);
  }

  #getReference(node, index) {
    index = index | 0;
    const { children } = node;
    const childCount = children.length;

    if (index < childCount) return children[index].node;
    if (node.endNode) return node.endNode;
    if (!node.isVirtual) return null;

    // unwrap the last child as long as we point to a fragment.
    // otherwise, the fragments next sibling would be the first child of the
    // fragment, not the first element after it.
    while (node.isVirtual && node.children.length) {
      if (node.endNode) {
        const sibling = this.#platform.next_sibling(node.endNode);
        return Result$isOk(sibling) ? Result$Ok$0(sibling) : null;
      }
      node = node.children[node.children.length - 1];
    }

    const sibling = this.#platform.next_sibling(node.node);
    return Result$isOk(sibling) ? Result$Ok$0(sibling) : null;
  }

  #move(parent, { key, before }) {
    before = before | 0;

    const { children, parentNode } = parent;

    // unlike insert, we always have to have the before element here!
    const beforeEl = children[before].node;

    let prev = children[before];
    // we only move items to earlier positions, so we can start searching at before + 1.
    for (let i = before + 1; i < children.length; ++i) {
      const next = children[i];
      // we shift items from before to the key over one-by-one, to make room
      // for the moved element at children[before].
      children[i] = prev;
      prev = next;

      if (next.key === key) {
        children[before] = next;
        break;
      }
    }

    // prev now is the same as `next` inside the loop, and points to the element
    // we found that matches the key! all that's left is to move it before `beforeEl`.
    this.#moveChild(parentNode, prev, beforeEl);
  }

  #moveChildren(domParent, children, beforeEl) {
    for (let i = 0; i < children.length; ++i) {
      this.#moveChild(domParent, children[i], beforeEl);
    }
  }

  #moveChild(domParent, child, beforeEl) {
    this.#platform.move_before(domParent, child.node, wrapRef(beforeEl));

    // child might be a fragment, in which case we need do move all its child dom nodes too
    if (child.isVirtual) {
      this.#moveChildren(domParent, child.children, beforeEl);
    }

    // if "endNode" is set, that node is also a sibling node that we need to move with the children
    if (child.endNode) {
      this.#platform.move_before(domParent, child.endNode, wrapRef(beforeEl));
    }
  }

  #remove(parent, { index }) {
    this.#removeChildren(parent, index, 1);
  }

  #removeChildren(parent, index, count) {
    const { children, parentNode } = parent;
    const deleted = children.splice(index, count);

    for (let i = 0; i < deleted.length; ++i) {
      const child = deleted[i];
      const { node, endNode, isVirtual, children: nestedChildren } = child;

      this.#platform.remove_child(parentNode, node);
      if (endNode) {
        this.#platform.remove_child(parentNode, endNode);
      }

      this.#removeDebouncers(child);

      if (isVirtual) {
        deleted.push(...nestedChildren);
      }
    }
  }

  #removeDebouncers(node) {
    const { debouncers, children } = node;
    for (const { timeout } of debouncers.values()) {
      if (timeout) {
        clearTimeout(timeout);
      }
    }

    debouncers.clear();

    iterate(children, (child) => this.#removeDebouncers(child));
  }

  #update({ node, handlers, throttles, debouncers }, { added, removed }) {
    iterate(removed, ({ name }) => {
      if (handlers.delete(name)) {
        this.#platform.remove_event_listener(node, name, handleEvent);
        this.#updateDebounceThrottle(throttles, name, 0);
        this.#updateDebounceThrottle(debouncers, name, 0);
      } else {
        this.#platform.remove_attribute(node, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });

    iterate(added, (attribute) => this.#createAttribute(node, attribute));
  }

  #replaceText({ node }, { content }) {
    this.#platform.set_text(node, content ?? "");
  }

  #replaceRawContent({ node }, { content }) {
    this.#platform.set_raw_content(node, content ?? "");
  }

  // INSERT --------------------------------------------------------------------

  #insertChildren(domParent, beforeEl, metaParent, index, children) {
    iterate(
      children,
      (child) =>
        this.#insertChild(domParent, beforeEl, metaParent, index++, child),
    );
  }

  #insertChild(domParent, beforeEl, metaParent, index, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        // Insert parent into tree BEFORE setting raw content.
        // Some platforms (like OpenTUI) require the parent node to be in the
        // layout tree before children can be added to it.
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        this.#insertChildren(node, null, node[meta], 0, vnode.children);

        break;
      }

      case text_kind: {
        const node = this.#createTextNode(metaParent, index, vnode);
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));

        break;
      }

      case fragment_kind: {
        const marker = "lustre:fragment";
        const head = this.#createHead(marker, metaParent, index, vnode);

        this.#platform.insert_before(domParent, head, wrapRef(beforeEl));
        this.#insertChildren(
          domParent,
          beforeEl,
          head[meta],
          0,
          vnode.children,
        );

        if (this.#debug) {
          head[meta].endNode = this.#platform.create_comment(` /${marker} `);
          this.#platform.insert_before(
            domParent,
            head[meta].endNode,
            wrapRef(beforeEl),
          );
        }

        break;
      }

      case raw_container_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        // Insert parent into tree BEFORE setting raw content.
        // Some platforms (like OpenTUI) require the parent node to be in the
        // layout tree before children can be added to it.
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        this.#replaceRawContent({ node }, vnode);

        break;
      }

      case map_kind: {
        // Map nodes are virtual like fragments; this allows us to track
        // subtree boundaries in the real DOM and construct event paths accordingly.
        const head = this.#createHead("lustre:map", metaParent, index, vnode);
        this.#platform.insert_before(domParent, head, wrapRef(beforeEl));
        this.#insertChild(domParent, beforeEl, head[meta], 0, vnode.child);

        break;
      }

      case memo_kind: {
        // NOTE: we do not get memo nodes when running as a server component!
        // Memo nodes are always transparent - they don't create DOM nodes even in debug mode.
        const child = this.#memos?.get(vnode.view) ?? vnode.view();
        this.#insertChild(domParent, beforeEl, metaParent, index, child);

        break;
      }
    }
  }

  #createElement(parent, index, { kind, key, tag, namespace, attributes }) {
    const node = this.#platform.create_element(
      namespace || NAMESPACE_HTML,
      tag,
    );
    insertMetadataChild(kind, parent, node, index, key);

    if (this.#debug && key) {
      this.#platform.set_attribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));

    return node;
  }

  #createTextNode(parent, index, { kind, key, content }) {
    const node = this.#platform.create_text_node(content ?? "");
    insertMetadataChild(kind, parent, node, index, key);

    return node;
  }

  #createHead(marker, parent, index, { kind, key }) {
    const node = this.#debug
      ? this.#platform.create_comment(markerComment(marker, key))
      : this.#platform.create_text_node("");
    insertMetadataChild(kind, parent, node, index, key);

    return node;
  }

  #createAttribute(node, attribute) {
    const { debouncers, handlers, throttles } = node[meta];

    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      debounce: debounceDelay,
      throttle: throttleDelay,
    } = attribute;

    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? "";
        if (name === "virtual:defaultValue") {
          this.#platform.set_property(node, "defaultValue", valueOrDefault);
          return;
        } else if (name === "virtual:defaultChecked") {
          this.#platform.set_property(node, "defaultChecked", true);
          return;
        } else if (name === "virtual:defaultSelected") {
          this.#platform.set_property(node, "defaultSelected", true);
          return;
        }

        const current = this.#platform.get_attribute(node, name);
        const currentValue = Result$isOk(current) ? Result$Ok$0(current) : null;
        if (valueOrDefault !== currentValue) {
          this.#platform.set_attribute(node, name, valueOrDefault);
        }

        SYNCED_ATTRIBUTES[name]?.added?.(node, valueOrDefault);

        break;
      }

      case property_kind:
        this.#platform.set_property(node, name, value);
        break;

      case event_kind: {
        if (handlers.has(name)) {
          // we re-attach an event listener on every change in case we need
          // to change the options we pass.
          this.#platform.remove_event_listener(node, name, handleEvent);
        }

        const passive = prevent.kind === never_kind;
        this.#platform.add_event_listener(node, name, handleEvent, passive);

        this.#updateDebounceThrottle(throttles, name, throttleDelay);
        this.#updateDebounceThrottle(debouncers, name, debounceDelay);

        handlers.set(name, (event) => this.#handleEvent(attribute, event));

        break;
      }
    }
  }

  #updateDebounceThrottle(map, name, delay) {
    const debounceOrThrottle = map.get(name);

    if (delay > 0) {
      if (debounceOrThrottle) {
        debounceOrThrottle.delay = delay;
      } else {
        map.set(name, { delay });
      }
    } else if (debounceOrThrottle) {
      const { timeout } = debounceOrThrottle;
      if (timeout) {
        clearTimeout(timeout);
      }
      map.delete(name);
    }
  }

  #handleEvent(attribute, event) {
    const { currentTarget, type } = event;
    const { debouncers, throttles } = currentTarget[meta];
    const path = getPath(currentTarget);

    const {
      prevent_default: prevent,
      stop_propagation: stop,
      include,
    } = attribute;

    if (prevent.kind === always_kind) event.preventDefault();
    if (stop.kind === always_kind) event.stopPropagation();

    if (type === "submit") {
      event.detail ??= {};
      event.detail.formData = [
        ...new FormData(event.target, event.submitter).entries(),
      ];
    }

    const data = this.#decodeEvent(event, path, type, include);

    const throttle = throttles.get(type);
    if (throttle) {
      const now = Date.now();
      const last = throttle.last || 0;

      if (now > last + throttle.delay) {
        throttle.last = now;
        throttle.lastEvent = event;
        this.#dispatch(event, data);
      }
    }

    const debounce = debouncers.get(type);
    if (debounce) {
      clearTimeout(debounce.timeout);

      debounce.timeout = setTimeout(() => {
        if (event === throttles.get(type)?.lastEvent) return;
        this.#dispatch(event, data);
      }, debounce.delay);
    }

    if (!throttle && !debounce) {
      this.#dispatch(event, data);
    }
  }
}

// UTILS -----------------------------------------------------------------------

const markerComment = (marker, key) => {
  if (key) {
    return ` ${marker} key="${escape(key)}" `;
  } else {
    return ` ${marker} `;
  }
};

// EVENTS ----------------------------------------------------------------------

/** Stable references to an element's event handler is necessary if you ever want
 *  to actually remove them. To achieve that we define this shell `handleEvent`
 *  function that just delegates to an actual event handler stored on the node
 *  itself.
 *
 *  Doing things this way lets us swap out the underlying handler – which may
 *  happen - without needing to rebind the event listener.
 */
const handleEvent = (event) => {
  const { currentTarget, type } = event;
  const handler = currentTarget[meta].handlers.get(type);
  handler(event);
};

// ATTRIBUTE SPECIAL CASES -----------------------------------------------------

/* @__NO_SIDE_EFFECTS__ */
const syncedBooleanAttribute = (name) => {
  return {
    added(node) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    },
  };
};

/* @__NO_SIDE_EFFECTS__ */
const syncedAttribute = (name) => {
  return {
    added(node, value) {
      node[name] = value;
    },
  };
};

const SYNCED_ATTRIBUTES = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),

  autofocus: {
    added(node) {
      queueMicrotask(() => {
        node.focus?.();
      });
    },
  },

  autoplay: {
    added(node) {
      try {
        node.play?.();
      } catch (e) {
        console.error(e);
      }
    },
  },
};
