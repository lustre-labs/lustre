// IMPORTS ---------------------------------------------------------------------

import {
  element_kind,
  text_kind,
  fragment_kind,
  unsafe_inner_html_kind,
} from "./vnode.mjs";

import {
  attribute_kind,
  property_kind,
  event_kind,
  never_kind,
  always_kind,
} from "./vattr.mjs";

import {
  insert_kind,
  move_kind,
  remove_kind,
  replace_kind,
  replace_inner_html_kind,
  replace_text_kind,
  update_kind,
} from "./patch.mjs";

import { separator_element } from "./path.mjs";

import {
  document,
  SUPPORTS_MOVE_BEFORE,
  NAMESPACE_HTML,
} from "../internals/constants.ffi.mjs";

//

// DOM API ---------------------------------------------------------------------

// We do this for 2 reasons:
// - Improve code size by only spelling out the property names in one place
// - Avoid megamorphic call sites by avoiding direct DOM accesses in the
//   reconciler main functions.
//
// We could directly store references to the Node.protoype functions too and
// avoid chasing the prototype chains -
// However that would break many DOM crimes we want to do, for example for the
// portal or the transition component

const setTimeout = globalThis.setTimeout;
const clearTimeout = globalThis.clearTimeout;
const createElementNS = (ns, name) => document().createElementNS(ns, name);
const createTextNode = (data) => document().createTextNode(data);
const createDocumentFragment = () => document().createDocumentFragment();
const insertBefore = (parent, node, reference) => parent.insertBefore(node, reference);
const moveBefore = SUPPORTS_MOVE_BEFORE
  ? (parent, node, reference) => parent.moveBefore(node, reference)
  : insertBefore;
const removeChild  = (parent, child) => parent.removeChild(child);
const getAttribute = (node, name) => node.getAttribute(name);
const setAttribute = (node, name, value) => node.setAttribute(name, value);
const removeAttribute = (node, name) => node.removeAttribute(name);
const addEventListener = (node, name, handler, options) => node.addEventListener(name, handler, options);
const removeEventListener = (node, name, handler) => node.removeEventListener(name, handler);
const setInnerHtml = (node, innerHtml) => (node.innerHTML = innerHtml);
const setData = (node, data) => (node.data = data);

// METADATA / STATEFUL TREE ----------------------------------------------------

// We store some additional data for every node that we create.
// We store that "metadata" using a symbol on each DOM node.

const meta = Symbol('lustre');

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

    // data for the event handlers and attached throttlers and debouncers.
    this.handlers = new Map();
    this.throttles = new Map();
    this.debouncers = new Map();
  }

  get parentNode() {
    return this.kind === fragment_kind ? this.node.parentNode : this.node;
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
  let path = '';

  for(let current = node[meta]; current.parent; current = current.parent) {
    if (current.key) {
      path = `${separator_element}${current.key}${path}`;
    } else {
      const index = current.parent.children.indexOf(current);
      path = `${separator_element}${index}${path}`;
    }
  }

  // remove the leading separator.
  return path.slice(1);
}

// RECONCILER ------------------------------------------------------------------

export class Reconciler {
  #root = null;

  #dispatch = () => {};

  #useServerEvents = false;
  #exposeKeys = false;

  constructor(
    root,
    dispatch,
    { useServerEvents = false, exposeKeys = false } = {},
  ) {
    this.#root = root;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
    this.#exposeKeys = exposeKeys;
  }

  mount(vdom) {
    insertMetadataChild(element_kind, null, this.#root, 0, null);
    this.#insertChild(this.#root, null, this.#root[meta], 0, vdom);
  }

  push(patch) {
    this.#stack.push({ node: this.#root[meta], patch: patch });
    this.#reconcile();
  }

  // PATCHING ------------------------------------------------------------------

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

      iterate(childPatches, childPatch => {
        const child = childNodes[childPatch.index|0];
        this.#stack.push({ node: child, patch: childPatch });
      });
    }
  }

  #patch(node, change) {
    switch (change.kind) {
      case replace_text_kind:
        this.#replaceText(node, change);
        break;

      case replace_inner_html_kind:
        this.#replaceInnerHtml(node, change);
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
    const fragment = createDocumentFragment();
    const beforeEl = this.#getReference(parent, before);

    this.#insertChildren(fragment, null, parent, before|0, children);

    insertBefore(parent.parentNode, fragment, beforeEl);
  }

  #replace(parent, { index, with: child }) {
    this.#removeChildren(parent, index|0, 1);
    const beforeEl = this.#getReference(parent, index);
    this.#insertChild(parent.parentNode, beforeEl, parent, index|0, child);
  }

  #getReference(node, index) {
    index = index|0;
    const { children } = node;
    const childCount = children.length;

    if (index < childCount) {
      return children[index].node;
    }

    let lastChild = children[childCount-1] ?? node;
    // unwrap the last child as long as we point to a fragment.
    // otherwise, the fragments next sibling would be the first child of the
    // fragment, not the first element after it.
    while (lastChild.kind === fragment_kind && lastChild.children.length) {
      lastChild = lastChild.children[lastChild.children.length-1];
    }

    return lastChild.node.nextSibling;
  }

  #move(parent, { key, before }) {
    before = before|0;

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

    const { kind, node, children: prevChildren } = prev;
    // prev now is the same as `next` inside the loop, and points to the element
    // we found that matches the key! all that's left is to move it before `beforeEl`.
    moveBefore(parentNode, node, beforeEl);

    // prev might be a fragment in which case we need do move all its child dom nodes too
    if (kind === fragment_kind) {
      this.#moveChildren(parentNode, prevChildren, beforeEl);
    }
  }

  #moveChildren(domParent, children, beforeEl) {
    for (let i = 0; i < children.length; ++i) {
      const { kind, node, children: nestedChildren } = children[i];
      moveBefore(domParent, node, beforeEl);

      if (kind === fragment_kind) {
        this.#moveChildren(domParent, nestedChildren, beforeEl);
      }
    }
  }

  #remove(parent, { index }) {
    this.#removeChildren(parent, index, 1);
  }

  #removeChildren(parent, index, count) {
    const { children, parentNode } = parent;
    const deleted = children.splice(index, count);

    for (let i = 0; i < deleted.length; ++i) {
      const { kind, node, children: nestedChildren } = deleted[i];

      removeChild(parentNode, node);
      this.#removeDebouncers(deleted[i]);

      if (kind === fragment_kind) {
        deleted.push(...nestedChildren);
      }
    }
  }

  #removeDebouncers(node) {
    const { debouncers, children } = node
    for (const { timeout } of debouncers.values()) {
      if (timeout) {
        clearTimeout(timeout);
      }
    }

    debouncers.clear();

    iterate(children, child => this.#removeDebouncers(child));
  }

  #update({ node, handlers, throttles, debouncers }, { added, removed }) {
    iterate(removed, ({ name }) => {
      if (handlers.delete(name)) {
        removeEventListener(node, name, handleEvent);
        this.#updateDebounceThrottle(throttles, name, 0);
        this.#updateDebounceThrottle(debouncers, name, 0);
      } else {
        removeAttribute(node, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });

    iterate(added, (attribute) => this.#createAttribute(node, attribute));
  }

  #replaceText({ node }, { content }) {
    setData(node, content ?? '');
  }

  #replaceInnerHtml({ node }, { inner_html }) {
    setInnerHtml(node, inner_html ?? '');
  }


  // INSERT --------------------------------------------------------------------


  #insertChildren(domParent, beforeEl, metaParent, index, children) {
    iterate(children, (child) =>
      this.#insertChild(domParent, beforeEl, metaParent, index++, child));
  }


  #insertChild(domParent, beforeEl, metaParent, index, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        this.#insertChildren(node, null, node[meta], 0, vnode.children);
        insertBefore(domParent, node, beforeEl);
      }; break;

      case text_kind: {
        const node = this.#createTextNode(metaParent, index, vnode);
        insertBefore(domParent, node, beforeEl);
      }; break;

      case fragment_kind: {
        const head = this.#createTextNode(metaParent, index, vnode);
        insertBefore(domParent, head, beforeEl);
        this.#insertChildren(domParent, beforeEl, head[meta], 0, vnode.children);
      }; break;

      case unsafe_inner_html_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        this.#replaceInnerHtml(node, vnode);
        insertBefore(domParent, node, beforeEl);
      }; break;
    }
  }

  #createElement(parent, index, { kind, key, tag, namespace, attributes }) {
    const node = createElementNS(namespace || NAMESPACE_HTML, tag);
    insertMetadataChild(kind, parent, node, index, key);

    if (this.#exposeKeys && key) {
      setAttribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));

    return node;
  }

  #createTextNode(parent, index, { kind, key, content }) {
    const node = createTextNode(content ?? '');
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
          node.defaultValue = valueOrDefault;
          return;
        }

        if (valueOrDefault !== getAttribute(node, name)) {
          setAttribute(node, name, valueOrDefault);
        }

        SYNCED_ATTRIBUTES[name]?.added?.(node, valueOrDefault);

        break;
      }

      case property_kind:
        node[name] = value;
        break;

      case event_kind: {
        if (handlers.has(name)) {
          // we re-attach an event listener on every change in case we need
          // to change the options we pass.
          removeEventListener(node, name, handleEvent);
        }

        const passive = prevent.kind === never_kind;
        addEventListener(node, name, handleEvent, { passive });

        this.#updateDebounceThrottle(throttles, name, throttleDelay);
        this.#updateDebounceThrottle(debouncers, name, debounceDelay);

        handlers.set(name, (event) => this.#handleEvent(attribute, event));

        break;
      }
    }
  }

  #updateDebounceThrottle(map, name, delay) {
    const debounceOrThrottle = map.get(name)

    if (delay > 0) {
      if (debounceOrThrottle) {
        debounceOrThrottle.delay = delay;
      } else {
        map.set(name, { delay });
      }
    } else if (debounceOrThrottle) {
      const { timeout } = debounceOrThrottle
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
      immediate
    } = attribute;

    if (prevent.kind === always_kind) event.preventDefault();
    if (stop.kind === always_kind) event.stopPropagation();

    if (type === 'submit') {
      event.detail ??= {};
      event.detail.formData = [...new FormData(event.target).entries()];
    }

    const data = this.#useServerEvents
      ? createServerEvent(event, include ?? [])
      : event;

    const throttle = throttles.get(type);
    if (throttle) {
      const now = Date.now();
      const last = throttle.last || 0;

      if (now > last + throttle.delay) {
        throttle.last = now;
        throttle.lastEvent = event;
        this.#dispatch(data, path, type, immediate);
      }
    }

    const debounce = debouncers.get(type);
    if (debounce) {
      clearTimeout(debounce.timeout);

      debounce.timeout = setTimeout(() => {
        if (event === throttles.get(type)?.lastEvent) return;
        this.#dispatch(data, path, type, immediate);
      }, debounce.delay);
    }

    if (!throttle && !debounce) {
      this.#dispatch(data, path, type, immediate);
    }
  }
}

// UTILS -----------------------------------------------------------------------

/** Our reconciler is written in such a way that it can work without modification
 *  both in typical client-side Lustre apps like SPAs and client components, but
 *  also in the server component runtime.
 *
 *  This is notable because the typical client runtimes are working directly with
 *  Gleam values, but the server component runtime is working with deserialised
 *  JSON.
 *
 *  The most immediate discrepancy is that Gleam uses linked lists but of course
 *  these are serialised as arrays in JSON. This function lets us iterate over
 *  both kinds of collection without dropping into Gleam's slow list iterator.
 *
 */
const iterate = (list, callback) => {
  if (Array.isArray(list)) {
    for (let i = 0; i < list.length; i++) {
      callback(list[i]);
    }
  } else if (list) {
    for (list; list.head; list = list.tail) {
      callback(list.head);
    }
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
 *
 */
const handleEvent = (event) => {
  const { currentTarget, type } = event;
  const handler = currentTarget[meta].handlers.get(type);
  handler(event);
};

/** Server components send the event data as a JSON object over the network to
 *  the server component runtime. Out of the box this would effectively do nothing
 *  because the event object is not serialisable: almost every property is
 *  non-enumerable.
 *
 *  To counter this, users can provide a list of properties they'd like the runtime
 *  to include in the event data. Each property is a dot-separated string that
 *  represents the traversal path to the desired property.
 *
 */
const createServerEvent = (event, include = []) => {
  const data = {};

  // It's overwhelmingly likely that if someone is listening for input or change
  // events that they're interested in the value of the input. Regardless of
  // whether they remember to include it in the event, we'll include it for them.
  if (event.type === "input" || event.type === "change") {
    include.push("target.value");
  }

  // We have non-standard handling of the submit event in Lustre. We automatically
  // extract the form fields into a special `formData` property on the event's
  // `detail` field. This is because we need a way for normal Lustre apps to get
  // at this data without needing to go through FFI to construct a `new FormData`
  // themselves – this would be impossible for server components!
  //
  // If the user is handling a submit event they almost definitely want to know
  // about the form's data, so we always include it.
  if (event.type === "submit") {
    include.push("detail.formData");
  }

  for (const property of include) {
    const path = property.split(".");

    for (let i = 0, input = event, output = data; i < path.length; i++) {
      // If we're at the end of the path we just do a straight assignment. If the
      // value at this path is an object it's likely the properties are still
      // unenumerable, but that's what they asked for!
      if (i === path.length - 1) {
        output[path[i]] = input[path[i]];
        break;
      }

      // For every step, we make sure to insert an empty object if we haven't
      // already visited this particular key in the path.
      output = output[path[i]] ??= {};
      input = input[path[i]];
    }
  }

  return data;
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
      queueMicrotask(() => node.focus?.());
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
