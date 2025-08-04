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
  remove_key_kind,
  replace_kind,
  replace_inner_html_kind,
  replace_text_kind,
  update_kind,
} from "./patch.mjs";

import { separator_element } from "./path.mjs";

import {
  document,
  ELEMENT_NODE,
  TEXT_NODE,
  DOCUMENT_FRAGMENT_NODE,
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

const isArray = Array.isArray;
const setTimeout = globalThis.setTimeout;
const clearTimeout = globalThis.clearTimeout;
const createElementNS = (ns, name) => document().createElementNS(ns, name);
const createTextNode = (data) => document().createTextNode(data);
const insertBefore = (parent, node, reference) => parent.insertBefore(node, reference);
const moveBefore = (parent, node, reference) => parent.moveBefore(node, reference);
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

// A node is a Lustre node if it has this metadata.
export const isLustreNode = (node) => !!node[meta];

//
export const insertMetadataChild = (parent, node, index = 0, key = "") => {
  const segment = `${key || index}`;

  switch (node.nodeType) {
    case ELEMENT_NODE:
    case DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        // store the key of the element to be able to reconstruct the path
        // once an event gets dispatched.
        key,
        path: segment,

        // navigating the DOM tree using our own navigations to improve stability
        // and support fragments.
        parent,
        children: [],
        keyedChildren: new Map(),

        // data for the event handlers and attached throttlers and debounccers.
        handlers: new Map(),
        throttles: new Map(),
        debouncers: new Map(),
      };
      break;

    case TEXT_NODE:
      node[meta] = {
        key,
        parent
      };
      break;
  }

  if (!parent) {
    return;
  }

  const parentMeta = parent[meta];
  if (parentMeta.path) {
    node[meta].path = `${parentMeta.path}${separator_element}${segment}`;
  }

  if (key) {
    parentMeta.keyedChildren.set(key, node);
  }

  parentMeta.children.splice(index, 0, node);
};

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
    insertMetadataChild(null, this.#root, '', '');
    this.#createAndInsertChild(this.#root, 0, vdom);
  }

  #stack = [];

  push(patch) {
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }

  // PATCHING ------------------------------------------------------------------

  #reconcile() {
    const self = this;
    const stack = this.#stack;

    while (stack.length) {
      const { node, patch } = stack.pop();

      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case update_kind:
            self.#update(node, change);
            break;

          case replace_text_kind:
            self.#replaceText(node, change);
            break;

          case replace_kind:
            self.#replace(node, change);
            break;

          case insert_kind:
            self.#insert(node, change);
            break;

          case remove_kind:
            self.#remove(node, change);
            break;

          case move_kind:
            self.#move(node, change);
            break;

          case remove_key_kind:
            self.#removeKey(node, change);
            break;

          case replace_inner_html_kind:
            self.#replaceInnerHtml(node, change);
            break;

        }
      });

      if (patch.removed) {
        this.#removeChildren(node, node[meta].children.length - patch.removed, patch.removed);
      }

      iterate(patch.children, (child) =>
        stack.push({ node: childAt(node, child.index), patch: child }));
    }
  }

  // CHANGES -------------------------------------------------------------------

  #insert(parent, { children, before }) {
    let childIndex = before | 0;
    iterate(children, (child) => {
      this.#createAndInsertChild(parent, childIndex, child);
      childIndex += advance(child);
    });
  }

  #replace(parent, { from, count, with: child }) {
    this.#removeChildren(parent, from|0, count);
    this.#createAndInsertChild(parent, from|0, child);
  }

  #move(node, { key, before, count }) {
    const { children, keyedChildren } = node[meta];
    const first = keyedChildren.get(key);
    // TODO: KeyedIndex
    const index = children.indexOf(first);
    if (index < 0) {
      return;
    }

    const beforeEl = children[before|0];

    // TODO: Speed - maybe a custom-built move loop is faster
    const toMove = children.splice(index, count);

    // TODO: bring back moveBefore
    for (
      let i = 0, beforeIndex = before|0;
      i < toMove.length;
      ++i, ++beforeIndex
    ) {
      insertBefore(node, toMove[i], beforeEl);
    }

    children.splice(before|0, 0, ...toMove);
  }

  #removeKey(node, { key, count }) {
    const { children, keyedChildren } = node[meta];
    const child = keyedChildren.get(key);
    // TODO: KeyedIndex
    const index = children.indexOf(child);

    if (index >= 0) {
      this.#removeChildren(node, index, count);
    }
  }

  #remove(node, { from, count }) {
    this.#removeChildren(node, from|0, count);
  }

  #removeChildren(parent, index, count) {
    const parentMeta = parent[meta];
    const deleted = parentMeta.children.splice(index, count);

    for (let i = 0; i < deleted.length; ++i) {
      const child = deleted[i];
      const { key, debouncers } = child[meta];

      if (key) {
        parentMeta.keyedChildren.delete(key);
      }

      if (debouncers) {
        for (const { timeout } of debouncers.values()) {
          clearTimeout(timeout);
        }
      }

      removeChild(parent, child);
    }
  }

  #update(node, { added, removed }) {
    const { handlers, throttles, debouncers } = node[meta];

    iterate(removed, ({ name }) => {
      if (handlers.has(name)) {
        removeEventListener(node, name, handleEvent);
        handlers.delete(name);

        if (throttles.has(name)) {
          throttles.delete(name);
        }

        if (debouncers.has(name)) {
          clearTimeout(debouncers.get(name).timeout);
          debouncers.delete(name);
        }
      } else {
        removeAttribute(node, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });

    iterate(added, (attribute) => this.#createAttribute(node, attribute));
  }

  #replaceText(node, { content }) {
    setData(node, content ?? '');
  }

  #replaceInnerHtml(node, { inner_html }) {
    setInnerHtml(node, inner_html ?? '');
  }


  // CONSTRUCTORS --------------------------------------------------------------

  #createAndInsertChild(parent, index, vnode) {
    const beforeEl = childAt(parent, index);

    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(parent, index, vnode);
        this.#insert(node, vnode);
        insertBefore(parent, node, beforeEl);
      }; break;

      case text_kind: {
        this.#createTextNode(parent, index, vnode);
      }; break;

      case fragment_kind: {
        this.#createTextNode(parent, index, vnode);
        this.#insert(parent, { children: vnode.children, before: index + 1 });
      }; break;

      case unsafe_inner_html_kind: {
        const node = this.#createElement(parent, index, vnode);
        this.#replaceInnerHtml(node, vnode);
        insertBefore(parent, node, beforeEl);
      }; break;
    }
  }

  #createElement(parent, index, { key, tag, namespace, attributes }) {
    const node = createElementNS(namespace || NAMESPACE_HTML, tag);
    insertMetadataChild(parent, node, index, key);

    if (this.#exposeKeys && key) {
      setAttribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));

    return node;
  }

  #createTextNode(parent, index, { key, content }) {
    const beforeEl = childAt(parent, index);
    const node = createTextNode(content ?? '');
    insertMetadataChild(parent, node, index, key);
    insertBefore(parent, node, beforeEl);
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

        if (throttleDelay > 0) {
          const throttle = throttles.get(name) ?? {};

          throttle.delay = throttleDelay;
          throttles.set(name, throttle);
        } else {
          throttles.delete(name);
        }

        if (debounceDelay > 0) {
          const debounce = debouncers.get(name) ?? {};

          debounce.delay = debounceDelay;
          debouncers.set(name, debounce);
        } else {
          clearTimeout(debouncers.get(name)?.timeout);
          debouncers.delete(name);
        }

        handlers.set(name, (event) => this.#handleEvent(attribute, event));

        break;
      }
    }
  }

  #handleEvent(attribute, event) {
    const { currentTarget, type } = event;
    const { path, debouncers, throttles } = currentTarget[meta];

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
  if (isArray(list)) {
    for (let i = 0; i < list.length; i++) {
      callback(list[i]);
    }
  } else if (list) {
    for (list; list.head; list = list.tail) {
      callback(list.head);
    }
  }
};


// METADATA --------------------------------------------------------------------

const keyedChild = (node, key) => node[meta].keyedChildren.get(key);
const childAt = (node, at) => node[meta].children[at | 0];

const advance = (vnode) => {
  return 1 + (vnode.children_count|0);
}

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
