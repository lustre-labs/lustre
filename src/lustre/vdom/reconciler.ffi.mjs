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
  remove_key_kind,
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

const isArray = Array.isArray;
const setTimeout = globalThis.setTimeout;
const clearTimeout = globalThis.clearTimeout;
const createElementNS = (ns, name) => document().createElementNS(ns, name);
const createTextNode = (data) => document().createTextNode(data);
const createDocumentFragment = () => document().createDocumentFragment();
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
export const insertMetadataChild = (parent, node, kind, key = null) => {
  switch (kind) {
    case element_kind:
    case unsafe_inner_html_kind:
      node[meta] = {
        kind,

        // store the key of the element to be able to reconstruct the path
        // once an event gets dispatched.
        key,

        // parent points to the _metadata_ parent node.
        parent,

        // navigating the DOM tree using our own navigations to improve stability
        // and support fragments.
        children: [],
        keyedChildren: new Map(),

        // data for the event handlers and attached throttlers and debounccers.
        handlers: new Map(),
        throttles: new Map(),
        debouncers: new Map(),
      };
      break;

    case fragment_kind:
      node[meta] = {
        kind,
        key,
        parent,
        children: [],
        keyedChildren: new Map(),
      };
      break;

    case text_kind:
      node[meta] = {
        kind,
        key,
        parent
      };
      break;
  }

  if (!parent) {
    return;
  }

  if (key) {
    parent.keyedChildren.set(key, node);
  }

  parent.children.push(node);
};

const getPath = (node) => {
  let path = '';

  for(let current = node[meta]; current.parent; current = current.parent) {
    if (current.key) {
      path = `${separator_element}${current.key}${path}`;
    } else {
      const index = current.parent.children.findIndex(child => child[meta] === current);
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

  #changes = {
    [replace_text_kind]: this.#replaceText,
    [replace_inner_html_kind]: this.#replaceInnerHtml,
    [update_kind]: this.#update,
    [move_kind]: this.#move,
    [remove_key_kind]: this.#removeKey,
    [replace_kind]: this.#replace,
    [insert_kind]: this.#insert,
  };

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
    insertMetadataChild(null, this.#root, fragment_kind);
    this.#createAndInsertChild(this.#root, this.#root[meta], vdom);
  }

  push(patch) {
    this.#reconcile(this.#root, this.#root, patch);
  }

  // PATCHING ------------------------------------------------------------------

  #reconcile(domNode, realNode, patch) {
    // console.log('reconcile', domNode, realNode, patch);
    const metaNode = realNode[meta];
    const { children } = metaNode;

    iterate(patch.changes, (change) =>
      this.#changes[change.kind].call(this, domNode, metaNode, change));

    iterate(patch.children, childPatch => {
        const child = children[childPatch.index|0];
        const childDomNode = child[meta].kind === fragment_kind
          ? domNode
          : child;

        this.#reconcile(childDomNode, child, childPatch)
    });
  }

  // CHANGES -------------------------------------------------------------------

  #insert(domParent, metaParent, { children, before }) {
    const beforeEl = this.#getReference(metaParent, before);

    iterate(children, (child) =>
      this.#createAndInsertChild(domParent, metaParent, child, beforeEl));
  }

  #replace(domParent, metaParent, { index, with: child }) {
    this.#removeChildren(domParent, metaParent, index|0, 1);
    const beforeEl = this.#getReference(metaParent, index);
    this.#createAndInsertChild(domParent, metaParent, child, beforeEl);
  }

  #move(domParent, metaParent, { key, before }) {
    const { children, keyedChildren } = metaParent;
    const first = keyedChildren.get(key);
    // TODO: KeyedIndex
    const index = children.indexOf(first);
    if (index < 0) {
      return;
    }

    const beforeEl = this.#getReference(metaParent, index);

    // TODO: Speed - maybe a custom-built move loop is faster
    const toMove = children.splice(index, count);

    // TODO: bring back moveBefore
    for (
      let i = 0, beforeIndex = before|0;
      i < toMove.length;
      ++i, ++beforeIndex
    ) {
      insertBefore(domParent, toMove[i], beforeEl);
    }

    children.splice(before|0, 0, ...toMove);
  }

  #getReference(metaParent, index) {
    const { children } = metaParent;

    const reference =
      children[index|0]
      ?? children[children.length-1]?.nextSibling
      ?? null;

    return reference;
  }

  #removeKey(domParent, metaParent, { key }) {
    const { children, keyedChildren } = metaParent;
    const child = keyedChildren.get(key);
    // TODO: KeyedIndex
    const index = children.indexOf(child);

    if (index >= 0) {
      this.#removeChildren(domParent, metaParent, index, 1);
    }
  }

  #removeChildren(domParent, metaParent, index, count) {
    const { children, keyedChildren } = metaParent;
    const deleted = children.splice(index, count);

    for (let i = 0; i < deleted.length; ++i) {
      const child = deleted[i];
      const childMeta = child[meta];
      const { kind, key, debouncers, children: childrenChildren } = childMeta;

      if (key) {
        keyedChildren.delete(key);
      }

      if (debouncers) {
        for (const { timeout } of debouncers.values()) {
          clearTimeout(timeout);
        }
      }

      removeChild(domParent, child);

      if (kind === fragment_kind) {
        deleted.push(...childrenChildren);
      }
    }
  }

  #update(domNode, metaNode, { added, removed }) {
    const { handlers, throttles, debouncers } = metaNode;

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
        removeAttribute(domNode, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });

    iterate(added, (attribute) => this.#createAttribute(domNode, attribute));
  }

  #replaceText(domNode, metaNode, { content }) {
    setData(domNode, content ?? '');
  }

  #replaceInnerHtml(domNode, metaNode, { inner_html }) {
    setInnerHtml(domNode, inner_html ?? '');
  }


  // CONSTRUCTORS --------------------------------------------------------------


  #createAndInsertChild(domParent, metaParent, vnode, beforeEl = null) {
    // console.log('createAndInsertChild', domParent, metaParent, vnode, beforeEl)
    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(metaParent, vnode);

        iterate(vnode.children, child =>
          this.#createAndInsertChild(node, node[meta], child));

        insertBefore(domParent, node, beforeEl);
      }; break;

      case text_kind: {
        const node = this.#createTextNode(metaParent, vnode);
        insertBefore(domParent, node, beforeEl);
      }; break;

      case fragment_kind: {
        const fragment = createDocumentFragment();
        const head = this.#createTextNode(metaParent, vnode);
        insertBefore(fragment, head, null);

        iterate(vnode.children, child =>
          this.#createAndInsertChild(fragment, head[meta], child));

        insertBefore(domParent, fragment, beforeEl);
      }; break;

      case unsafe_inner_html_kind: {
        const node = this.#createElement(metaParent, vnode);
        this.#replaceInnerHtml(node, vnode);
        insertBefore(domParent, node, beforeEl);
      }; break;
    }
  }

  #createElement(metaParent, { kind, key, tag, namespace, attributes }) {
    const node = createElementNS(namespace || NAMESPACE_HTML, tag);
    insertMetadataChild(metaParent, node, kind, key);

    if (this.#exposeKeys && key) {
      setAttribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));

    return node;
  }

  #createTextNode(metaParent, { kind, key, content }) {
    const node = createTextNode(content ?? '');
    insertMetadataChild(metaParent, node, kind, key);

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
    const { debouncers, throttles } = currentTarget[meta];
    const path = getPath(currentTarget);

    console.log(type, path);

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
