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

import { separator_index, separator_key } from "./path.mjs";

import {
  document,
  ELEMENT_NODE,
  TEXT_NODE,
  DOCUMENT_FRAGMENT_NODE,
  SUPPORTS_MOVE_BEFORE,
  NAMESPACE_HTML,
} from "../internals/constants.ffi.mjs";

//

export class Reconciler {
  offset = 0;

  #root = null;
  #dispatch = () => {};

  #useServerEvents = false;

  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
  }

  mount(vdom) {
    appendChild(this.#root, this.#createElement(vdom));
  }

  #stack = [];

  push(patch) {
    const offset = this.offset;
    if (offset) {
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
          case move_kind:
            change.before = (change.before | 0) + offset;
            break;

          case remove_kind:
          case replace_kind:
            change.from = (change.from | 0) + offset;
            break;
        }
      });

      iterate(patch.children, (child) => {
        child.index = (child.index | 0) + offset;
      });
    }

    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }

  // PATCHING ------------------------------------------------------------------

  #reconcile() {
    const self = this;

    while (self.#stack.length) {
      const { node, patch } = self.#stack.pop();

      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            self.#insert(node, change.children, change.before);
            break;

          case move_kind:
            self.#move(node, change.key, change.before, change.count);
            break;

          case remove_key_kind:
            self.#removeKey(node, change.key, change.count);
            break;

          case remove_kind:
            self.#remove(node, change.from, change.count);
            break;

          case replace_kind:
            self.#replace(node, change.from, change.count, change.with);
            break;

          case replace_text_kind:
            self.#replaceText(node, change.content);
            break;

          case replace_inner_html_kind:
            self.#replaceInnerHtml(node, change.inner_html);
            break;

          case update_kind:
            self.#update(node, change.added, change.removed);
            break;
        }
      });

      if (patch.removed) {
        self.#remove(
          node,
          node.childNodes.length - patch.removed,
          patch.removed,
        );
      }

      let lastIndex = -1;
      let lastChild = null;
      iterate(patch.children, (child) => {
        const index = child.index|0;
        
        const next = lastChild && lastIndex - index === 1
          ? lastChild.previousSibling
          : childAt(node, index);

        self.#stack.push({ node: next, patch: child });

        lastChild = next;
        lastIndex = index;
      });
    }
  }

  // CHANGES -------------------------------------------------------------------

  #insert(node, children, before) {
    const fragment = createDocumentFragment();

    iterate(children, (child) => {
      const el = this.#createElement(child);

      addKeyedChild(node, el);
      appendChild(fragment, el);
    });

    insertBefore(node, fragment, childAt(node, before));
  }

  #move(node, key, before, count) {
    let el = getKeyedChild(node, key);
    const beforeEl = childAt(node, before);
    for (let i = 0; i < count && el !== null; ++i) {
      const next = el.nextSibling;
      if (SUPPORTS_MOVE_BEFORE) {
        node.moveBefore(el, beforeEl);
      } else {
        insertBefore(node, el, beforeEl);
      }
      el = next;
    }
  }

  #removeKey(node, key, count) {
    this.#removeFromChild(node, getKeyedChild(node, key), count);
  }

  #remove(node, from, count) {
    this.#removeFromChild(node, childAt(node, from), count);
  }

  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;

      const key = child[meta].key;

      if (key) {
        parent[meta].keyedChildren.delete(key);
      }

      for (const [_, { timeout }] of child[meta].debouncers) {
        clearTimeout(timeout);
      }

      parent.removeChild(child);
      child = next;
    }
  }

  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);

    const el = this.#createElement(child);

    addKeyedChild(parent, el);
    insertBefore(parent, el, childAt(parent, from));
  }

  #replaceText(node, content) {
    node.data = content ?? "";
  }

  #replaceInnerHtml(node, inner_html) {
    node.innerHTML = inner_html ?? "";
  }

  #update(node, added, removed) {
    iterate(removed, (attribute) => {
      const name = attribute.name;

      if (node[meta].handlers.has(name)) {
        node.removeEventListener(name, handleEvent);
        node[meta].handlers.delete(name);

        if (node[meta].throttles.has(name)) {
          node[meta].throttles.delete(name);
        }

        if (node[meta].debouncers.has(name)) {
          clearTimeout(node[meta].debouncers.get(name).timeout);
          node[meta].debouncers.delete(name);
        }
      } else {
        node.removeAttribute(name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });

    iterate(added, (attribute) => {
      this.#createAttribute(node, attribute);
    });
  }

  // CONSTRUCTORS --------------------------------------------------------------

  #createElement(vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = createElement(vnode);
        this.#createAttributes(node, vnode);

        this.#insert(node, vnode.children, 0);

        return node;
      }

      case text_kind: {
        const node = createTextNode(vnode.content);

        initialiseMetadata(node, vnode.key);

        return node;
      }

      case fragment_kind: {
        const node = createDocumentFragment();
        const head = createTextNode();

        initialiseMetadata(head, vnode.key);
        appendChild(node, head);

        iterate(vnode.children, (child) => {
          appendChild(node, this.#createElement(child));
        });

        return node;
      }

      case unsafe_inner_html_kind: {
        const node = createElement(vnode);
        this.#createAttributes(node, vnode);

        this.#replaceInnerHtml(node, vnode.inner_html);

        return node;
      }
    }
  }

  #createAttributes(node, { attributes }) {
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));
  }

  #createAttribute(node, attribute) {
    const { debouncers, handlers, throttles } = node[meta];

    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      stop_propagation: stop,
      immediate,
      include,
      debounce: debounceDelay,
      throttle: throttleDelay
    } = attribute
    

    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? ""
        if (name === "virtual:defaultValue") {
          node.defaultValue = valueOrDefault;
          return;
        }

        if (valueOrDefault !== node.getAttribute(name)) {
          node.setAttribute(name, valueOrDefault);
        }

        SYNCED_ATTRIBUTES[name]?.added?.(node, value);

        break;
      }

      case property_kind:
        node[name] = value;
        break;

      case event_kind: {
        if (!handlers.has(name)) {
          node.addEventListener(name, handleEvent, {
            passive: !attribute.prevent_default,
          });
        }

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

        handlers.set(name, (event) => {
          if (prevent) event.preventDefault();
          if (stop) event.stopPropagation();

          const type = event.type;
          let path = "";
          let pathNode = event.currentTarget;

          while (pathNode !== this.#root) {
            const key = pathNode[meta].key;
            const parent = pathNode.parentNode;
            if (key) {
              path = `${separator_key}${key}${path}`;
            } else {
              const siblings = parent.childNodes;
              let index = [].indexOf.call(siblings, pathNode);
              if (parent === this.#root) {
                index -= this.offset;
              }

              path = `${separator_index}${index}${path}`;
            }

            pathNode = parent;
          }

          // remove the leading separator
          path = path.slice(1);

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
            } else {
              event.preventDefault();
            }
          }

          const debounce = debouncers.get(type);
          if (debounce) {
            clearTimeout(debounce.timeout);

            debounce.timeout = setTimeout(() => {
              if (event === throttles.get(type)?.lastEvent) return;
              this.#dispatch(data, path, type, immediate);
            }, debounce.delay);
          } else {
            this.#dispatch(data, path, type, immediate);
          }
        });

        break;
      }
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
    for (list; list.tail; list = list.tail) {
      callback(list.head);
    }
  }
};

const appendChild = (node, child) => node.appendChild(child);
const insertBefore = (parent, node, referenceNode) =>
  parent.insertBefore(node, referenceNode ?? null);

const createElement = ({ key, tag, namespace }) => {
  const node = document.createElementNS(namespace || NAMESPACE_HTML, tag);
  initialiseMetadata(node, key);
  return node;
};

const createTextNode = (text) => document.createTextNode(text ?? "");
const createDocumentFragment = () => document.createDocumentFragment();
const childAt = (node, at) => node.childNodes[at | 0];

// METADATA --------------------------------------------------------------------

const meta = Symbol("lustre");

export const initialiseMetadata = (node, key = "") => {
  switch (node.nodeType) {
    case ELEMENT_NODE:
    case DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        keyedChildren: new Map(),
        handlers: new Map(),
        throttles: new Map(),
        debouncers: new Map(),
      };
      break;

    case TEXT_NODE:
      node[meta] = { key, debouncers: new Map() };
      break;
  }
};

export const isLustreNode = (node) => {
  while (node) {
    if (node[meta]) return true;
    // we need to also check every parent because we might be inside of an
    // unsafe_raw_html element, which does not have metadata attached.
    node = node.parentNode;
  }

  return false;
};

const addKeyedChild = (node, child) => {
  if (child.nodeType === DOCUMENT_FRAGMENT_NODE) {
    for (child = child.firstChild; child; child = child.nextSibling) {
      addKeyedChild(node, child);
    }

    return;
  }

  const key = child[meta].key;

  if (key) {
    node[meta].keyedChildren.set(key, new WeakRef(child));
  }
};

const getKeyedChild = (node, key) => node[meta].keyedChildren.get(key).deref();

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
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);

  if (event.type === "submit") {
    event.detail ??= {};
    event.detail.formData = [...new FormData(event.target).entries()];
  }

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
