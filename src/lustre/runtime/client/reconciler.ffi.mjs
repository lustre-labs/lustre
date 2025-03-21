// IMPORTS ---------------------------------------------------------------------

import {
  element_kind,
  text_kind,
  fragment_kind,
  unsafe_inner_html_kind,
} from "../../vdom/node.mjs";

import {
  attribute_kind,
  property_kind,
  event_kind,
} from "../../vdom/attribute.mjs";

import {
  insert_kind,
  move_kind,
  remove_kind,
  remove_key_kind,
  replace_kind,
  replace_inner_html_kind,
  replace_text_kind,
  update_kind,
} from "../../vdom/patch.mjs";

//

const SUPPORTS_MOVE_BEFORE = !!HTMLElement.prototype.moveBefore;

export class Reconciler {
  #root = null;
  #dispatch = () => {};

  #useServerEvents = false;

  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
  }

  mount(vdom) {
    this.#root.appendChild(this.#createElement(vdom));
  }

  #stack = [];

  push(patch, offset = 0) {
    if (offset) {
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
          case move_kind:
            change.before = (change.before|0) + offset;
            break;

          case remove_kind:
          case replace_kind:
            change.from = (change.from|0) + offset;
            break;
        }
      });

      iterate(patch.children, (child) => {
        child.index = (child.index|0) + offset;
      });
    }

    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }

  // PATCHING ------------------------------------------------------------------

  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();

      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            this.#insert(node, change.children, change.before);
            break;

          case move_kind:
            this.#move(node, change.key, change.before, change.count);
            break;

          case remove_key_kind:
            this.#removeKey(node, change.key, change.count);
            break;

          case remove_kind:
            this.#remove(node, change.from, change.count);
            break;

          case replace_kind:
            this.#replace(node, change.from, change.count, change.with);
            break;

          case replace_text_kind:
            this.#replaceText(node, change.content);
            break;

          case replace_inner_html_kind:
            this.#replaceInnerHtml(node, change.inner_html);
            break;

          case update_kind:
            this.#update(node, change.added, change.removed);
            break;
        }
      });

      if (patch.removed) {
        this.#remove(
          node,
          node.childNodes.length - patch.removed,
          patch.removed,
        );
      }

      iterate(patch.children, (child) => {
        // TODO: use linked-list style but skip to indices if distance is great enough?

        this.#stack.push({
          node: node.childNodes[child.index|0],
          patch: child,
        });
      });
    }
  }

  // CHANGES -------------------------------------------------------------------

  #insert(node, children, before) {
    const fragment = document.createDocumentFragment();

    iterate(children, (child) => {
      const el = this.#createElement(child);

      addKeyedChild(node, el);
      fragment.appendChild(el);
    });

    node.insertBefore(fragment, node.childNodes[before|0] ?? null);
  }

  #move(node, key, before, count) {
    let el = node[meta].keyedChildren.get(key).deref();
    const beforeEl = node.childNodes[before] ?? null;
    for (let i = 0; i < count && el !== null; ++i) {
      const next = el.nextSibling;
      if (SUPPORTS_MOVE_BEFORE) {
        node.moveBefore(el, beforeEl);
      } else {
        node.insertBefore(el, beforeEl);
      }
      el = next;
    }
  }

  #removeKey(node, key, count) {
    this.#removeFromChild(
      node,
      node[meta].keyedChildren.get(key).deref(),
      count,
    );
  }

  #remove(node, from, count) {
    this.#removeFromChild(node, node.childNodes[from|0], count);
  }

  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;

      const key = child[meta].key;
      if (key) {
        parent[meta].keyedChildren.delete(key);
      }

      parent.removeChild(child);
      child = next;
    }
  }

  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);

    const el = this.#createElement(child);

    addKeyedChild(parent, el);
    parent.insertBefore(el, parent.childNodes[from|0] ?? null);
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
      } else {
        node.removeAttribute(name);
        ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
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
        const node = vnode.namespace
          ? document.createElementNS(vnode.namespace, vnode.tag)
          : document.createElement(vnode.tag);

        initialiseMetadata(node, vnode.key);

        iterate(vnode.attributes, (attribute) => {
          this.#createAttribute(node, attribute);
        });

        this.#insert(node, vnode.children, 0);

        return node;
      }

      case text_kind: {
        const node = document.createTextNode(vnode.content ?? "");

        initialiseMetadata(node, vnode.key);

        return node;
      }

      case fragment_kind: {
        const node = document.createDocumentFragment();
        const head = document.createTextNode("");

        initialiseMetadata(head, vnode.key);
        node.appendChild(head);

        iterate(vnode.children, (child) => {
          node.appendChild(this.#createElement(child));
        });

        return node;
      }

      case unsafe_inner_html_kind: {
        const node = vnode.namespace
          ? document.createElementNS(vnode.namespace, vnode.tag)
          : document.createElement(vnode.tag);

        initialiseMetadata(node, vnode.key);

        iterate(vnode.attributes, (attribute) => {
          this.#createAttribute(node, attribute);
        });

        this.#replaceInnerHtml(node, vnode.inner_html);

        return node;
      }
    }
  }

  #createAttribute(node, attribute) {
    switch (attribute.kind) {
      case attribute_kind: {
        const name = attribute.name;
        const value = attribute.value ?? "";

        if (value !== node.getAttribute(name)) {
          node.setAttribute(name, value);
        }

        ATTRIBUTE_HOOKS[name]?.added?.(node, value);

        break;
      }

      case property_kind:
        node[attribute.name] = attribute.value;
        break;

      case event_kind: {
        if (!node[meta].handlers.has(attribute.name)) {
          node.addEventListener(attribute.name, handleEvent, {
            passive: !attribute.prevent_default,
          });
        }

        const prevent = attribute.prevent_default;
        const stop = attribute.stop_propagation;
        const immediate = attribute.immediate;

        const include = Array.isArray(attribute.include)
          ? attribute.include
          : [];

        node[meta].handlers.set(attribute.name, (event) => {
          if (prevent) event.preventDefault();
          if (stop) event.stopPropagation();

          let path = [];
          let node = event.currentTarget;

          while (node !== this.#root) {
            const key = node[meta].key;

            if (key) {
              path.push(key);
            } else {
              const index = [].indexOf.call(node.parentNode.childNodes, node);
              path.push(index.toString());
            }

            node = node.parentNode;
          }

          path.reverse();

          const data = this.#useServerEvents
            ? createServerEvent(event, include)
            : event;

          this.#dispatch(data, path, event.type, immediate);
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
function iterate(list, callback) {
  if (Array.isArray(list)) {
    for (let i = 0; i < list.length; i++) {
      callback(list[i]);
    }
  } else if (list) {
    for (list; list.tail; list = list.tail) {
      callback(list.head);
    }
  }
}

// METADATA --------------------------------------------------------------------

const meta = Symbol("metadata");

export function initialiseMetadata(node, key = "") {
  switch (node.nodeType) {
    case Node.ELEMENT_NODE:
    case Node.DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        keyedChildren: new Map(),
        handlers: new Map(),
      };
      break;

    case Node.TEXT_NODE:
      node[meta] = { key };
      break;
  }
}

function addKeyedChild(node, child) {
  if (child.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
    for (child = child.firstChild; child; child = child.nextSibling) {
      addKeyedChild(node, child);
    }

    return;
  }

  const key = child[meta].key;

  if (key) {
    node[meta].keyedChildren.set(key, new WeakRef(child));
  }
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
function handleEvent(event) {
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);

  handler(event);
}

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
function createServerEvent(event, include = []) {
  const data = {};

  // It's overwhelmingly likely that if someone is listening for input or change
  // events that they're interested in the value of the input. Regardless of
  // whether they remember to include it in the event, we'll include it for them.
  if (event.type === "input" || event.type === "change") {
    include.push("target.value");
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
}

// ATTRIBUTE SPECIAL CASES -----------------------------------------------------

const ATTRIBUTE_HOOKS = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),

  autofocus: {
    added(node) {
      node.focus?.();
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

function syncedBooleanAttribute(name) {
  return {
    added(node, _value) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    },
  };
}

function syncedAttribute(name) {
  return {
    added(node, value) {
      node[name] = value;
    },
  };
}
