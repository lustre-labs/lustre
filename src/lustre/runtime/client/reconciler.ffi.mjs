import { Element, Text, Fragment, UnsafeInnerHtml } from "../../vdom/node.mjs";
import { Attribute, Property, Event } from "../../vdom/attribute.mjs";
import {
  Insert,
  SetKey,
  Move,
  Remove,
  RemoveKey,
  Replace,
  ReplaceInnerHtml,
  ReplaceText,
  Update,
} from "../../vdom/diff.mjs";

const meta = Symbol("metadata");

export class Reconciler {
  #root = null;
  #dispatch = () => {};
  #stack = [];

  constructor(root, dispatch) {
    this.#root = root;
    this.#dispatch = dispatch;
  }

  push(patch) {
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }

  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();

      for (let list = patch.changes; list.tail; list = list.tail) {
        const change = list.head;

        switch (change.constructor) {
          case Insert:
            insert(
              node,
              change.children,
              change.before,
              this.#dispatch,
              this.#root,
            );
            break;

          case SetKey:
            setKey(node, change.index, change.key);
            break;

          case Move:
            move(node, change.key, change.before, change.count);
            break;

          case RemoveKey:
            removeKey(node, change.key, change.count);
            break;

          case Remove:
            remove(node, change.from, change.count);
            break;

          case Replace:
            replace(node, change.element, this.#dispatch, this.#root);
            break;

          case ReplaceText:
            replaceText(node, change.content);
            break;

          case ReplaceInnerHtml:
            replaceInnerHtml(node, change.inner_html);
            break;

          case Update:
            update(
              node,
              change.added,
              change.removed,
              this.#dispatch,
              this.#root,
            );
            break;
        }
      }

      for (let i = 0; i < patch.removed; ++i) {
        const child = node.lastChild;
        const key = child[meta].key;

        if (key) {
          node[meta].keyedChildren.delete(key);
        }

        node.removeChild(child);
      }

      for (let list = patch.children; list.tail; list = list.tail) {
        const child = list.head;

        this.#stack.push({
          node: node.childNodes[child.index],
          patch: child,
        });
      }
    }
  }
}

// CHANGES ---------------------------------------------------------------------

function insert(node, children, before, dispatch, root) {
  const fragment = document.createDocumentFragment();

  for (let list = children; list.tail; list = list.tail) {
    const child = list.head;
    const el = createElement(child, dispatch, root);

    if (child.key) {
      const ref = new WeakRef(unwrapFragment(el));
      node[meta].keyedChildren.set(child.key, ref);
    }

    fragment.appendChild(el);
  }

  node.insertBefore(fragment, node.childNodes[before] ?? null);
}

function setKey(node, index, key) {
  const el = node.childNodes[index];
  if (el[meta].key) {
    node[meta].keyedChildren.delete(el[meta].key);
  }

  el[meta].key = key;
  node[meta].keyedChildren.set(key, new WeakRef(el));
}

function move(node, key, before, count) {
  let el = node[meta].keyedChildren.get(key).deref();

  if (count > 1) {
    const fragment = document.createDocumentFragment();

    for (let i = 0; i < count && el !== null; ++i) {
      let next = el.nextSibling;
      fragment.append(el);
      el = next;
    }

    el = fragment;
  }

  node.insertBefore(el, node.childNodes[before] ?? null);
}

function removeKey(node, key, count) {
  let el = node[meta].keyedChildren.get(key).deref();
  node[meta].keyedChildren.delete(key);

  while (count-- > 0 && el !== null) {
    let next = el.nextSibling;
    node.removeChild(el);
    el = next;
  }
}

function remove(node, from, count) {
  let el = node.childNodes[from];

  while (count-- > 0 && el !== null) {
    const next = el.nextSibling;
    node.removeChild(el);
    el = next;
  }
}

function replace(node, child, dispatch, root) {
  const el = createElement(child, dispatch, root);
  const parent = node.parentNode;

  if (child.key) {
    const ref = new WeakRef(unwrapFragment(el));
    parent[meta].keyedChildren.set(child.key, ref);
  }

  parent.replaceChild(el, node);
}

function replaceText(node, content) {
  node.data = content;
}

function replaceInnerHtml(node, inner_html) {
  node.innerHTML = inner_html;
}

function update(node, added, removed, dispatch, root) {
  for (let list = removed; list.tail; list = list.tail) {
    const name = list.head.name;

    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
      ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
    }
  }

  for (let list = added; list.tail; list = list.tail) {
    createAttribute(node, list.head, dispatch, root);
  }
}

// ELEMENTS --------------------------------------------------------------------

function unwrapFragment(node) {
  while (node.nodeType === DocumentFragment.DOCUMENT_FRAGMENT_NODE) {
    node = node.firstChild;
  }

  return node;
}

function createElement(vnode, dispatch, root) {
  switch (vnode.constructor) {
    case Element: {
      const node = vnode.namespace
        ? document.createElementNS(vnode.namespace, vnode.tag)
        : document.createElement(vnode.tag);

      initialiseMetadata(node, vnode.key);

      for (let list = vnode.attributes; list.tail; list = list.tail) {
        createAttribute(node, list.head, dispatch, root);
      }

      insert(node, vnode.children, 0, dispatch, root);

      return node;
    }

    case Text: {
      const node = document.createTextNode(vnode.content);
      initialiseMetadata(node, vnode.key);

      return node;
    }

    case Fragment: {
      const node = document.createDocumentFragment();

      for (let list = vnode.children; list.tail; list = list.tail) {
        node.appendChild(createElement(list.head, dispatch, root));
      }

      return node;
    }

    case UnsafeInnerHtml: {
      const node = vnode.namespace
        ? document.createElementNS(vnode.namespace, vnode.tag)
        : document.createElement(vnode.tag);

      initialiseMetadata(node, vnode.key);

      for (let list = vnode.attributes; list.tail; list = list.tail) {
        createAttribute(node, list.head, dispatch, root);
      }

      replaceInnerHtml(node, vnode.inner_html);

      return node;
    }
  }
}

/// @internal
export function initialiseMetadata(node, key = '') {
  switch (node.nodeType) {
    case Node.ELEMENT_NODE:
    case Node.DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        keyedChildren: new Map(),
        handlers: new Map(),
      };
      break;

    case Node.ELEMENT_TEXT:
      node[meta] = { key };
      break;
  }
}

// ATTRIBUTES ------------------------------------------------------------------

function createAttribute(node, attribute, dispatch, root) {
  switch (attribute.constructor) {
    case Attribute: {
      const name = attribute.name;
      const value = attribute.value;

      if (value !== node.getAttribute(name)) {
        node.setAttribute(name, value);
      }

      ATTRIBUTE_HOOKS[name]?.added?.(node, value);
    } break;

    case Property:
      node[attribute.name] = attribute.value;
      break;

    case Event: {
      if (!node[meta].handlers.has(attribute.name)) {
        node.addEventListener(attribute.name, handleEvent, {
          passive: !attribute.prevent_default,
        });
      }

      const prevent = attribute.prevent_default;
      const stop = attribute.stop_propagation;
      const immediate =
        attribute.immediate || IMMEDIATE_EVENTS.includes(attribute.name);

      node[meta].handlers.set(attribute.name, (event) => {
        if (prevent) event.preventDefault();
        if (stop) event.stopPropagation();

        let node = event.target;
        let node = event.currentTarget;
        let path =
          node[meta].key ||
          [].indexOf.call(node.parentNode.childNodes, node).toString();

        node = node.parentNode;

        while (node !== root) {
          const key = node[meta].key;

          if (key) {
            path = `${key}.${path}`;
          } else {
            const index = [].indexOf.call(node.parentNode.childNodes, node);
            path = `${index}.${path}`;
          }

          node = node.parentNode;
        }

        dispatch(event, path, event.type, immediate);
      });
    } break;
  }
}

function handleEvent(event) {
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);

  handler(event);
}

const ATTRIBUTE_HOOKS = {
  checked: syncedBooleanAttribute('checked'),
  selected: syncedBooleanAttribute('selected'),
  value: syncedAttribute('value'),

  autofocus: {
    added(node) {
      node.focus?.()
    }
  },

  autoplay: {
    added(node) {
      node.play?.()
    }
  }
}

function syncedBooleanAttribute(name) {
  return {
    added(node, value) {
      node[name] = true
    },
    removed(node) {
      node[name] = false
    }
  }
}

function syncedAttribute(name) {
  return {
    added(node, value) {
      node[name] = value
    }
  }
}

const IMMEDIATE_EVENTS = [
  // Input synchronization
  "input",
  "change",

  // Focus management
  "focusin",
  "focusout",
  "focus",
  "blur",

  // Text selection
  "select",
];
