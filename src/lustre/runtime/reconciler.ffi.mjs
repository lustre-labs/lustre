import { Element, Text, Fragment } from "../vdom/node.mjs";
import { Attribute, Property, Event } from "../vdom/attribute.mjs";
import {
  InsertMany,
  Insert,
  Move,
  Remove,
  RemoveKey,
  Replace,
  ReplaceText,
  Update,
} from "../vdom/diff.mjs";

const meta = Symbol("metadata");

export class Reconciler {
  #root = null;
  #dispatch = () => {};
  #stack = [];

  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
  }

  mount(vnode) {
    this.#root.appendChild(createElement(vnode, this.#dispatch, this.#root));
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
          case InsertMany:
            insertMany(
              node,
              change.children,
              change.before,
              this.#dispatch,
              this.#root,
            );
            break;

          case Insert:
            insert(
              node,
              change.child,
              change.before,
              this.#dispatch,
              this.#root,
            );
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

      while (patch.removed-- > 0) {
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

function insertMany(node, children, before, dispatch, root) {
  const fragment = document.createDocumentFragment();

  for (let list = children; list.tail; list = list.tail) {
    const child = list.head;
    const el = createElement(child, dispatch, root);

    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
    }

    fragment.appendChild(el);
  }

  node.insertBefore(fragment, node.childNodes[before] ?? null);
}

function insert(node, child, before, dispatch, root) {
  const el = createElement(child, dispatch, root);

  if (child.key) {
    node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }

  node.insertBefore(el, node.childNodes[before] ?? null);
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
    parent[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }

  parent.replaceChild(el, node);
}

function replaceText(node, content) {
  node.data = content;
}

function update(node, added, removed, dispatch, root) {
  for (let list = removed; list.tail; list = list.tail) {
    const name = list.head.name;

    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
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

      node[meta] = {
        key: vnode.key,
        keyedChildren: new Map(),
        handlers: new Map(),
      };

      for (let list = vnode.attributes; list.tail; list = list.tail) {
        createAttribute(node, list.head, dispatch, root);
      }

      insertMany(node, vnode.children, 0, dispatch, root);

      return node;
    }

    case Text: {
      const node = document.createTextNode(vnode.content);

      node[meta] = { key: vnode.key };

      return node;
    }

    case Fragment: {
      const node = document.createDocumentFragment();

      for (let list = vnode.children; list.tail; list = list.tail) {
        node.appendChild(createElement(list.head, dispatch, root));
      }

      return node;
    }
  }
}

// ATTRIBUTES ------------------------------------------------------------------

function createAttribute(node, attribute, dispatch, root) {
  switch (attribute.constructor) {
    case Attribute:
      if (attribute.value !== node.getAttribute(attribute.name)) {
        node.setAttribute(attribute.name, attribute.value);

        if (SYNCED_ATTRIBUTES.includes(attribute.name)) {
          node[attribute.name] = attribute.value;
        }
      }
      break;

    case Property:
      node[attribute.name] = attribute.value;
      break;

    case Event:
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
        
        let path =
          node[meta].key ||
          [].indexOf.call(node.parentNode.childNodes, node).toString();

        node = node.parentNode;

        while (node !== root) {
          const key = node[meta].key;
          if (key) {
            path = `${key}.${path}`
          } else {
            const index = [].indexOf.call(node.parentNode.childNodes, node);
            path = `${index}.${path}`
          }

          node = node.parentNode;
        }

        dispatch(event, path, event.type, immediate);
      });
      break;
  }
}

function handleEvent(event) {
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);

  handler(event);
}

const SYNCED_ATTRIBUTES = ["checked", "disabled", "selected", "value"];

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
