import {
  // ELEMENTS
  Node,
  Text,
  Fragment,
  // ATTRIBUTES
  Attribute,
  Property,
  Event,
  // PATCHES
  InsertMany,
  Insert,
  Move,
  Remove,
  RemoveKey,
  Replace,
  ReplaceText,
  Update,
} from "./lustre/runtime/vdom.mjs";

const meta = Symbol("metadata");

export class LustreReconciler {
  #root = null;
  #dispatch = () => { };

  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
  }

  mount(vnode) {
    this.#root.appendChild(createElement(vnode, this.#dispatch));
  }

  push(patch) {
    reconcile(this.#root, patch, this.#dispatch);
  }
}

const stack = [];
function reconcile(root, patch, dispatch) {
  stack.push({ node: root, patch });

  while (stack.length) {
    const { node, patch } = stack.pop();

    for (let changePtr = patch.changes; changePtr.tail; changePtr = changePtr.tail) {
      const change = changePtr.head;

      switch (change.constructor) {
        case InsertMany:
          insertMany(node, change.children, change.before, dispatch);
          break;

        case Insert:
          insert(node, change.child, change.before, dispatch);
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
          replace(node, change.element, dispatch);
          break;

        case ReplaceText:
          replaceText(node, change.content);
          break;

        case Update:
          update(node, change.added, change.removed, dispatch);
          break;
      }
    }

    while (patch.remove_count-- > 0) {
      const child = node.lastChild;
      const key = child[meta].key;
      if (key) {
        node[meta].keyedChildren.delete(key);
      }
      node.removeChild(child);
    }

    for (let child = patch.children; child.tail; child = child.tail) {
      stack.push({ node: node.childNodes[child.head.index], patch: child.head })
    }
  }
}

// CHANGES ---------------------------------------------------------------------

function insertMany(node, children, before, dispatch) {
  const fragment = document.createDocumentFragment();

  for (let childPtr = children; childPtr.tail; childPtr = childPtr.tail) {
    const child = childPtr.head;
    const el = createElement(child, dispatch);

    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
    }

    fragment.appendChild(el);
  }

  node.insertBefore(fragment, node.childNodes[before]);
}

function insert(node, child, before, dispatch) {
  const el = createElement(child, dispatch);

  if (child.key) {
    node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }

  node.insertBefore(el, node.childNodes[before]);
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

  node.insertBefore(el, node.childNodes[before]);
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
  let el = node.childNodes[from]
  while (count-- > 0 && el !== null) {
    const next = el.nextSibling;
    node.removeChild(el);
    el = next;
  }
}

function replace(node, child, dispatch) {
  const el = createElement(child, dispatch);
  const parent = node.parentNode;

  if (child.key) {
    parent[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }

  parent.replaceChild(el, node);

}

function replaceText(node, content) {
  node.data = content;
}

function update(node, added, removed, dispatch) {
  for (let attribute = removed; attribute.tail; attribute = attribute.tail) {
    const name = attribute.head.name
    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
    }
  }

  for (let attribute = added; attribute.tail; attribute = attribute.tail) {
    createAttribute(node, attribute.head, dispatch);
  }
}

// ELEMENTS --------------------------------------------------------------------

function unwrapFragment(node) {
  while (node.nodeType === DocumentFragment.DOCUMENT_FRAGMENT_NODE) {
    node = node.firstChild;
  }
  return node;
}

function createElement(vnode, dispatch) {
  switch (vnode.constructor) {
    case Node: {
      const node = vnode.namespace
        ? document.createElementNS(vnode.namespace, vnode.tag)
        : document.createElement(vnode.tag);

      node[meta] = {
        constructor: Node,
        key: vnode.key,
        keyedChildren: new Map(),
        handlers: new Map(),
      };

      for (let attributePtr = vnode.attributes; attributePtr.tail; attributePtr = attributePtr.tail) {
        const attribute = attributePtr.head;
        createAttribute(node, attribute, dispatch);
      }

      insertMany(node, vnode.children, 0, dispatch);

      return node;
    }

    case Text: {
      const node = document.createTextNode(vnode.content);

      node[meta] = { constructor: Text, key: vnode.key };

      return node;
    }

    case Fragment: {
      const node = document.createDocumentFragment();

      for (let childPtr = vnode.children; childPtr.tail; childPtr = childPtr.tail) {
        const child = childPtr.head;
        node.appendChild(createElement(child, dispatch));
      }

      return node;
    }
  }
}

// ATTRIBUTES ------------------------------------------------------------------

function createAttribute(node, attribute, dispatch) {
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

      node[meta].handlers.set(attribute.name, (event) => {
        if (attribute.prevent_default) event.preventDefault();
        if (attribute.stop_propagation) event.stopPropagation();

        const msg = attribute.handler(event);

        dispatch(
          node[meta].mapper ? node[meta].mapper(msg) : msg,
          attribute.immediate || IMMEDIATE_EVENTS.includes(event.type),
        );
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
