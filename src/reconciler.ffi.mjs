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
  Append,
  Insert,
  Move,
  Remove,
  // RemoveAll,
  RemoveKey,
  Replace,
  ReplaceText,
  Update,
} from "./lustre/runtime/vdom.mjs";

const meta = Symbol("metadata");

export class LustreReconciler {
  #root = null;
  #dispatch = () => {};

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

let nodesStack = []
let patchesStack = []
function reconcile(root, patch, dispatch) {
  // console.log(nodesStack.length)
  let stackPtr = 0
  nodesStack[0] = root
  patchesStack[0] = patch
  // const stack = [{ node: root, patch }];

  while (stackPtr >= 0) {
    const node = nodesStack[stackPtr];
    const patch = patchesStack[stackPtr];
    stackPtr -= 1;
    // const { node, patch } = stack.pop();
    
    // if (!patch.changes.tail && !patch.children.tail) {
    //   console.log("EMPTY PATCH FOR ", node)
    // }
    //
    

    for (let changePtr = patch.changes; changePtr.tail; changePtr = changePtr.tail) {
      const change = changePtr.head;
      // if (change instanceof Update) {
      //  const isInteresting = (c) => (
      //    !(c instanceof Event)
      //    && !['value', 'checked', 'selected', 'scrollLeft', 'scrollTop'].includes(c.name ?? c)
      //  );

      //  const added = [...change.added].filter(isInteresting);
      //  const removed = [...change.removed].filter(isInteresting);

      //  if (added.length || removed.length) {
      //    console.log(node, { added, removed })
      //  }
      // } else {
      //  console.log(node, change)
      // }
      switch (change.constructor) {
        case Append:
          append(node, change.children, dispatch);
          break;

        case Insert:
          insert(node, change.child, change.before, dispatch);
          break;

        case Move:
          move(node, change.key, change.before);
          break;

        // case RemoveAll:
        //   removeAll(node, change.from);
        //   break;

        case RemoveKey:
          removeKey(node, change.key);
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

    for (let child = patch.children; child.tail; child = child.tail) {
      stackPtr += 1;
      nodesStack[stackPtr] = node.childNodes[child.head.index];
      patchesStack[stackPtr] = child.head;
    }

    if (patch.size > 0) {
      while (node.childNodes.length > patch.size) {
        const child = node.lastChild;
        const key = child[meta].key;
        if (key) {
          node[meta].keyedChildren.delete(key)
        }
        child.remove();
      }
    }
    
    // for (const child of patch.children) {
    // }
  }
}

// CHANGES ---------------------------------------------------------------------

function append(node, children, dispatch) {
  const fragment = document.createDocumentFragment();

  for (const child of children) {
    const el = createElement(child, dispatch);

    fragment.appendChild(el);

    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(el));
    }
  }

  node.appendChild(fragment);
}

function insert(node, child, before, dispatch) {
  const el = createElement(child, dispatch);

  node.insertBefore(el, node[meta].keyedChildren.get(before).deref());
 
  if (child.key) {
    node[meta].keyedChildren.set(child.key, new WeakRef(el));
  }
}

function move(node, key, before) {
  node.insertBefore(
    node[meta].keyedChildren.get(key).deref(),
    node[meta].keyedChildren.get(before).deref(),
  );
}

function removeAll(node, from) {
  while (node.children[from]) {
    if (node.children[from].key) {
      node[meta].keyedChildren.delete(node.children[from].key);
    }

    node.removeChild(node.children[from]);
  }
}

function removeKey(node, key) {
  const el = node[meta].keyedChildren.get(key).deref();

  node.removeChild(el);
  node[meta].keyedChildren.delete(key);
}

function remove(node, from, count) {
  while (count && node.children[from]) {
    const el = node.children[from];

    node.removeChild(el);

    if (el.key) {
      node[meta].keyedChildren.delete(el.key);
    }
  }
}

function replace(node, child, dispatch) {
  const el = createElement(child, dispatch);

  node.parentNode.replaceChild(el, node);

  if (child.key) {
    node.parentNode[meta].keyedChildren.set(child.key, new WeakRef(el));
  }
}

function replaceText(node, content) {
  node.data = content;
}

function update(node, added, removed, dispatch) {
  for (let attribute= removed; attribute.tail; attribute = attribute.tail) {
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

      for (const attribute of vnode.attributes) {
        createAttribute(node, attribute, dispatch);
      }

      append(node, vnode.children, dispatch);

      return node;
    }

    case Text: {
      const node = document.createTextNode(vnode.content);

      node[meta] = { constructor: Text, key: vnode.key };

      return node;
    }

    case Fragment: {
      const node = document.createDocumentFragment();

      for (const child of vnode.children) {
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
