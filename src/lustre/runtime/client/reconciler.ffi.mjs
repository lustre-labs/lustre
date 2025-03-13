import { Element, Text, Fragment, UnsafeInnerHtml } from "../../vdom/node.mjs";
import { Attribute, Property, Event } from "../../vdom/attribute.mjs";
import {
  Insert,
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
            replace(node, change.from, change.count, change.with, this.#dispatch, this.#root);
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

      remove(node, node.childNodes.length - patch.removed, patch.removed);

      for (let list = patch.children; list.tail; list = list.tail) {
        const child = list.head;

        // TODO: use linked-list style but skip to indices if distance is great enough?

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
    addToMetadata(node, el);
    fragment.appendChild(el);
  }

  node.insertBefore(fragment, node.childNodes[before] ?? null);
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
  removeFromChild(node, node[meta].keyedChildren.get(key).deref(), count);
}

function remove(node, from, count) {
  removeFromChild(node, node.childNodes[from], count);
}

function removeFromChild(parent, child, count) {
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

function replace(parent, from, count, child, dispatch, root) {
  remove(parent, from, count);

  const el = createElement(child, dispatch, root);
  addToMetadata(parent, el);
  parent.insertBefore(el, parent.childNodes[from] ?? null);
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

function addToMetadata(node, child) {
  if (child.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
    for (child = child.firstChild; child; child = child.nextSibling) {
      addToMetadata(node, child);
    }
  } else {
    const key=  child[meta].key;
    if (key) {
      node[meta].keyedChildren.set(key, new WeakRef(child));
    }
  }
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

      const head = document.createTextNode('');
      initialiseMetadata(head, vnode.key);
      node.appendChild(head);

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

    case Node.TEXT_NODE:
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

      node[meta].handlers.set(attribute.name, (event) => {
        if (prevent) event.preventDefault();
        if (stop) event.stopPropagation();

        let path = [];
        for (let node = event.currentTarget; node !== root; node = node.parentNode) {
          const key = node[meta].key;
          if (key) {
            path.push(key);
          } else {
            const index = [].indexOf.call(node.parentNode.childNodes, node);
            path.push(index.toString());
          }
        }
        path.reverse();

        dispatch(event, path, event.type);
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
      try {
        node.play?.()
      } catch(e) {
        console.error(e)
      }
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
