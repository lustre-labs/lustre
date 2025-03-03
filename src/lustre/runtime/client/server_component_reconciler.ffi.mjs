import {
  fragment_variant,
  fragment_children,
  element_variant,
  element_key,
  element_tag,
  element_namespace,
  element_attributes,
  element_children,
  text_variant,
  text_key,
  text_content,
  attribute_variant,
  attribute_name,
  attribute_value,
  property_variant,
  property_name,
  property_value,
  event_variant,
  event_name,
  event_include,
  event_prevent_default,
  event_stop_propagation,
  event_immediate,
  patch_index,
  patch_removed,
  patch_changes,
  patch_children,
  replace_variant,
  replace_element,
  replace_text_variant,
  replace_text_content,
  update_variant,
  update_added,
  update_removed,
  insert_variant,
  insert_child,
  insert_before,
  move_variant,
  move_key,
  move_before,
  move_count,
  remove_key_variant,
  remove_key_key,
  remove_key_count,
  insert_many_variant,
  insert_many_children,
  insert_many_before,
  remove_variant,
  remove_from,
  remove_count,
} from "../transport.mjs";

const meta = Symbol("metadata");

export class Reconciler {
  #root = null;
  #dispatch = () => {};
  #stack = [];

  constructor(root, dispatch) {
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

      for (let i = 0; i < patch[patch_changes].length; i++) {
        const change = patch[patch_changes][i];

        switch (change[0]) {
          case insert_many_variant:
            insertMany(
              node,
              change[insert_many_children],
              change[insert_many_before],
              this.#dispatch,
              this.#root,
            );
            break;

          case insert_variant:
            insert(
              node,
              change[insert_child],
              change[insert_before],
              this.#dispatch,
              this.#root,
            );
            break;

          case move_variant:
            move(
              node,
              change[move_key],
              change[move_before],
              change[move_count],
            );
            break;

          case remove_key_variant:
            removeKey(node, change[remove_key_key], change[remove_key_count]);
            break;

          case remove_variant:
            remove(node, change[remove_from], change[remove_count]);
            break;

          case replace_variant:
            replace(node, change[replace_element], this.#dispatch, this.#root);
            break;

          case replace_text_variant:
            replaceText(node, change[replace_text_content]);
            break;

          case update_variant:
            update(
              node,
              change[update_added],
              change[update_removed],
              this.#dispatch,
              this.#root,
            );
            break;
        }
      }

      while (patch[patch_removed]-- > 0) {
        const child = node.lastChild;
        const key = child[meta].key;

        if (key) {
          node[meta].keyedChildren.delete(key);
        }

        node.removeChild(child);
      }

      for (let i = 0; i < patch[patch_children].length; i++) {
        const child = patch[patch_children][i];

        this.#stack.push({
          node: node.childNodes[child[patch_index]],
          patch: child,
        });
      }
    }
  }
}

// CHANGES ---------------------------------------------------------------------

function insertMany(node, children, before, dispatch, root) {
  const fragment = document.createDocumentFragment();

  for (let i = 0; i < children.length; i++) {
    const child = children[i];
    const el = createElement(child, dispatch, root);

    if (el[meta].key) {
      node[meta].keyedChildren.set(
        el[meta].key,
        new WeakRef(unwrapFragment(el)),
      );
    }

    fragment.appendChild(el);
  }

  node.insertBefore(fragment, node.childNodes[before]);
}

function insert(node, child, before, dispatch, root) {
  const el = createElement(child, dispatch, root);

  if (child[element_key]) {
    node[meta].keyedChildren.set(
      child[element_key],
      new WeakRef(unwrapFragment(el)),
    );
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

  if (child[element_key]) {
    parent[meta].keyedChildren.set(
      child[element_key],
      new WeakRef(unwrapFragment(el)),
    );
  }

  parent.replaceChild(el, node);
}

function replaceText(node, content) {
  node.data = content;
}

function update(node, added, removed, dispatch, root) {
  for (let i = 0; i < removed.length; i++) {
    const name = removed[i][attribute_name];

    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
    }
  }

  for (let i = 0; i < added.length; i++) {
    createAttribute(node, added[i], dispatch, root);
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
  switch (vnode[0]) {
    case element_variant: {
      const node = vnode[element_namespace]
        ? document.createElementNS(vnode[element_namespace], vnode[element_tag])
        : document.createElement(vnode[element_tag]);

      node[meta] = {
        key: vnode[element_key],
        keyedChildren: new Map(),
        handlers: new Map(),
      };

      for (let i = 0; i < vnode[element_attributes].length; i++) {
        createAttribute(node, vnode[element_attributes][i], dispatch, root);
      }

      insertMany(node, vnode[element_children], 0, dispatch, root);

      return node;
    }

    case text_variant: {
      const node = document.createTextNode(vnode[text_content]);

      node[meta] = { key: vnode[text_key] };

      return node;
    }

    case fragment_variant: {
      const node = document.createDocumentFragment();

      for (let i = 0; i < vnode[fragment_children].length; i++) {
        node.appendChild(
          createElement(vnode[fragment_children][i], dispatch, root),
        );
      }

      return node;
    }
  }
}

// ATTRIBUTES ------------------------------------------------------------------

function createAttribute(node, attribute, dispatch, root) {
  switch (attribute[0]) {
    case attribute_variant:
      if (
        attribute[attribute_value] !==
        node.getAttribute(attribute[attribute_name])
      ) {
        node.setAttribute(
          attribute[attribute_name],
          attribute[attribute_value],
        );

        if (SYNCED_ATTRIBUTES.includes(attribute[attribute_name])) {
          node[attribute[attribute_name]] = attribute[attribute_value];
        }
      }
      break;

    case property_variant:
      node[attribute[property_name]] = attribute[property_value];
      break;

    case event_variant:
      if (!node[meta].handlers.has(attribute[event_name])) {
        node.addEventListener(attribute[event_name], handleEvent, {
          passive: !attribute[event_prevent_default],
        });
      }

      const prevent = attribute[event_prevent_default];
      const stop = attribute[event_stop_propagation];
      const immediate =
        attribute[event_immediate] ||
        IMMEDIATE_EVENTS.includes(attribute[event_name]);

      node[meta].handlers.set(attribute[event_name], (event) => {
        if (prevent) event.preventDefault();
        if (stop) event.stopPropagation();

        let node = event.target;
        let path =
          node[meta].key ||
          Array.from(node.parentNode.childNodes).indexOf(node);

        node = node.parentNode;

        while (node !== root) {
          const key = node[meta].key;
          const index = Array.from(node.parentNode.childNodes).indexOf(node);

          path = key ? `${key}.${path}` : `${index}.${path}`;
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
