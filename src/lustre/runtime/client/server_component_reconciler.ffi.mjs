import {
  fragment_variant,
  fragment_children,
  element_variant,
  element_key,
  element_tag,
  element_namespace,
  element_attributes,
  element_children,
  unsafe_inner_html_variant,
  unsafe_inner_html_key,
  unsafe_inner_html_tag,
  unsafe_inner_html_namespace,
  unsafe_inner_html_attributes,
  unsafe_inner_html_inner_html,
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
  replace_inner_html_variant,
  replace_inner_html_inner_html,
  update_variant,
  update_added,
  update_removed,
  move_variant,
  move_key,
  move_before,
  move_count,
  remove_key_variant,
  remove_key_key,
  remove_key_count,
  insert_variant,
  insert_children,
  insert_before,
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

  mount(vdom) {
    this.#root.appendChild(createElement(vdom, this.#dispatch, this.#root));
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
          case insert_variant:
            insert(
              node,
              change[insert_children],
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

          case replace_inner_html_variant:
            replaceInnerHtml(node, change[replace_inner_html_inner_html]);
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

      for (let i = 0; i < patch[patch_removed]; ++i) {
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

function insert(node, children, before, dispatch, root) {
  const fragment = document.createDocumentFragment();

  for (let i = 0; i < children.length; i++) {
    const child = children[i];
    const el = createElement(child, dispatch, root);

    if (child[element_key]) {
      const ref = new WeakRef(unwrapFragment(el));
      node[meta].keyedChildren.set(child[element_key], ref);
    }

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
    const ref = new WeakRef(unwrapFragment(el));
    parent[meta].keyedChildren.set(child[element_key], ref);
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
  for (let i = 0; i < removed.length; i++) {
    const name = removed[i][attribute_name];

    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
      ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
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

      insert(node, vnode[element_children], 0, dispatch, root);

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

    case unsafe_inner_html_variant: {
      const node = vnode[unsafe_inner_html_namespace]
        ? document.createElementNS(
            vnode[unsafe_inner_html_namespace],
            vnode[unsafe_inner_html_tag],
          )
        : document.createElement(vnode[unsafe_inner_html_tag]);

      node[meta] = {
        key: vnode[unsafe_inner_html_key],
        handlers: new Map(),
      };

      for (let i = 0; i < vnode[unsafe_inner_html_attributes].length; i++) {
        createAttribute(
          node,
          vnode[unsafe_inner_html_attributes][i],
          dispatch,
          root,
        );
      }

      replaceInnerHtml(node, vnode[unsafe_inner_html_inner_html]);

      return node;
    }
  }
}

// ATTRIBUTES ------------------------------------------------------------------

function createAttribute(node, attribute, dispatch, root) {
  switch (attribute[0]) {
    case attribute_variant:
      {
        const name = attribute[attribute_name];
        const value = attribute[attribute_value];

        if (value !== node.getAttribute(name)) {
          node.setAttribute(name, value);
        }

        ATTRIBUTE_HOOKS[name]?.added?.(node, value);
      }
      break;

    case property_variant:
      node[attribute[property_name]] = attribute[property_value];
      break;

    case event_variant:
      {
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
      }
      break;
  }
}

function handleEvent(event) {
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);

  handler(event);
}

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
      node.play?.();
    },
  },
};

function syncedBooleanAttribute(name) {
  return {
    added(node, value) {
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
