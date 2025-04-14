import { Empty, NonEmpty } from "../../gleam.mjs";
import { element, namespaced, fragment, text, none } from "../element.mjs";
import { attribute } from "../attribute.mjs";
import { to_keyed } from "./vnode.mjs";
import { empty_list } from "../internals/constants.mjs";
import { initialiseMetadata } from "./reconciler.ffi.mjs";

import {
  document,
  ELEMENT_NODE,
  TEXT_NODE,
  DOCUMENT_FRAGMENT_NODE,
  NAMESPACE_HTML
} from "../internals/constants.ffi.mjs";

export const virtualise = (root) => {
  const vdom = virtualise_node(root);
  // at this point we know the element is empty - but we have to have at least
  // an empty text node child in the root element to be able to mount
  if (vdom === null || vdom.children instanceof Empty) {
    const empty = empty_text_node();
    initialiseMetadata(empty);
    root.appendChild(empty);
    return none();
  } else if (
    vdom.children instanceof NonEmpty &&
    vdom.children.tail instanceof Empty
  ) {
    return vdom.children.head;
  } else {
    const head = empty_text_node();
    initialiseMetadata(head);
    root.insertBefore(head, root.firstChild);
    return fragment(vdom.children);
  }
}

const empty_text_node = () => {
  return document.createTextNode("");
}

const virtualise_node = (node) => {
  switch (node.nodeType) {
    case ELEMENT_NODE: {
      const key = node.getAttribute("data-lustre-key");
      initialiseMetadata(node, key);

      if (key) {
        node.removeAttribute("data-lustre-key");
      }

      const tag = node.localName;
      const namespace = node.namespaceURI;
      const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;

      if (isHtmlElement && input_elements.includes(tag)) {
        virtualise_input_events(tag, node);
      }

      const attributes = virtualise_attributes(node);
      const children = virtualise_child_nodes(node);

      const vnode =
        isHtmlElement
          ? element(tag, attributes, children)
          : namespaced(namespace, tag, attributes, children);

      return key ? to_keyed(key, vnode) : vnode;
    }

    case TEXT_NODE:
      initialiseMetadata(node);
      return text(node.data);

    case DOCUMENT_FRAGMENT_NODE: // shadowRoot
      initialiseMetadata(node);
      return node.childNodes.length > 0
        ? fragment(virtualise_child_nodes(node))
        : null;

    default:
      return null;
  }
}

const input_elements = ["input", "select", "textarea"];

const virtualise_input_events = (tag, node) => {
  const value = node.value;
  const checked = node.checked;
  // For inputs that reflect their default state (eg not checked for checkboxes
  // and radios, empty for all other inputs) then we don't need to schedule any
  // virtual events.
  if (tag === "input" && node.type === "checkbox" && !checked) return;
  if (tag === "input" && node.type === "radio" && !checked) return;
  if (node.type !== "checkbox" && node.type !== "radio" && !value) return;

  // We schedule a microtask instead of dispatching the events immediately to
  // give the runtime a chance to finish virtualising the DOM and set up the
  // runtime.
  //
  // Microtasks are flushed once the current task has completed, and will block
  // the browser from painting until the queue is empty, so we can be sure that
  // these events will be processed before the user sees the first render.
  queueMicrotask(() => {
    // Since the first patch will have overridden our values, we will reset them
    // here and trigger events, which the runtime can then pick up.
    node.value = value;
    node.checked = checked;

    node.dispatchEvent(new Event("input", { bubbles: true }));
    node.dispatchEvent(new Event("change", { bubbles: true }));

    // User apps may be using semi-controlled inputs where they listen to blur
    // events to save the value rather than using the input event. To account for
    // those, we dispatch a blur event if the input is not currently focused.
    if (document.activeElement !== node) {
      node.dispatchEvent(new Event("blur", { bubbles: true }));
    }
  });
}

const virtualise_child_nodes = (node) => {
  let children = empty_list;

  let child = node.lastChild;
  while (child) {
    const vnode = virtualise_node(child);
    const next = child.previousSibling;
    if (vnode) {
      children = new NonEmpty(vnode, children);
    } else {
      node.removeChild(child);
    }
    child = next;
  }

  return children;
}

const virtualise_attributes = (node) => {
  let index = node.attributes.length;

  let attributes = empty_list;
  while (index-- > 0) {
    attributes = new NonEmpty(
      virtualise_attribute(node.attributes[index]),
      attributes,
    );
  }

  return attributes;
}

const virtualise_attribute = (attr) => {
  const name = attr.localName;
  const value = attr.value;
  return attribute(name, value);
}
