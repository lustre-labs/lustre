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
  NAMESPACE_HTML
} from "../internals/constants.ffi.mjs";

export const virtualise = (root) => {
  // no matter what, we want to initialise the metadata for our root element.
  // we pass an empty stringh here as the index to make sure that the root node
  // does not have a path.
  initialiseMetadata(null, root, '', '');

  // we need to do different things depending on how many children we have,
  // and if we are a fragment or not.
  let virtualisableRootChildren = 0;
  for (let child = root.firstChild; child; child = child.nextSibling) {
    if(canVirtualiseNode(child)) virtualisableRootChildren += 1;
  }

  // no virtualisable children, we can empty the node and return our default text node.
  if (virtualisableRootChildren === 0) {
    root.replaceChildren(emptyTextNode(root));
    return none();
  }

  // a single virtualisable child, so we assume the view function returned that element.
  if (virtualisableRootChildren === 1) {
    const children = virtualiseChildNodes(root);
    return children.head;
  }

  // any other number of virtualisable children > 1, the view function had to
  // return a fragment node.


  // offset of 1 to account for the fragment head element we're going to insert.
  const children = virtualiseChildNodes(root, 1);

  const fragmentHead = emptyTextNode(root);
  root.insertBefore(fragmentHead, root.firstChild);

  return fragment(children);
}

const emptyTextNode = (parent) => {
  const node = document().createTextNode("");
  initialiseMetadata(parent, node);
  return node;
}

const canVirtualiseNode = (node) => {
  switch (node.nodeType) {
    case ELEMENT_NODE: return true;
    case TEXT_NODE: return !!node.data;
    default: return false;
  }
}

const virtualiseNode = (parent, node, index) => {
  if (!canVirtualiseNode(node)) {
    return null;
  }

  switch (node.nodeType) {
    case ELEMENT_NODE: {
      const key = node.getAttribute("data-lustre-key");
      initialiseMetadata(parent, node, index, key);

      if (key) {
        node.removeAttribute("data-lustre-key");
      }

      const tag = node.localName;
      const namespace = node.namespaceURI;
      const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;

      if (isHtmlElement && INPUT_ELEMENTS.includes(tag)) {
        virtualiseInputEvents(tag, node);
      }

      const attributes = virtualiseAttributes(node);
      const children = virtualiseChildNodes(node);

      const vnode =
        isHtmlElement
          ? element(tag, attributes, children)
          : namespaced(namespace, tag, attributes, children);

      return key ? to_keyed(key, vnode) : vnode;
    }

    case TEXT_NODE:
      initialiseMetadata(parent, node, index);
      return text(node.data);

    default:
      return null;
  }
}

const INPUT_ELEMENTS = ["input", "select", "textarea"];

const virtualiseInputEvents = (tag, node) => {
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
    if (document().activeElement !== node) {
      node.dispatchEvent(new Event("blur", { bubbles: true }));
    }
  });
}

const virtualiseChildNodes = (node, index = 0) => {
  let children = null;

  let child = node.firstChild;
  let ptr = null;

  while (child) {
    const vnode = virtualiseNode(node, child, index);
    const next = child.nextSibling;
    if (vnode) {
      const list_node = new NonEmpty(vnode, null);
      if (ptr) {
        ptr = ptr.tail = list_node;
      } else {
        ptr = children = list_node;
      }

      index += 1;
    } else {
      node.removeChild(child);
    }

    child = next;
  }

  if (!ptr) return empty_list;

  ptr.tail = empty_list;
  return children;
}

const virtualiseAttributes = (node) => {
  let index = node.attributes.length;

  let attributes = empty_list;
  while (index-- > 0) {
    const attr = node.attributes[index];
    if (attr.name === "xmlns") {
      continue;
    }

    attributes = new NonEmpty(virtualiseAttribute(attr), attributes);
  }

  return attributes;
}

const virtualiseAttribute = (attr) => {
  const name = attr.localName;
  const value = attr.value;
  return attribute(name, value);
}
