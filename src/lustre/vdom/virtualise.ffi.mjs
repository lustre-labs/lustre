import { toList } from "../../gleam.mjs";
import { text, none, memo, ref, map } from "../element.mjs";
import { element, namespaced, fragment } from "../element/keyed.mjs";
import { attribute } from "../attribute.mjs";
import { insertMetadataChild } from "./reconciler.ffi.mjs";
import { element_kind, fragment_kind, text_kind, map_kind } from "./vnode.mjs";

import {
  document,
  ELEMENT_NODE,
  TEXT_NODE,
  COMMENT_NODE,
  NAMESPACE_HTML,
} from "../internals/constants.ffi.mjs";

export const virtualise = (root) => {
  // no matter what, we want to initialise the metadata for our root element.
  // we pass an empty stringh here as the index to make sure that the root node
  // does not have a path.
  const rootMeta = insertMetadataChild(element_kind, null, root, 0, null);

  for (let child = root.firstChild; child; child = child.nextSibling) {
    const result = virtualiseChild(rootMeta, root, child, 0);
    // lustre view functions always return a single root element inside the root.
    // even if we could virtualise multiple children, we will ignore them and
    // return the first child as the one we'll take over.
    //
    // A top-level key is impossible and always ignored.
    if (result) return result.vnode;
  }

  // no virtualisable children, we can empty the node and return our default text node.
  const placeholder = document().createTextNode("");
  insertMetadataChild(text_kind, rootMeta, placeholder, 0, null);
  root.insertBefore(placeholder, root.firstChild);
  return none();
};

const virtualiseChild = (meta, domParent, child, index) => {
  if (child.nodeType === COMMENT_NODE) {
    const data = child.data.trim();

    if (data.startsWith("lustre:fragment")) {
      return virtualiseFragment(meta, domParent, child, index);
    }

    if (data.startsWith("lustre:map")) {
      return virtualiseMap(meta, domParent, child, index);
    }

    if (data.startsWith("lustre:memo")) {
      return virtualiseMemo(meta, domParent, child, index);
    }

    return null;
  }
  if (child.nodeType === ELEMENT_NODE) {
    return virtualiseElement(meta, child, index);
  }

  if (child.nodeType === TEXT_NODE) {
    return virtualiseText(meta, child, index);
  }

  return null;
};

const virtualiseElement = (metaParent, node, index) => {
  const key = node.getAttribute("data-lustre-key") ?? "";
  if (key) {
    node.removeAttribute("data-lustre-key");
  }

  const meta = insertMetadataChild(element_kind, metaParent, node, index, key);

  const tag = node.localName;
  const namespace = node.namespaceURI;
  const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;

  if (isHtmlElement && INPUT_ELEMENTS.includes(tag)) {
    virtualiseInputEvents(tag, node);
  }

  const attributes = virtualiseAttributes(node);
  const children = [];

  for (let childNode = node.firstChild; childNode; ) {
    const child = virtualiseChild(meta, node, childNode, children.length);

    if (child) {
      children.push([child.key, child.vnode]);
      childNode = child.next;
    } else {
      childNode = childNode.nextSibling;
    }
  }

  const vnode = isHtmlElement
    ? element(tag, attributes, toList(children))
    : namespaced(namespace, tag, attributes, toList(children));

  return childResult(key, vnode, node.nextSibling);
};

const virtualiseText = (meta, node, index) => {
  insertMetadataChild(text_kind, meta, node, index, null);
  return childResult("", text(node.data), node.nextSibling);
};

const virtualiseFragment = (metaParent, domParent, node, index) => {
  const key = parseKey(node.data);

  const meta = insertMetadataChild(fragment_kind, metaParent, node, index, key);

  const children = [];

  node = node.nextSibling;
  while (
    node &&
    (node.nodeType !== COMMENT_NODE || node.data.trim() !== "/lustre:fragment")
  ) {
    const child = virtualiseChild(meta, domParent, node, children.length);

    if (child) {
      children.push([child.key, child.vnode]);
      node = child.next;
    } else {
      node = node.nextSibling;
    }
  }

  meta.endNode = node;

  const vnode = fragment(toList(children));
  return childResult(key, vnode, node?.nextSibling);
};

const virtualiseMap = (metaParent, domParent, node, index) => {
  const key = parseKey(node.data);

  const meta = insertMetadataChild(map_kind, metaParent, node, index, key);

  const child = virtualiseNextChild(meta, domParent, node, 0);
  if (!child) return null;

  const vnode = map(child.vnode, (x) => x);
  return childResult(key, vnode, child.next);
};

const virtualiseMemo = (meta, domParent, node, index) => {
  const key = parseKey(node.data);

  // Memo nodes are transparent - they don't create metadata nodes!
  // Just virtualise the child directly with the parent metadata
  const child = virtualiseNextChild(meta, domParent, node, index);
  if (!child) return null;

  domParent.removeChild(node);

  // We cannot recover the dependencies -
  // so we add an anonymous object here that for sure compares falsy with
  // anything the user will pass us.
  const vnode = memo(toList([ref({})]), () => child.vnode);
  return childResult(key, vnode, child.next);
};

const virtualiseNextChild = (meta, domParent, node, index) => {
  while (true) {
    node = node.nextSibling;
    if (!node) return null;

    const child = virtualiseChild(meta, domParent, node, index);
    if (child) return child;
  }
};

const childResult = (key, vnode, next) => {
  return { key, vnode, next };
};

const virtualiseAttributes = (node) => {
  const attributes = [];
  for (let i = 0; i < node.attributes.length; i++) {
    const attr = node.attributes[i];
    if (attr.name !== "xmlns") {
      attributes.push(attribute(attr.localName, attr.value));
    }
  }
  return toList(attributes);
};

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
};

const parseKey = (data) => {
  const keyMatch = data.match(/key="([^"]*)"/);
  if (!keyMatch) return "";
  return unescapeKey(keyMatch[1]);
};

const unescapeKey = (key) => {
  return key
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&amp;/g, "&")
    .replace(/&#39;/g, "'");
};
