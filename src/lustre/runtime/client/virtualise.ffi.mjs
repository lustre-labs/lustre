import { Empty, NonEmpty } from '../../../gleam.mjs';
import { element, namespaced, fragment, text, none } from '../../element.mjs';
import { attribute } from '../../attribute.mjs';
import { to_keyed } from '../../vdom/node.mjs';
import { empty_list } from '../../internals/constants.mjs';
import { initialiseMetadata } from './reconciler.ffi.mjs';

const HTML_NAMESPACE = "http://www.w3.org/1999/xhtml";

export function virtualise(root) {
  const vdom = virtualise_node(root);
  // at this point we know the element is empty - but we have to have at least
  // an empty text node child in the root element to be able to mount
  if (vdom === null || vdom.children instanceof Empty) {
    const empty = document.createTextNode('');
    initialiseMetadata(empty);
    root.appendChild(empty);
    return none();
  } else if (vdom.children instanceof NonEmpty && vdom.children.tail instanceof Empty) {
    return vdom.children.head;
  } else {
    const head = document.createTextNode('');
    initialiseMetadata(head);
    root.insertBefore(head, root.firstChild);
    return fragment(vdom.children);
  }
}

function virtualise_node(node) {
  switch (node.nodeType) {
    case Node.ELEMENT_NODE: {
      const key = node.getAttribute('data-lustre-key');
      initialiseMetadata(node, key);
 
      const tag = node.localName;
      const namespace = node.namespaceURI;

      const attributes = virtualise_attributes(node);
      const children = virtualise_child_nodes(node);

      const vnode = !namespace || namespace === HTML_NAMESPACE
        ? element(tag, attributes, children)
        : namespaced(namespace, tag, attributes, children);

      return key ? to_keyed(key, vnode) : vnode;
    };

    case Node.TEXT_NODE:
      initialiseMetadata(node);
      return text(node.data);

    case Node.DOCUMENT_FRAGMENT_NODE: // shadowRoot
      initialiseMetadata(node);
      return node.childNodes.length > 0
        ? fragment(virtualise_child_nodes(node))
        : null;

    default:
      return null;
  }
}

function virtualise_child_nodes(node) {
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

function virtualise_attributes(node) {
  let index = node.attributes.length;

  let attributes = empty_list;
  while (index-- > 0) {
    attributes = new NonEmpty(virtualise_attribute(node.attributes[index]), attributes);
  }

  return attributes;
}

function virtualise_attribute(attr) {
  const name = attr.localName;
  const value = attr.value;
  return attribute(name, value);
}
