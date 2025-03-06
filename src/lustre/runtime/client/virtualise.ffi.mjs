import { Empty, NonEmpty } from '../../../gleam.mjs';
import { element, namespaced, fragment, text, none } from '../../element.mjs';
import { attribute } from '../../attribute.mjs';
import { empty_list } from '../../internals/constants.mjs';

const HTML_NAMESPACE = "http://www.w3.org/1999/xhtml";

export function virtualise(root) {
  const vdom = virtualise_node(root);
  // at this point we know the element is empty - but we have to have at least
  // an empty text node child in the root element to be able to mount
  if (vdom === null || vdom.children instanceof Empty) {
    root.appendChild(document.createTextNode(''));
    return none();
  } else if (vdom.children instanceof NonEmpty && vdom.children.tail instanceof Empty) {
    return vdom.children.head;
  } else {
    return fragment(vdom.children);
  }
}

function virtualise_node(node) {
  switch (node.nodeType) {
    case Node.ELEMENT_NODE: {
      const tag = node.localName;
      const namespace = node.namespaceURI;

      const attributes = virtualise_attributes(node);
      const children = virtualise_child_nodes(node);

      return !namespace || namespace === HTML_NAMESPACE
        ? element(tag, attributes, children)
        : namespaced(namespace, tag, attributes, children);
    };

    case Node.TEXT_NODE:
      return text(node.data);

    case Node.DOCUMENT_FRAGMENT_NODE: // shadowRoot
      return node.childNodes.length > 0
        ? fragment(virtualise_child_nodes(node))
        : null;

    default:
      return null;
  }
}

function virtualise_child_nodes(node) {
  let index = node.childNodes.length;

  let children = empty_list;
  const nodesToRemove = [];
  while (index-- > 0) {
    const child = virtualise_node(node.childNodes[index]);
    if (child) {
      children = new NonEmpty(child, children);
    } else {
      nodesToRemove.push(node.childNodes[index]);
    }
  }

  // we have to remove all nodes we cannot virtualise to make sure the indices
  // used in the patch line up.
  for (const child of nodesToRemove) {
    node.removeChild(child);
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
