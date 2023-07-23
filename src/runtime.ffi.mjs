import { element, namespaced, text } from "./lustre/element.mjs";
import { List, Empty } from "./gleam.mjs";
import { Some, None } from "../gleam_stdlib/gleam/option.mjs";

const Element = element("").constructor;
const ElementNs = namespaced("", "").constructor;
const Text = text("").constructor;

export function morph(prev, curr, parent) {
  if (curr instanceof ElementNs)
    return prev?.nodeType === 1 &&
      prev.nodeName === curr[0].toUpperCase() &&
      prev.namespaceURI === curr[3]
      ? morphElement(prev, curr, curr[3], parent)
      : createElement(prev, curr, curr[3], parent);

  if (curr instanceof Element) {
    return prev?.nodeType === 1 && prev.nodeName === curr[0].toUpperCase()
      ? morphElement(prev, curr, null, parent)
      : createElement(prev, curr, null, parent);
  }

  if (curr instanceof Text) {
    return prev?.nodeType === 3
      ? morphText(prev, curr)
      : createText(prev, curr);
  }

  return document.createComment(
    [
      "[internal lustre error] I couldn't work out how to render this element. This",
      "function should only be called internally by lustre's runtime: if you think",
      "this is an error, please open an issue at",
      "https://github.come/hayleigh-dot-dev/lustre/issues/new.",
    ].join(" ")
  );
}

// ELEMENTS --------------------------------------------------------------------

function createElement(prev, curr, ns, parent = null) {
  const el = ns
    ? document.createElementNS(ns, curr[0])
    : document.createElement(curr[0]);

  let attr = curr[1];
  while (attr.head) {
    morphAttr(el, attr.head[0], attr.head[1]);
    attr = attr.tail;
  }

  if (customElements.get(curr[0])) {
    el._slot = curr[2];
  } else if (curr[0] === "slot") {
    let child = new Empty();
    let parentWithSlot = parent;

    while (parentWithSlot) {
      if (parentWithSlot._slot) {
        child = parentWithSlot._slot;
        break;
      } else {
        parentWithSlot = parentWithSlot.parentNode;
      }
    }

    while (child.head) {
      el.appendChild(morph(null, child.head, el));
      child = child.tail;
    }
  } else {
    let child = curr[2];
    while (child.head) {
      el.appendChild(morph(null, child.head, el));
      child = child.tail;
    }

    if (prev) prev.replaceWith(el);
  }
  return el;
}

function morphElement(prev, curr, ns, parent) {
  const prevAttrs = prev.attributes;
  const currAttrs = new Map();

  let currAttr = curr[1];
  while (currAttr.head) {
    currAttrs.set(currAttr.head[0], currAttr.head[1]);
    currAttr = currAttr.tail;
  }

  for (const { name, value: prevValue } of prevAttrs) {
    if (!currAttrs.has(name)) {
      prev.removeAttribute(name);
    } else {
      const value = currAttrs.get(name);

      if (value !== prevValue) {
        morphAttr(prev, name, value);
        currAttrs.delete(name);
      }
    }
  }

  for (const [name, value] of currAttrs) {
    morphAttr(prev, name, value);
  }

  if (customElements.get(curr[0])) {
    prev._slot = curr[2];
  } else if (curr[0] === "slot") {
    let prevChild = prev.firstChild;
    let currChild = new Empty();
    let parentWithSlot = parent;

    while (parentWithSlot) {
      if (parentWithSlot._slot) {
        currChild = parentWithSlot._slot;
        break;
      } else {
        parentWithSlot = parentWithSlot.parentNode;
      }
    }

    while (prevChild) {
      if (currChild.head) {
        morph(prevChild, currChild.head, prev);
        currChild = currChild.tail;
      }

      prevChild = prevChild.nextSibling;
    }

    while (currChild.head) {
      prev.appendChild(morph(null, currChild.head, prev));
      currChild = currChild.tail;
    }
  } else {
    let prevChild = prev.firstChild;
    let currChild = curr[2];

    while (prevChild) {
      if (currChild.head) {
        morph(prevChild, currChild.head, prev);
        currChild = currChild.tail;
        prevChild = prevChild.nextSibling;
      } else {
        const next = prevChild.nextSibling;
        prevChild.remove();
        prevChild = next;
      }
    }

    while (currChild.head) {
      prev.appendChild(morph(null, currChild.head, prev));
      currChild = currChild.tail;
    }
  }

  return prev;
}

// ATTRIBUTES ------------------------------------------------------------------

function morphAttr(el, name, value) {
  switch (typeof value) {
    case "string":
      el.setAttribute(name, value);
      break;

    // Boolean attributes work a bit differently in HTML. Their presence always
    // implies true: to set an attribute to false you need to remove it entirely.
    case "boolean":
      value ? el.setAttribute(name, name) : el.removeAttribute(name);
      break;

    // Event listeners need to be handled slightly differently because we need
    // to be able to support custom events. We
    case name.startsWith("on") && "function": {
      const event = name.slice(2).toLowerCase();

      if (el[`_${name}`] === value) break;

      el.removeEventListener(event, el[`_${name}`]);
      el.addEventListener(event, value);
      el[`_${name}`] = value;
      break;
    }

    default: {
      el[name] = toJsValue(value);
    }
  }
}

function toJsValue(value) {
  if (value instanceof List) {
    return value.toArray().map(toJsValue);
  }

  if (value instanceof Some) {
    return toJsValue(value[0]);
  }

  if (value instanceof None) {
    return null;
  }

  return value;
}

// TEXT ------------------------------------------------------------------------

function createText(prev, curr) {
  const el = document.createTextNode(curr[0]);

  if (prev) prev.replaceWith(el);
  return el;
}

function morphText(prev, curr) {
  const prevValue = prev.nodeValue;
  const currValue = curr[0];

  if (prevValue !== currValue) prev.nodeValue = currValue;

  return prev;
}
