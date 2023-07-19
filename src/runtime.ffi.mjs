import { h, t } from "./lustre/element.mjs";
import { fold } from "../gleam_stdlib/gleam/list.mjs";

const Element = h("").constructor;
const Text = t("").constructor;

export function morph(prev, curr) {
  if (curr instanceof Element) {
    return prev?.nodeType === 1 && prev.nodeName === curr[0].toUpperCase()
      ? morphElement(prev, curr)
      : createElement(prev, curr);
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

function createElement(prev, curr) {
  const el = document.createElement(curr[0]);

  let attr = curr[1];
  while (attr.head) {
    morphAttr(el, attr.head[0], attr.head[1]);
    attr = attr.tail;
  }

  let child = curr[2];
  while (child.head) {
    el.appendChild(morph(null, child.head));
    child = child.tail;
  }

  if (prev) prev.replaceWith(el);
  return el;
}

function morphElement(prev, curr) {
  const prevAttrs = prev.attributes;
  const currAttrs = new Map();

  let currAttr = curr[1];
  while (currAttr.head) {
    currAttrs.set(currAttr.head[0], currAttr.head[1]);
    currAttr = currAttr.tail;
  }

  for (const { name, value: prevValue } of prevAttrs) {
    if (!currAttrs.has(name)) prev.removeAttribute(name);
    const value = currAttrs.get(name);

    if (value !== prevValue) {
      morphAttr(prev, name, value);
      currAttrs.delete(name);
    }
  }

  for (const [name, value] of currAttrs) {
    morphAttr(prev, name, value);
  }

  let prevChild = prev.firstChild;
  let currChild = curr[2];

  while (prevChild) {
    if (currChild.head) {
      morph(prevChild, currChild.head);
      currChild = currChild.tail;
    }

    prevChild = prevChild.nextSibling;
  }

  while (currChild.head) {
    prev.appendChild(morph(null, currChild.head));
    currChild = currChild.tail;
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

    // For properties, we're leaning on reference equality to avoid unnecessary
    // updates.
    default:
      if (el[name] !== value) el[name] = value;
  }
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
