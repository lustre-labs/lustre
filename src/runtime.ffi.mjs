import { Empty } from "./gleam.mjs";
import { map as result_map } from "../gleam_stdlib/gleam/result.mjs";

export function morph(prev, curr, dispatch, parent) {
  if (curr[3]) {
    return prev?.nodeType === 1 &&
      prev.nodeName === curr[0].toUpperCase() &&
      prev.namespaceURI === curr[3]
      ? morphElement(prev, curr, curr[3], dispatch, parent)
      : createElement(prev, curr, curr[3], dispatch, parent);
  }

  if (curr[2]) {
    return prev?.nodeType === 1 && prev.nodeName === curr[0].toUpperCase()
      ? morphElement(prev, curr, null, dispatch, parent)
      : createElement(prev, curr, null, dispatch, parent);
  }

  if (typeof curr?.[0] === "string") {
    return prev?.nodeType === 3
      ? morphText(prev, curr)
      : createText(prev, curr);
  }

  return document.createComment(
    [
      "[internal lustre error] I couldn't work out how to render this element. This",
      "function should only be called internally by lustre's runtime: if you think",
      "this is an error, please open an issue at",
      "https://github.com/hayleigh-dot-dev/gleam-lustre/issues/new",
    ].join(" ")
  );
}

// ELEMENTS --------------------------------------------------------------------

function createElement(prev, curr, ns, dispatch, parent = null) {
  const el = ns
    ? document.createElementNS(ns, curr[0])
    : document.createElement(curr[0]);

  el.$lustre = {};

  let attr = curr[1];
  while (attr.head) {
    if (attr.head[0] === "class") {
      morphAttr(el, attr.head[0], `${el.className} ${attr.head[1]}`);
    } else if (attr.head[0] === "style") {
      morphAttr(el, attr.head[0], `${el.style.cssText} ${attr.head[1]}`);
    } else {
      morphAttr(el, attr.head[0], attr.head[1], dispatch);
    }

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
      el.appendChild(morph(null, child.head, dispatch, el));
      child = child.tail;
    }
  } else {
    let child = curr[2];
    while (child.head) {
      el.appendChild(morph(null, child.head, dispatch, el));
      child = child.tail;
    }
  }

  if (prev) prev.replaceWith(el);

  return el;
}

function morphElement(prev, curr, ns, dispatch, parent) {
  const prevAttrs = prev.attributes;
  const currAttrs = new Map();

  // This can happen if we're morphing an existing DOM element that *wasn't*
  // initially created by lustre.
  prev.$lustre ??= {};

  let currAttr = curr[1];
  while (currAttr.head) {
    if (currAttr.head[0] === "class" && currAttrs.has("class")) {
      currAttrs.set(
        currAttr.head[0],
        `${currAttrs.get("class")} ${currAttr.head[1]}`
      );
    } else if (currAttr.head[0] === "style" && currAttrs.has("style")) {
      currAttrs.set(
        currAttr.head[0],
        `${currAttrs.get("style")} ${currAttr.head[1]}`
      );
    } else {
      currAttrs.set(currAttr.head[0], currAttr.head[1]);
    }

    currAttr = currAttr.tail;
  }

  for (const { name, value: prevValue } of prevAttrs) {
    if (!currAttrs.has(name)) {
      prev.removeAttribute(name);
    } else {
      const value = currAttrs.get(name);

      if (value !== prevValue) {
        morphAttr(prev, name, value, dispatch);
        currAttrs.delete(name);
      }
    }
  }

  for (const [name, value] of currAttrs) {
    morphAttr(prev, name, value, dispatch);
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
        morph(prevChild, currChild.head, dispatch, prev);
        currChild = currChild.tail;
      }

      prevChild = prevChild.nextSibling;
    }

    while (currChild.head) {
      prev.appendChild(morph(null, currChild.head, dispatch, prev));
      currChild = currChild.tail;
    }
  } else {
    let prevChild = prev.firstChild;
    let currChild = curr[2];

    while (prevChild) {
      if (currChild.head) {
        const next = prevChild.nextSibling;
        morph(prevChild, currChild.head, dispatch, prev);
        currChild = currChild.tail;
        prevChild = next;
      } else {
        const next = prevChild.nextSibling;
        prevChild.remove();
        prevChild = next;
      }
    }

    while (currChild.head) {
      prev.appendChild(morph(null, currChild.head, dispatch, prev));
      currChild = currChild.tail;
    }
  }

  return prev;
}

// ATTRIBUTES ------------------------------------------------------------------

function morphAttr(el, name, value, dispatch) {
  switch (typeof value) {
    case "string":
      if (el.getAttribute(name) !== value) el.setAttribute(name, value);
      if (value === "") el.removeAttribute(name);
      if (name === "value" && el.value !== value) el.value = value;
      break;

    // Event listeners need to be handled slightly differently because we need
    // to be able to support custom events. We
    case name.startsWith("on") && "function": {
      if (el.$lustre[name] === value) break;

      const event = name.slice(2).toLowerCase();
      const handler = (e) => result_map(value(e), dispatch);

      if (el.$lustre[`${name}Handler`]) {
        el.removeEventListener(event, el.$lustre[`${name}Handler`]);
      }

      el.addEventListener(event, handler);

      el.$lustre[name] = value;
      el.$lustre[`${name}Handler`] = handler;

      break;
    }

    default:
      el[name] = value;
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

  if (!currValue) {
    prev?.remove();
    return null;
  }

  if (prevValue !== currValue) prev.nodeValue = currValue;

  return prev;
}
