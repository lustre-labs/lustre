import { h, t } from "./lustre/element.mjs";
import { fold, each } from "../gleam_stdlib/gleam/list.mjs";

const Element = h("").constructor;
const Text = t("").constructor;

export function morph(prev, curr) {
  if (curr instanceof Element) {
    if (prev?.nodeType === 1 && prev.nodeName !== curr[0].toUpperCase()) {
      return morphElement(prev, curr);
    } else {
      const el = document.createElement(curr[0]);

      each(curr[1], (attr) => {
        const name = attr[0];
        const value = attr[1];

        morphAttr(el, name, value);
      });

      each(curr[2], (child) => {
        el.appendChild(morph(null, child));
      });

      if (prev) prev.replaceWith(el);

      return el;
    }
  }

  if (curr instanceof Text) {
    if (prev?.nodeType === 3) {
      return morphText(prev, curr);
    } else {
      const el = document.createTextNode(curr[0]);
      if (prev) prev.replaceWith(el);
      return el;
    }
  }

  return null;
}

function morphElement(prev, curr) {
  const prevAttrs = prev.attributes;
  const currAttrs = fold(curr[1], new Map(), (acc, attr) => {
    const name = attr[0];
    const value = attr[1];

    acc.set(name, value);
    return acc;
  });
  const currChildren = curr[2].toArray();

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

  for (let child = prev.firstChild; child; child = child.nextSibling) {
    if (currChildren.length) {
      morph(child, currChildren.shift());
    } else {
      prev.removeChild(child);
    }
  }

  for (const child of currChildren) {
    prev.appendChild(morph(null, child));
  }

  return prev;
}

function morphText(prev, curr) {
  if (prev.nodeValue !== curr[0]) prev.nodeValue = curr[0];
  return prev;
}

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
