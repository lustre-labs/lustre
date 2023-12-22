import { Empty } from "./gleam.mjs";
import { map as result_map } from "../gleam_stdlib/gleam/result.mjs";

export function morph(prev, curr, dispatch, parent) {
  // The current node is an `Element` and the previous DOM node is also a DOM
  // element.
  if (curr?.tag && prev?.nodeType === 1) {
    const nodeName = curr.tag.toUpperCase();
    const ns = curr.namespace || null;

    // If the current node and the existing DOM node have the same tag and
    // namespace, we can morph them together: keeping the DOM node intact and just
    // updating its attributes and children.
    if (prev.nodeName === nodeName && prev.namespaceURI === ns) {
      return morphElement(prev, curr, dispatch, parent);
    }
    // Otherwise, we need to replace the DOM node with a new one. The `createElement`
    // function will handle replacing the existing DOM node for us.
    else {
      return createElement(prev, curr, dispatch, parent);
    }
  }

  // The current node is an `Element` but the previous DOM node either did not
  // exist or it is not a DOM element (eg it might be a text or comment node).
  if (curr?.tag) {
    return createElement(prev, curr, dispatch, parent);
  }

  // The current node is a `Text`.
  if (typeof curr?.content === "string") {
    return prev?.nodeType === 3
      ? morphText(prev, curr)
      : createText(prev, curr);
  }

  // If someone was naughty and tried to pass in something other than a Lustre
  // element (or if there is an actual bug with the runtime!) we'll render a
  // comment and ask them to report the issue.
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

function createElement(prev, curr, dispatch, parent = null) {
  const el = curr.namespace
    ? document.createElementNS(curr.namespace, curr.tag)
    : document.createElement(curr.tag);

  el.$lustre = {
    __registered_events: new Set(),
  };

  let attr = curr.attrs;
  let dangerousUnescapedHtml = "";

  while (attr.head) {
    if (attr.head[0] === "class") {
      morphAttr(el, attr.head[0], `${el.className} ${attr.head[1]}`);
    } else if (attr.head[0] === "style") {
      morphAttr(el, attr.head[0], `${el.style.cssText} ${attr.head[1]}`);
    } else if (attr.head[0] === "dangerous-unescaped-html") {
      dangerousUnescapedHtml += attr.head[1];
    } else if (attr.head[0] !== "") {
      morphAttr(el, attr.head[0], attr.head[1], dispatch);
    }

    attr = attr.tail;
  }

  if (customElements.get(curr.tag)) {
    el._slot = curr.children;
  } else if (curr.tag === "slot") {
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
  } else if (dangerousUnescapedHtml) {
    el.innerHTML = dangerousUnescapedHtml;
  } else {
    let child = curr.children;
    while (child.head) {
      el.appendChild(morph(null, child.head, dispatch, el));
      child = child.tail;
    }
  }

  if (prev) prev.replaceWith(el);

  return el;
}

function morphElement(prev, curr, dispatch, parent) {
  const prevAttrs = Object.fromEntries(prev.attributes);
  const currAttrs = new Map();

  // This can happen if we're morphing an existing DOM element that *wasn't*
  // initially created by lustre.
  prev.$lustre ??= { __registered_events: new Set() };

  // We're going to convert the Gleam List of attributes into a JavaScript Map
  // so its easier to lookup specific attributes.
  let currAttr = curr.attrs;
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
    } else if (
      currAttr.head[0] === "dangerous-unescaped-html" &&
      currAttrs.has("dangerous-unescaped-html")
    ) {
      currAttrs.set(
        currAttr.head[0],
        `${currAttrs.get("dangerous-unescaped-html")} ${currAttr.head[1]}`
      );
    } else if (currAttr.head[0] !== "") {
      currAttrs.set(currAttr.head[0], currAttr.head[1]);
    }

    currAttr = currAttr.tail;
  }

  // TODO: Event listeners aren't currently removed when they are removed from
  // the attributes list. This is a bug!
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

  for (const name of prev.$lustre.__registered_events) {
    if (!currAttrs.has(name)) {
      const event = name.slice(2).toLowerCase();

      prev.removeEventListener(event, prev.$lustre[`${name}Handler`]);
      prev.$lustre.__registered_events.delete(name);

      delete prev.$lustre[name];
      delete prev.$lustre[`${name}Handler`];
    }
  }

  for (const [name, value] of currAttrs) {
    morphAttr(prev, name, value, dispatch);
  }

  if (customElements.get(curr.tag)) {
    prev._slot = curr.children;
  } else if (curr.tag === "slot") {
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
  } else if (currAttrs.has("dangerous-unescaped-html")) {
    prev.innerHTML = currAttrs.get("dangerous-unescaped-html");
  } else {
    let prevChild = prev.firstChild;
    let currChild = curr.children;

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
      el.$lustre.__registered_events.add(name);

      break;
    }

    default:
      el[name] = value;
  }
}

// TEXT ------------------------------------------------------------------------

function createText(prev, curr) {
  const el = document.createTextNode(curr.content);

  if (prev) prev.replaceWith(el);
  return el;
}

function morphText(prev, curr) {
  const prevValue = prev.nodeValue;
  const currValue = curr.text;

  if (!currValue) {
    prev?.remove();
    return null;
  }

  if (prevValue !== currValue) prev.nodeValue = currValue;

  return prev;
}
