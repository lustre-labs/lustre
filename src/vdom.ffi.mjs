import { Empty } from "./gleam.mjs";
import { map as result_map } from "../gleam_stdlib/gleam/result.mjs";

export function morph(prev, curr, dispatch, parent) {
  // The current node is an `Element` and the previous DOM node is also a DOM
  // element.
  if (curr?.tag && prev?.nodeType === 1) {
    const nodeName = curr.tag.toUpperCase();
    const ns = curr.namespace || "http://www.w3.org/1999/xhtml";

    // If the current node and the existing DOM node have the same tag and
    // namespace, we can morph them together: keeping the DOM node intact and just
    // updating its attributes and children.
    if (prev.nodeName === nodeName && prev.namespaceURI == ns) {
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
    ].join(" "),
  );
}

export function patch(root, diff, dispatch) {
  for (const created of diff[0]) {
    const key = created[0];

    if (key === "0") {
      morph(root, created[1], dispatch, root.parentNode);
    } else {
      const segments = Array.from(key);
      const parentKey = segments.slice(0, -1).join("");
      const indexKey = segments.slice(-1)[0];
      const prev =
        root.querySelector(`[data-lustre-key="${key}"]`) ??
        root.querySelector(`[data-lustre-key="${parentKey}"]`).childNodes[
          indexKey
        ];

      morph(prev, created[1], dispatch, prev.parentNode);
    }
  }

  for (const removed of diff[1]) {
    const key = removed[0];
    const segments = Array.from(key);
    const parentKey = segments.slice(0, -1).join("");
    const indexKey = segments.slice(-1)[0];
    const prev =
      root.querySelector(`[data-lustre-key="${key}"]`) ??
      root.querySelector(`[data-lustre-key="${parentKey}"]`).childNodes[
        indexKey
      ];

    prev.remove();
  }

  for (const updated of diff[2]) {
    const key = updated[0];
    const prev =
      key === "0" ? root : root.querySelector(`[data-lustre-key="${key}"]`);

    prev.$lustre ??= { __registered_events: new Set() };

    for (const created of updated[0]) {
      morphAttr(prev, created.name, created.value, dispatch);
    }

    for (const removed of updated[1]) {
      if (prev.$lustre.__registered_events.has(removed)) {
        const event = removed.slice(2).toLowerCase();

        prev.removeEventListener(event, prev.$lustre[`${removed}Handler`]);
        prev.$lustre.__registered_events.delete(removed);

        delete prev.$lustre[removed];
        delete prev.$lustre[`${removed}Handler`];
      } else {
        prev.removeAttribute(removed);
      }
    }
  }

  return root;
}

// ELEMENTS --------------------------------------------------------------------

function createElement(prev, curr, dispatch, parent = null) {
  const el = curr.namespace
    ? document.createElementNS(curr.namespace, curr.tag)
    : document.createElement(curr.tag);

  el.$lustre = {
    __registered_events: new Set(),
  };

  let dangerousUnescapedHtml = "";

  for (const attr of curr.attrs) {
    if (attr[0] === "class") {
      morphAttr(el, attr[0], `${el.className} ${attr[1]}`);
    } else if (attr[0] === "style") {
      morphAttr(el, attr[0], `${el.style.cssText} ${attr[1]}`);
    } else if (attr[0] === "dangerous-unescaped-html") {
      dangerousUnescapedHtml += attr[1];
    } else if (attr[0] !== "") {
      morphAttr(el, attr[0], attr[1], dispatch);
    }
  }

  if (customElements.get(curr.tag)) {
    el._slot = curr.children;
  } else if (curr.tag === "slot") {
    let children = new Empty();
    let parentWithSlot = parent;

    while (parentWithSlot) {
      if (parentWithSlot._slot) {
        children = parentWithSlot._slot;
        break;
      } else {
        parentWithSlot = parentWithSlot.parentNode;
      }
    }

    for (const child of children) {
      el.appendChild(morph(null, child, dispatch, el));
    }
  } else if (dangerousUnescapedHtml) {
    el.innerHTML = dangerousUnescapedHtml;
  } else {
    for (const child of curr.children) {
      el.appendChild(morph(null, child, dispatch, el));
    }
  }

  if (prev) prev.replaceWith(el);

  return el;
}

function morphElement(prev, curr, dispatch, parent) {
  const prevAttrs = prev.attributes;
  const currAttrs = new Map();

  // This can happen if we're morphing an existing DOM element that *wasn't*
  // initially created by lustre.
  prev.$lustre ??= { __registered_events: new Set() };

  // We're going to convert the Gleam List of attributes into a JavaScript Map
  // so its easier to lookup specific attributes.
  for (const currAttr of curr.attrs) {
    if (currAttr[0] === "class" && currAttrs.has("class")) {
      currAttrs.set(currAttr[0], `${currAttrs.get("class")} ${currAttr[1]}`);
    } else if (currAttr[0] === "style" && currAttrs.has("style")) {
      currAttrs.set(currAttr[0], `${currAttrs.get("style")} ${currAttr[1]}`);
    } else if (
      currAttr[0] === "dangerous-unescaped-html" &&
      currAttrs.has("dangerous-unescaped-html")
    ) {
      currAttrs.set(
        currAttr[0],
        `${currAttrs.get("dangerous-unescaped-html")} ${currAttr[1]}`,
      );
    } else if (currAttr[0] !== "") {
      currAttrs.set(currAttr[0], currAttr[1]);
    }
  }

  for (const { name } of prevAttrs) {
    if (!currAttrs.has(name)) {
      prev.removeAttribute(name);
    } else {
      const value = currAttrs.get(name);

      morphAttr(prev, name, value, dispatch);
      currAttrs.delete(name);
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
      if (Array.isArray(currChild) && currChild.length) {
        morph(prevChild, currChild.shift(), dispatch, prev);
      } else if (currChild.head) {
        morph(prevChild, currChild.head, dispatch, prev);
        currChild = currChild.tail;
      }

      prevChild = prevChild.nextSibling;
    }

    for (const child of currChild) {
      prev.appendChild(morph(null, child, dispatch, prev));
    }
  } else if (currAttrs.has("dangerous-unescaped-html")) {
    prev.innerHTML = currAttrs.get("dangerous-unescaped-html");
  } else {
    let prevChild = prev.firstChild;
    let currChild = curr.children;

    while (prevChild) {
      if (Array.isArray(currChild) && currChild.length) {
        const next = prevChild.nextSibling;
        morph(prevChild, currChild.shift(), dispatch, prev);
        prevChild = next;
      } else if (currChild.head) {
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

    for (const child of currChild) {
      prev.appendChild(morph(null, child, dispatch, prev));
    }
  }

  return prev;
}

// ATTRIBUTES ------------------------------------------------------------------

function morphAttr(el, name, value, dispatch) {
  switch (typeof value) {
    case name.startsWith("data-lustre-on-") && "string": {
      if (!value) {
        el.removeAttribute(name);
        el.removeEventListener(event, el.$lustre[`${name}Handler`]);

        break;
      }
      if (el.hasAttribute(name)) break;

      const event = name.slice(15).toLowerCase();
      const handler = (e) => dispatch(serverEventHandler(e));

      if (el.$lustre[`${name}Handler`]) {
        el.removeEventListener(event, el.$lustre[`${name}Handler`]);
      }

      el.addEventListener(event, handler);

      el.$lustre[name] = value;
      el.$lustre[`${name}Handler`] = handler;
      el.$lustre.__registered_events.add(name);
      el.setAttribute(name, value);

      break;
    }

    case "string":
      if (name === "value") el.value = value;
      if (value === "") {
        el.removeAttribute(name);
      } else {
        el.setAttribute(name, value);
      }

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
  const currValue = curr.content;

  if (!currValue) {
    prev?.remove();
    return null;
  }

  if (prevValue !== currValue) prev.nodeValue = currValue;

  return prev;
}

// UTILS -----------------------------------------------------------------------

function serverEventHandler(event) {
  const el = event.target;
  const tag = el.getAttribute(`data-lustre-on-${event.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");

  switch (event.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }

  return {
    tag,
    data: include.reduce((data, property) => {
      const path = property.split(".");

      for (let i = 0, o = data, e = event; i < path.length; i++) {
        if (i === path.length - 1) {
          o[path[i]] = e[path[i]];
        } else {
          o[path[i]] ??= {};
          e = e[path[i]];
          o = o[path[i]];
        }
      }

      return data;
    }, data),
  };
}
