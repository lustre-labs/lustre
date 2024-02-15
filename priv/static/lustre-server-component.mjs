// build/dev/javascript/lustre/lustre/internals/constants.mjs
var diff = 0;
var emit = 1;
var init = 2;
var event = 4;
var attrs = 5;

// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array, tail) {
    let t = tail || new Empty();
    return array.reduceRight((xs, x) => new NonEmpty(x, xs), t);
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  countLength() {
    let length2 = 0;
    for (let _ of this)
      length2++;
    return length2;
  }
};
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var Result = class _Result extends CustomType {
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  isOk() {
    return false;
  }
};

// build/dev/javascript/gleam_stdlib/dict.mjs
var tempDataView = new DataView(new ArrayBuffer(8));
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map2(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, curr, dispatch, parent) {
  if (curr?.tag && prev?.nodeType === 1) {
    const nodeName = curr.tag.toUpperCase();
    const ns = curr.namespace || "http://www.w3.org/1999/xhtml";
    if (prev.nodeName === nodeName && prev.namespaceURI == ns) {
      return morphElement(prev, curr, dispatch, parent);
    } else {
      return createElement(prev, curr, dispatch, parent);
    }
  }
  if (curr?.tag) {
    return createElement(prev, curr, dispatch, parent);
  }
  if (typeof curr?.content === "string") {
    return prev?.nodeType === 3 ? morphText(prev, curr) : createText(prev, curr);
  }
  return document.createComment(
    [
      "[internal lustre error] I couldn't work out how to render this element. This",
      "function should only be called internally by lustre's runtime: if you think",
      "this is an error, please open an issue at",
      "https://github.com/hayleigh-dot-dev/gleam-lustre/issues/new"
    ].join(" ")
  );
}
function patch(root, diff2, dispatch) {
  for (const created of diff2[0]) {
    const key = created[0];
    if (key === "0") {
      morph(root, created[1], dispatch, root.parentNode);
    } else {
      const segments = Array.from(key);
      const parentKey = segments.slice(0, -1).join("");
      const indexKey = segments.slice(-1)[0];
      const prev = root.querySelector(`[data-lustre-key="${key}"]`) ?? root.querySelector(`[data-lustre-key="${parentKey}"]`).childNodes[indexKey];
      morph(prev, created[1], dispatch, prev.parentNode);
    }
  }
  for (const removed of diff2[1]) {
    const key = removed[0];
    const segments = Array.from(key);
    const parentKey = segments.slice(0, -1).join("");
    const indexKey = segments.slice(-1)[0];
    const prev = root.querySelector(`[data-lustre-key="${key}"]`) ?? root.querySelector(`[data-lustre-key="${parentKey}"]`).childNodes[indexKey];
    prev.remove();
  }
  for (const updated of diff2[2]) {
    const key = updated[0];
    const prev = key === "0" ? root : root.querySelector(`[data-lustre-key="${key}"]`);
    prev.$lustre ??= { __registered_events: /* @__PURE__ */ new Set() };
    for (const created of updated[0]) {
      morphAttr(prev, created.name, created.value, dispatch);
    }
    for (const removed of updated[1]) {
      if (prev.$lustre.__registered_events.has(removed)) {
        const event2 = removed.slice(2).toLowerCase();
        prev.removeEventListener(event2, prev.$lustre[`${removed}Handler`]);
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
function createElement(prev, curr, dispatch, parent = null) {
  const el = curr.namespace ? document.createElementNS(curr.namespace, curr.tag) : document.createElement(curr.tag);
  el.$lustre = {
    __registered_events: /* @__PURE__ */ new Set()
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
  if (prev)
    prev.replaceWith(el);
  return el;
}
function morphElement(prev, curr, dispatch, parent) {
  const prevAttrs = prev.attributes;
  const currAttrs = /* @__PURE__ */ new Map();
  prev.$lustre ??= { __registered_events: /* @__PURE__ */ new Set() };
  for (const currAttr of curr.attrs) {
    if (currAttr[0] === "class" && currAttrs.has("class")) {
      currAttrs.set(currAttr[0], `${currAttrs.get("class")} ${currAttr[1]}`);
    } else if (currAttr[0] === "style" && currAttrs.has("style")) {
      currAttrs.set(currAttr[0], `${currAttrs.get("style")} ${currAttr[1]}`);
    } else if (currAttr[0] === "dangerous-unescaped-html" && currAttrs.has("dangerous-unescaped-html")) {
      currAttrs.set(
        currAttr[0],
        `${currAttrs.get("dangerous-unescaped-html")} ${currAttr[1]}`
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
      const event2 = name.slice(2).toLowerCase();
      prev.removeEventListener(event2, prev.$lustre[`${name}Handler`]);
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
function morphAttr(el, name, value, dispatch) {
  switch (typeof value) {
    case (name.startsWith("data-lustre-on-") && "string"): {
      if (!value) {
        el.removeAttribute(name);
        el.removeEventListener(event2, el.$lustre[`${name}Handler`]);
        break;
      }
      if (el.hasAttribute(name))
        break;
      const event2 = name.slice(15).toLowerCase();
      const handler = (e) => dispatch(serverEventHandler(e));
      if (el.$lustre[`${name}Handler`]) {
        el.removeEventListener(event2, el.$lustre[`${name}Handler`]);
      }
      el.addEventListener(event2, handler);
      el.$lustre[name] = value;
      el.$lustre[`${name}Handler`] = handler;
      el.$lustre.__registered_events.add(name);
      el.setAttribute(name, value);
      break;
    }
    case "string":
      if (name === "value")
        el.value = value;
      if (value === "") {
        el.removeAttribute(name);
      } else {
        el.setAttribute(name, value);
      }
      break;
    case (name.startsWith("on") && "function"): {
      if (el.$lustre[name] === value)
        break;
      const event2 = name.slice(2).toLowerCase();
      const handler = (e) => map2(value(e), dispatch);
      if (el.$lustre[`${name}Handler`]) {
        el.removeEventListener(event2, el.$lustre[`${name}Handler`]);
      }
      el.addEventListener(event2, handler);
      el.$lustre[name] = value;
      el.$lustre[`${name}Handler`] = handler;
      el.$lustre.__registered_events.add(name);
      break;
    }
    default:
      el[name] = value;
  }
}
function createText(prev, curr) {
  const el = document.createTextNode(curr.content);
  if (prev)
    prev.replaceWith(el);
  return el;
}
function morphText(prev, curr) {
  const prevValue = prev.nodeValue;
  const currValue = curr.content;
  if (!currValue) {
    prev?.remove();
    return null;
  }
  if (prevValue !== currValue)
    prev.nodeValue = currValue;
  return prev;
}
function serverEventHandler(event2) {
  const el = event2.target;
  const tag = el.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce((data2, property) => {
      const path = property.split(".");
      for (let i = 0, o = data2, e = event2; i < path.length; i++) {
        if (i === path.length - 1) {
          o[path[i]] = e[path[i]];
        } else {
          o[path[i]] ??= {};
          e = e[path[i]];
          o = o[path[i]];
        }
      }
      return data2;
    }, data)
  };
}

// src/server-component.mjs
var LustreServerComponent = class extends HTMLElement {
  static get observedAttributes() {
    return ["route"];
  }
  #observer = null;
  #root = null;
  #socket = null;
  constructor() {
    super();
    this.#observer = new MutationObserver((mutations) => {
      const changed = [];
      for (const mutation of mutations) {
        if (mutation.type === "attributes") {
          const { attributeName: name, oldValue: prev } = mutation;
          const next = this.getAttribute(name);
          if (prev !== next) {
            try {
              changed.push([name, JSON.parse(next)]);
            } catch {
              changed.push([name, next]);
            }
          }
        }
      }
      if (changed.length) {
        this.#socket?.send(JSON.stringify([attrs, changed]));
      }
    });
  }
  connectedCallback() {
    this.#root = document.createElement("div");
    this.appendChild(this.#root);
  }
  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case "route": {
        if (!next) {
          this.#socket?.close();
          this.#socket = null;
        } else if (prev !== next) {
          const id = this.getAttribute("id");
          const route = next + (id ? `?id=${id}` : "");
          this.#socket?.close();
          this.#socket = new WebSocket(`ws://${window.location.host}${route}`);
          this.#socket.addEventListener(
            "message",
            (message) => this.messageReceivedCallback(message)
          );
        }
      }
    }
  }
  messageReceivedCallback({ data }) {
    const [kind, ...payload] = JSON.parse(data);
    switch (kind) {
      case diff:
        return this.diff(payload);
      case emit:
        return this.emit(payload);
      case init:
        return this.init(payload);
    }
  }
  init([attrs2, vdom]) {
    const initial = [];
    for (const attr of attrs2) {
      if (attr in this) {
        initial.push([attr, this[attr]]);
      } else if (this.hasAttribute(attr)) {
        initial.push([attr, this.getAttribute(attr)]);
      }
      Object.defineProperty(this, attr, {
        get() {
          return this[`_${attr}`] ?? this.getAttribute(attr);
        },
        set(value) {
          const prev = this[attr];
          if (typeof value === "string") {
            this.setAttribute(attr, value);
          } else {
            this[`_${attr}`] = value;
          }
          if (prev !== value) {
            this.#socket?.send(
              JSON.stringify([attrs, [[attr, value]]])
            );
          }
        }
      });
    }
    this.#observer.observe(this, {
      attributeFilter: attrs2,
      attributeOldValue: true,
      attributes: true,
      characterData: false,
      characterDataOldValue: false,
      childList: false,
      subtree: false
    });
    this.morph(vdom);
    if (initial.length) {
      this.#socket?.send(JSON.stringify([attrs, initial]));
    }
  }
  morph(vdom) {
    this.#root = morph(this.#root, vdom, (msg) => {
      this.#socket?.send(JSON.stringify([event, msg.tag, msg.data]));
    });
  }
  diff([diff2]) {
    this.#root = patch(this.#root, diff2, (msg) => {
      this.#socket?.send(JSON.stringify([event, msg.tag, msg.data]));
    });
  }
  emit([event2, data]) {
    this.dispatchEvent(new CustomEvent(event2, { detail: data }));
  }
  disconnectedCallback() {
    this.#socket?.close();
  }
};
window.customElements.define("lustre-server-component", LustreServerComponent);
export {
  LustreServerComponent
};
