// build/dev/javascript/lustre/lustre/internals/constants.mjs
var diff = 0;
var emit = 1;
var init = 2;
var event = 4;
var attrs = 5;

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch, isComponent = false) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    if (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack,
        isComponent
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    }
  }
  return out;
}
function patch(root, diff2, dispatch) {
  const rootParent = root.parentNode;
  for (const created of diff2[0]) {
    const key = created[0].split("-");
    const next = created[1];
    const prev = getDeepChild(rootParent, key);
    let result;
    if (prev !== null && prev !== rootParent) {
      result = morph(prev, next, dispatch);
    } else {
      const parent = getDeepChild(rootParent, key.slice(0, -1));
      const temp = document.createTextNode("");
      parent.appendChild(temp);
      result = morph(temp, next, dispatch);
    }
    if (key === "0") {
      root = result;
    }
  }
  for (const removed of diff2[1]) {
    const key = removed[0].split("-");
    const deletedNode = getDeepChild(rootParent, key);
    deletedNode.remove();
  }
  for (const updated of diff2[2]) {
    const key = updated[0].split("-");
    const patches = updated[1];
    const prev = getDeepChild(rootParent, key);
    const handlersForEl = registeredHandlers.get(prev);
    for (const created of patches[0]) {
      const name = created[0];
      const value = created[1];
      if (name.startsWith("data-lustre-on-")) {
        const eventName = name.slice(15);
        const callback = dispatch(lustreServerEventHandler);
        if (!handlersForEl.has(eventName)) {
          el.addEventListener(eventName, lustreGenericEventHandler);
        }
        handlersForEl.set(eventName, callback);
        el.setAttribute(name, value);
      } else {
        prev.setAttribute(name, value);
        prev[name] = value;
      }
    }
    for (const removed of patches[1]) {
      if (removed[0].startsWith("data-lustre-on-")) {
        const eventName = removed[0].slice(15);
        prev.removeEventListener(eventName, lustreGenericEventHandler);
        handlersForEl.delete(eventName);
      } else {
        prev.removeAttribute(removed[0]);
      }
    }
  }
  return root;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style = null;
  let innerHTML = null;
  for (const attr of next.attrs) {
    const name = attr[0];
    const value = attr[1];
    const isProperty = attr[2];
    if (isProperty) {
      el2[name] = value;
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value);
    } else if (name === "class") {
      className = className === null ? value : className + " " + value;
    } else if (name === "style") {
      style = style === null ? value : style + value;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value;
    } else {
      el2.setAttribute(name, value);
      if (name === "value")
        el2[name] = value;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style !== null) {
    el2.setAttribute("style", style);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;
  if (firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    if (child.key !== void 0 && seenKeys !== null) {
      while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
        const nextChild = prevChild.nextSibling;
        el2.removeChild(prevChild);
        prevChild = nextChild;
      }
      if (keyedChildren.size === 0) {
        stack.unshift({ prev: prevChild, next: child, parent: el2 });
        prevChild = prevChild?.nextSibling;
        continue;
      }
      if (seenKeys.has(child.key)) {
        console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
        stack.unshift({ prev: null, next: child, parent: el2 });
        continue;
      }
      seenKeys.add(child.key);
      const keyedChild = keyedChildren.get(child.key);
      if (!keyedChild && !prevChild) {
        stack.unshift({ prev: null, next: child, parent: el2 });
        continue;
      }
      if (!keyedChild && prevChild !== null) {
        const placeholder = document.createTextNode("");
        el2.insertBefore(placeholder, prevChild);
        stack.unshift({ prev: placeholder, next: child, parent: el2 });
        continue;
      }
      if (!keyedChild || keyedChild === prevChild) {
        stack.unshift({ prev: prevChild, next: child, parent: el2 });
        prevChild = prevChild?.nextSibling;
        continue;
      }
      el2.insertBefore(keyedChild, prevChild);
      stack.unshift({ prev: keyedChild, next: child, parent: el2 });
    } else {
      stack.unshift({ prev: prevChild, next: child, parent: el2 });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target = event2.currentTarget;
  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target);
  if (!handlersForEventTarget.has(event2.type)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.target;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property) => {
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
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of el2.children) {
      const key = child.key || child?.getAttribute("data-lustre-key");
      if (key)
        keyedChildren.set(key, child);
    }
  }
  return keyedChildren;
}
function getDeepChild(el2, path) {
  let n;
  let rest;
  let child = el2;
  while ([n, ...rest] = path, n !== void 0) {
    child = child.childNodes.item(n);
    path = rest;
  }
  return child;
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
    this.#root = morph(this.#root, vdom, (handler) => (event2) => {
      const msg = handler(event2);
      this.#socket?.send(JSON.stringify([event, msg.tag, msg.data]));
    });
  }
  diff([diff2]) {
    this.#root = patch(this.#root, diff2, (handler) => (event2) => {
      const msg = handler(event2);
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
