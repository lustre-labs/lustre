// build/dev/javascript/gleam_stdlib/dict.mjs
var tempDataView = new DataView(new ArrayBuffer(8));
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var unequalDictSymbol = Symbol();

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join("");
var trim_start_regex = new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = new RegExp(`[${unicode_whitespaces}]*$`);

// build/dev/javascript/lustre/lustre/vdom/attribute.mjs
var attribute_kind = 0;
var property_kind = 1;
var event_kind = 2;

// build/dev/javascript/lustre/lustre/vdom/node.mjs
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var unsafe_inner_html_kind = 3;

// build/dev/javascript/lustre/lustre/vdom/patch.mjs
var replace_text_kind = 0;
var replace_inner_html_kind = 1;
var update_kind = 2;
var move_kind = 3;
var remove_key_kind = 4;
var replace_kind = 5;
var insert_kind = 6;
var remove_kind = 7;

// build/dev/javascript/lustre/lustre/runtime/client/reconciler.ffi.mjs
var SUPPORTS_MOVE_BEFORE = !!HTMLElement.prototype.moveBefore;
var Reconciler = class {
  #root = null;
  #dispatch = () => {
  };
  #useServerEvents = false;
  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
  }
  mount(vdom) {
    this.#root.appendChild(this.#createElement(vdom));
  }
  #stack = [];
  push(patch, offset = 0) {
    if (offset) {
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
          case move_kind:
            change.before = (change.before | 0) + offset;
            break;
          case remove_kind:
          case replace_kind:
            change.from = (change.from | 0) + offset;
            break;
        }
      });
      iterate(patch.children, (child) => {
        child.index = (child.index | 0) + offset;
      });
    }
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }
  // PATCHING ------------------------------------------------------------------
  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            this.#insert(node, change.children, change.before);
            break;
          case move_kind:
            this.#move(node, change.key, change.before, change.count);
            break;
          case remove_key_kind:
            this.#removeKey(node, change.key, change.count);
            break;
          case remove_kind:
            this.#remove(node, change.from, change.count);
            break;
          case replace_kind:
            this.#replace(node, change.from, change.count, change.with);
            break;
          case replace_text_kind:
            this.#replaceText(node, change.content);
            break;
          case replace_inner_html_kind:
            this.#replaceInnerHtml(node, change.inner_html);
            break;
          case update_kind:
            this.#update(node, change.added, change.removed);
            break;
        }
      });
      if (patch.removed) {
        this.#remove(
          node,
          node.childNodes.length - patch.removed,
          patch.removed
        );
      }
      iterate(patch.children, (child) => {
        this.#stack.push({
          node: node.childNodes[child.index | 0],
          patch: child
        });
      });
    }
  }
  // CHANGES -------------------------------------------------------------------
  #insert(node, children, before) {
    const fragment3 = document.createDocumentFragment();
    iterate(children, (child) => {
      const el = this.#createElement(child);
      addKeyedChild(node, el);
      fragment3.appendChild(el);
    });
    node.insertBefore(fragment3, node.childNodes[before | 0] ?? null);
  }
  #move(node, key, before, count) {
    let el = node[meta].keyedChildren.get(key).deref();
    const beforeEl = node.childNodes[before] ?? null;
    for (let i = 0; i < count && el !== null; ++i) {
      const next = el.nextSibling;
      if (SUPPORTS_MOVE_BEFORE) {
        node.moveBefore(el, beforeEl);
      } else {
        node.insertBefore(el, beforeEl);
      }
      el = next;
    }
  }
  #removeKey(node, key, count) {
    this.#removeFromChild(
      node,
      node[meta].keyedChildren.get(key).deref(),
      count
    );
  }
  #remove(node, from, count) {
    this.#removeFromChild(node, node.childNodes[from | 0], count);
  }
  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;
      const key = child[meta].key;
      if (key) {
        parent[meta].keyedChildren.delete(key);
      }
      parent.removeChild(child);
      child = next;
    }
  }
  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);
    const el = this.#createElement(child);
    addKeyedChild(parent, el);
    parent.insertBefore(el, parent.childNodes[from | 0] ?? null);
  }
  #replaceText(node, content) {
    node.data = content ?? "";
  }
  #replaceInnerHtml(node, inner_html) {
    node.innerHTML = inner_html ?? "";
  }
  #update(node, added, removed) {
    iterate(removed, (attribute3) => {
      const name = attribute3.name;
      if (node[meta].handlers.has(name)) {
        node.removeEventListener(name, handleEvent);
        node[meta].handlers.delete(name);
      } else {
        node.removeAttribute(name);
        ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute3) => {
      this.#createAttribute(node, attribute3);
    });
  }
  // CONSTRUCTORS --------------------------------------------------------------
  #createElement(vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
        initialiseMetadata(node, vnode.key);
        iterate(vnode.attributes, (attribute3) => {
          this.#createAttribute(node, attribute3);
        });
        this.#insert(node, vnode.children, 0);
        return node;
      }
      case text_kind: {
        const node = document.createTextNode(vnode.content ?? "");
        initialiseMetadata(node, vnode.key);
        return node;
      }
      case fragment_kind: {
        const node = document.createDocumentFragment();
        const head = document.createTextNode("");
        initialiseMetadata(head, vnode.key);
        node.appendChild(head);
        iterate(vnode.children, (child) => {
          node.appendChild(this.#createElement(child));
        });
        return node;
      }
      case unsafe_inner_html_kind: {
        const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
        initialiseMetadata(node, vnode.key);
        iterate(vnode.attributes, (attribute3) => {
          this.#createAttribute(node, attribute3);
        });
        this.#replaceInnerHtml(node, vnode.inner_html);
        return node;
      }
    }
  }
  #createAttribute(node, attribute3) {
    switch (attribute3.kind) {
      case attribute_kind: {
        const name = attribute3.name;
        const value = attribute3.value ?? "";
        if (value !== node.getAttribute(name)) {
          node.setAttribute(name, value);
        }
        ATTRIBUTE_HOOKS[name]?.added?.(node, value);
        break;
      }
      case property_kind:
        node[attribute3.name] = attribute3.value;
        break;
      case event_kind: {
        if (!node[meta].handlers.has(attribute3.name)) {
          node.addEventListener(attribute3.name, handleEvent, {
            passive: !attribute3.prevent_default
          });
        }
        const prevent = attribute3.prevent_default;
        const stop = attribute3.stop_propagation;
        const immediate = attribute3.immediate;
        const include = Array.isArray(attribute3.include) ? attribute3.include : [];
        node[meta].handlers.set(attribute3.name, (event2) => {
          if (prevent)
            event2.preventDefault();
          if (stop)
            event2.stopPropagation();
          let path = [];
          let node2 = event2.currentTarget;
          while (node2 !== this.#root) {
            const key = node2[meta].key;
            if (key) {
              path.push(key);
            } else {
              const index2 = [].indexOf.call(node2.parentNode.childNodes, node2);
              path.push(index2.toString());
            }
            node2 = node2.parentNode;
          }
          path.reverse();
          const data = this.#useServerEvents ? createServerEvent(event2, include) : event2;
          this.#dispatch(data, path, event2.type, immediate);
        });
        break;
      }
    }
  }
};
function iterate(list4, callback) {
  if (Array.isArray(list4)) {
    for (let i = 0; i < list4.length; i++) {
      callback(list4[i]);
    }
  } else if (list4) {
    for (list4; list4.tail; list4 = list4.tail) {
      callback(list4.head);
    }
  }
}
var meta = Symbol("metadata");
function initialiseMetadata(node, key = "") {
  switch (node.nodeType) {
    case Node.ELEMENT_NODE:
    case Node.DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        keyedChildren: /* @__PURE__ */ new Map(),
        handlers: /* @__PURE__ */ new Map()
      };
      break;
    case Node.TEXT_NODE:
      node[meta] = { key };
      break;
  }
}
function addKeyedChild(node, child) {
  if (child.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
    for (child = child.firstChild; child; child = child.nextSibling) {
      addKeyedChild(node, child);
    }
    return;
  }
  const key = child[meta].key;
  if (key) {
    node[meta].keyedChildren.set(key, new WeakRef(child));
  }
}
function handleEvent(event2) {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  handler(event2);
}
function createServerEvent(event2, include = []) {
  const data = {};
  if (event2.type === "input" || event2.type === "change") {
    include.push("target.value");
  }
  for (const property2 of include) {
    const path = property2.split(".");
    for (let i = 0, input = event2, output = data; i < path.length; i++) {
      if (i === path.length - 1) {
        output[path[i]] = input[path[i]];
        break;
      }
      output = output[path[i]] ??= {};
      input = input[path[i]];
    }
  }
  return data;
}
var ATTRIBUTE_HOOKS = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),
  autofocus: {
    added(node) {
      node.focus?.();
    }
  },
  autoplay: {
    added(node) {
      try {
        node.play?.();
      } catch (e) {
        console.error(e);
      }
    }
  }
};
function syncedBooleanAttribute(name) {
  return {
    added(node, _value) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
}
function syncedAttribute(name) {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
}

// build/dev/javascript/lustre/lustre/runtime/client/core.ffi.mjs
var copiedStyleSheets = /* @__PURE__ */ new WeakMap();
async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of document.querySelectorAll("link[rel=stylesheet], style")) {
    if (node.sheet)
      continue;
    pendingParentStylesheets.push(
      new Promise((resolve, reject) => {
        node.addEventListener("load", resolve);
        node.addEventListener("error", reject);
      })
    );
  }
  await Promise.allSettled(pendingParentStylesheets);
  if (!shadowRoot.host.isConnected) {
    return [];
  }
  shadowRoot.adoptedStyleSheets = shadowRoot.host.getRootNode().adoptedStyleSheets;
  const pending = [];
  for (const sheet of document.styleSheets) {
    try {
      shadowRoot.adoptedStyleSheets.push(sheet);
    } catch {
      try {
        let copiedSheet = copiedStyleSheets.get(sheet);
        if (!copiedSheet) {
          copiedSheet = new CSSStyleSheet();
          for (const rule of sheet.cssRules) {
            copiedSheet.insertRule(rule.cssText, copiedSheet.cssRules.length);
          }
          copiedStyleSheets.set(sheet, copiedSheet);
        }
        shadowRoot.adoptedStyleSheets.push(copiedSheet);
      } catch {
        const node = sheet.ownerNode.cloneNode();
        shadowRoot.prepend(node);
        pending.push(node);
      }
    }
  }
  return pending;
}

// build/dev/javascript/lustre/lustre/runtime/transport.mjs
var mount_kind = 0;
var reconcile_kind = 1;
var emit_kind = 2;
var attributes_changed_kind = 0;
var event_fired_kind = 1;

// src/lustre/runtime/client/server_component.ffi.mjs
var ServerComponent = class extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method"];
  }
  #method = "ws";
  #route = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #observer = new MutationObserver((mutations) => {
    const attributes = [];
    for (const mutation of mutations) {
      if (mutation.type !== "attributes")
        continue;
      const name = mutation.attributeName;
      if (!this.#remoteObservedAttributes.includes(name))
        continue;
      attributes.push([name, this.getAttribute(name)]);
    }
    if (attributes.length && this.#connected) {
      this.#transport?.send({ kind: attributes_changed_kind, attributes });
    } else {
      this.#changedAttributesQueue.push(...attributes);
    }
  });
  #remoteObservedAttributes = [];
  #connected = false;
  #changedAttributesQueue = [];
  constructor() {
    super();
    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }
    this.internals = this.attachInternals();
    this.#reconciler = new Reconciler(
      this.shadowRoot,
      (event2, path, name) => {
        this.#transport?.send({ kind: event_fired_kind, path, name, event: event2 });
      },
      {
        useServerEvents: true
      }
    );
    this.#observer.observe(this, {
      attributes: true
    });
  }
  connectedCallback() {
    this.#adoptStyleSheets();
    this.#method = this.getAttribute("method") || "ws";
    if (this.hasAttribute("route")) {
      this.#route = new URL(this.getAttribute("route"), window.location.href);
      this.#connect();
    }
  }
  adoptedCallback() {
    this.#adoptStyleSheets();
  }
  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case prev !== next: {
        this.#route = new URL(next, window.location.href);
        this.#connect();
        return;
      }
      case "method": {
        const normalised = next.toLowerCase();
        if (normalised == this.#method)
          return;
        if (["ws", "sse", "polling"].includes(normalised)) {
          this.#method = normalised;
          if (this.#method == "ws") {
            if (this.#route.protocol == "https:")
              this.#route.protocol = "wss:";
            if (this.#route.protocol == "http:")
              this.#route.protocol = "ws:";
          }
          this.#connect();
        }
        return;
      }
    }
  }
  messageReceivedCallback(data) {
    switch (data.kind) {
      case mount_kind: {
        this.#reconciler.mount(data.vdom);
        this.dispatchEvent(new CustomEvent("lustre:mount"));
        break;
      }
      case reconcile_kind: {
        this.#reconciler.push(data.patch, this.#adoptedStyleNodes.length);
        break;
      }
      case emit_kind: {
        this.dispatchEvent(new CustomEvent(data.name, { detail: data.data }));
        break;
      }
    }
  }
  //
  #connect() {
    if (!this.#route || !this.#method)
      return;
    if (this.#transport)
      this.#transport.close();
    const onConnect = () => {
      this.#connected = true;
      this.dispatchEvent(new CustomEvent("lustre:connect"), {
        detail: {
          route: this.#route,
          method: this.#method
        }
      });
      if (this.#changedAttributesQueue.length) {
        this.#transport.send({
          kind: attributes_changed_kind,
          attributes: this.#changedAttributesQueue
        });
        this.#changedAttributesQueue = [];
      }
    };
    const onMessage = (data) => {
      this.messageReceivedCallback(data);
    };
    const onClose = () => {
      this.#connected = false;
      this.dispatchEvent(new CustomEvent("lustre:close"), {
        detail: {
          route: this.#route,
          method: this.#method
        }
      });
    };
    const options = { onConnect, onMessage, onClose };
    switch (this.#method) {
      case "ws":
        this.#transport = new WebsocketTransport(this.#route, options);
        break;
      case "sse":
        this.#transport = new SseTransport(this.#route, options);
        break;
      case "polling":
        this.#transport = new PollingTransport(this.#route, options);
        break;
    }
  }
  //
  async #adoptStyleSheets() {
    while (this.#adoptedStyleNodes.length) {
      this.#adoptedStyleNodes.pop().remove();
      this.shadowRoot.firstChild.remove();
    }
    this.#adoptedStyleNodes = await adoptStylesheets(this.shadowRoot);
  }
};
var WebsocketTransport = class {
  #url;
  #socket;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose }) {
    this.#url = url;
    this.#socket = new WebSocket(this.#url);
    this.#onConnect = onConnect;
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#socket.onopen = () => {
      this.#onConnect();
    };
    this.#socket.onmessage = ({ data }) => {
      try {
        this.#onMessage(JSON.parse(data));
      } catch {
      }
    };
    this.#socket.onclose = () => {
      this.#onClose();
    };
  }
  send(data) {
    this.#socket.send(JSON.stringify(data));
  }
  close() {
    this.#socket.close();
  }
};
var SseTransport = class {
  #url;
  #eventSource;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose }) {
    this.#url = url;
    this.#eventSource = new EventSource(this.#url);
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#eventSource.onopen = () => {
      this.#onConnect();
    };
    this.#eventSource.onmessage = ({ data }) => {
      try {
        this.#onMessage(JSON.parse(data));
      } catch {
      }
    };
  }
  send(data) {
  }
  close() {
    this.#eventSource.close();
    this.#onClose();
  }
};
var PollingTransport = class {
  #url;
  #interval;
  #timer;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose, ...opts }) {
    this.#url = url;
    this.#onConnect = onConnect;
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#interval = opts.interval ?? 5e3;
    this.#fetch().finally(() => {
      this.#onConnect();
      this.#timer = window.setInterval(() => this.#fetch(), this.#interval);
    });
  }
  async send(data) {
  }
  close() {
    clearInterval(this.#timer);
    this.#onClose();
  }
  #fetch() {
    return fetch(this.#url).then((response) => response.json()).then(this.#onMessage).catch(console.error);
  }
};
window.customElements.define("lustre-server-component", ServerComponent);
export {
  ServerComponent
};
