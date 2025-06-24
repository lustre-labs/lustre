// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};

// build/dev/javascript/gleam_stdlib/dict.mjs
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;

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
var trim_start_regex = /* @__PURE__ */ new RegExp(
  `^[${unicode_whitespaces}]*`
);
var trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);

// build/dev/javascript/lustre/lustre/internals/constants.ffi.mjs
var document = () => globalThis?.document;
var NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
var ELEMENT_NODE = 1;
var TEXT_NODE = 3;
var DOCUMENT_FRAGMENT_NODE = 11;
var SUPPORTS_MOVE_BEFORE = !!globalThis.HTMLElement?.prototype?.moveBefore;

// build/dev/javascript/lustre/lustre/vdom/vattr.mjs
var attribute_kind = 0;
var property_kind = 1;
var event_kind = 2;
var never_kind = 0;
var always_kind = 2;

// build/dev/javascript/lustre/lustre/vdom/vnode.mjs
var Fragment = class extends CustomType {
  constructor(kind, key, mapper, children, keyed_children, children_count) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.children = children;
    this.keyed_children = keyed_children;
    this.children_count = children_count;
  }
};
function advance(node) {
  if (node instanceof Fragment) {
    let children_count = node.children_count;
    return 1 + children_count;
  } else {
    return 1;
  }
}
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var unsafe_inner_html_kind = 3;

// build/dev/javascript/lustre/lustre/vdom/path.mjs
var separator_element = "	";

// build/dev/javascript/lustre/lustre/vdom/patch.mjs
var replace_text_kind = 0;
var replace_inner_html_kind = 1;
var update_kind = 2;
var move_kind = 3;
var remove_key_kind = 4;
var replace_kind = 5;
var insert_kind = 6;
var remove_kind = 7;

// build/dev/javascript/lustre/lustre/vdom/reconciler.ffi.mjs
var Reconciler = class {
  offset = 0;
  #root = null;
  #dispatch = () => {
  };
  #useServerEvents = false;
  #exposeKeys = false;
  constructor(root2, dispatch, { useServerEvents = false, exposeKeys = false } = {}) {
    this.#root = root2;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
    this.#exposeKeys = exposeKeys;
  }
  mount(vdom) {
    appendChild(this.#root, this.#createChild(this.#root, 0, vdom));
  }
  #stack = [];
  push(patch) {
    const offset = this.offset;
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
    const self = this;
    while (self.#stack.length) {
      const { node, patch } = self.#stack.pop();
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            self.#insert(node, change.children, change.before);
            break;
          case move_kind:
            self.#move(node, change.key, change.before, change.count);
            break;
          case remove_key_kind:
            self.#removeKey(node, change.key, change.count);
            break;
          case remove_kind:
            self.#remove(node, change.from, change.count);
            break;
          case replace_kind:
            self.#replace(node, change.from, change.count, change.with);
            break;
          case replace_text_kind:
            self.#replaceText(node, change.content);
            break;
          case replace_inner_html_kind:
            self.#replaceInnerHtml(node, change.inner_html);
            break;
          case update_kind:
            self.#update(node, change.added, change.removed);
            break;
        }
      });
      if (patch.removed) {
        self.#remove(
          node,
          node.childNodes.length - patch.removed,
          patch.removed
        );
      }
      let lastIndex = -1;
      let lastChild = null;
      iterate(patch.children, (child) => {
        const index2 = child.index | 0;
        const next = lastChild && lastIndex - index2 === 1 ? lastChild.previousSibling : childAt(node, index2);
        self.#stack.push({ node: next, patch: child });
        lastChild = next;
        lastIndex = index2;
      });
    }
  }
  // CHANGES -------------------------------------------------------------------
  #insert(node, children, before) {
    const fragment3 = createDocumentFragment();
    let childIndex = before | 0;
    iterate(children, (child) => {
      const el = this.#createChild(node, childIndex, child);
      appendChild(fragment3, el);
      childIndex += advance(child);
    });
    insertBefore(node, fragment3, childAt(node, before));
  }
  #move(node, key, before, count) {
    let el = getKeyedChild(node, key);
    const beforeEl = childAt(node, before);
    for (let i = 0; i < count && el !== null; ++i) {
      const next = el.nextSibling;
      if (SUPPORTS_MOVE_BEFORE) {
        node.moveBefore(el, beforeEl);
      } else {
        insertBefore(node, el, beforeEl);
      }
      el = next;
    }
  }
  #removeKey(node, key, count) {
    this.#removeFromChild(node, getKeyedChild(node, key), count);
  }
  #remove(node, from, count) {
    this.#removeFromChild(node, childAt(node, from), count);
  }
  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;
      const key = child[meta].key;
      if (key) {
        parent[meta].keyedChildren.delete(key);
      }
      for (const [_, { timeout }] of child[meta].debouncers ?? []) {
        clearTimeout(timeout);
      }
      parent.removeChild(child);
      child = next;
    }
  }
  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);
    const el = this.#createChild(parent, from, child);
    insertBefore(parent, el, childAt(parent, from));
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
        if (node[meta].throttles.has(name)) {
          node[meta].throttles.delete(name);
        }
        if (node[meta].debouncers.has(name)) {
          clearTimeout(node[meta].debouncers.get(name).timeout);
          node[meta].debouncers.delete(name);
        }
      } else {
        node.removeAttribute(name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute3) => {
      this.#createAttribute(node, attribute3);
    });
  }
  // CONSTRUCTORS --------------------------------------------------------------
  #createChild(parent, index2, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = createChildElement(parent, index2, vnode);
        this.#createAttributes(node, vnode);
        this.#insert(node, vnode.children);
        return node;
      }
      case text_kind: {
        return createChildText(parent, index2, vnode);
      }
      case fragment_kind: {
        const node = createDocumentFragment();
        const head = createChildText(parent, index2, vnode);
        appendChild(node, head);
        let childIndex = index2 + 1;
        iterate(vnode.children, (child) => {
          appendChild(node, this.#createChild(parent, childIndex, child));
          childIndex += advance(child);
        });
        return node;
      }
      case unsafe_inner_html_kind: {
        const node = createChildElement(parent, index2, vnode);
        this.#createAttributes(node, vnode);
        this.#replaceInnerHtml(node, vnode.inner_html);
        return node;
      }
    }
  }
  #createAttributes(node, { key, attributes }) {
    if (this.#exposeKeys && key) {
      node.setAttribute("data-lustre-key", key);
    }
    iterate(attributes, (attribute3) => this.#createAttribute(node, attribute3));
  }
  #createAttribute(node, attribute3) {
    const { debouncers, handlers, throttles } = node[meta];
    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      stop_propagation: stop,
      immediate,
      include,
      debounce: debounceDelay,
      throttle: throttleDelay
    } = attribute3;
    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? "";
        if (name === "virtual:defaultValue") {
          node.defaultValue = valueOrDefault;
          return;
        }
        if (valueOrDefault !== node.getAttribute(name)) {
          node.setAttribute(name, valueOrDefault);
        }
        SYNCED_ATTRIBUTES[name]?.added?.(node, value);
        break;
      }
      case property_kind:
        node[name] = value;
        break;
      case event_kind: {
        if (handlers.has(name)) {
          node.removeEventListener(name, handleEvent);
        }
        node.addEventListener(name, handleEvent, {
          passive: prevent.kind === never_kind
        });
        if (throttleDelay > 0) {
          const throttle = throttles.get(name) ?? {};
          throttle.delay = throttleDelay;
          throttles.set(name, throttle);
        } else {
          throttles.delete(name);
        }
        if (debounceDelay > 0) {
          const debounce = debouncers.get(name) ?? {};
          debounce.delay = debounceDelay;
          debouncers.set(name, debounce);
        } else {
          clearTimeout(debouncers.get(name)?.timeout);
          debouncers.delete(name);
        }
        handlers.set(name, (event2) => {
          if (prevent.kind === always_kind)
            event2.preventDefault();
          if (stop.kind === always_kind)
            event2.stopPropagation();
          const type = event2.type;
          const path = event2.currentTarget[meta].path;
          const data = this.#useServerEvents ? createServerEvent(event2, include ?? []) : event2;
          const throttle = throttles.get(type);
          if (throttle) {
            const now = Date.now();
            const last = throttle.last || 0;
            if (now > last + throttle.delay) {
              throttle.last = now;
              throttle.lastEvent = event2;
              this.#dispatch(data, path, type, immediate);
            }
          }
          const debounce = debouncers.get(type);
          if (debounce) {
            clearTimeout(debounce.timeout);
            debounce.timeout = setTimeout(() => {
              if (event2 === throttles.get(type)?.lastEvent)
                return;
              this.#dispatch(data, path, type, immediate);
            }, debounce.delay);
          }
          if (!throttle && !debounce) {
            this.#dispatch(data, path, type, immediate);
          }
        });
        break;
      }
    }
  }
};
var iterate = (list4, callback) => {
  if (Array.isArray(list4)) {
    for (let i = 0; i < list4.length; i++) {
      callback(list4[i]);
    }
  } else if (list4) {
    for (list4; list4.tail; list4 = list4.tail) {
      callback(list4.head);
    }
  }
};
var appendChild = (node, child) => node.appendChild(child);
var insertBefore = (parent, node, referenceNode) => parent.insertBefore(node, referenceNode ?? null);
var createChildElement = (parent, index2, { key, tag, namespace }) => {
  const node = document().createElementNS(namespace || NAMESPACE_HTML, tag);
  initialiseMetadata(parent, node, index2, key);
  return node;
};
var createChildText = (parent, index2, { key, content }) => {
  const node = document().createTextNode(content ?? "");
  initialiseMetadata(parent, node, index2, key);
  return node;
};
var createDocumentFragment = () => document().createDocumentFragment();
var childAt = (node, at) => node.childNodes[at | 0];
var meta = Symbol("lustre");
var initialiseMetadata = (parent, node, index2 = 0, key = "") => {
  const segment = `${key || index2}`;
  switch (node.nodeType) {
    case ELEMENT_NODE:
    case DOCUMENT_FRAGMENT_NODE:
      node[meta] = {
        key,
        path: segment,
        keyedChildren: /* @__PURE__ */ new Map(),
        handlers: /* @__PURE__ */ new Map(),
        throttles: /* @__PURE__ */ new Map(),
        debouncers: /* @__PURE__ */ new Map()
      };
      break;
    case TEXT_NODE:
      node[meta] = { key };
      break;
  }
  if (parent && parent[meta] && key) {
    parent[meta].keyedChildren.set(key, new WeakRef(node));
  }
  if (parent && parent[meta] && parent[meta].path) {
    node[meta].path = `${parent[meta].path}${separator_element}${segment}`;
  }
};
var getKeyedChild = (node, key) => node[meta].keyedChildren.get(key).deref();
var handleEvent = (event2) => {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  if (event2.type === "submit") {
    event2.detail ??= {};
    event2.detail.formData = [...new FormData(event2.target).entries()];
  }
  handler(event2);
};
var createServerEvent = (event2, include = []) => {
  const data = {};
  if (event2.type === "input" || event2.type === "change") {
    include.push("target.value");
  }
  if (event2.type === "submit") {
    include.push("detail.formData");
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
};
var syncedBooleanAttribute = (name) => {
  return {
    added(node) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
};
var syncedAttribute = (name) => {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
};
var SYNCED_ATTRIBUTES = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),
  autofocus: {
    added(node) {
      queueMicrotask(() => node.focus?.());
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

// build/dev/javascript/lustre/lustre/runtime/client/runtime.ffi.mjs
var copiedStyleSheets = /* @__PURE__ */ new WeakMap();
async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of document().querySelectorAll(
    "link[rel=stylesheet], style"
  )) {
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
  for (const sheet of document().styleSheets) {
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
var attribute_changed_kind = 0;
var event_fired_kind = 1;
var property_changed_kind = 2;
var batch_kind = 3;

// src/lustre/runtime/client/server_component.ffi.mjs
var ServerComponent = class extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method"];
  }
  #shadowRoot;
  #method = "ws";
  #route = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #remoteObservedAttributes = /* @__PURE__ */ new Set();
  #remoteObservedProperties = /* @__PURE__ */ new Set();
  #connected = false;
  #changedAttributesQueue = [];
  #observer = new MutationObserver((mutations) => {
    const attributes = [];
    for (const mutation of mutations) {
      if (mutation.type !== "attributes")
        continue;
      const name = mutation.attributeName;
      if (!this.#connected || this.#remoteObservedAttributes.has(name)) {
        attributes.push([name, this.getAttribute(name)]);
      }
    }
    if (attributes.length === 1) {
      const [name, value] = attributes[0];
      this.#transport?.send({ kind: attribute_changed_kind, name, value });
    } else if (attributes.length) {
      this.#transport?.send({
        kind: batch_kind,
        messages: attributes.map(([name, value]) => ({
          kind: attribute_changed_kind,
          name,
          value
        }))
      });
    } else {
      this.#changedAttributesQueue.push(...attributes);
    }
  });
  constructor() {
    super();
    this.internals = this.attachInternals();
    this.#observer.observe(this, {
      attributes: true
    });
  }
  connectedCallback() {
    for (const attribute3 of this.attributes) {
      this.#changedAttributesQueue.push([attribute3.name, attribute3.value]);
    }
  }
  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case (prev !== next && "route"): {
        this.#route = new URL(next, location.href);
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
  async messageReceivedCallback(data) {
    switch (data.kind) {
      case mount_kind: {
        this.#shadowRoot ??= this.attachShadow({
          mode: data.open_shadow_root ? "open" : "closed"
        });
        initialiseMetadata(null, this.#shadowRoot, "");
        while (this.#shadowRoot.firstChild) {
          this.#shadowRoot.firstChild.remove();
        }
        this.#reconciler = new Reconciler(
          this.#shadowRoot,
          (event2, path, name) => {
            this.#transport?.send({
              kind: event_fired_kind,
              path,
              name,
              event: event2
            });
          },
          {
            useServerEvents: true
          }
        );
        this.#remoteObservedAttributes = new Set(data.observed_attributes);
        const filteredQueuedAttributes = this.#changedAttributesQueue.filter(
          ([name]) => this.#remoteObservedAttributes.has(name)
        );
        if (filteredQueuedAttributes.length) {
          this.#transport.send({
            kind: batch_kind,
            messages: filteredQueuedAttributes.map(([name, value]) => ({
              kind: attribute_changed_kind,
              name,
              value
            }))
          });
        }
        this.#changedAttributesQueue = [];
        this.#remoteObservedProperties = new Set(data.observed_properties);
        for (const name of this.#remoteObservedProperties) {
          Object.defineProperty(this, name, {
            get() {
              return this[`_${name}`];
            },
            set(value) {
              this[`_${name}`] = value;
              this.#transport?.send({
                kind: property_changed_kind,
                name,
                value
              });
            }
          });
        }
        if (data.will_adopt_styles) {
          await this.#adoptStyleSheets();
        }
        this.#reconciler.mount(data.vdom);
        this.dispatchEvent(new CustomEvent("lustre:mount"));
        break;
      }
      case reconcile_kind: {
        this.#reconciler.push(data.patch);
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
      this.#shadowRoot.firstChild.remove();
    }
    this.#adoptedStyleNodes = await adoptStylesheets(this.#shadowRoot);
    this.#reconciler.offset = this.#adoptedStyleNodes.length;
  }
};
var WebsocketTransport = class {
  #url;
  #socket;
  #waitingForResponse = false;
  #queue = [];
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
      } finally {
        if (this.#queue.length) {
          this.#socket.send(
            JSON.stringify({
              kind: batch_kind,
              messages: this.#queue
            })
          );
        } else {
          this.#waitingForResponse = false;
        }
        this.#queue = [];
      }
    };
    this.#socket.onclose = () => {
      this.#onClose();
    };
  }
  send(data) {
    if (this.#waitingForResponse || this.#socket.readyState !== WebSocket.OPEN) {
      this.#queue.push(data);
      return;
    } else {
      this.#socket.send(JSON.stringify(data));
      this.#waitingForResponse = true;
    }
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
    this.#onConnect = onConnect;
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
      this.#timer = setInterval(() => this.#fetch(), this.#interval);
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
customElements.define("lustre-server-component", ServerComponent);
export {
  ServerComponent
};
