// IMPORTS ---------------------------------------------------------------------

import * as Constants from "../build/dev/javascript/lustre/lustre/internals/constants.mjs";
import { morph, patch } from "../build/dev/javascript/lustre/vdom.ffi.mjs";
import { isEqual } from "../build/dev/javascript/prelude.mjs";

// SERVER COMPONENT ------------------------------------------------------------

export class LustreServerComponent extends HTMLElement {
  static get observedAttributes() {
    return ["route"];
  }

  constructor() {
    super();

    this.attachShadow({ mode: "open" });
    this.#observer = new MutationObserver((mutations) => {
      const changed = [];

      for (const mutation of mutations) {
        if (mutation.type === "attributes") {
          const { attributeName } = mutation;
          const next = this.getAttribute(attributeName);

          this[attributeName] = next;
        }
      }

      if (changed.length) {
        this.#socket?.send(JSON.stringify([Constants.attrs, changed]));
      }
    });
  }

  connectedCallback() {
    this.#observer.observe(this, { attributes: true, attributeOldValue: true });
    this.#adoptStyleSheets().finally(() => (this.#connected = true));
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
          const protocol = window.location.protocol === "https:" ? "wss" : "ws";

          this.#socket?.close();
          this.#socket = new WebSocket(
            `${protocol}://${window.location.host}${route}`,
          );
          this.#socket.addEventListener("message", (message) =>
            this.messageReceivedCallback(message),
          );
        }
      }
    }
  }

  messageReceivedCallback({ data }) {
    const [kind, ...payload] = JSON.parse(data);

    switch (kind) {
      case Constants.diff:
        return this.#diff(payload);

      case Constants.emit:
        return this.#emit(payload);

      case Constants.init:
        return this.#init(payload);
    }
  }

  disconnectedCallback() {
    this.#socket?.close();
  }

  /** @type {MutationObserver} */ #observer;
  /** @type {WebSocket | null} */ #socket;
  /** @type {boolean} */ #connected = false;
  /** @type {Element[]} */ #adoptedStyleElements = [];

  #init([attrs, vdom]) {
    const initial = [];

    for (const attr of attrs) {
      if (attr in this) {
        initial.push([attr, this[attr]]);
      } else if (this.hasAttribute(attr)) {
        initial.push([attr, this.getAttribute(attr)]);
      }

      Object.defineProperty(this, attr, {
        get() {
          return this[`__mirrored__${attr}`];
        },
        set(value) {
          const prev = this[`__mirrored__${attr}`];
          if (isEqual(prev, value)) return;
          this[`__mirrored__${attr}`] = value;
          this.#socket?.send(
            JSON.stringify([Constants.attrs, [[attr, value]]]),
          );
        },
      });
    }

    this.#observer.observe(this, {
      attributeFilter: attrs,
      attributeOldValue: true,
      attributes: true,
      characterData: false,
      characterDataOldValue: false,
      childList: false,
      subtree: false,
    });

    const prev = this.shadowRoot.childNodes[this.#adoptedStyleElements.lemgth] ??
      this.shadowRoot.appendChild(document.createTextNode(""));
    const dispatch = (handler) => (event) => {
      const data = JSON.parse(this.getAttribute("data-lustre-data") || "{}");
      const msg = handler(event);

      msg.data = deep_merge(data, msg.data);

      this.#socket?.send(JSON.stringify([Constants.event, msg.tag, msg.data]));
    };

    morph(prev, vdom, dispatch);

    if (initial.length) {
      this.#socket?.send(JSON.stringify([Constants.attrs, initial]));
    }
  }

  #diff([diff]) {
    const prev = this.shadowRoot.childNodes[this.#adoptedStyleElements.length - 1] ??
      this.shadowRoot.appendChild(document.createTextNode(""));
    const dispatch = (handler) => (event) => {
      const msg = handler(event);
      this.#socket?.send(JSON.stringify([Constants.event, msg.tag, msg.data]));
    };

    patch(prev, diff, dispatch, this.#adoptedStyleElements.length);
  }

  #emit([event, data]) {
    this.dispatchEvent(new CustomEvent(event, { detail: data }));
  }

  async #adoptStyleSheets() {
    const pendingParentStylesheets = [];

    for (const link of document.querySelectorAll("link[rel=stylesheet]")) {
      if (link.sheet) continue

      pendingParentStylesheets.push(
        new Promise((resolve, reject) => {
          link.addEventListener("load", resolve);
          link.addEventListener("error", reject);
        }),
      );
    }

    await Promise.allSettled(pendingParentStylesheets);

    while (this.#adoptedStyleElements.length) {
      this.#adoptedStyleElements.shift().remove();
      this.shadowRoot.firstChild.remove();
    }

    this.shadowRoot.adoptedStyleSheets = this.getRootNode().adoptedStyleSheets;

    const pending = [];

    for (const sheet of document.styleSheets) {
      try {
        this.shadowRoot.adoptedStyleSheets.push(sheet);
      } catch {

        try {
          const adoptedSheet = new CSSStyleSheet();
          for (const rule of sheet.cssRules) {
            adoptedSheet.insertRule(rule.cssText, adoptedSheet.cssRules.length);
          }

          this.shadowRoot.adoptedStyleSheets.push(adoptedSheet);
        } catch {
          const node = sheet.ownerNode.cloneNode();

          this.shadowRoot.prepend(node);
          this.#adoptedStyleElements.push(node);

          pending.push(
            new Promise((resolve, reject) => {
              node.onload = resolve;
              node.onerror = reject;
            }),
          );
        }
      }
    }

    return Promise.allSettled(pending);
  }
}

window.customElements.define("lustre-server-component", LustreServerComponent);


const deep_merge = (target, source) => {
  for (const key in source) {
    if (source[key] instanceof Object)
      Object.assign(source[key], deep_merge(target[key], source[key]));
  }

  Object.assign(target || {}, source);
  return target;
};
