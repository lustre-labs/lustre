// IMPORTS ---------------------------------------------------------------------

// 🚨 These imports need to point to the *build* directory. This module will be
// used as the entry module when running esbuild so we *cant* use imports relative
// to src/.

import { Reconciler } from "../../../../build/dev/javascript/lustre/lustre/runtime/client/server_component_reconciler.ffi.mjs";
import { adoptStylesheets } from "../../../../build/dev/javascript/lustre/lustre/runtime/client/core.ffi.mjs";
import {
  mount_variant,
  mount_vdom,
  reconcile_variant,
  reconcile_patch,
  emit_variant,
  emit_name,
  emit_data,
  attributes_changed_variant,
  event_fired_variant,
} from "../../../../build/dev/javascript/lustre/lustre/runtime/transport.mjs";

//

class WebsocketTransport {
  #url;
  #socket;

  constructor(url, onMessage, {}) {
    this.#url = url;
    this.#socket = new WebSocket(this.#url);
    this.#socket.onmessage = ({ data }) => {
      try {
        onMessage(JSON.parse(data));
      } catch {}
    };
  }

  send(data) {
    this.#socket.send(JSON.stringify(data));
  }

  close() {
    this.#socket.close();
  }
}

class SseTransport {
  #url;
  #eventSource;

  constructor(url, onMessage, {}) {
    this.#url = url;
    this.#eventSource = new EventSource(url);
    this.#eventSource.onmessage = ({ data }) => {
      try {
        onMessage(JSON.parse(data));
      } catch {}
    };
  }

  send(data) {}

  close() {
    this.#eventSource.close();
  }
}

class PollingTransport {
  #url;
  #onMessage;
  #interval;
  #timer;

  constructor(url, onMessage, opts = {}) {
    this.#url = url;
    this.#onMessage = onMessage;
    this.#interval = opts.interval ?? 5000;

    this.#fetch().finally(() => {
      this.#timer = window.setInterval(() => this.#fetch(), this.#interval);
    });
  }

  async send(data) {}

  close() {
    clearInterval(this.#timer);
  }

  #fetch() {
    return fetch(this.#url)
      .then((response) => response.json())
      .then(this.#onMessage)
      .catch(console.error);
  }
}

//

export class ServerComponent extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method"];
  }

  #method = "ws";
  #route = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #observer;
  #remoteObservedAttributes = [];

  constructor() {
    super();

    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }

    this.internals = this.attachInternals();
    this.#reconciler = new Reconciler(this.shadowRoot, (event, path, name) => {
      this.#transport?.send([event_fired_variant, path, name, event]);
    });

    this.#observer = new MutationObserver((mutations) => {
      const changed = [];

      for (const mutation of mutations) {
        if (mutation.type !== "attributes") continue;
        const name = mutation.attributeName;
        if (!this.#remoteObservedAttributes.includes(name)) continue;

        changed.push([name, this.getAttribute(name)]);
      }

      if (changed.length) {
        this.#transport?.send([attributes_changed_variant, changed]);
      }
    });
  }

  connectedCallback() {
    this.#adoptStyleSheets();
    this.#observer.observe(this, {
      attributes: true,
    });

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
      case "route" && prev !== next: {
        this.#route = new URL(next, window.location.href);
        this.#connect();
        return;
      }

      case "method": {
        const normalised = next.toLowerCase();

        if (normalised == this.#method) return;
        if (["ws", "sse", "polling", "http"].includes(normalised)) {
          this.#method = normalised;

          if (this.#method == "ws") {
            if (this.#route.protocol == "https:") this.#route.protocol = "wss:";
            if (this.#route.protocol == "http:") this.#route.protocol = "ws:";
          }

          this.#connect();
        }

        return;
      }
    }
  }

  eventReceivedCallback(event, path, name) {
    this.#transport?.send("hi!");
  }

  messageReceivedCallback(data) {
    switch (data[0]) {
      case mount_variant: {
        while (this.shadowRoot.children[this.#adoptedStyleNodes.length]) {
          this.shadowRoot.children[this.#adoptedStyleNodes.length].remove();
        }

        this.#reconciler.mount(data[mount_vdom]);

        break;
      }

      case reconcile_variant: {
        this.#reconciler.push(data[reconcile_patch]);

        break;
      }

      case emit_variant: {
        this.dispatchEvent(
          new CustomEvent(data[emit_name], { detail: data[emit_data] }),
        );

        break;
      }
    }
  }

  //

  #connect() {
    if (!this.#route || !this.#method) return;
    if (this.#transport) this.#transport.close();

    const onMessage = (data) => {
      this.messageReceivedCallback(data);
    };

    switch (this.#method) {
      case "ws":
        this.#transport = new WebsocketTransport(this.#route, onMessage, {});
        break;

      case "sse":
        this.#transport = new SseTransport(this.#route, onMessage, {});
        break;

      case "polling":
        this.#transport = new PollingTransport(this.#route, onMessage, {});
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
}

window.customElements.define("lustre-server-component", ServerComponent);
