// IMPORTS ---------------------------------------------------------------------

// 🚨 These imports need to point to the *build* directory. This module will be
// used as the entry module when running esbuild so we *cant* use imports relative
// to src/.

import { Reconciler } from "../../../../build/dev/javascript/lustre/lustre/runtime/client/reconciler.ffi.mjs";
import { adoptStylesheets } from "../../../../build/dev/javascript/lustre/lustre/runtime/client/core.ffi.mjs";
import {
  mount_kind,
  reconcile_kind,
  emit_kind,
  attributes_changed_kind,
  event_fired_kind,
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
    this.#reconciler = new Reconciler(
      this.shadowRoot,
      (event, path, name) => {
        this.#transport?.send({ kind: event_fired_kind, path, name, event });
      },
      {
        useServerEvents: true,
      },
    );

    this.#observer = new MutationObserver((mutations) => {
      const attributes = [];

      for (const mutation of mutations) {
        if (mutation.type !== "attributes") continue;
        const name = mutation.attributeName;
        if (!this.#remoteObservedAttributes.includes(name)) continue;

        attributes.push([name, this.getAttribute(name)]);
      }

      if (attributes.length) {
        this.#transport?.send({ kind: attributes_changed_kind, attributes });
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
    console.log(data);

    switch (data.kind) {
      case mount_kind: {
        while (this.shadowRoot.children[this.#adoptedStyleNodes.length]) {
          this.shadowRoot.children[this.#adoptedStyleNodes.length].remove();
        }

        this.#reconciler.mount(data.vdom);

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
