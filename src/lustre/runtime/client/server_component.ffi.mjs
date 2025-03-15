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

export class ServerComponent extends HTMLElement {
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
      if (mutation.type !== "attributes") continue;
      const name = mutation.attributeName;
      if (!this.#remoteObservedAttributes.includes(name)) continue;

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
      (event, path, name) => {
        this.#transport?.send({ kind: event_fired_kind, path, name, event });
      },
      {
        useServerEvents: true,
      },
    );

    this.#observer.observe(this, {
      attributes: true,
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
      case "route" && prev !== next: {
        this.#route = new URL(next, window.location.href);
        this.#connect();
        return;
      }

      case "method": {
        const normalised = next.toLowerCase();

        if (normalised == this.#method) return;
        if (["ws", "sse", "polling"].includes(normalised)) {
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

  messageReceivedCallback(data) {
    switch (data.kind) {
      case mount_kind: {
        this.#reconciler.mount(data.vdom);

        // Once the component is mounted there is finally something displayed on
        // the screen! Occassionally clients will want to know when this happens
        // so they can kick off other work.
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
    if (!this.#route || !this.#method) return;
    if (this.#transport) this.#transport.close();

    const onConnect = () => {
      this.#connected = true;
      this.dispatchEvent(new CustomEvent("lustre:connect"), {
        detail: {
          route: this.#route,
          method: this.#method,
        },
      });

      if (this.#changedAttributesQueue.length) {
        this.#transport.send({
          kind: attributes_changed_kind,
          attributes: this.#changedAttributesQueue,
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
          method: this.#method,
        },
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
}

// TRANSPORT OPTIONS -----------------------------------------------------------

class WebsocketTransport {
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
      } catch {}
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
}

class SseTransport {
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
      } catch {}
    };
  }

  send(data) {}

  close() {
    this.#eventSource.close();
    this.#onClose();
  }
}

class PollingTransport {
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
    this.#interval = opts.interval ?? 5000;

    this.#fetch().finally(() => {
      this.#onConnect();
      this.#timer = window.setInterval(() => this.#fetch(), this.#interval);
    });
  }

  async send(data) {}

  close() {
    clearInterval(this.#timer);
    this.#onClose();
  }

  #fetch() {
    return fetch(this.#url)
      .then((response) => response.json())
      .then(this.#onMessage)
      .catch(console.error);
  }
}

// UTILS -----------------------------------------------------------------------

// It's important that this comes right at the bottom, otherwise the different
// transport classes would be undefined when the custom element is defined!
window.customElements.define("lustre-server-component", ServerComponent);
