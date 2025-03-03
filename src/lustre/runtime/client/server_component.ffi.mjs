// IMPORTS ---------------------------------------------------------------------

// 🚨 These imports need to point to the *build* directory. This module will be
// used as the entry module when running esbuild so we *cant* use imports relative
// to src/.

import { Reconciler } from "../../../../build/dev/javascript/lustre/lustre/client_runtime/server_component_reconciler.ffi.mjs";
import { adoptStylesheets } from "../../../../build/dev/javascript/lustre/lustre/client_runtime/core.ffi.mjs";

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
  #remoteObservedAttributes = [];

  construct() {
    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }

    this.internals = this.attachInternals();
    this.#reconciler = new Reconciler(this.shadowRoot, (event, path, name) => {
      this.eventReceivedCallback(event, path, name);
    });
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

  messageReceivedCallback(data) {}

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

      case "http":
        this.#transport = new HttpTransport(this.#route, onMessage);
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

//

class WebsocketTransport {
  #url;
  #socket;

  constructor(url, onMessage, {}) {
    this.#url = url;
    this.#socket = new WebSocket(this.#url);
    this.#socket.onmessage = onMessage;
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
    this.#eventSource.onmessage = onMessage;
  }

  send(data) {
    fetch(this.#url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    });
  }

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
    this.#interval = opts.interval ?? 1000;
    this.#timer = window.setInterval(() => {
      fetch(this.#url)
        .then((response) => response.json())
        .then(this.#onMessage);
    }, this.#interval);
  }

  async send(data) {
    const res = await fetch(this.#url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    });
    const json = await res.json();

    window.clearInterval(this.#timer);

    this.#onMessage(json);
    this.#timer = window.setInterval(() => {
      fetch(this.#url)
        .then((response) => response.json())
        .then(this.#onMessage);
    }, this.#interval);
  }

  close() {
    clearInterval(this.#timer);
  }
}

class HttpTransport {
  #url;
  #onMessage;

  constructor(url, onMessage) {
    this.#url = url;
    this.#onMessage = onMessage;
  }

  send(data) {
    fetch(this.#url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data),
    })
      .then((res) => res.json())
      .then((data) => this.#onMessage(data));
  }

  close() {}
}
