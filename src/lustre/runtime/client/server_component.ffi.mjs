// IMPORTS ---------------------------------------------------------------------

// 🚨 These imports need to point to the *build* directory. This module will be
// used as the entry module when running esbuild so we *cant* use imports relative
// to src/.

import { Reconciler } from "../../../../build/dev/javascript/lustre/lustre/vdom/reconciler.ffi.mjs";
import { adoptStylesheets, ContextRequestEvent } from "../../../../build/dev/javascript/lustre/lustre/runtime/client/runtime.ffi.mjs";
import {
  mount_kind,
  reconcile_kind,
  emit_kind,
  provide_kind,
  attribute_changed_kind,
  property_changed_kind,
  event_fired_kind,
  batch_kind,
  context_provided_kind,
} from "../../../../build/dev/javascript/lustre/lustre/runtime/transport.mjs";

//

export class ServerComponent extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method"];
  }

  #shadowRoot;
  #method = "ws";
  #route = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #remoteObservedAttributes = new Set();
  #remoteObservedProperties = new Set();
  #connected = false;
  #changedAttributesQueue = [];
  #contexts = new Map();
  #contextSubscriptions = new Set();

  #observer = new MutationObserver((mutations) => {
    const attributes = [];

    for (const mutation of mutations) {
      if (mutation.type !== "attributes") continue;
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
          value,
        })),
      });
    } else {
      this.#changedAttributesQueue.push(...attributes);
    }
  });

  constructor() {
    super();

    this.internals = this.attachInternals();
    this.#observer.observe(this, {
      attributes: true,
    });
  }

  connectedCallback() {
    for (const attribute of this.attributes) {
      this.#changedAttributesQueue.push([attribute.name, attribute.value]);
    }
  }

  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case prev !== next && "route": {
        this.#route = new URL(next, location.href);
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

  async messageReceivedCallback(data) {
    switch (data.kind) {
      case mount_kind: {
        this.#shadowRoot ??= this.attachShadow({
          mode: data.open_shadow_root ? "open" : "closed",
        });

        while (this.#shadowRoot.firstChild) {
          this.#shadowRoot.firstChild.remove();
        }

        this.#reconciler = new Reconciler(
          this.#shadowRoot,
          (event, path, name) => {
            this.#transport?.send({
              kind: event_fired_kind,
              path,
              name,
              event,
            });
          },
          {
            useServerEvents: true,
          },
        );

        this.#remoteObservedAttributes = new Set(data.observed_attributes);
        const filteredQueuedAttributes = this.#changedAttributesQueue.filter(
          ([name]) => this.#remoteObservedAttributes.has(name),
        );

        const messages = filteredQueuedAttributes.map(([name, value]) => ({
          kind: attribute_changed_kind,
          name,
          value,
        }));

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
                value,
              });
            },
          });
        }

        for (const [key, value] of Object.entries(data.provided_contexts)) {
          this.provide(key, value);
        }

        for (const key of [...new Set(data.requested_contexts)]) {
          this.dispatchEvent(
            new ContextRequestEvent(key, (value, unsubscribe) => {
              this.#transport?.send({
                kind: context_provided_kind,
                key,
                value,
              });

              this.#contextSubscriptions.add(unsubscribe);
            }),
          );
        }

        if (messages.length) {
          this.#transport.send({
            kind: batch_kind,
            messages,
          });
        }

        if (data.will_adopt_styles) {
          await this.#adoptStyleSheets();
        }

        // Listen for context requests from child elements
        this.#shadowRoot.addEventListener("context-request", (event) => {
          // Verify this is a valid context request event
          if (!event.context || !event.callback) return;
          if (!this.#contexts.has(event.context)) return;

          event.stopImmediatePropagation();

          const context = this.#contexts.get(event.context);

          if (event.subscribe) {
            const callbackRef = new WeakRef(event.callback);
            const unsubscribe = () => {
              context.subscribers = context.subscribers.filter(
                (subscriber) => subscriber !== callbackRef,
              );
            };

            context.subscribers.push([callbackRef, unsubscribe]);
            event.callback(context.value, unsubscribe);
          } else {
            event.callback(context.value);
          }
        });

        this.#reconciler.mount(data.vdom);

        // Once the component is mounted there is finally something displayed on
        // the screen! Occassionally clients will want to know when this happens
        // so they can kick off other work.
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

      case provide_kind: {
        this.provide(data.key, data.value);
        break;
      }
    }
  }

  //

  disconnectedCallback() {
    // Clean up any context subscriptions
    for (const unsubscribe of this.#contextSubscriptions) {
      unsubscribe();
    }

    this.#contextSubscriptions.clear();
  }

  // Context provider method
  provide(key, value) {
    if (!this.#contexts.has(key)) {
      this.#contexts.set(key, { value, subscribers: [] });
    } else {
      const context = this.#contexts.get(key);

      context.value = value;

      for (let i = context.subscribers.length - 1; i >= 0; i--) {
        const [subscriberRef, unsubscribe] = context.subscribers[i];
        const subscriber = subscriberRef.deref();

        // If the subscriber has been garbage collected, we remove it from the
        // list of subscribers.
        if (!subscriber) {
          context.subscribers.splice(i, 1);
          continue;
        }

        // Otherwise, we call the subscriber with the new value and the
        // unsubscribe function.
        subscriber(value, unsubscribe);
      }
    }
  }

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
    };

    const onMessage = (data) => {
      this.messageReceivedCallback(data);
    };

    const onClose = () => {
      this.#connected = false;
      this.dispatchEvent(
        new CustomEvent("lustre:close", {
          detail: {
            route: this.#route,
            method: this.#method,
          },
        }),
      );
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
  }
}

// TRANSPORT OPTIONS -----------------------------------------------------------

class WebsocketTransport {
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
              messages: this.#queue,
            }),
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
    if (
      this.#waitingForResponse ||
      this.#socket.readyState !== WebSocket.OPEN
    ) {
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
    this.#onConnect = onConnect;
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
      this.#timer = setInterval(() => this.#fetch(), this.#interval);
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
customElements.define("lustre-server-component", ServerComponent);
