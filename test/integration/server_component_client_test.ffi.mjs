import { register, unregister } from "./happy-dom.ffi.mjs";
import { toList } from "../gleam.mjs";
import {
  provide_kind,
  subscribe_kind,
  unsubscribe_kind,
  context_provided_kind,
} from "../lustre/runtime/transport.mjs";

// The server component client runtime is the entry module for its esbuild
// bundle, so it imports its dependencies from the build directory relative to
// `src/`. The compiled copy in the build directory can't resolve those imports,
// so we load the source module instead.
const server_component_runtime = new URL(
  "../../../../../src/lustre/runtime/client/server_component.ffi.mjs",
  import.meta.url,
);

// A stand-in for the browser's WebSocket that records every message the server
// component client sends.
class FakeWebSocket {
  static OPEN = 1;
  static instances = [];

  readyState = FakeWebSocket.OPEN;
  closed = false;
  sent = [];

  constructor(url) {
    this.url = new URL(url);
    FakeWebSocket.instances.push(this);
    queueMicrotask(() => this.onopen?.());
  }

  send(data) {
    const message = JSON.parse(data);

    if (Array.isArray(message.messages)) {
      this.sent.push(...message.messages);
    } else {
      this.sent.push(message);
    }
  }

  close() {
    this.closed = true;
  }

  // The transport only sends one message at a time and queues the rest until
  // the server responds. Delivering an empty message flushes that queue.
  flush() {
    if (!this.closed) this.onmessage?.({ data: JSON.stringify({}) });
  }
}

export async function with_server_components(provides, callback) {
  register({ width: 1920, height: 1080, url: "https://localhost:1234" });

  try {
    // happy-dom doesn't implement ElementInternals.
    HTMLElement.prototype.attachInternals ??= () => ({});
    globalThis.WebSocket = FakeWebSocket;
    FakeWebSocket.instances = [];

    // This registers the `lustre-server-component` element, so it has to
    // happen inside the browser context. Modules are only evaluated once, so
    // only one test can use these server components.
    await import(server_component_runtime.href);

    document.body.innerHTML = `
      <lustre-server-component id="provider" route="/provider" provides="${provides}">
        <lustre-server-component id="subscriber" route="/subscriber"></lustre-server-component>
      </lustre-server-component>
    `;

    const subscriber = document.querySelector("#subscriber");
    const components = {
      provider: document.querySelector("#provider"),
      subscriber,
      socket: FakeWebSocket.instances.find(
        (socket) => socket.url.pathname === "/subscriber",
      ),
      callbacks: 0,
    };

    // The runtime's `ContextRequestEvent` extends whichever `Event` existed
    // when the runtime module was first loaded, which in this test suite is
    // Node's rather than happy-dom's, and happy-dom won't dispatch it. We
    // re-dispatch an equivalent happy-dom event instead, counting every time
    // the provider calls back.
    const redispatched = Symbol("redispatched");

    subscriber.dispatchEvent = (event) => {
      // happy-dom calls `dispatchEvent` again on each element along the
      // event's path, so let our own re-dispatched event through untouched.
      if (event.type !== "context-request" || event[redispatched]) {
        return HTMLElement.prototype.dispatchEvent.call(subscriber, event);
      }

      const request = new Event("context-request", {
        bubbles: true,
        composed: true,
      });

      request[redispatched] = true;
      request.context = event.context;
      request.subscribe = event.subscribe;
      request.callback = (...args) => {
        components.callbacks += 1;
        return event.callback(...args);
      };

      return HTMLElement.prototype.dispatchEvent.call(subscriber, request);
    };

    await callback(components);
  } finally {
    await unregister();
  }
}

export function provide(components, key, value) {
  components.provider.messageReceivedCallback({
    kind: provide_kind,
    key,
    value,
  });
}

export function subscribe(components, key) {
  components.subscriber.messageReceivedCallback({ kind: subscribe_kind, key });
}

export function unsubscribe(components, key) {
  components.subscriber.messageReceivedCallback({
    kind: unsubscribe_kind,
    key,
  });
}

export function disconnect(components) {
  components.subscriber.remove();
}

export function sent_context_values(components, key) {
  components.socket.flush();

  return toList(
    components.socket.sent
      .filter((message) => message.kind === context_provided_kind)
      .filter((message) => message.key === key)
      .map((message) => message.value),
  );
}

export function provider_callbacks(components) {
  return components.callbacks;
}
