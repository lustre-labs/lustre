// Note that this path is relative to the built Gleam project, not the source files
// in `src/`. This particular module is not used by the Lustre package itself, but
// is instead bundled and made available to package users in the `priv/` directory.
//
// It makes obvious sense to co-locate the source with the rest of the package
// source code, but if we use relative imports here the bundle will fail because
// `vdom.ffi.mjs` is importing things from the Gleam standard library and expects
// to be placed in the `build/dev/javascript/lustre/` directory.
//
import * as Constants from "../build/dev/javascript/lustre/lustre/internals/constants.mjs";
import { patch, morph } from "../build/dev/javascript/lustre/vdom.ffi.mjs";

export class LustreServerComponent extends HTMLElement {
  static get observedAttributes() {
    return ["route"];
  }

  #root = null;
  #socket = null;

  constructor() {
    super();
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
          this.#socket.addEventListener("message", ({ data }) => {
            const [kind, ...payload] = JSON.parse(data);

            switch (kind) {
              case Constants.diff:
                return this.diff(payload);

              case Constants.emit:
                return this.emit(payload);

              case Constants.morph:
                return this.morph(payload);
            }
          });
        }
      }
    }
  }

  morph([vdom]) {
    this.#root = morph(this.#root, vdom, (msg) => {
      this.#socket?.send(
        JSON.stringify({ $: "Event", tag: msg.tag, event: msg.data })
      );
    });
  }

  diff([diff]) {
    this.#root = patch(this.#root, diff, (msg) => {
      this.#socket?.send(
        JSON.stringify({ $: "Event", tag: msg.tag, event: msg.data })
      );
    });
  }

  emit([event, data]) {
    this.dispatchEvent(new CustomEvent(event, { detail: data }));
  }

  disconnectedCallback() {
    this.#socket?.close();
  }
}

window.customElements.define("lustre-server-component", LustreServerComponent);
