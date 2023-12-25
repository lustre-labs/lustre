// Note that this is a path to the built Gleam project, not the original file in
// `src/`. This is important because the imports inside `runtime.ffi.mjs` are
// relative to stuff in `build/dev/javascript/lustre/`, not `src/`.
//
import { morph } from "../build/dev/javascript/lustre/runtime.ffi.mjs";

export class LustreServerComponent extends HTMLElement {
  static get observedAttributes() {
    return ["route"];
  }

  #root = null;
  #ws = null;

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
          this.#ws?.close();
          this.#ws = null;
        } else if (prev !== next) {
          const id = this.getAttribute("id");
          const route = next + (id ? `?id=${id}` : "");

          this.#ws?.close();
          this.#ws = new WebSocket(`ws://${window.location.host}${route}`);
          this.#ws.addEventListener("message", ({ data }) => {
            const msg = JSON.parse(data);

            switch (msg.$) {
              case "Patch": {
                this.patch(msg.vdom);
                break;
              }
            }
          });
        }
      }
    }
  }

  patch(vdom) {
    const dispatch = ({ tag, data }) =>
      this.#ws?.send(JSON.stringify({ $: "Event", tag, event: data }));

    this.#root = morph(this.#root, vdom, dispatch);
  }

  disconnectedCallback() {
    this.#ws?.close();
  }
}

customElements.define("lustre-server-component", LustreServerComponent);
