import { Ok, Error, isEqual } from "./gleam.mjs";
import { Dispatch, Shutdown } from "./lustre/runtime.mjs";
import {
  ComponentAlreadyRegistered,
  BadComponentName,
  NotABrowser,
} from "./lustre.mjs";
import { LustreClientApplication } from "./client-runtime.ffi.mjs";

export function register({ init, update, view, on_attribute_change }, name) {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  window.customElements.define(
    name,
    class LustreClientComponent extends HTMLElement {
      #root = document.createElement("div");
      #application = null;
      #dispatch = null;

      static get observedAttributes() {
        return on_attribute_change.entries().map(([name, _]) => name);
      }

      constructor() {
        super();
        on_attribute_change.forEach((decoder, name) => {
          Object.defineProperty(this, name, {
            get() {
              return this[`_${name}`] || this.getAttribute(name);
            },

            set(value) {
              const prev = this[name];
              const decoded = decoder(value);

              if (decoded.isOk() && !isEqual(prev, value)) {
                this.#dispatch?.(decoded[0]);
              }

              if (typeof value === "string") {
                this.setAttribute(name, value);
              } else {
                this[`_${name}`] = value;
              }
            },
          });
        });
      }

      connectedCallback() {
        this.#application = new LustreClientApplication(
          init,
          update,
          view,
          this.#root
        )[0];
        this.#dispatch = (msg) => this.#application.send(new Dispatch(msg));
      }

      disconnectedCallback() {
        this.#application.send(new Shutdown());
      }
    }
  );

  new Ok(null);
}
