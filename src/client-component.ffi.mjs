import { Ok, Error, isEqual } from "./gleam.mjs";
import { Dispatch, Shutdown } from "./lustre/internals/runtime.mjs";
import {
  ComponentAlreadyRegistered,
  BadComponentName,
  NotABrowser,
} from "./lustre.mjs";
import { LustreClientApplication, is_browser } from "./client-runtime.ffi.mjs";

export function register({ init, update, view, on_attribute_change }, name) {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  window.customElements.define(
    name,
    makeComponent(init, update, view, on_attribute_change),
  );

  return new Ok(undefined);
}

function makeComponent(init, update, view, on_attribute_change) {
  return class LustreClientComponent extends HTMLElement {
    #root = document.createElement("div");
    #application = null;

    slotContent = [];

    static get observedAttributes() {
      return on_attribute_change[0]?.entries().map(([name, _]) => name) ?? [];
    }

    constructor() {
      super();
      on_attribute_change[0]?.forEach((decoder, name) => {
        Object.defineProperty(this, name, {
          get() {
            return this[`_${name}`] || this.getAttribute(name);
          },

          set(value) {
            const prev = this[name];
            const decoded = decoder(value);

            if (decoded instanceof Ok && !isEqual(prev, value)) {
              this.#application
                ? this.#application.send(new Dispatch(decoded[0]))
                : window.requestAnimationFrame(() =>
                    this.#application.send(new Dispatch(decoded[0])),
                  );
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
        init(),
        update,
        view,
        this.#root,
        true,
      );
      this.appendChild(this.#root);
    }

    disconnectedCallback() {
      this.#application.send(new Shutdown());
    }
  };
}
