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

  const Component = makeComponent(init, update, view, on_attribute_change);

  window.customElements.define(name, Component);

  for (const el of document.querySelectorAll(name)) {
    const replaced = new Component();

    for (const attr of el.attributes) {
      replaced.setAttribute(attr.name, attr.value);
    }

    el.replaceWith(replaced);
  }

  return new Ok(undefined);
}

function makeComponent(init, update, view, on_attribute_change) {
  return class LustreClientComponent extends HTMLElement {
    #root = document.createElement("div");
    #application = null;
    #shadow = null;

    slotContent = [];

    static get observedAttributes() {
      return on_attribute_change[0]?.entries().map(([name, _]) => name) ?? [];
    }

    constructor() {
      super();
      this.#shadow = this.attachShadow({ mode: "closed" });

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

            this[`_${name}`] = value;
          },
        });
      });
    }

    connectedCallback() {
      for (const link of document.querySelectorAll("link")) {
        if (link.rel === "stylesheet") {
          this.#shadow.appendChild(link.cloneNode(true));
        }
      }

      for (const style of document.querySelectorAll("style")) {
        this.#shadow.appendChild(style.cloneNode(true));
      }

      this.#application = new LustreClientApplication(
        init(),
        update,
        view,
        this.#root,
        true,
      );
      this.#shadow.append(this.#root);
    }

    attributeChangedCallback(key, _, next) {
      this[key] = next;
    }

    disconnectedCallback() {
      this.#application.send(new Shutdown());
    }

    get adoptedStyleSheets() {
      return this.#shadow.adoptedStyleSheets;
    }

    set adoptedStyleSheets(value) {
      this.#shadow.adoptedStyleSheets = value;
    }
  };
}
