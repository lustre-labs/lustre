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
    #styles = [];

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

            const shouldDispatch =
              decoded instanceof Ok &&
              (!this.#application || !isEqual(prev, value));

            if (shouldDispatch) {
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
      this.#application = new LustreClientApplication(
        init(),
        update,
        view,
        this.#root,
        true,
      );

      this.#adoptStyleSheets().finally(() => {
        this.#shadow.append(this.#root);
      });
    }

    attributeChangedCallback(key, _, next) {
      this[key] = next;
    }

    disconnectedCallback() {
      this.#application.send(new Shutdown());
    }

    #adoptStyleSheets() {
      // Remove any existing style or link nodes that we've added to the shadow
      // root
      while (this.#styles.length) this.#styles.shift().remove();

      const pending = [];

      // Adopt any stylesheets that are present in the parent root
      //
      this.#shadow.adoptedStyleSheets = this.getRootNode().adoptedStyleSheets;

      // Iterate over all the stylesheets in the document and add them to the
      // shadow root. If the stylesheet is cross-origin, we need to clone the
      // node and add it to the shadow root instead.
      //
      for (const sheet of document.styleSheets) {
        try {
          this.#shadow.adoptedStyleSheets.push(sheet);
        } catch {
          const node = sheet.ownerNode.cloneNode();

          this.#shadow.appendChild(node);
          pending.push(
            new Promise((resolve, reject) => {
              node.onload = resolve;
              node.onerror = reject;
            }),
          );
        }
      }

      return Promise.allSettled(pending);
    }

    adoptStyleSheet(sheet) {
      this.#shadow.adoptedStyleSheets.push(sheet);
    }

    get adoptedStyleSheets() {
      return this.#shadow.adoptedStyleSheets;
    }
  };
}
