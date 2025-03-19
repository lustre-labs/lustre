// IMPORTS ---------------------------------------------------------------------

import { Ok, Error, isEqual } from "../../../gleam.mjs";
import { run as decode } from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";

import {
  BadComponentName,
  ComponentAlreadyRegistered,
  NotABrowser,
  is_browser,
} from "../../../lustre.mjs";
import { Runtime, adoptStylesheets } from "./core.ffi.mjs";

//

export const make_component = (
  { init, update, view, on_attribute_change },
  name,
) => {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  const [model, effects] = init(undefined);
  const observedAttributes = on_attribute_change
    .entries()
    .map(([name]) => name);

  const component = class Component extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    #runtime;
    #adoptedStyleNodes = [];

    constructor() {
      super();
      // a shadow root may have already been constructed through declarative
      this.internals = this.attachInternals();
      // shadow root elements.
      if (!this.shadowRoot) {
        this.attachShadow({ mode: "open" });
      }

      this.#adoptStyleSheets();
      this.#runtime = new Runtime(
        this.shadowRoot,
        [model, effects],
        view,
        update,
      );
    }

    adoptedCallback() {
      this.#adoptStyleSheets();
    }

    dispatch(msg, immediate = false) {
      this.#runtime.dispatch(msg, immediate);
    }

    async #adoptStyleSheets() {
      while (this.#adoptedStyleNodes.length) {
        this.#adoptedStyleNodes.pop().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.#adoptedStyleNodes = await adoptStylesheets(this.shadowRoot);
      this.#runtime.initialNodeOffset = this.#adoptedStyleNodes.length;
    }
  };

  on_attribute_change.forEach((decoder, name) => {
    Object.defineProperty(component.prototype, name, {
      get() {
        return this[`_${name}`];
      },

      set(value) {
        const prev = this[`_${name}`];
        if (isEqual(prev, value)) return;

        this[`_${name}`] = value;
        const decoded = decode(value, decoder);

        if (decoded.constructor === Ok) {
          this.dispatch(decoded[0]);
        }
      },
    });
  });

  window.customElements.define(name, component);

  return new Ok(undefined);
};
