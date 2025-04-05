// IMPORTS ---------------------------------------------------------------------

import { Ok, Error } from "../../../gleam.mjs";
import { run as decode } from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { Some } from "../../../../gleam_stdlib/gleam/option.mjs";

import {
  BadComponentName,
  ComponentAlreadyRegistered,
  NotABrowser,
  is_browser,
} from "../../../lustre.mjs";
import { Runtime, adoptStylesheets } from "./runtime.ffi.mjs";
import {
  EffectDispatchedMessage,
  EffectEmitEvent,
  SystemRequestedShutdown,
} from "../server/runtime.mjs";

//

export const make_component = ({ init, update, view, config }, name) => {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  const [model, effects] = init(undefined);
  const observedAttributes = config.attributes.entries().map(([name]) => name);

  const component = class Component extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    static formAssociated = config.is_form_associated;

    #runtime;
    #adoptedStyleNodes = [];
    #shadowRoot;

    constructor() {
      super();
      // a shadow root may have already been constructed through declarative
      this.internals = this.attachInternals();
      // shadow root elements.
      if (!this.internals.shadowRoot) {
        this.#shadowRoot = this.attachShadow({
          mode: config.open_shadow_root ? "open" : "closed",
        });
      } else {
        this.#shadowRoot = this.internals.shadowRoot;
      }

      if (config.adopt_styles) {
        this.#adoptStyleSheets();
      }

      this.#runtime = new Runtime(
        this.#shadowRoot,
        [model, effects],
        view,
        update,
      );
    }

    adoptedCallback() {
      if (config.adopt_styles) {
        this.#adoptStyleSheets();
      }
    }

    attributeChangedCallback(name, _, value) {
      const decoded = config.attributes.get(name)(value)

      if (decoded.constructor === Ok) {
        this.dispatch(decoded[0]);
      }
    }

    formResetCallback() {
      if (config.on_form_reset instanceof Some) {
        this.dispatch(config.on_form_reset[0]);
      }
    }

    formStateRestoreCallback(state, reason) {
      switch (reason) {
        case "restore":
          if (config.on_form_restore instanceof Some) {
            this.dispatch(config.on_form_restore[0](state));
          }
          break;

        case "autocomplete":
          if (config.on_form_populate instanceof Some) {
            this.dispatch(config.on_form_autofill[0](state));
          }
          break;
      }
    }

    send(message) {
      switch (message.constructor) {
        case EffectDispatchedMessage: {
          this.dispatch(message.message, false);
          break;
        }

        case EffectEmitEvent: {
          this.emit(message.name, message.data);
          break;
        }

        case SystemRequestedShutdown:
          //TODO
          break;
      }
    }

    dispatch(msg, immediate = false) {
      this.#runtime.dispatch(msg, immediate);
    }

    emit(event, data) {
      this.#runtime.emit(event, data);
    }

    async #adoptStyleSheets() {
      while (this.#adoptedStyleNodes.length) {
        this.#adoptedStyleNodes.pop().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.#adoptedStyleNodes = await adoptStylesheets(this.#shadowRoot);
      this.#runtime.initialNodeOffset = this.#adoptedStyleNodes.length;
    }
  };

  config.properties.forEach((decoder, name) => {
    Object.defineProperty(component.prototype, name, {
      get() {
        return this[`_${name}`];
      },

      set(value) {
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

//

export const set_form_value = (internals, value) => {
  if (!is_browser()) return;
  if (internals instanceof ElementInternals) {
    console.log(internals);
    internals.setFormValue(value);
  }
};

export const clear_form_value = (internals) => {
  if (!is_browser()) return;
  if (internals instanceof ElementInternals) {
    internals.setFormValue(undefined);
  }
};

export const set_psuedo_state = (internals, value) => {
  if (!is_browser()) return;
  if (internals instanceof ElementInternals) {
    internals.states.add(value);
  }
};

export const remove_psuedo_state = (internals, value) => {
  if (!is_browser()) return;
  if (internals instanceof ElementInternals) {
    internals.states.delete(value);
  }
};
