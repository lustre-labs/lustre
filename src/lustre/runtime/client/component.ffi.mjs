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
import {
  Runtime,
  ContextRequestEvent,
  adoptStylesheets,
} from "./runtime.ffi.mjs";
import {
  EffectDispatchedMessage,
  EffectEmitEvent,
  SystemRequestedShutdown,
} from "../server/runtime.mjs";

//

export const make_component = ({ init, update, view, config }, name) => {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  const attributes = new Map();
  const observedAttributes = [];
  for (let attr = config.attributes; attr.tail; attr = attr.tail) {
    const [name, decoder] = attr.head;
    if (attributes.has(name)) continue;

    attributes.set(name, decoder);
    observedAttributes.push(name);
  }

  const [model, effects] = init(undefined);

  const component = class Component extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    static formAssociated = config.is_form_associated;

    #runtime;
    #adoptedStyleNodes = [];
    #shadowRoot;
    #contextSubscriptions = new Set();

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

    // CUSTOM ELEMENT LIFECYCLE METHODS ----------------------------------------

    connectedCallback() {
      // Keep track of the requested contexts so we don't request the same one
      // twice.
      const requested = new Set();

      for (const ctx = config.contexts; ctx.tail; ctx = ctx.tail) {
        const [key, decoder] = ctx.head;

        // An empty key is not valid so we skip over any of those.
        if (!key) continue;
        // Likewise if we've requested a context for this key already then we
        // don't want to dispatch a second event, even if the user provided a
        // different decoder.
        if (requested.has(key)) continue;

        this.dispatchEvent(
          new ContextRequestEvent(
            key,
            (value, unsubscribe) => {
              const decoded = decode(value, decoder);

              if (decoded.isOk()) {
                this.#contextSubscriptions.add(unsubscribe);
                this.dispatch(decoded[0]);
              }
            },
            true,
          ),
        );

        requested.add(key);
      }
    }

    adoptedCallback() {
      if (config.adopt_styles) {
        this.#adoptStyleSheets();
      }
    }

    attributeChangedCallback(name, _, value) {
      const decoded = attributes.get(name)(value);

      if (decoded.isOk()) {
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

    disconnectedCallback() {
      for (const unsubscribe of this.#contextSubscriptions) {
        unsubscribe();
      }

      this.#contextSubscriptions.clear();
    }

    // LUSTRE RUNTIME METHODS --------------------------------------------------

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

    provide(key, value) {
      this.#runtime.provide(key, value);
    }

    async #adoptStyleSheets() {
      while (this.#adoptedStyleNodes.length) {
        this.#adoptedStyleNodes.pop().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.#adoptedStyleNodes = await adoptStylesheets(this.#shadowRoot);
      this.#runtime.offset = this.#adoptedStyleNodes.length;
    }
  };

  for (let prop = config.properties; prop.tail; prop = prop.tail) {
    const [name, decoder] = prop.head;
    if (Object.hasOwn(component.prototype, name)) {
      continue;
    }

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
  }

  customElements.define(name, component);

  return new Ok(undefined);
};

//

export const set_form_value = (root, value) => {
  if (!is_browser()) return;
  if (root instanceof ShadowRoot) {
    root.host.internals.setFormValue(value);
  }
};

export const clear_form_value = (internals) => {
  if (!is_browser()) return;
  if (root instanceof ShadowRoot) {
    root.host.internals.setFormValue(undefined);
  }
};

export const set_pseudo_state = (root, value) => {
  if (!is_browser()) return;
  if (root instanceof ShadowRoot) {
    root.host.internals.states.add(value);
  }
};

export const remove_pseudo_state = (root, value) => {
  if (!is_browser()) return;
  if (root instanceof ShadowRoot) {
    root.host.internals.states.delete(value);
  }
};
