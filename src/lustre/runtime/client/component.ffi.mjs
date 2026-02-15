// IMPORTS ---------------------------------------------------------------------

import {
  Result$Ok,
  Result$Error,
  Result$isOk,
  Result$Ok$0,
} from "../../../gleam.mjs";
import { run as decode } from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import {
  Option$isSome,
  Option$Some$0,
} from "../../../../gleam_stdlib/gleam/option.mjs";

import {
  Error$BadComponentName,
  Error$ComponentAlreadyRegistered,
  Error$NotABrowser,
} from "../../../lustre.mjs";
import {
  Runtime,
  ContextRequestEvent,
  adoptStylesheets,
  is_browser,
} from "./runtime.ffi.mjs";
import {
  Message$isEffectDispatchedMessage,
  Message$isEffectEmitEvent,
  Message$isSystemRequestedShutdown,
} from "../server/runtime.mjs";
import { iterate } from "../../internals/list.ffi.mjs";

//

export const make_component = ({ init, update, view, config }, name) => {
  if (!is_browser()) return Result$Error(Error$NotABrowser());
  if (!name.includes("-")) return Result$Error(Error$BadComponentName(name));
  if (customElements.get(name)) {
    return Result$Error(Error$ComponentAlreadyRegistered(name));
  }

  const attributes = new Map();
  const observedAttributes = [];
  iterate(config.attributes, ([name, decoder]) => {
    if (attributes.has(name)) return;

    attributes.set(name, decoder);
    observedAttributes.push(name);
  });

  const [model, effects] = init(undefined);

  const component = class Component extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    static formAssociated = config.is_form_associated;

    #runtime;
    #adoptedStyleNodes = [];
    #contextSubscriptions = new Map();

    constructor() {
      super();

      // There are talks of potentially having `attachInternals` set `.internals`
      // automatically in the future.
      this.internals = this.attachInternals();

      // Only attach a shadow root if we don't already have one from the declarative
      // shadow DOM. This means components can be SSR'd and then hydrated like
      // normal apps.
      if (!this.internals.shadowRoot) {
        this.attachShadow({
          mode: config.open_shadow_root ? "open" : "closed",
          delegatesFocus: config.delegates_focus,
        });
      }

      if (config.adopt_styles) {
        this.#adoptStyleSheets();
      }

      this.#runtime = new Runtime(
        this.internals.shadowRoot,
        [model, effects],
        view,
        update,
      );
    }

    // CUSTOM ELEMENT LIFECYCLE METHODS ----------------------------------------

    // When an element is constructed by `document.createElement` and then added
    // to the DOM, the lifecycle callbacks run in this order:
    //
    //   constructor -> attributeChangedCallback -> connectedCallback
    //
    // If the element is added to the document through `document.importNode` then
    // we get:
    //
    //   constructor -> connectedCallback
    //
    // The connectedCallback is also called when the element is moved to a new
    // position in the same document, so it's important we don't do any *one-time*
    // work here.
    //
    connectedCallback() {
      this.#requestContexts();

      if (Option$isSome(config.on_connect)) {
        this.dispatch(Option$Some$0(config.on_connect));
      }
    }

    // If the element is imported into the document through `document.adoptNode`
    // then the lifecycle callbacks are:
    //
    //   disconnectedCallback -> adoptedCallback -> connectedCallback
    //
    adoptedCallback() {
      if (config.adopt_styles) {
        this.#adoptStyleSheets();
      }

      this.#unsubscribeContexts();

      if (Option$isSome(config.on_adopt)) {
        this.dispatch(Option$Some$0(config.on_adopt));
      }
    }

    // The disconnected callback is also called when the element is disconnected
    // from the document even if it is reconnected somewhere else. It's important
    // we use this callback just for DOM-related cleanup.
    //
    disconnectedCallback() {
      this.#unsubscribeContexts();

      if (Option$isSome(config.on_disconnect)) {
        this.dispatch(Option$Some$0(config.on_disconnect));
      }
    }

    attributeChangedCallback(name, _, value) {
      const decoded = attributes.get(name)(value ?? "");

      if (Result$isOk(decoded)) {
        this.dispatch(Result$Ok$0(decoded), true);
      }
    }

    formResetCallback() {
      if (Option$isSome(config.on_form_reset)) {
        this.dispatch(Option$Some$0(config.on_form_reset));
      }
    }

    formStateRestoreCallback(state, reason) {
      switch (reason) {
        case "restore":
          if (Option$isSome(config.on_form_restore)) {
            this.dispatch(Option$Some$0(config.on_form_restore)(state));
          }
          break;

        case "autocomplete":
          if (Option$isSome(config.on_form_autofill)) {
            this.dispatch(Option$Some$0(config.on_form_autofill)(state));
          }
          break;
      }
    }

    disconnectedCallback() {
      for (const [_, unsubscribe] of this.#contextSubscriptions) {
        unsubscribe?.();
      }

      this.#contextSubscriptions.clear();
    }

    // LUSTRE RUNTIME METHODS --------------------------------------------------

    send(message) {
      if (Message$isEffectDispatchedMessage(message)) {
        this.dispatch(message.message, false);
      } else if (Message$isEffectEmitEvent(message)) {
        this.emit(message.name, message.data);
      } else if (Message$isSystemRequestedShutdown(message)) {
        // TODO
      }
    }

    dispatch(msg, shouldFlush = false) {
      this.#runtime.dispatch(msg, shouldFlush);
    }

    emit(event, data) {
      this.#runtime.emit(event, data);
    }

    provide(key, value) {
      this.#runtime.provide(key, value);
    }

    // INTERNAL METHODS --------------------------------------------------------

    #requestContexts() {
      const requested = new Set();

      iterate(config.contexts, ([key, decoder]) => {
        // An empty key is not valid so we skip over any of those.
        if (!key) return;

        // Likewise if we've requested a context for this key already then we
        // don't want to dispatch a second event, even if the user provided a
        // different decoder.
        if (requested.has(key)) return;

        this.dispatchEvent(
          new ContextRequestEvent(
            key,
            (value, unsubscribe) => {
              const previousUnsubscribe = this.#contextSubscriptions.get(key);

              // Call the old unsubscribe callback if it has changed. This probably
              // means we have a new provider.
              if (previousUnsubscribe !== unsubscribe) {
                previousUnsubscribe?.();
              }

              const decoded = decode(value, decoder);
              this.#contextSubscriptions.set(key, unsubscribe);

              if (Result$isOk(decoded)) {
                this.dispatch(Result$Ok$0(decoded), true);
              }
            },
            true,
          ),
        );

        requested.add(key);
      });
    }

    #unsubscribeContexts() {
      for (const [_, unsubscribe] of this.#contextSubscriptions) {
        unsubscribe?.();
      }

      this.#contextSubscriptions.clear();
    }

    async #adoptStyleSheets() {
      while (this.#adoptedStyleNodes.length) {
        this.#adoptedStyleNodes.pop().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.#adoptedStyleNodes = await adoptStylesheets(
        this.internals.shadowRoot,
      );
    }
  };

  iterate(config.properties, ([name, decoder]) => {
    if (Object.hasOwn(component.prototype, name)) {
      return;
    }

    Object.defineProperty(component.prototype, name, {
      get() {
        return this[`_${name}`];
      },

      set(value) {
        this[`_${name}`] = value;
        const decoded = decode(value, decoder);

        if (Result$isOk(decoded)) {
          this.dispatch(Result$Ok$0(decoded), true);
        }
      },
    });
  });

  customElements.define(name, component);

  return Result$Ok(undefined);
};

//

export const set_form_value = (root, value) => {
  if (!is_browser()) return;
  if (root instanceof ShadowRoot) {
    root.host.internals.setFormValue(value);
  }
};

export const clear_form_value = (root) => {
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
