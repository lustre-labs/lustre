// IMPORTS ---------------------------------------------------------------------

import { Dispatch } from "./lustre/internals/runtime.mjs";
import { ElementNotFound, NotABrowser } from "./lustre.mjs";
import { LustreReconciler } from "./reconciler.ffi.mjs";
import { Ok, Error, NonEmpty, isEqual } from "./gleam.mjs";
import { Some } from "../gleam_stdlib/gleam/option.mjs";
import { diff } from "./lustre/runtime/vdom.mjs";
import * as Events from "./lustre/internals/events.mjs";

// UTILS -----------------------------------------------------------------------

export const is_browser = () => globalThis.window && window.document;

export const is_registered = (name) =>
  globalThis.window && globalThis.window.customElements.get(name);

export const is_reference_equal = (a, b) => a === b;

export const throw_server_component_error = () => {
  throw new window.Error(
    [
      "It looks like you're trying to use the server component runtime written ",
      "using `gleam_otp`. You can only end up here if you were poking around ",
      "the internals and started calling functions you shouldn't be!",
      "\n\n",
      "If you're just looking to start a server component in a JavaScript app,",
      "you can use `lustre.start_server_component`.",
      "\n\n",
      "If you're seeing this error and you think it's a bug. Please open an ",
      "issue over on Github: https://github.com/lustre-labs/lustre/issues/new",
    ].join(""),
  );
};

// SPA -------------------------------------------------------------------------

export class LustreSPA {
  static start({ init, update, view }, selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());

    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);

    if (!root) return new Error(new ElementNotFound(selector));

    const app = new LustreSPA(root, init(flags), update, view);

    return new Ok((action) => app.send(action));
  }

  #model;
  #update;
  #view;

  #prev;
  #reconciler;
  #events = Events.new$();

  constructor(root, [init, effects], update, view) {
    this.root = root;

    this.#model = init;
    this.#update = update;
    this.#view = view;

    this.#reconciler = new LustreReconciler(root, (msg, immediate) => {
      if (msg.constructor === Ok) {
        this.#dispatch(msg[0], immediate);
      }
    });

    this.#prev = view(init);
    this.#reconciler.mount(this.#prev);

    if (effects.all instanceof NonEmpty) {
      this.#tick(effects.all);
    }
  }

  send(action) {
    switch (action.constructor) {
      case Dispatch: {
        this.#dispatch(action[0], action[1]);
        break;
      }
    }
  }

  #dispatch(msg, immediate = false) {
    const [next, effects] = this.#update(this.#model, msg);

    this.#model = next;
    this.#tick(effects.all, immediate);
  }

  #tick(effects, immediate = false) {
    const dispatch = (msg, immediate) => {
      this.#dispatch(msg, immediate);
    };

    const emit = (event, data) =>
      this.root.dispatchEvent(
        new CustomEvent(event, {
          detail: data,
          bubbles: true,
          composed: true,
        }),
      );
    const select = () => {};
    const root = this.root;

    for (const effect of effects) {
      effect({ dispatch, emit, select, root });
    }

    const next = this.#view(this.#model);
    const { patch, events } = diff(this.#prev, next, Events.next(this.#events));

    this.#events = events;
    this.#reconciler.push(patch);
    this.#prev = next;
  }
}

export const start = LustreSPA.start;

// COMPONENT -------------------------------------------------------------------

export const make_lustre_client_component = (
  { init, update, view, on_attribute_change },
  name,
) => {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  const [model, effects] = init(undefined);
  const initialView = view(model);
  const hasAttributes = on_attribute_change instanceof Some;
  const observedAttributes = hasAttributes
    ? on_attribute_change[0].entries().map(([name]) => name)
    : [];

  const component = class LustreClientComponent extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    #model = model;
    #update = update;
    #view = view;

    #prev = initialView;
    #reconciler;
    #events = Events.new$();

    constructor() {
      super();
      this.attachShadow({ mode: "open" });
      this.internals = this.attachInternals();
      this.root = this.shadowRoot;

      this.#reconciler = new LustreReconciler(
        this.shadowRoot,
        (msg, immediate) => {
          if (msg.constructor === Ok) {
            this.#dispatch(msg[0], immediate);
          }
        },
      );

      this.#reconciler.mount(this.#prev);

      if (effects.all instanceof NonEmpty) {
        this.#tick(effects.all);
      }

      if (hasAttributes) {
        on_attribute_change[0].forEach((decoder, name) => {
          Object.defineProperty(this, name, {
            get() {
              return this[`_${name}`];
            },

            set(value) {
              const prev = this[`_${name}`];
              if (isEqual(prev, value)) return;
              this[`_${name}`] = value;
              const decoded = decoder(value);

              if (decoded.constructor === Ok) {
                this.#dispatch(decoded[0]);
              }
            },
          });
        });
      }
    }

    send(action) {
      switch (action.constructor) {
        case Dispatch: {
          this.#dispatch(action[0], action[1]);
          break;
        }
      }
    }

    #dispatch(msg, immediate = false) {
      const [next, effects] = this.#update(this.#model, msg);

      this.#model = next;
      this.#tick(effects.all, immediate);
    }

    #tick(effects, immediate = false) {
      const dispatch = (msg, immediate) => {
        this.#dispatch(msg, immediate);
      };

      const emit = (event, data) =>
        this.dispatchEvent(
          new CustomEvent(event, {
            detail: data,
            bubbles: true,
            composed: true,
          }),
        );
      const select = () => {};
      const root = this.shadowRoot;

      for (const effect of effects) {
        effect({ dispatch, emit, select, root });
      }

      const next = this.#view(this.#model);
      const { patch, events } = diff(
        this.#prev,
        next,
        Events.next(this.#events),
      );

      this.#events = events;
      this.#reconciler.push(patch);
      this.#prev = next;
    }
  };

  window.customElements.define(name, component);

  return new Ok(undefined);
};
