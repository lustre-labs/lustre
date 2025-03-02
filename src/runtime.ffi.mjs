// IMPORTS ---------------------------------------------------------------------

import { Ok, Error, isEqual } from "./gleam.mjs";
import { Some } from "../gleam_stdlib/gleam/option.mjs";

import * as Diff from "./lustre/vdom/diff.mjs";
import * as Events from "./lustre/vdom/events.mjs";
import adoptStylesheets from "./adopt_stylesheets.mjs";
import { ElementNotFound, NotABrowser } from "./lustre.mjs";
import { LustreReconciler } from "./reconciler.ffi.mjs";

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

  #runtime;

  constructor(root, [init, effects], update, view) {
    this.#runtime = new LustreClientRuntime(
      root,
      [init, effects],
      view,
      update,
    );
  }

  send(action) {}
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
  const hasAttributes = on_attribute_change instanceof Some;
  const observedAttributes = hasAttributes
    ? on_attribute_change[0].entries().map(([name]) => name)
    : [];

  const component = class LustreClientComponent extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    #runtime;
    #adoptedStyleNodes = [];

    constructor() {
      super();
      this.internals = this.attachInternals();
      // a shadow root may have already been constructed through declarative
      // shadow root elements.
      if (!this.shadowRoot) {
        this.attachShadow({ mode: "open" });
      }
      this.#adoptStyleSheets();

      this.#runtime = new LustreClientRuntime(
        this.shadowRoot,
        [model, effects],
        view,
        update,
      );
    }

    adoptedCallback() {
      this.#adoptStyleSheets();
    }

    send(action) {}

    async #adoptStyleSheets() {
      while (this.#adoptedStyleNodes.length) {
        this.#adoptedStyleNodes.pop().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.#adoptedStyleNodes = await adoptStylesheets(this.shadowRoot);
      this.#runtime.initialNodeOffset = this.#adoptedStyleNodes.length;
    }
  };

  if (hasAttributes) {
    on_attribute_change[0].forEach((decoder, name) => {
      Object.defineProperty(component.prototype, name, {
        get() {
          return this[`_${name}`];
        },

        set(value) {
          const prev = this[`_${name}`];
          if (isEqual(prev, value)) return;

          this[`_${name}`] = value;
          const decoded = decoder(value);

          if (decoded.constructor === Ok) {
            this.send(new Dispatch(decoded[0]));
          }
        },
      });
    });
  }

  window.customElements.define(name, component);

  return new Ok(undefined);
};

// COMMON RUNTIME CODE ---------------------------------------------------------

class LustreClientRuntime {
  #root;

  #model;
  #view;
  #update;

  #vdom;
  #events;
  #reconciler;
  #viewTimer = null;

  initialNodeOffset = 0;

  constructor(root, [model, effects], view, update) {
    this.#root = root;
    this.#model = model;
    this.#view = view;
    this.#update = update;

    this.#vdom = this.#view(this.#model);
    this.#events = Events.add_child(Events.new$(), (msg) => msg, 0, this.#vdom);

    this.#reconciler = new LustreReconciler(
      this.#root,
      (event, id, immediate) => {
        this.#handleEvent(event, id, immediate);
      },
    );

    this.#reconciler.mount(this.#vdom);
    this.#tick(effects.all, false);
  }

  dispatch(msg, immediate = false) {
    const [model, effects] = this.#update(this.#model, msg);
    this.#model = model;

    this.#tick(effects.all, immediate);
  }

  #handleEvent(event, path, name, immediate) {
    const msg = Events.handle(this.#events, path, name, event);

    if (msg.isOk()) {
      this.dispatch(msg[0], immediate);
    }
  }

  #tick(effects, immediate = false) {
    const queue = [];

    const effect_params = {
      root: this.#root,
      emit: (event, data) => this.#emit(event, data),
      dispatch: (msg) => queue.push(msg),
      select: () => {},
    };

    while (true) {
      for (let effect = effects; effect.tail; effect = effect.tail) {
        effect.head(effect_params);
      }

      if (!queue.length) {
        break;
      }

      const msg = queue.shift();
      [this.#model, effects] = this.#update(this.#model, msg);
    }

    this.#render();
  }

  #render() {
    this.#viewTimer = null;

    const next = this.#view(this.#model);
    const { patch, events } = Diff.diff(
      this.#vdom,
      next,
      this.#events,
      this.initialNodeOffset,
    );
    this.#events = events;
    this.#vdom = next;

    this.#reconciler.push(patch, this.#events);
  }

  #emit(event, data) {
    const targetElement = this.#root.host ?? this.#root;
    targetElement.dispatchEvent(
      new CustomEvent(event, {
        detail: data,
        bubbles: true,
        composed: true,
      }),
    );
  }
}
