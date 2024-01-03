import {
  AppAlreadyStarted,
  AppNotYetStarted,
  BadComponentName,
  ComponentAlreadyRegistered,
  ElementNotFound,
  NotABrowser,
} from "./lustre.mjs";
import { handlers } from "./lustre/element.mjs";
import { morph } from "./runtime.ffi.mjs";
import { Ok, Error, isEqual } from "./gleam.mjs";
import {
  new$ as dict_new,
  get as dict_get,
  has_key as dict_has_key,
} from "../gleam_stdlib/gleam/dict.mjs";

// RUNTIME ---------------------------------------------------------------------

///
///
export class ClientApp {
  #root = null;
  #state = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;

  #init = null;
  #update = null;
  #view = null;

  constructor(init, update, render) {
    this.#init = init;
    this.#update = update;
    this.#view = render;
  }

  start(selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());
    if (this.#root) return new Error(new AppAlreadyStarted());

    this.#root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);

    if (!this.#root) return new Error(new ElementNotFound());

    const [next, effects] = this.#init(flags);

    this.#state = next;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;

    window.requestAnimationFrame(() => this.#tick());

    return new Ok((msg) => this.dispatch(msg));
  }

  dispatch(msg) {
    this.#queue.push(msg);
    this.#tick();
  }

  emit(name, event = null) {
    this.#root.dispatchEvent(
      new CustomEvent(name, {
        bubbles: true,
        detail: event,
        composed: true,
      })
    );
  }

  destroy() {
    if (!this.#root) return new Error(new AppNotYetStarted());

    this.#root.remove();
    this.#root = null;
    this.#state = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {};
    this.#view = () => {};
  }

  #tick() {
    this.#flush();

    if (this.#didUpdate) {
      const vdom = this.#view(this.#state);

      this.#root = morph(this.#root, vdom, (msg) => this.dispatch(msg));
      this.#didUpdate = false;
    }
  }

  #flush(times = 0) {
    if (!this.#root) return;
    if (this.#queue.length) {
      while (this.#queue.length) {
        const [next, effects] = this.#update(this.#state, this.#queue.shift());
        // If the user returned their model unchanged and not reconstructed then
        // we don't need to trigger a re-render.
        this.#didUpdate ||= this.#state !== next;
        this.#state = next;
        this.#effects = this.#effects.concat(effects.all.toArray());
      }
    }

    // Each update can produce effects which must now be executed.
    while (this.#effects.length)
      this.#effects.shift()(
        (msg) => this.dispatch(msg),
        (name, data) => this.emit(name, data)
      );

    // Synchronous effects will immediately queue a message to be processed. If
    // it is reasonable, we can process those updates too before proceeding to
    // the next render.
    if (this.#queue.length) {
      if (times < 5) {
        this.#flush(++times);
      } else {
        console.warn(tooManyUpdates);
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
}

export class ServerApp {
  #queue = [];
  #effects = [];
  #didUpdate = false;

  #model = null;
  #html = null;

  #init = null;
  #update = null;
  #view = null;

  #renderers = new Set();
  #handlers = dict_new();

  constructor(init, update, render) {
    this.#init = init;
    this.#update = update;
    this.#view = render;
  }

  start(flags) {
    const [next, effects] = this.#init(flags);

    this.#model = next;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;

    window.queueMicrotask(() => this.#tick());

    return new Ok((msg) => this.dispatch(msg));
  }

  dispatch(msg) {
    this.#queue.push(msg);
    this.#tick();
  }

  handle_client_event(tag, event) {
    if (!dict_has_key(this.#handlers, tag)) return;

    const handler = dict_get(this.#handlers, tag);
    const msg = handler[0](event);

    if (msg.isOk()) {
      this.dispatch(msg[0]);
    }
  }

  add_renderer(renderer) {
    this.#renderers.add(renderer);
    renderer(this.#html);
  }

  remove_renderer(renderer) {
    this.#renderers.delete(renderer);
  }

  destroy() {
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {};
    this.#view = () => {};
    this.#renderers = new Set();
    this.#handlers = dict_new();
  }

  #tick() {
    this.#flush();

    if (this.#didUpdate) {
      this.#html = this.#view(this.#model);
      this.#handlers = handlers(this.#html);

      for (const renderer of this.#renderers) {
        renderer(this.#html);
      }

      this.#didUpdate = false;
    }
  }

  #flush(times = 0) {
    if (this.#queue.length) {
      while (this.#queue.length) {
        const [next, effects] = this.#update(this.#model, this.#queue.shift());
        // If the user returned their model unchanged and not reconstructed then
        // we don't need to trigger a re-render.
        this.#didUpdate ||= this.#model !== next;
        this.#model = next;
        this.#effects = this.#effects.concat(effects.all.toArray());
      }
    }

    // Each update can produce effects which must now be executed.
    while (this.#effects.length)
      this.#effects.shift()(
        (msg) => this.dispatch(msg),
        () => {}
      );

    // Synchronous effects will immediately queue a message to be processed. If
    // it is reasonable, we can process those updates too before proceeding to
    // the next render.
    if (this.#queue.length) {
      if (times < 5) {
        this.#flush(++times);
      } else {
        console.warn(tooManyUpdates);
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
}

export const App = is_browser() ? ClientApp : ServerApp;

export const setup = (init, update, render) => new App(init, update, render);
export const start = (app, selector, flags) => app.start(selector, flags);
export const start_server = (app, flags) => app.start(flags);
export const destroy = (app) => app.destroy();

// HTML EVENTS -----------------------------------------------------------------

export const prevent_default = (e) => e.preventDefault?.();
export const stop_propagation = (e) => e.stopPropagation?.();

// CUSTOM ELEMENTS -------------------------------------------------------------

export const setup_component = (
  name,
  init,
  update,
  render,
  on_attribute_change
) => {
  if (!name.includes("-")) return new Error(new BadComponentName());
  if (!is_browser()) return new Error(new NotABrowser());
  if (customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered());
  }

  customElements.define(
    name,
    class extends HTMLElement {
      static get observedAttributes() {
        return on_attribute_change.entries().map(([name, _]) => name);
      }

      #container = document.createElement("div");
      #app = null;
      #dispatch = null;

      constructor() {
        super();

        this.#app = new App(init, update, render);

        on_attribute_change.forEach((decoder, name) => {
          Object.defineProperty(this, name, {
            get: () => {
              return this[`_${name}`] || this.getAttribute(name);
            },

            set: (value) => {
              const prev = this[name];
              const decoded = decoder(value);

              // We need this equality check to prevent constantly dispatching
              // messages when the value is an object or array: it might not have
              // changed but its reference might have and we don't want to trigger
              // useless updates.
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
        const dispatch = this.#app.start(this.#container);

        this.#dispatch = dispatch[0];
        this.appendChild(this.#container);
      }

      attributeChangedCallback(name, prev, next) {
        if (prev !== next) {
          this[name] = next;
        }
      }

      disconnectedCallback() {
        this.#app.destroy();
      }
    }
  );

  return new Ok(null);
};

// UTLS ------------------------------------------------------------------------

export const is_browser = () => window && window.document;
export const is_registered = (name) => !!customElements.get(name);
