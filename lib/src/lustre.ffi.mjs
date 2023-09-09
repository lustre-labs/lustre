import {
  AppAlreadyStarted,
  AppNotYetStarted,
  BadComponentName,
  ComponentAlreadyRegistered,
  ElementNotFound,
  NotABrowser,
} from "./lustre.mjs";
import { from } from "./lustre/effect.mjs";
import { morph } from "./runtime.ffi.mjs";
import { Ok, Error, isEqual } from "./gleam.mjs";

// RUNTIME ---------------------------------------------------------------------

///
///
export class App {
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
    this.#effects = effects[0].toArray();
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
        this.#effects = this.#effects.concat(effects[0].toArray());
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
      times >= 5 ? console.warn(tooManyUpdates) : this.#flush(++times);
    }
  }
}

export const setup = (init, update, render) => new App(init, update, render);
export const start = (app, selector, flags) => app.start(selector, flags);
export const destroy = (app) => app.destroy();

export const emit = (name, data) =>
  // Normal `Effect`s constructed in Gleam from `effect.from` don't get told
  // about the second argument, but it's there 👀.
  from((_, emit) => {
    emit(name, data);
  });

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
        // This is necessary for ✨ reasons ✨. Clearly there's a bug in the
        // implementation of either the `App` or the runtime but I con't work it
        // out.
        //
        // If we pass the container to the app directly then the component fails
        // to render anything to the ODM.
        this.#container.appendChild(document.createElement("div"));

        const dispatch = this.#app.start(this.#container.firstChild);
        this.#dispatch = dispatch[0];

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
              if (decoded.isOk() && !isEqual(prev, decoded[0])) {
                this.#dispatch(decoded[0]);
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
        this.appendChild(this.#container.firstChild);
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
