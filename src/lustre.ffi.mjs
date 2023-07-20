import { ElementNotFound, ComponentAlreadyRegistered } from "./lustre.mjs";
import { from } from "./lustre/effect.mjs";
import { map } from "./lustre/element.mjs";
import { morph } from "./runtime.ffi.mjs";
import { Ok, Error, isEqual } from "./gleam.mjs";

// RUNTIME ---------------------------------------------------------------------

///
///
export class App {
  #root = null;
  #el = null;
  #state = null;
  #queue = [];
  #effects = [];
  #willUpdate = false;
  #didUpdate = false;

  #init = null;
  #update = null;
  #view = null;

  constructor(init, update, render) {
    this.#init = init;
    this.#update = update;
    this.#view = render;
  }

  start(selector = "body") {
    if (this.#root) return this;

    try {
      const el =
        selector instanceof HTMLElement
          ? selector
          : document.querySelector(selector);
      const [next, effects] = this.#init();

      this.#root = el;
      this.#state = next;
      this.#effects = effects[0].toArray();
      this.#didUpdate = true;

      window.requestAnimationFrame(() => this.#tick());

      return new Ok((msg) => this.dispatch(msg));
    } catch (_) {
      return new Error(new ElementNotFound());
    }
  }

  dispatch(msg) {
    if (!this.#willUpdate) window.requestAnimationFrame(() => this.#tick());

    this.#queue.push(msg);
    this.#willUpdate = true;
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
    this.#root = null;
    this.#el.remove();
    this.#state = null;
    this.#queue = [];
    this.#effects = [];
    this.#willUpdate = false;
    this.#didUpdate = false;
    this.#update = () => {};
    this.#view = () => {};
  }

  #render() {
    const node = this.#view(this.#state);
    const vdom = map(node, (msg) => this.dispatch(msg));

    this.#el = morph(this.#root, vdom);
  }

  #tick() {
    this.#flush();
    this.#didUpdate && this.#render();
    this.#didUpdate = false;
    this.#willUpdate = false;
  }

  #flush(times = 0) {
    if (this.#queue.length) {
      while (this.#queue.length) {
        const [next, effects] = this.#update(this.#state, this.#queue.shift());

        this.#state = next;
        this.#effects = this.#effects.concat(effects[0].toArray());
      }
      this.#didUpdate = true;
    }

    // Each update can produce effects which must now be executed.
    while (this.#effects[0])
      this.#effects.shift()(this.dispatch, (name, data) =>
        this.emit(name, data)
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
export const start = (app, selector) => app.start(selector);

export const emit = (name, data) =>
  // Normal `Effect`s constructed in Gleam from `effect.from` don't get told
  // about the second argument, but it's there 👀.
  from((_, emit) => {
    emit(name, data);
  });

// CUSTOM ELEMENTS -------------------------------------------------------------

export const setup_component = (
  name,
  init,
  update,
  render,
  on_attribute_change
) => {
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
        const dispatch = this.#app.start(this.#container);
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
