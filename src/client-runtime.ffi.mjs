import { ElementNotFound, NotABrowser } from "./lustre.mjs";
import { Dispatch, Shutdown } from "./lustre/internals/runtime.mjs";
import { morph } from "./vdom.ffi.mjs";
import { Ok, Error, isEqual } from "./gleam.mjs";

export class LustreClientApplication {
  #root = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;
  #isComponent = false;

  #model = null;
  #update = null;
  #view = null;

  static start(flags, selector, init, update, view) {
    if (!is_browser()) return new Error(new NotABrowser());
    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);
    if (!root) return new Error(new ElementNotFound(selector));
    const app = new LustreClientApplication(init(flags), update, view, root);

    return new Ok((msg) => app.send(msg));
  }

  constructor(
    [model, effects],
    update,
    view,
    root = document.body,
    isComponent = false,
  ) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#root = root;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;
    this.#isComponent = isComponent;

    window.requestAnimationFrame(() => this.#tick());
  }

  send(action) {
    switch (true) {
      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();

        return;
      }

      case action instanceof Shutdown: {
        this.#shutdown();
        return;
      }

      default:
        return;
    }
  }

  emit(event, data) {
    this.#root.dispatchEvent(
      new CustomEvent(event, {
        bubbles: true,
        detail: data,
        composed: true,
      }),
    );
  }

  #tick() {
    this.#flush_queue();

    const vdom = this.#view(this.#model);
    const dispatch = (handler) => (e) => {
      const result = handler(e);

      if (result instanceof Ok) {
        this.send(new Dispatch(result[0]));
      }
    };

    this.#didUpdate = false;
    this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
  }

  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());

      this.#didUpdate ||= !isEqual(this.#model, next);
      this.#model = next;
      this.#effects = this.#effects.concat(effects.all.toArray());
    }

    while (this.#effects.length) {
      this.#effects.shift()(
        (msg) => this.send(new Dispatch(msg)),
        (event, data) => this.emit(event, data),
      );
    }

    if (this.#queue.length) {
      if (iterations < 5) {
        this.#flush_queue(++iterations);
      } else {
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }

  #shutdown() {
    this.#root.remove();
    this.#root = null;
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {};
    this.#view = () => {};
  }
}

export const start = (app, selector, flags) =>
  LustreClientApplication.start(
    flags,
    selector,
    app.init,
    app.update,
    app.view,
  );

// UTILS -----------------------------------------------------------------------

export const is_browser = () => window && window.document;
export const is_registered = (name) =>
  is_browser() && !!window.customElements.get(name);
export const prevent_default = (event) => event.preventDefault();
export const stop_propagation = (event) => event.stopPropagation();
