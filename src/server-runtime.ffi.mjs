import { Ok, isEqual } from "./gleam.mjs";
import {
  Subscribe,
  Dispatch,
  Event,
  Unsubscribe,
  Shutdown,
} from "./lustre/internals/runtime.mjs";

export class LustreServerApplication {
  #queue = [];
  #effects = [];
  #didUpdate = false;

  #vdom = null;
  #handlers = new Map();
  #renderers = new Set();

  #model = null;
  #update = null;
  #view = null;

  static start(flags, init, update, view) {
    const app = new LustreServerApplication(init(flags), update, view, root);

    return new Ok((msg) => app.send(msg));
  }

  // PUBLIC METHODS ------------------------------------------------------------

  constructor([model, effects], update, view) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#vdom = this.#view(this.#model);
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;

    globalThis.queueMicrotask(() => this.#tick());
  }

  send(action) {
    switch (true) {
      case action instanceof Subscribe: {
        this.#renderers.add(action[0]);
        return;
      }

      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();

        return;
      }

      case action instanceof Event: {
        const [event, data] = action;

        if (this.#handlers.has(event)) {
          const msg = this.#handlers.get(event)(data);

          if (msg.isOk()) {
            this.#queue.push(msg[0]);
            this.#tick();
          }
        }
      }

      case action instanceof Unsubscribe: {
        this.#renderers.delete(action[0]);
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

  // PRIVATE METHODS -----------------------------------------------------------

  #tick() {
    this.#flush_queue();

    if (this.#didUpdate) {
      this.#vdom = this.#view(this.#model);

      for (const renderer of this.#renderers) {
        renderer.render(this.#vdom);
      }
    }
  }

  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());

      this.#model = next;
      this.#didUpdate ||= !isEqual(this.#model, next);
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
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {};
    this.#view = () => {};
    this.#vdom = null;
    this.#handlers = new Map();
    this.#renderers = new Set();
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
