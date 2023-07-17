import { morphdom } from "./runtime.ffi.mjs";
import { Ok, Error } from "./gleam.mjs";
import { map } from "./lustre/element.mjs";

// RUNTIME ---------------------------------------------------------------------

///
///
export class App {
  #root = null;
  #state = null;
  #queue = [];
  #commands = [];
  #willUpdate = false;
  #didUpdate = false;

  constructor(init, update, render) {
    this.#init = init;
    this.#update = update;
    this.#view = render;
  }

  start(selector = "body") {
    if (this.#root) return this;

    try {
      const el = document.querySelector(selector);
      const [next, cmds] = this.#init();

      this.#root = el;
      this.#state = next;
      this.#commands = cmds[0].toArray();
      this.#didUpdate = true;

      window.requestAnimationFrame(() => this.#tick());

      return new Ok((msg) => this.dispatch(msg));
    } catch (_) {
      return new Error(undefined);
    }
  }

  dispatch(msg) {
    if (!this.#willUpdate) window.requestAnimationFrame(() => this.#tick());

    this.#queue.push(msg);
    this.#willUpdate = true;
  }

  #render() {
    const node = this.#view(this.#state);
    const vdom = map(node, (msg) => this.dispatch(msg));

    morphdom(this.#root.firstChild, vdom);
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
        const [next, cmds] = this.#update(this.#state, this.#queue.shift());

        this.#state = next;
        this.#commands.concat(cmds[0].toArray());
      }

      this.#didUpdate = true;
    }

    // Each update can produce commands which must now be executed.
    while (this.#commands.length) this.#commands.shift()(this.dispatch);

    // Synchronous commands will immediately queue a message to be processed. If
    // it is reasonable, we can process those updates too before proceeding to
    // the next render.
    if (this.#queue.length) {
      times >= 5 ? console.warn(tooManyUpdates) : this.#flush(++times);
    }
  }
}

export const setup = (init, update, render) => new App(init, update, render);
export const start = (app, selector) => app.start(selector);
