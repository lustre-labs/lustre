import { innerHTML, createTree } from "./runtime.ffi.mjs";
import { Ok, Error, List } from "./gleam.mjs";
import { Some, Option } from "../gleam_stdlib/gleam/option.mjs";

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

  // These are the three functions that the user provides to the runtime.
  #__init;
  #__update;
  #__render;

  constructor(init, update, render) {
    this.#__init = init;
    this.#__update = update;
    this.#__render = render;
  }

  start(selector = "body") {
    if (this.#root) return this;

    try {
      this.#root = document.querySelector(selector);
    } catch (_) {
      return new Error(undefined);
    }

    const [next, cmds] = this.#__init();
    this.#state = next;
    this.#commands = cmds[0].toArray();
    this.#didUpdate = true;

    window.requestAnimationFrame(this.#tick.bind(this));
    return new Ok((msg) => this.dispatch(msg));
  }

  dispatch(msg) {
    if (!this.#willUpdate) window.requestAnimationFrame(this.#tick.bind(this));

    this.#queue.push(msg);
    this.#willUpdate = true;
  }

  #render() {
    const node = this.#__render(this.#state);
    const tree = createTree(
      map(node, (msg) => {
        if (msg instanceof Some) this.dispatch(msg[0]);
      })
    );

    innerHTML(this.#root, tree);
  }

  #tick() {
    this.#flush();
    this.#didUpdate && this.#render();
    this.#willUpdate = false;
  }

  #flush(times = 0) {
    if (this.#queue.length) {
      while (this.#queue.length) {
        const [next, cmds] = this.#__update(this.#state, this.#queue.shift());

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
export const start = (app, selector = "body") => app.start(selector);

// VDOM ------------------------------------------------------------------------

export const node = (tag, attrs, children) =>
  createTree(tag, Object.fromEntries(attrs.toArray()), children.toArray());
export const text = (content) => content;
export const attr = (key, value) => {
  if (value instanceof List) return [key, value.toArray()];
  if (value instanceof Option) return [key, value?.[0]];

  return [key, value];
};
export const on = (event, handler) => [`on${event}`, handler];
export const map = (node, f) => ({
  ...node,
  attributes: Object.entries(node.attributes).reduce((attrs, [key, value]) => {
    // It's safe to mutate the `attrs` object here because we created it at
    // the start of the reduce: it's not shared with any other code.

    // If the attribute is an event handler, wrap it in a function that
    // transforms
    if (key.startsWith("on") && typeof value === "function") {
      attrs[key] = (e) => f(value(e));
    } else {
      attrs[key] = value;
    }

    return attrs;
  }, {}),
  childNodes: node.childNodes.map((child) => map(child, f)),
});
export const styles = (list) => Object.fromEntries(list.toArray());
