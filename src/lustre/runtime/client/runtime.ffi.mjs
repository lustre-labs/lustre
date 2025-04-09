// IMPORTS ---------------------------------------------------------------------

import { NonEmpty, Empty } from "../../../gleam.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import { empty_list } from "../../internals/constants.mjs";
import { diff } from "../../vdom/diff.mjs";
import * as Events from "../../vdom/events.mjs";
import { Reconciler } from "../../vdom/reconciler.ffi.mjs";
import { virtualise } from "../../vdom/virtualise.ffi.mjs";

//

export const is_browser = () => globalThis.window && window.document;

export const is_registered = (name) =>
  globalThis.window && globalThis.window.customElements.get(name);

export const is_reference_equal = (a, b) => a === b;

export const throw_server_component_error = () => {
  throw new globalThis.Error(
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

//

export class Runtime {
  constructor(root, [model, effects], view, update) {
    this.root = root;
    this.#model = model;
    this.#view = view;
    this.#update = update;

    this.#reconciler = new Reconciler(this.root, (event, path, name) => {
      const [events, msg] = Events.handle(this.#events, path, name, event);
      this.#events = events;

      if (msg.isOk()) {
        this.dispatch(msg[0], false);
      }
    });

    // We want the first render to be synchronous too
    // The initial vdom is whatever we can virtualise from the root node when we
    // mount on to it.
    this.#vdom = virtualise(this.root);
    // The initial set of events is empty, since we just virtualised.
    this.#events = Events.new$();

    // We want the first render to be synchronous...
    this.#shouldFlush = true;

    // ... and force it immediately. Tick will render our app, attach event
    // listeners, and reset `#shouldFlush`.
    // Afterwards, events triggered by virtualisation will dispatch, if any.
    this.#tick(effects);
  }

  // PUBLIC API ----------------------------------------------------------------

  root = null;

  get initialNodeOffset() {
    return this.#reconciler.initialNodeOffset;
  }

  set initialNodeOffset(offset) {
    this.#reconciler.initialNodeOffset = offset;
  }

  dispatch(msg, immediate = false) {
    this.#shouldFlush ||= immediate;

    if (this.#shouldQueue) {
      this.#queue.push(msg);
    } else {
      const [model, effects] = this.#update(this.#model, msg);

      this.#model = model;
      this.#tick(effects);
    }
  }

  emit(event, data) {
    const target = this.root.host ?? this.root;

    target.dispatchEvent(
      new CustomEvent(event, {
        detail: data,
        bubbles: true,
        composed: true,
      }),
    );
  }

  // PRIVATE API ---------------------------------------------------------------

  #model;
  #view;
  #update;

  #vdom;
  #events;
  #reconciler;

  #shouldQueue = false;
  #queue = [];

  #beforePaint = empty_list;
  #afterPaint = empty_list;
  #renderTimer = null;
  #shouldFlush = false;

  #actions = {
    dispatch: (msg, immediate) => this.dispatch(msg, immediate),
    emit: (event, data) => this.emit(event, data),
    select: () => {},
    root: () => this.root,
  };

  // A `#tick` is where we process effects and trigger any synchronous updates.
  // Once a tick has been processed a render will be scheduled if none is already.
  // p0
  #tick(effects) {
    // By flipping this on before we process the list of synchronous effects, we
    // make it so that any messages dispatched immediately will be queued up and
    // applied before the next render.
    this.#shouldQueue = true;

    // We step into this loop to process any synchronous effects and batch any
    // deferred ones. When a synchronous effect immediately dispatches a message,
    // we add it to a queue and process another `update` cycle. This continues
    // until there are no more synchronous effects or messages to process.
    while (true) {
      for (let list = effects.synchronous; list.tail; list = list.tail) {
        // We pass the runtime directly to each effect. It has all the methods
        // of the `Actions` record define in the effect module.
        list.head(this.#actions);
      }

      // Both `before_paint` and `after_paint` are lists of effects that should
      // be deferred until we next perform a render. That means we need to collect
      // them all up in order and save them for later.
      this.#beforePaint = listAppend(this.#beforePaint, effects.before_paint);
      this.#afterPaint = listAppend(this.#afterPaint, effects.after_paint);

      // Once we've batched any deferred effects, we check if there are any
      // messages in the queue. If not, we can break out of the loop and continue
      // with the render.
      if (!this.#queue.length) break;

      // This is a destructuring assignment pattern that is mutating both
      // `this.#model` and the argument to this function: `effects`!
      [this.#model, effects] = this.#update(this.#model, this.#queue.shift());
    }

    // Remember to flip this off so subsequent messages trigger another tick.
    this.#shouldQueue = false;

    // Work out whether we need to schedule a render or if we need to
    if (this.#shouldFlush) {
      window.cancelAnimationFrame(this.#renderTimer);
      this.#render();
    } else if (!this.#renderTimer) {
      this.#renderTimer = window.requestAnimationFrame(() => {
        this.#render();
      });
    }
  }

  #render() {
    this.#shouldFlush = false;
    this.#renderTimer = null;

    const next = this.#view(this.#model);
    const { patch, events } = diff(this.#events, this.#vdom, next);

    this.#events = events;
    this.#vdom = next;
    this.#reconciler.push(patch);

    // We have performed a render, the DOM has been updated but the browser has
    // not yet been given the opportunity to paint. We queue a microtask to block
    // the browser from painting until we have processed any effects that need to
    // be run first.
    if (this.#beforePaint instanceof NonEmpty) {
      const effects = makeEffect(this.#beforePaint);
      this.#beforePaint = empty_list;

      // We explicitly queue a microtask instead of synchronously calling the
      // `#tick` function to allow the runtime to process any microtasks queued
      // by synchronous effects first such as promise callbacks.
      window.queueMicrotask(() => {
        this.#shouldFlush = true;
        this.#tick(effects);
      });
    }

    // If there are effects to schedule for after the browser has painted, we can
    // request an animation frame and process them then.
    if (this.#afterPaint instanceof NonEmpty) {
      const effects = makeEffect(this.#afterPaint);
      this.#afterPaint = empty_list;

      window.requestAnimationFrame(() => {
        this.#shouldFlush = true;
        this.#tick(effects);
      });
    }
  }
}

export const send = (runtime, message) => {
  runtime.send(message);
};

//

function makeEffect(synchronous) {
  return {
    synchronous,
    after_paint: empty_list,
    before_paint: empty_list,
  };
}

function listAppend(a, b) {
  if (a instanceof Empty) {
    return b;
  } else if (b instanceof Empty) {
    return a;
  } else {
    return $list.append(a, b);
  }
}

const copiedStyleSheets = new WeakMap();

export async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of document.querySelectorAll("link[rel=stylesheet], style")) {
    if (node.sheet) continue;

    pendingParentStylesheets.push(
      new Promise((resolve, reject) => {
        node.addEventListener("load", resolve);
        node.addEventListener("error", reject);
      }),
    );
  }

  await Promise.allSettled(pendingParentStylesheets);

  // the element might have been removed while we were waiting.
  if (!shadowRoot.host.isConnected) {
    return [];
  }

  shadowRoot.adoptedStyleSheets =
    shadowRoot.host.getRootNode().adoptedStyleSheets;

  const pending = [];

  for (const sheet of document.styleSheets) {
    try {
      shadowRoot.adoptedStyleSheets.push(sheet);
    } catch {
      try {
        let copiedSheet = copiedStyleSheets.get(sheet);
        if (!copiedSheet) {
          copiedSheet = new CSSStyleSheet();
          for (const rule of sheet.cssRules) {
            copiedSheet.insertRule(rule.cssText, copiedSheet.cssRules.length);
          }
          copiedStyleSheets.set(sheet, copiedSheet);
        }

        shadowRoot.adoptedStyleSheets.push(copiedSheet);
      } catch {
        const node = sheet.ownerNode.cloneNode();

        shadowRoot.prepend(node);
        pending.push(node);
      }
    }
  }

  return pending;
}
