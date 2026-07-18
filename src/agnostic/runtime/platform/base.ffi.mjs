// IMPORTS ---------------------------------------------------------------------

import {
  Result$isOk,
  Result$Ok$0,
} from "../../../gleam.mjs";
import { empty_list } from "../../internals/constants.mjs";
import { diff } from "../../vdom/diff.mjs";
import * as Cache from "../../vdom/cache.mjs";
import { Reconciler } from "../../vdom/reconciler.ffi.mjs";
import { isEqual } from "../../internals/equals.ffi.mjs";
import { iterate, toList } from "../../internals/list.ffi.mjs";
import { run as decode } from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";

//

export const is_browser = () => !!globalThis.document;

export const is_registered = (name) => is_browser() && customElements.get(name);

export const throw_server_component_error = () => {
  throw new globalThis.Error(
    [
      "It looks like you're trying to use the server component runtime written ",
      "using `gleam_otp`. You can only end up here if you were poking around ",
      "the internals and started calling functions you shouldn't be!",
      "\n\n",
      "If you're just looking to start a server component in a JavaScript app,",
      "you can use `agnostic.start` with `platform.headless()`.",
      "\n\n",
      "If you're seeing this error and you think it's a bug. Please open an ",
      "issue over on Github: https://github.com/lustre-labs/lustre/issues/new",
    ].join(""),
  );
};

//

export class Runtime {
  constructor(root, initialVdom, [model, effects], view, update, platform, options) {
    this.root = root;
    this.#model = model;
    this.#view = view;
    this.#update = update;
    this.#platformScheduleRender = platform.schedule_render;
    this.#platformAfterRender = platform.after_render;

    // The platform declares an ordered list of deferred-effect phases. The
    // pending Map is seeded with the declared names and doubles as the
    // declared-phase set: tasks tagged with a name that has no entry are
    // silently dropped.
    iterate(platform.phases, (phase) => {
      this.#phases.push(phase);
      this.#pending.set(phase.name, []);
    });

    this.root.addEventListener("context-request", (event) => {
      // So that we're compatible with other implementations of the proposed
      // protocol, we don't check the event constructor here because other
      // implementations will have defined their own event type.
      if (!(event.context && event.callback)) return;
      if (!this.#contexts.has(event.context)) return;

      event.stopImmediatePropagation();

      const context = this.#contexts.get(event.context);

      if (event.subscribe) {
        const unsubscribe = () => {
          context.subscribers = context.subscribers.filter(
            (subscriber) => subscriber !== event.callback,
          );
        };

        context.subscribers.push([event.callback, unsubscribe]);
        event.callback(context.value, unsubscribe);
      } else {
        event.callback(context.value);
      }
    });

    const decodeEvent = (event, path, name) =>
      Cache.decode(this.#cache, path, name, event);

    const dispatch = (event, data) => {
      const [cache, result] = Cache.dispatch(this.#cache, data);
      this.#cache = cache;

      if (Result$isOk(result)) {
        const handler = Result$Ok$0(result);

        if (handler.stop_propagation) event.stopPropagation();
        if (handler.prevent_default) event.preventDefault();

        this.dispatch(handler.message, false);
      }
    };

    this.#reconciler = new Reconciler(this.root, decodeEvent, dispatch, platform, options);

    // The initial vdom is provided by platform.mount() — no virtualise call.
    this.#vdom = initialVdom;
    // The initial set of events is empty, since we just virtualised.
    this.#cache = Cache.new$();

    // We want the first render to be synchronous and force it immediately.
    // Afterwards, events triggered by virtualisation will dispatch, if any.
    this.#handleEffects(effects);
    this.#render();
  }

  // PUBLIC API ----------------------------------------------------------------

  root = null;

  dispatch(message, shouldFlush = false) {
    if (this.#shouldQueue) {
      this.#queue.push(message);
    } else {
      const [model, effects] = this.#update(this.#model, message);

      this.#model = model;

      // Since we called update, we always want to schedule a new frame.
      this.#scheduleRender(shouldFlush);
      this.#handleEffects(effects);
    }
  }

  emit(event, data) {
    const target = this.root.host ?? this.root;

    target.dispatchEvent(new LustreEvent(event, data));
  }

  // Provide a context value for any child nodes that request it using the given
  // key. If the key already exists, any existing subscribers will be notified
  // of the change. Otherwise, we store the value and wait for any `context-request`
  // events to come in.
  provide(key, value) {
    if (!this.#contexts.has(key)) {
      this.#contexts.set(key, { value, subscribers: [] });
    } else {
      const context = this.#contexts.get(key);

      // if the new context we provide is equal to the current context,
      // we don't have to notify our subscribers about the change.
      if (isEqual(context.value, value)) {
        return;
      }

      context.value = value;

      for (let i = context.subscribers.length - 1; i >= 0; i--) {
        const [subscriber, unsubscribe] = context.subscribers[i];

        // If the subscriber has been garbage collected, we remove it from the
        // list of subscribers.
        if (!subscriber) {
          context.subscribers.splice(i, 1);
          continue;
        }

        // Otherwise, we call the subscriber with the new value and the
        // unsubscribe function.
        subscriber(value, unsubscribe);
      }
    }
  }

  subscribe(key, decoder) {
    // An empty key is not valid so we skip over any of those.
    if (!key) return;

    // If we were previously subscribed to this context, we should unsubscribe
    // before before subscribing again with the new decoder.
    this.#contextSubscriptions.get(key)?.();

    const target = this.root.host ?? this.root;

    target.dispatchEvent(
      new ContextRequestEvent(
        key,
        (value, unsubscribe) => {
          const previousUnsubscribe = this.#contextSubscriptions.get(key);

          // Call the old unsubscribe callback if it has changed. This probably
          // means we have a new provider.
          if (previousUnsubscribe !== unsubscribe) {
            previousUnsubscribe?.();
          }

          const decoded = decode(value, decoder);
          this.#contextSubscriptions.set(key, unsubscribe);

          if (Result$isOk(decoded)) {
            this.dispatch(Result$Ok$0(decoded), true);
          }
        },
        true,
      )
    );
  }

  unsubscribe(key) {
    const unsubscribe = this.#contextSubscriptions.get(key);

    if (unsubscribe) {
      unsubscribe();
      this.#contextSubscriptions.delete(key);
    }
  }

  unsubscribeAll() {
    for (const [_, unsubscribe] of this.#contextSubscriptions) {
      unsubscribe?.();
    }

    this.#contextSubscriptions.clear();
  }

  // PRIVATE API ---------------------------------------------------------------

  #model;
  #view;
  #update;

  #vdom;
  #cache;
  #reconciler;
  #contexts = new Map();
  #contextSubscriptions = new Map();

  #shouldQueue = false;
  #queue = [];

  #phases = [];
  #pending = new Map();
  #renderTimer = null;

  #platformScheduleRender;
  #platformAfterRender;
  #cancelRender = null;

  #actions = {
    dispatch: (message) => this.dispatch(message),
    emit: (event, data) => this.emit(event, data),
    select: () => {},
    root: () => this.root,
    provide: (key, value) => this.provide(key, value),
    subscribe: (key, decoder) => this.subscribe(key, decoder),
    unsubscribe: (key) => this.unsubscribe(key),
  };

  #scheduleRender(shouldFlush = false) {
    if (this.#renderTimer) return;

    if (shouldFlush) {
      // when rendering synchronously, we still want to delay using a microtask
      // to batch all attribute/property updates.
      this.#renderTimer = "sync";
      queueMicrotask(() => this.#render());
    } else {
      this.#renderTimer = true;
      this.#cancelRender = this.#platformScheduleRender(() => this.#render());
    }
  }

  // #handleEffects processes all effects, without scheduling a render.
  #handleEffects(effects) {
    // By flipping this on before we process the list of synchronous effects, we
    // make it so that any messages dispatched immediately will be queued up and
    // applied before the next render.
    this.#shouldQueue = true;

    // Deferred effects get run without a preceeding `update`. To know if we
    // need to schedule another frame, we need to know if the `model` has been
    // touched while processing effects.
    let updateCalledDuringEffects = false;

    // We step into this loop to process any synchronous effects and batch any
    // deferred ones. When a synchronous effect immediately dispatches a message,
    // we add it to a queue and process another `update` cycle. This continues
    // until there are no more synchronous effects or messages to process.
    while (true) {
      // We pass the runtime directly to each effect. It has all the methods
      // of the `Actions` record define in the effect module.
      iterate(effects.synchronous, (effect) => effect(this.#actions));

      // `deferred` is a list of phase-tagged tasks that should be deferred
      // until we next perform a render. Each entry is a Gleam tuple of a phase
      // name and a task; we batch each task into its phase's pending queue in
      // order. Tasks tagged with a phase this platform does not declare are
      // silently dropped — the defined semantics for undeclared phases.
      iterate(effects.deferred, ([name, task]) => {
        const pending = this.#pending.get(name);
        if (pending) pending.push(task);
      });

      // Once we've batched any deferred effects, we check if there are any
      // messages in the queue. If not, we can break out of the loop and continue
      // with the render.
      if (!this.#queue.length) break;

      // This is a destructuring assignment pattern that is mutating both
      // `this.#model` and the argument to this function: `effects`!
      const message = this.#queue.shift();
      [this.#model, effects] = this.#update(this.#model, message);

      updateCalledDuringEffects = true;
    }

    // Remember to flip this off so subsequent messages trigger another tick.
    this.#shouldQueue = false;

    return updateCalledDuringEffects;
  }

  // Async effects (drained phase tasks) can trigger without causing a new
  // model update. Here we process these effects and schedules the next
  // (synchronous) frame if required.
  #handleAsyncEffects(effects) {
    if (this.#handleEffects(effects)) {
      this.#scheduleRender(true);
    }
  }

  #render() {
    this.#renderTimer = null;
    this.#cancelRender = null;

    const next = this.#view(this.#model);
    const { patch, cache } = diff(this.#cache, this.#vdom, next);

    this.#cache = cache;
    this.#vdom = next;
    this.#reconciler.push(patch, Cache.memos(cache));

    this.#platformAfterRender();

    // We have performed a render; the platform's target has been updated. Now
    // invoke the schedulers of phases with pending tasks in the platform's
    // declaration order — the relative timing between phases is determined by
    // the schedulers themselves. `splice(0)` empties each buffer before
    // scheduling so tasks scheduled during a phase callback accumulate for the
    // next render instead.
    for (const phase of this.#phases) {
      const pending = this.#pending.get(phase.name);
      if (!pending.length) continue;

      const effects = makeEffect(toList(pending.splice(0)));

      phase.schedule(() => this.#handleAsyncEffects(effects));
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
    deferred: empty_list,
  };
}

const copiedStyleSheets = new WeakMap();

export async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of globalThis.document.querySelectorAll(
    "link[rel=stylesheet], style",
  )) {
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

  for (const sheet of globalThis.document.styleSheets) {
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

export class ContextRequestEvent extends Event {
  constructor(context, callback, subscribe) {
    super("context-request", { bubbles: true, composed: true });

    this.context = context;
    this.callback = callback;
    this.subscribe = subscribe;
  }
}

// We subclass `CustomEvent` so we know when it's safe to automatically include
// the `detail` field of an event in server component event handlers. In the future
// we may be able to derive what fields to include by introspecting the decoders
// itself and then this can go away.
export class LustreEvent extends CustomEvent {
  // We can't rely on `instanceof` checks because the server component client
  // runtime is bundled on its own and thus will have its own copy of this class.
  isLustreEvent = true;

  constructor(name, detail) {
    super(name, { detail, bubbles: true, composed: true });
  }
}