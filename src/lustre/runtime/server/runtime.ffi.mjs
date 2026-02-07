import {
  Result$Ok,
  Result$Ok$0,
  Result$isOk,
  List$NonEmpty$rest,
  List$NonEmpty$first,
} from "../../../gleam.mjs";
import * as Decode from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as Dict from "../../../../gleam_stdlib/gleam/dict.mjs";
import * as Diff from "../../vdom/diff.mjs";
import * as Cache from "../../vdom/cache.mjs";
import { isEqual } from "../../internals/equals.ffi.mjs";
import {
  Message$isClientDispatchedMessage,
  Message$isClientRegisteredCallback,
  Message$isClientDeregisteredCallback,
  //
  Message$EffectDispatchedMessage,
  Message$isEffectDispatchedMessage,
  Message$EffectEmitEvent,
  Message$isEffectEmitEvent,
  Message$EffectProvidedValue,
  Message$isEffectProvidedValue,
  Message$EffectRegisteredCleanup,
  Message$isEffectRegisteredCleanup,
  Message$EffectRequestedCleanup,
  Message$isEffectRequestedCleanup,
  //
  Message$isSystemRequestedShutdown,
} from "./runtime.mjs";
import * as Component from "../../component.mjs";
import * as Effect from "../effect.mjs";
import * as Transport from "../transport.mjs";
import {
  ServerMessage$isBatch,
  ServerMessage$isAttributeChanged,
  ServerMessage$isPropertyChanged,
  ServerMessage$isEventFired,
  ServerMessage$isContextProvided,
} from "../transport.mjs";
import { toList } from "../../internals/list.ffi.mjs";

//

export class Runtime {
  #inert = false;
  #model;
  #update;
  #view;
  #config;

  #vdom;
  #cache;
  #providers = Dict.new$();

  #callbacks = /* @__PURE__ */ new Set();

  #actions;
  #effects = /* @__PURE__ */ new Map();

  constructor(_, init, update, view, config, start_arguments) {
    const [model, effects] = init(start_arguments);
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#config = config;

    this.#vdom = this.#view(this.#model);
    this.#cache = Cache.from_node(this.#vdom);

    this.#actions = Effect.Actions$Actions(
      /*  cleanup */ (key) => {
        this.send(Message$EffectRequestedCleanup(key));
      },
      /* dispatch */ (message) => {
        this.send(Message$EffectDispatchedMessage(message));
      },
      /*     emit */ (name, data) => {
        this.send(Message$EffectEmitEvent(name, data));
      },
      /*  provide */ (key, value) => {
        this.send(Message$EffectProvidedValue(key, value));
      },
      /* register */ (key, cleanup) => {
        this.send(Message$EffectRegisteredCleanup(key, cleanup));
      },
      /*     root */ () => undefined,
      /*   select */ (selector) => undefined,
    );

    this.#handle_effect(effects);
  }

  send(msg) {
    if (this.#inert) return;
    if (Message$isClientDispatchedMessage(msg)) {
      const { message } = msg;
      const next = this.#handle_client_message(message);
      const diff = Diff.diff(this.#cache, this.#vdom, next);

      this.#vdom = next;
      this.#cache = diff.cache;

      this.broadcast(Transport.reconcile(diff.patch, Cache.memos(diff.cache)));
    } else if (Message$isClientRegisteredCallback(msg)) {
      const { callback } = msg;
      this.#callbacks.add(callback);

      callback(
        Transport.mount(
          this.#config.open_shadow_root,
          this.#config.adopt_styles,
          Dict.keys(this.#config.attributes),
          Dict.keys(this.#config.properties),
          Dict.keys(this.#config.contexts),
          this.#providers,
          this.#vdom,
          Cache.memos(this.#cache),
        ),
      );
    } else if (Message$isClientDeregisteredCallback(msg)) {
      const { callback } = msg;
      this.#callbacks.delete(callback);
    } else if (Message$isEffectDispatchedMessage(msg)) {
      const { message } = msg;
      const [model, effect] = this.#update(this.#model, message);
      const next = this.#view(model);
      const diff = Diff.diff(this.#cache, this.#vdom, next);

      this.#handle_effect(effect);

      this.#model = model;
      this.#vdom = next;
      this.#cache = diff.cache;

      this.broadcast(Transport.reconcile(diff.patch, Cache.memos(diff.cache)));
    } else if (Message$isEffectEmitEvent(msg)) {
      const { name, data } = msg;
      this.broadcast(Transport.emit(name, data));
    } else if (Message$isEffectProvidedValue(msg)) {
      const { key, value } = msg;
      const existing = Dict.get(this.#providers, key);
      // we do not need to broadcast an update if the provided value is the same.
      if (Result$isOk(existing) && isEqual(Result$Ok$0(existing), value)) {
        return;
      }

      this.#providers = Dict.insert(this.#providers, key, value);
      this.broadcast(Transport.provide(key, value));
    } else if (Message$isEffectRegisteredCleanup(msg)) {
      const { key, callback } = msg;

      this.#effects.set(key, [callback, ...(this.#effects.get(key) ?? [])]);
    } else if (Message$isEffectRequestedCleanup(msg)) {
      const { key } = msg;

      if (this.#effects.has(key)) {
        for (const callback of this.#effects.get(key)) {
          callback();
        }

        this.#effects.delete(key);
      }
    } else if (Message$isSystemRequestedShutdown(msg)) {
      this.#inert = true;
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#config = null;
      this.#vdom = null;
      this.#cache = null;
      this.#providers = null;
      this.#callbacks.clear();

      for (const cleanup of this.#effects.values()) {
        for (const callback of cleanup) {
          callback();
        }
      }

      this.#effects.clear();
    }
  }

  broadcast(msg) {
    if (this.#inert) return;
    for (const callback of this.#callbacks) {
      callback(msg);
    }
  }

  #handle_client_message(msg) {
    if (ServerMessage$isBatch(msg)) {
      const { messages } = msg;
      let model = this.#model;
      let effect = Effect.none();

      for (
        let list = messages;
        List$NonEmpty$rest(list);
        list = List$NonEmpty$rest(list)
      ) {
        const result = this.#handle_client_message(List$NonEmpty$first(list));
        if (Result$isOk(result)) {
          model = Result$Ok$0(result)[0];
          effect = Effect.batch(toList([effect, Result$Ok$0(result)[1]]));
          break;
        }
      }

      this.#handle_effect(effect);
      this.#model = model;

      return this.#view(model);
    } else if (ServerMessage$isAttributeChanged(msg)) {
      const { name, value } = msg;
      const result = this.#handle_attribute_change(name, value);
      if (!Result$isOk(result)) {
        return this.#vdom;
      }

      return this.#dispatch(Result$Ok$0(result));
    } else if (ServerMessage$isPropertyChanged(msg)) {
      const { name, value } = msg;
      const result = this.#handle_properties_change(name, value);
      if (!Result$isOk(result)) {
        return this.#vdom;
      }

      return this.#dispatch(Result$Ok$0(result));
    } else if (ServerMessage$isEventFired(msg)) {
      const { path, name, event } = msg;
      const [cache, result] = Cache.handle(this.#cache, path, name, event);

      this.#cache = cache;
      if (!Result$isOk(result)) {
        return this.#vdom;
      }

      const { message } = Result$Ok$0(result);
      return this.#dispatch(message);
    } else if (ServerMessage$isContextProvided(msg)) {
      const { key, value } = msg;
      let result = Dict.get(this.#config.contexts, key);
      if (!Result$isOk(result)) {
        return this.#vdom;
      }

      result = Decode.run(value, Result$Ok$0(result));
      if (!Result$isOk(result)) {
        return this.#vdom;
      }

      return this.#dispatch(Result$Ok$0(result));
    }
  }

  #dispatch(msg) {
    const [model, effects] = this.#update(this.#model, msg);
    this.#handle_effect(effects);
    this.#model = model;

    return this.#view(this.#model);
  }

  #handle_attribute_change(name, value) {
    const result = Dict.get(this.#config.attributes, name);
    if (!Result$isOk(result)) {
      return result;
    }

    return Result$Ok$0(result)(value);
  }

  #handle_properties_change(name, value) {
    const result = Dict.get(this.#config.properties, name);
    if (!Result$isOk(result)) {
      return result;
    }

    return Result$Ok$0(result)(value);
  }

  #handle_effect(effect) {
    globalThis.queueMicrotask(() => {
      Effect.perform(effect, this.#actions);
    });
  }
}

export const start = (app, start_arguments) => {
  const config = Component.to_server_component_config(app.config);

  return Result$Ok(
    new Runtime(app.init, app.update, app.view, config, start_arguments),
  );
};

export const send = (runtime, message) => {
  runtime.send(message);
};
