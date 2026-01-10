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
  //
  Message$isSystemRequestedShutdown,
} from "./runtime.mjs";
import * as Component from "../../component.mjs";
import * as Effect from "../../effect.mjs";
import * as Transport from "../transport.mjs";
import {
  ServerMessage$isBatch,
  ServerMessage$isAttributeChanged,
  ServerMessage$isPropertyChanged,
  ServerMessage$isEventFired,
  ServerMessage$isContextProvided,
} from "../transport.mjs";
import { iterate, toList } from "../../internals/list.ffi.mjs";

//

export class Runtime {
  #model;
  #update;
  #view;
  #config;

  #vdom;
  #cache;
  #providers = Dict.new$();

  #callbacks = /* @__PURE__ */ new Set();

  constructor([model, effects], update, view, config) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#config = config;

    this.#vdom = this.#view(this.#model);
    this.#cache = Cache.from_node(this.#vdom);

    this.#handle_effect(effects);
  }

  send(msg) {
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
    } else if (Message$isSystemRequestedShutdown(msg)) {
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#config = null;
      this.#vdom = null;
      this.#cache = null;
      this.#providers = null;
      this.#callbacks.clear();
    }
  }

  broadcast(msg) {
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
    const dispatch = (message) =>
      this.send(Message$EffectDispatchedMessage(message));
    const emit = (name, data) => this.send(Message$EffectEmitEvent(name, data));
    const select = () => undefined;
    const internals = () => undefined;
    const provide = (key, value) =>
      this.send(Message$EffectProvidedValue(key, value));

    globalThis.queueMicrotask(() => {
      Effect.perform(effect, dispatch, emit, select, internals, provide);
    });
  }
}

export const start = (app, flags) => {
  const config = Component.to_server_component_config(app.config);

  return Result$Ok(new Runtime(app.init(flags), app.update, app.view, config));
};

export const send = (runtime, message) => {
  runtime.send(message);
};
