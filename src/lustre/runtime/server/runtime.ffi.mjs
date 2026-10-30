import {
  Result$Ok,
  Result$Ok$0,
  Result$isOk,
  List$isNonEmpty,
  List$NonEmpty$rest,
  List$NonEmpty$first,
} from "../../../gleam.mjs";
import * as Decode from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as Dict from "../../../../gleam_stdlib/gleam/dict.mjs";
import * as Option from "../../../../gleam_stdlib/gleam/option.mjs";
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
  Message$EffectRequestedContextSubscription,
  Message$isEffectRequestedContextSubscription,
  Message$EffectRemovedContextSubscription,
  Message$isEffectRemovedContextSubscription,
  //
  Message$isSystemRequestedShutdown,
} from "./runtime.mjs";
import * as App from "../app.mjs";
import * as Effect from "../../effect.mjs";
import * as Transport from "../transport.mjs";
import {
  ServerMessage$isBatch,
  ServerMessage$isAttributeChanged,
  ServerMessage$isPropertyChanged,
  ServerMessage$isEventFired,
  ServerMessage$isContextProvided,
} from "../transport.mjs";

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

  constructor(init, update, view, config, start_arguments) {
    const [model, effects] = init(start_arguments);
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#config = config;

    this.#vdom = this.#view(this.#model);
    this.#cache = Cache.from_node(this.#vdom);

    this.#handle_effect(effects);
  }

  send(message) {
    if (Message$isClientDispatchedMessage(message)) {
      const { message: clientMessage } = message;
      const effects = [];

      this.#handle_client_message(clientMessage, effects);
      this.#render_and_broadcast();

      for (const effect of effects) {
        this.#handle_effect(effect);
      }
    } else if (Message$isClientRegisteredCallback(message)) {
      const { callback } = message;
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

      if (Option.Option$isSome(this.#config.on_connect)) {
        const connectMessage = Option.Option$Some$0(this.#config.on_connect);
        const [model, effect] = this.#update(this.#model, connectMessage);

        this.#model = model;

        this.#render_and_broadcast();
        this.#handle_effect(effect);
      }
    } else if (Message$isClientDeregisteredCallback(message)) {
      const { callback } = message;
      this.#callbacks.delete(callback);

      if (Option.Option$isSome(this.#config.on_disconnect)) {
        const disconnectMessage = Option.Option$Some$0(
          this.#config.on_disconnect,
        );
        const [model, effect] = this.#update(this.#model, disconnectMessage);

        this.#model = model;

        this.#render_and_broadcast();
        this.#handle_effect(effect);
      }
    } else if (Message$isEffectDispatchedMessage(message)) {
      const { message: appMessage } = message;
      const [model, effect] = this.#update(this.#model, appMessage);

      this.#model = model;

      this.#render_and_broadcast();
      this.#handle_effect(effect);
    } else if (Message$isEffectEmitEvent(message)) {
      const { name, data } = message;
      this.broadcast(Transport.emit(name, data));
    } else if (Message$isEffectProvidedValue(message)) {
      const { key, value } = message;
      const existing = Dict.get(this.#providers, key);
      // we do not need to broadcast an update if the provided value is the same.
      if (Result$isOk(existing) && isEqual(Result$Ok$0(existing), value)) {
        return;
      }

      this.#providers = Dict.insert(this.#providers, key, value);
      this.broadcast(Transport.provide(key, value));
    } else if (Message$isEffectRequestedContextSubscription(message)) {
      const { key, decoder } = message;

      this.broadcast(Transport.subscribe(key));
      this.#config.contexts = Dict.insert(this.#config.contexts, key, decoder);
    } else if (Message$isEffectRemovedContextSubscription(message)) {
      const { key } = message;

      this.broadcast(Transport.unsubscribe(key));
      this.#config.contexts = Dict.delete$(this.#config.contexts, key);
    } else if (Message$isSystemRequestedShutdown(message)) {
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

  broadcast(message) {
    for (const callback of this.#callbacks) {
      callback(message);
    }
  }

  // Renders the current model, computes the diff, and broadcasts the patch to
  // all connected clients.
  #render_and_broadcast() {
    const next = this.#view(this.#model);
    const diff = Diff.diff(this.#cache, this.#vdom, next);

    this.#vdom = next;
    this.#cache = diff.cache;

    this.broadcast(Transport.reconcile(diff.patch, Cache.memos(diff.cache)));
  }

  // Handles a message or batch of messages received from the client without
  // rendering. The provided `effects` array is mutated to accumulate effects
  // from batched messages.
  #handle_client_message(message, effects) {
    if (ServerMessage$isBatch(message)) {
      const { messages } = message;

      for (
        let list = messages;
        List$isNonEmpty(list);
        list = List$NonEmpty$rest(list)
      ) {
        this.#handle_client_message(List$NonEmpty$first(list), effects);
      }
    } else if (ServerMessage$isAttributeChanged(message)) {
      const { name, value } = message;
      const result = this.#handle_attribute_change(name, value);

      if (Result$isOk(result)) {
        effects.push(this.#dispatch(Result$Ok$0(result)));
      }
    } else if (ServerMessage$isPropertyChanged(message)) {
      const { name, value } = message;
      const result = this.#handle_properties_change(name, value);

      if (Result$isOk(result)) {
        effects.push(this.#dispatch(Result$Ok$0(result)));
      }
    } else if (ServerMessage$isEventFired(message)) {
      const { path, name, event } = message;
      const [cache, result] = Cache.handle(this.#cache, path, name, event);

      this.#cache = cache;

      if (Result$isOk(result)) {
        effects.push(this.#dispatch(Result$Ok$0(result).message));
      }
    } else if (ServerMessage$isContextProvided(message)) {
      const { key, value } = message;
      const decoder = Dict.get(this.#config.contexts, key);

      if (Result$isOk(decoder)) {
        const result = Decode.run(value, Result$Ok$0(decoder));

        if (Result$isOk(result)) {
          effects.push(this.#dispatch(Result$Ok$0(result)));
        }
      }
    }
  }

  #dispatch(message) {
    const [model, effect] = this.#update(this.#model, message);
    this.#model = model;

    return effect;
  }

  #handle_attribute_change(name, value) {
    const result = Dict.get(this.#config.attributes, name);
    if (!Result$isOk(result)) {
      return result;
    }

    return Result$Ok$0(result)(value);
  }

  #handle_properties_change(name, value) {
    const decoder = Dict.get(this.#config.properties, name);
    if (!Result$isOk(decoder)) {
      return decoder;
    }

    return Decode.run(value, Result$Ok$0(decoder));
  }

  #handle_effect(effect) {
    const dispatch = (message) =>
      this.send(Message$EffectDispatchedMessage(message));
    const emit = (name, data) => this.send(Message$EffectEmitEvent(name, data));
    const select = () => undefined;
    const internals = () => undefined;
    const provide = (key, value) =>
      this.send(Message$EffectProvidedValue(key, value));
    const subscribe = (key, decoder) =>
      this.send(Message$EffectRequestedContextSubscription(key, decoder));
    const unsubscribe = (key) =>
      this.send(Message$EffectRemovedContextSubscription(key));

    globalThis.queueMicrotask(() => {
      Effect.perform(
        effect,
        dispatch,
        emit,
        select,
        internals,
        provide,
        subscribe,
        unsubscribe,
      );
    });
  }
}

export const start = (app, start_arguments) => {
  const config = App.configure_server_component(app.config);

  return Result$Ok(
    new Runtime(app.init, app.update, app.view, config, start_arguments),
  );
};

export const send = (runtime, message) => {
  runtime.send(message);
};
