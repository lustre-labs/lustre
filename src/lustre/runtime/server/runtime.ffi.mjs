import * as Diff from "../../vdom/diff.mjs";
import * as Events from "../../vdom/events.mjs";
import {
  ClientDispatchedMessage,
  ClientRegisteredCallback,
  ClientDeregisteredCallback,
  //
  EffectDispatchedMessage,
  EffectEmitEvent,
  EffectProvidedValue,
  //
  SystemRequestedShutdown,
} from "./runtime.mjs";
import * as Component from "../../component.mjs";
import * as Effect from "../../effect.mjs";
import * as Transport from "../transport.mjs";
import {
  Batch,
  AttributeChanged,
  PropertyChanged,
  EventFired,
  ContextProvided,
} from "../transport.mjs";
import * as Decode from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { Ok, Error, List } from "../../../gleam.mjs";
import * as Dict from "../../../../gleam_stdlib/gleam/dict.mjs";

//

export class Runtime {
  #model;
  #update;
  #view;
  #config;

  #vdom;
  #events;
  #providers = Dict.new$();

  #callbacks = /* @__PURE__ */ new Set();

  constructor([model, effects], update, view, config) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#config = config;

    this.#vdom = this.#view(this.#model);
    this.#events = Events.from_node(this.#vdom);

    this.#handle_effect(effects);
  }

  send(msg) {
    switch (msg.constructor) {
      case ClientDispatchedMessage: {
        const { message } = msg;
        const next = this.#handle_client_message(message);
        const diff = Diff.diff(this.#events, this.#vdom, next);

        this.#vdom = next;
        this.#events = diff.events;

        this.broadcast(Transport.reconcile(diff.patch));

        return undefined;
      }

      case ClientRegisteredCallback: {
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
          ),
        );

        return undefined;
      }

      case ClientDeregisteredCallback: {
        const { callback } = msg;
        this.#callbacks.delete(callback);

        return undefined;
      }

      case EffectDispatchedMessage: {
        const { message } = msg;
        const [model, effect] = this.#update(this.#model, message);
        const next = this.#view(model);
        const diff = Diff.diff(this.#events, this.#vdom, next);

        this.#handle_effect(effect);

        this.#model = model;
        this.#vdom = next;
        this.#events = diff.events;

        this.broadcast(Transport.reconcile(diff.patch));

        return undefined;
      }

      case EffectEmitEvent: {
        const { name, data } = msg;
        this.broadcast(Transport.emit(name, data));

        return undefined;
      }

      case EffectProvidedValue: {
        const { key, value } = msg;
        this.#providers = Dict.insert(this.#providers, key, value);
        this.broadcast(Transport.provide(key, value));

        return undefined;
      }

      case SystemRequestedShutdown: {
        this.#model = null;
        this.#update = null;
        this.#view = null;
        this.#config = null;
        this.#vdom = null;
        this.#events = null;
        this.#providers = null;
        this.#callbacks.clear();

        return undefined;
      }

      default:
        return undefined;
    }
  }

  broadcast(msg) {
    for (const callback of this.#callbacks) {
      callback(msg);
    }
  }

  #handle_client_message(msg) {
    switch (msg.constructor) {
      case Batch: {
        const { messages } = msg;
        let model = this.#model;
        let effect = Effect.none();

        for (let list = messages; list.head; list = list.tail) {
          const result = this.#handle_client_message(list.head);

          if (result instanceof Ok) {
            model = result[0][0];
            effect = Effect.batch(List.fromArray([effect, result[0][1]]));
            break;
          }
        }

        this.#handle_effect(effect);
        this.#model = model;

        return this.#view(this.#model);
      }

      case AttributeChanged: {
        const { name, value } = msg;
        const result = this.#handle_attribute_change(name, value);

        if (result instanceof Error) {
          return this.#vdom;
        } else {
          const [model, effects] = this.#update(this.#model, result[0]);
          this.#handle_effect(effects);
          this.#model = model;

          return this.#view(this.#model);
        }
      }

      case PropertyChanged: {
        const { name, value } = msg;
        const result = this.#handle_properties_change(name, value);

        if (result instanceof Error) {
          return this.#vdom;
        } else {
          const [model, effects] = this.#update(this.#model, result[0]);
          this.#handle_effect(effects);
          this.#model = model;

          return this.#view(this.#model);
        }
      }

      case EventFired: {
        const { path, name, event } = msg;
        const [events, result] = Events.handle(this.#events, path, name, event);

        this.#events = events;

        if (result instanceof Error) {
          return this.#vdom;
        } else {
          const [model, effects] = this.#update(this.#model, result[0].message);
          this.#handle_effect(effects);
          this.#model = model;

          return this.#view(this.#model);
        }
      }

      case ContextProvided: {
        const { key, value } = msg;
        let result = Dict.get(this.#config.contexts, key);

        if (result instanceof Error) {
          return this.#vdom;
        }

        result = Decode.run(value, result[0]);

        if (result instanceof Error) {
          return this.#vdom;
        }

        const [model, effects] = this.#update(this.#model, result[0]);
        this.#handle_effect(effects);
        this.#model = model;

        return this.#view(this.#model);
      }
    }
  }

  #handle_attribute_change(name, value) {
    const result = Dict.get(this.#config.attributes, name);

    switch (result.constructor) {
      case Ok:
        return result[0](value);

      case Error:
        return new Error(undefined);
    }
  }

  #handle_properties_change(name, value) {
    const result = Dict.get(this.#config.properties, name);

    switch (result.constructor) {
      case Ok:
        return result[0](value);

      case Error:
        return new Error(undefined);
    }
  }

  #handle_effect(effect) {
    const dispatch = (message) =>
      this.send(new EffectDispatchedMessage(message));
    const emit = (name, data) => this.send(new EffectEmitEvent(name, data));
    const select = () => undefined;
    const internals = () => undefined;
    const provide = (key, value) =>
      this.send(new EffectProvidedValue(key, value));

    globalThis.queueMicrotask(() => {
      Effect.perform(effect, dispatch, emit, select, internals, provide);
    });
  }
}

export const start = (app, flags) => {
  const config = Component.to_server_component_config(app.config);

  return new Ok(new Runtime(app.init(flags), app.update, app.view, config));
};

export const send = (runtime, message) => {
  runtime.send(message);
};
