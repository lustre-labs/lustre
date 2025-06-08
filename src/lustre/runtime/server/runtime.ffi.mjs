import { diff } from "../../vdom/diff.mjs";
import * as Events from "../../vdom/events.mjs";
import {
  ClientDispatchedMessage,
  ClientRegisteredCallback,
  ClientDeregisteredCallback,
  EffectDispatchedMessage,
  EffectEmitEvent,
  SelfDispatchedMessages,
  SystemRequestedShutdown,
} from "./runtime.mjs";
import {
  //
  Mount,
  Reconcile,
  Emit,
  //
  AttributeChanged,
  EventFired,
} from "../transport.mjs";
import { run as decode } from "../../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import { Ok } from "../../../gleam.mjs";

//

export class Runtime {
  #model;
  #update;
  #view;
  #on_attribute_change;

  #vdom;
  #events;

  #callbacks = new Set();

  constructor([model, effects], view, update, on_attribute_change) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#on_attribute_change = on_attribute_change;

    this.#vdom = this.#view(this.#model);
    this.#events = Events.from_node(this.#vdom);

    this.#tick(effects.all, false);
  }

  send(message) {
    if (this.#model === null) return;

    switch (message.constructor) {
      case ClientDispatchedMessage:
        switch (message.message.constructor) {
          case AttributeChanged: {
            const { name, value } = message.message;
            let effects = [];

            const decoder = this.#on_attribute_change.get(name);
            if (!decoder) break;

            const result = decode(value, decoder);
            if (result.constructor !== Ok) break;

            const [model, more_effects] = this.#update(
              this.#model,
              result[0],
            );

            this.#model = model;
            effects.push(more_effects);

            while (effects.length) {
              this.#tick(effects.shift().all);
            }
          }

          case EventFired: {
            const { path, name, event } = message.message;
            const [events, result] = Events.handle(this.#events, path, name, event);

            this.#events = events;

            if (result.constructor === Ok) {
              this.dispatch(result[0]);
            }

            return;
          }
        }

      case ClientRegisteredCallback: {
        if (this.#callbacks.has(message.callback)) return;

        const mount = new Mount(this.#vdom);

        this.#callbacks.add(message.callback);
        message.callback(mount);

        return;
      }

      case ClientDeregisteredCallback: {
        this.#callbacks.delete(message.callback);

        return;
      }

      case EffectDispatchedMessage: {
        this.dispatch(message.message);

        return;
      }

      case EffectEmitEvent: {
        const event = new Emit(message.name, message.data);

        for (const callback of this.#callbacks) {
          callback(event);
        }

        return;
      }

      case SelfDispatchedMessages: {
        let messages = message.messages;
        let effects = [message.effect];

        for (let list = messages; messages.tail; list = list.tail) {
          const [model, more_effects] = this.#update(this.#model, list.head);

          this.#model = model;
          effects.push(more_effects);
        }

        while (effects.length) {
          this.#tick(effects.shift().all);
        }

        return;
      }

      case SystemRequestedShutdown: {
        this.#model = null;
        this.#update = null;
        this.#view = null;
        this.#on_attribute_change = null;
        this.#events = Events.new$();
        this.#callbacks.clear();
      }
    }
  }

  dispatch(msg) {
    const [model, effects] = this.#update(this.#model, msg);

    this.#model = model;
    this.#tick(effects.all, immediate);
  }

  #tick(effects) {
    const queue = [];
    const effect_params = {
      root: null,
      emit: (event, data) => this.#emit(event, data),
      dispatch: (msg) => queue.push(msg),
      select: () => {},
    };

    while (true) {
      for (let list = effects; list.tail; list = list.tail) {
        list.head(effect_params);
      }

      if (!queue.length) {
        break;
      }

      const msg = queue.shift();

      [this.#model, effects] = this.#update(this.#model, msg);
    }

    this.#render();
  }

  #render() {
    const next = this.#view(this.#model);
    const { patch, events } = diff(this.#events, this.#vdom, next);
    this.#events = events;
    this.#vdom = next;

    const reconcile = new Reconcile(patch);

    for (const callback of this.#callbacks) {
      callback(reconcile);
    }
  }

  #emit(event, data) {
    const message = new Emit(event, data);

    for (const callback of this.#callbacks) {
      callback(message);
    }
  }
}

export const start = (app, flags) => {
  return new Runtime(
    app.init(flags),
    app.update,
    app.view,
    app.on_attribute_change,
  );
};

export const send = (runtime, message) => {
  runtime.send(message);
};
