// IMPORTS ---------------------------------------------------------------------

import { Ok, Error } from "../../../gleam.mjs";
import { ElementNotFound, NotABrowser } from "../../../lustre.mjs";
import { is_browser, Runtime } from "./runtime.ffi.mjs";
import {
  EffectDispatchedMessage,
  EffectEmitEvent,
  SystemRequestedShutdown,
} from "../server/runtime.mjs";
import { document } from "../../internals/constants.ffi.mjs";

//

export class Spa {
  static start({ init, update, view }, selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());

    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);

    if (!root) return new Error(new ElementNotFound(selector));

    return new Ok(new Spa(root, init(flags), update, view));
  }

  #runtime;

  constructor(root, [init, effects], update, view) {
    this.#runtime = new Runtime(root, [init, effects], view, update);
  }

  send(message) {
    switch (message.constructor) {
      case EffectDispatchedMessage: {
        this.dispatch(message.message, false);
        break;
      }

      case EffectEmitEvent: {
        this.emit(message.name, message.data);
        break;
      }

      case SystemRequestedShutdown:
        //TODO
        break;
    }
  }

  dispatch(msg, immediate) {
    this.#runtime.dispatch(msg, immediate);
  }

  emit(event, data) {
    this.#runtime.emit(event, data);
  }
}

export const start = Spa.start;
