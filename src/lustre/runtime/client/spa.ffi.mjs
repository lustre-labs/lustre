// IMPORTS ---------------------------------------------------------------------

import { Result$Ok, Result$Error } from "../../../gleam.mjs";
import { Error$ElementNotFound, Error$NotABrowser } from "../../../lustre.mjs";
import { is_browser, Runtime } from "./runtime.ffi.mjs";
import {
  Message$isEffectDispatchedMessage,
  Message$isEffectEmitEvent,
  Message$isSystemRequestedShutdown,
} from "../server/runtime.mjs";
import { document } from "../../internals/constants.ffi.mjs";

//

export class Spa {
  #runtime;

  constructor(root, [init, effects], update, view) {
    this.#runtime = new Runtime(root, [init, effects], view, update);
  }

  send(message) {
    if (Message$isEffectDispatchedMessage(message)) {
      this.dispatch(message.message, false);
    } else if (Message$isEffectEmitEvent(message)) {
      this.emit(message.name, message.data);
    } else if (Message$isSystemRequestedShutdown(message)) {
      // TODO
    }
  }

  dispatch(msg) {
    this.#runtime.dispatch(msg);
  }

  emit(event, data) {
    this.#runtime.emit(event, data);
  }
}

export const start = ({ init, update, view }, selector, flags) => {
  if (!is_browser()) return Result$Error(Error$NotABrowser());

  const root =
    selector instanceof HTMLElement
      ? selector
      : document().querySelector(selector);

  if (!root) return Result$Error(Error$ElementNotFound(selector));

  return Result$Ok(new Spa(root, init(flags), update, view));
};
