// IMPORTS ---------------------------------------------------------------------

import { Ok, Error } from "../../../gleam.mjs";
import { ElementNotFound, NotABrowser } from "../../../lustre.mjs";
import { is_browser, Runtime } from "./core.ffi.mjs";

//

export class Spa {
  static start({ init, update, view }, selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());

    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);

    if (!root) return new Error(new ElementNotFound(selector));

    return new Spa(root, init(flags), update, view);
  }

  #runtime;

  constructor(root, [init, effects], update, view) {
    this.#runtime = new Runtime(root, [init, effects], view, update);
  }

  send(action) {}
}

export const start = Spa.start;
