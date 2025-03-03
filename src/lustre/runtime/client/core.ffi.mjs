// IMPORTS ---------------------------------------------------------------------

import { diff } from "../../vdom/diff.mjs";
import * as Events from "../../vdom/events.mjs";
import { Reconciler } from "./reconciler.ffi.mjs";

//

export const is_browser = () => globalThis.window && window.document;

export const is_registered = (name) =>
  globalThis.window && globalThis.window.customElements.get(name);

export const is_reference_equal = (a, b) => a === b;

export const throw_server_component_error = () => {
  throw new window.Error(
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
  #root;

  #model;
  #view;
  #update;

  #vdom;
  #events;
  #reconciler;
  #viewTimer = null;

  initialNodeOffset = 0;

  constructor(root, [model, effects], view, update) {
    this.#root = root;
    this.#model = model;
    this.#view = view;
    this.#update = update;

    this.#vdom = this.#view(this.#model);
    this.#events = Events.add_child(Events.new$(), (msg) => msg, 0, this.#vdom);

    this.#reconciler = new Reconciler(
      this.#root,
      (event, path, name, immediate) => {
        const msg = Events.handle(this.#events, path, name, event);

        if (msg.isOk()) {
          this.dispatch(msg[0], immediate);
        }
      },
    );

    this.#reconciler.mount(this.#vdom);
    this.#tick(effects.all, false);
  }

  dispatch(msg, immediate = false) {
    const [model, effects] = this.#update(this.#model, msg);

    this.#model = model;
    this.#tick(effects.all, immediate);
  }

  #tick(effects, immediate = false) {
    const queue = [];
    const effect_params = {
      root: this.#root,
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
    this.#viewTimer = null;

    const next = this.#view(this.#model);
    const { patch, events } = diff(
      this.#vdom,
      next,
      this.#events,
      this.initialNodeOffset,
    );
    this.#events = events;
    this.#vdom = next;
    this.#reconciler.push(patch, this.#events);
  }

  #emit(event, data) {
    const target = this.#root.host ?? this.#root;

    target.dispatchEvent(
      new CustomEvent(event, {
        detail: data,
        bubbles: true,
        composed: true,
      }),
    );
  }
}

//

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
