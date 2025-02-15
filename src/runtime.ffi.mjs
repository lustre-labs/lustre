import Dict from "../gleam_stdlib/dict.mjs";
import { Dispatch } from "./lustre/internals/runtime.mjs";
import { ElementNotFound, NotABrowser } from "./lustre.mjs";
import { LustreReconciler } from "./reconciler.ffi.mjs";
import { Ok, Error, NonEmpty, isEqual } from "./gleam.mjs";
import { diff } from "./lustre/runtime/vdom.mjs";
import { Some } from "../gleam_stdlib/gleam/option.mjs";

export const is_browser = () => globalThis.window && window.document;

export const is_reference_equal = (a, b) => a === b;

// SPA -------------------------------------------------------------------------

export class LustreSPA {
  static start({ init, update, view }, selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());

    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);

    if (!root) return new Error(new ElementNotFound(selector));

    const app = new LustreSPA(root, init(flags), update, view);

    return new Ok((action) => app.send(action));
  }

  #model;
  #update;
  #view;

  #prev;
  #reconciler;
  #reconciler_handlers = new Dict();

  constructor(root, [init, effects], update, view) {
    this.root = root;

    this.#model = init;
    this.#update = update;
    this.#view = view;

    this.#reconciler = new LustreReconciler(root, (msg, immediate) => {
      if (msg.constructor === Ok) {
        this.#dispatch(msg[0], immediate);
      }
    });

    this.#prev = view(init);
    this.#reconciler.mount(this.#prev);

    if (effects.all instanceof NonEmpty) {
      window.requestAnimationFrame(() => {
        this.#tick(effects.all.toArray());
      });
    }
  }

  send(action) {
    switch (action.constructor) {
      case Dispatch: {
        this.#dispatch(action[0], action[1]);
        break;
      }
    }
  }

  #dispatch(msg, immediate = false) {
    const [next, effects] = this.#update(this.#model, msg);

    this.#model = next;
    this.#tick(effects.all, immediate);
  }

  #tick(effects, immediate = false) {
    const dispatch = (msg, immediate) => {
      this.#dispatch(msg, immediate);
    };

    const emit = (event, data) =>
      this.root.dispatchEvent(
        new CustomEvent(event, {
          detail: data,
          bubbles: true,
          composed: true,
        }),
      );
    const select = () => {};
    const root = this.root;

    for (const effect of effects) {
      effect({ dispatch, emit, select, root });
    }

    console.group("render");
    console.time("render");
    console.time("view");
    const next = this.#view(this.#model);
    console.timeEnd("view");

    console.time("diff");
    const { patch, handlers } = diff(
      this.#prev,
      next,
      this.#reconciler_handlers,
    );
    console.timeEnd("diff");

    this.#reconciler_handlers = handlers;

    this.#reconciler.push(patch, { flush: true });
    this.#prev = next;
    console.timeEnd("render");
    console.groupEnd("render");
  }
}

export const start = LustreSPA.start;

// COMPONENT -------------------------------------------------------------------

export const make_lustre_client_component = (
  { init, update, view, on_attribute_change },
  name,
) => {
  if (!is_browser()) return new Error(new NotABrowser());
  if (!name.includes("-")) return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }

  const [model, effects] = init(undefined);
  const hasAttributes = on_attribute_change instanceof Some;
  const component = class LustreClientComponent extends HTMLElement {
    static get observedAttributes() {
      if (hasAttributes) {
        return on_attribute_change[0].entries().map(([name]) => name);
      } else {
        return [];
      }
    }

    #connected = false;
    #adoptedStyleElements = [];

    #model = model;

    #prev = view(model);
    #reconciler;
    #reconciler_handlers = new Dict();

    constructor() {
      super();
      this.attachShadow({ mode: "open" });
      this.internals = this.attachInternals();

      this.#reconciler = new LustreReconciler(
        this.shadowRoot,
        (msg, immediate) => {
          if (msg.constructor === Ok) {
            this.#dispatch(msg[0], immediate);
          }
        },
      );
      this.#reconciler.mount(this.#prev);

      if (hasAttributes) {
        on_attribute_change[0].forEach((decoder, name) => {
          Object.defineProperty(this, name, {
            get() {
              return this[`__mirrored__${name}`];
            },

            set(value) {
              const prev = this[`__mirrored__${name}`];
              if (this.#connected && isEqual(prev, value)) return;
              this[`__mirrorred__${name}`] = value;
              const decoded = decoder(value);

              if (decoded.constructor === Ok) {
                this.#dispatch(decoded[0]);
              }
            },
          });
        });
      }
    }

    connectedCallback() {
      this.#adoptStyleSheets().finally(() => {
        this.#tick(effects.all, true);
        this.#connected = true;
      });
    }

    attributeChangedCallback(key, prev, next) {
      if (prev !== next) this[key] = next;
    }

    disconnectedCallback() {
      this.#model = null;
      this.#connected = false;
    }

    send(action) {
      switch (action.constructor) {
        case Dispatch: {
          this.#dispatch(action[0], action[1]);
          break;
        }
      }
    }

    #dispatch(msg, immediate = false) {
      const [next, effects] = update(this.#model, msg);

      this.#model = next;
      this.#tick(effects.all, immediate);
    }

    #tick(effects, immediate = false) {
      this.#flush(effects.all, immediate);

      const next = view(this.#model);
      const { patch, handlers } = diff(
        this.#prev,
        next,
        this.#reconciler_handlers,
      );

      this.#reconciler_handlers = handlers;

      this.#reconciler.push(patch, { flush: immediate });
      this.#prev = next;
    }

    #flush(effects = []) {
      const dispatch = (msg, immediate) => this.#dispatch(msg, immediate);
      const emit = (event, data) =>
        this.dispatchEvent(
          new CustomEvent(event, {
            detail: data,
            bubbles: true,
            composed: true,
          }),
        );
      const select = () => {};
      const root = this.shadowRoot;

      for (const effect of effects) {
        effect({ dispatch, emit, select, root });
      }
    }

    async #adoptStyleSheets() {
      const pendingParentStylesheets = [];
      for (const link of document.querySelectorAll("link[rel=stylesheet]")) {
        if (link.sheet) continue;

        pendingParentStylesheets.push(
          new Promise((resolve, reject) => {
            link.addEventListener("load", resolve);
            link.addEventListener("error", reject);
          }),
        );
      }

      await Promise.allSettled(pendingParentStylesheets);

      while (this.#adoptedStyleElements.length) {
        this.#adoptedStyleElements.shift().remove();
        this.shadowRoot.firstChild.remove();
      }

      this.shadowRoot.adoptedStyleSheets =
        this.getRootNode().adoptedStyleSheets;

      const pending = [];

      for (const sheet of document.styleSheets) {
        try {
          this.shadowRoot.adoptedStyleSheets.push(sheet);
        } catch {
          try {
            const adoptedSheet = new CSSStyleSheet();
            for (const rule of sheet.cssRules) {
              adoptedSheet.insertRule(
                rule.cssText,
                adoptedSheet.cssRules.length,
              );
            }

            this.shadowRoot.adoptedStyleSheets.push(adoptedSheet);
          } catch {
            const node = sheet.ownerNode.cloneNode();

            this.shadowRoot.prepend(node);
            this.#adoptedStyleElements.push(node);

            pending.push(
              new Promise((resolve, reject) => {
                node.onload = resolve;
                node.onerror = reject;
              }),
            );
          }
        }
      }

      return Promise.allSettled(pending);
    }
  };

  window.customElements.define(name, component);

  return new Ok(undefined);
};
