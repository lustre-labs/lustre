// IMPORTS ---------------------------------------------------------------------

import {
  BadComponentName,
  ComponentAlreadyRegistered,
  ElementNotFound,
  NotABrowser,
} from "./lustre.mjs";
import {
  Attrs,
  Batch,
  Debug,
  Dispatch,
  Emit,
  Event,
  ForceModel,
  Model,
  Shutdown,
  Subscribe,
  Unsubscribe,
  View,
} from "./lustre/internals/runtime.mjs";
import { morph } from "./vdom.ffi.mjs";
import {
  Emit as EmitPatch,
  Diff,
  Init,
  is_empty_element_diff,
} from "./lustre/internals/patch.mjs";
import { handlers } from "./lustre/internals/vdom.mjs";
import { Ok, Error, isEqual } from "./gleam.mjs";
import { Some } from "../gleam_stdlib/gleam/option.mjs";
import { keys } from "../gleam_stdlib/gleam/dict.mjs";

// CLIENT RUNTIME --------------------------------------------------------------

/**
 * @template Msg, Model
 */
export class LustreClientApplication {
  /**
   * @template Flags
   *
   * @param {object} app
   * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
   * @param {(msg: Msg, model: Model) => [Model, Lustre.Effect<Msg>]} app.update
   * @param {(model: Model) => Lustre.Element<Msg>} app.view
   * @param {string | HTMLElement} selector
   * @param {Flags} flags
   *
   * @returns {Gleam.Ok<(action: Lustre.Action<Lustre.Client, Msg>>) => void>}
   */
  static start({ init, update, view }, selector, flags) {
    if (!is_browser()) return new Error(new NotABrowser());
    const root =
      selector instanceof HTMLElement
        ? selector
        : document.querySelector(selector);
    if (!root) return new Error(new ElementNotFound(selector));
    const app = new LustreClientApplication(root, init(flags), update, view);

    return new Ok((action) => app.send(action));
  }

  /**
   * @param {Element} root
   * @param {[Model, Lustre.Effect<Msg>]} init
   * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} update
   * @param {(model: Model) => Lustre.Element<Msg>} view
   *
   * @returns {LustreClientApplication}
   */
  constructor(root, [init, effects], update, view) {
    this.root = root;

    this.#model = init;
    this.#update = update;
    this.#view = view;

    this.#tickScheduled = window.requestAnimationFrame(() =>
      this.#tick(effects.all.toArray(), true),
    );
  }

  /** @type {Element} */ root;

  /**
   * @param {Lustre.Action<Lustre.Client, Msg>} action
   *
   * @returns {void}
   */
  send(action) {
    // Debug actions allow external programs to interact with the runtime in ways
    // that shouldn't be necessary for normal use but may be helpful for debuggers,
    // tooling, and testing.
    //
    if (action instanceof Debug) {
      // The `ForceModel` debug action
      if (action[0] instanceof ForceModel) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);

        this.#queue = [];
        this.#model = action[0][0];

        const vdom = this.#view(this.#model);
        const dispatch =
          (handler, immediate = false) =>
          (event) => {
            const result = handler(event);

            if (result instanceof Ok) {
              this.send(new Dispatch(result[0], immediate));
            }
          };
        const prev =
          this.root.firstChild ??
          this.root.appendChild(document.createTextNode(""));

        morph(prev, vdom, dispatch);
      }
    }

    // The dispatch action is how the outside world can send messages to a user's
    // Lustre application. Normally, messages being dispatched are batched and
    // processed in the next animation frame to allow for multiple messages to
    // be sent at once or in rapid success (capturing mouse movement for example).
    //
    // Some events do require being processed synchronously, however, such as
    // those coming from input events. In those cases a second 'immediate' parameter
    // is used to to flush the message queue and re-render immediately.
    //
    else if (action instanceof Dispatch) {
      const msg = action[0];
      const immediate = action[1] ?? false;

      this.#queue.push(msg);

      if (immediate) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#tick();
      } else if (!this.#tickScheduled) {
        this.#tickScheduled = window.requestAnimationFrame(() => this.#tick());
      }
    }

    //
    else if (action instanceof Emit) {
      const event = action[0];
      const data = action[1];

      this.root.dispatchEvent(
        new CustomEvent(event, {
          detail: data,
          bubbles: true,
          composed: true,
        }),
      );
    }

    // The `Shutdown` action
    else if (action instanceof Shutdown) {
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);

      this.#model = null;
      this.#update = null;
      this.#view = null;

      this.#queue = null;

      while (this.root.firstChild) {
        this.root.firstChild.remove();
      }
    }
  }

  /** @type {Model} */ #model;
  /** @type {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} */ #update;
  /** @type {(model: Model) => Lustre.Element<Msg>} */ #view;

  /** @type {Array<Msg>} */ #queue = [];
  /** @type {number | undefined} */ #tickScheduled;

  /**
   * @param {Lustre.Effect<Msg>[]} effects
   * @param {boolean} isFirstRender
   */
  #tick(effects = [], isFirstRender = false) {
    this.#tickScheduled = undefined;

    if (!this.#flush(effects, isFirstRender)) return;

    const vdom = this.#view(this.#model);
    const dispatch =
      (handler, immediate = false) =>
      (event) => {
        const result = handler(event);

        if (result instanceof Ok) {
          this.send(new Dispatch(result[0], immediate));
        }
      };
    const prev =
      this.root.firstChild ??
      this.root.appendChild(document.createTextNode(""));

    morph(prev, vdom, dispatch);
  }

  #flush(effects = [], didUpdate = false) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);

      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());

      this.#model = next;
    }

    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit = (event, data) =>
        this.root.dispatchEvent(
          new CustomEvent(event, {
            detail: data,
            bubbles: true,
            composed: true,
          }),
        );
      const select = () => {};

      effect({ dispatch, emit, select });
    }

    // If any effects immediately dispatched a message we can process it
    // synchronously before the next render.
    if (this.#queue.length > 0) {
      return this.#flush(effects, didUpdate);
    } else {
      return didUpdate;
    }
  }
}

/**
 * @template Flags, Msg, Model
 *
 * @param {object} app
 * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
 * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} app.update
 * @param {(model: Model) => Lustre.Element<Msg>} app.view
 * @param {string} selector - the CSS selector for the element this app should
 *  mount onto.
 * @param {Flags} flags - any data to pass to the app's init function when it is
 *  first started.
 *
 * @returns {LustreClientApplication<Model, Msg>}
 */
export const start = LustreClientApplication.start;

// CLIENT COMPONENT ------------------------------------------------------------

/**
 * @template Msg, Model
 *
 * @param {object} app
 * @param {() => [Model, Lustre.Effect<Msg>]} app.init
 * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} app.update
 * @param {(model: Model) => Lustre.Element<Msg>} app.view
 * @param {Gleam.Dict<string, Gleam.Decoder<Msg>>} app.on_attribute_change
 * @param {string} name
 *
 * @returns {Lustre.Result<undefined, Lustre.Error>}
 */
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
    /**
     * @returns {string[]}
     */
    static get observedAttributes() {
      if (hasAttributes) {
        return on_attribute_change[0].entries().map(([name]) => name);
      } else {
        return [];
      }
    }

    /**
     * @returns {LustreClientComponent}
     */
    constructor() {
      super();
      this.attachShadow({ mode: "open" });

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
              if (decoded instanceof Error) return;
              this.#queue.push(decoded[0]);

              if (this.#connected && !this.#tickScheduled) {
                this.#tickScheduled = window.requestAnimationFrame(() =>
                  this.#tick(),
                );
              }
            },
          });
        });
      }
    }

    /**
     *
     */
    connectedCallback() {
      this.#adoptStyleSheets().finally(() => {
        this.#tick(effects.all.toArray(), true);
        this.#connected = true;
      });
    }

    /**
     * @param {string} key
     * @param {string} prev
     * @param {string} next
     */
    attributeChangedCallback(key, prev, next) {
      if (prev !== next) this[key] = next;
    }

    /**
     *
     */
    disconnectedCallback() {
      this.#model = null;
      this.#queue = [];
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#connected = false;
    }

    /**
     * @param {Lustre.Action<Msg, Lustre.ClientSpa>} action
     */
    send(action) {
      // Debug actions allow external programs to interact with the runtime in ways
      // that shouldn't be necessary for normal use but may be helpful for debuggers,
      // tooling, and testing.
      //
      if (action instanceof Debug) {
        // The `ForceModel` debug action
        if (action[0] instanceof ForceModel) {
          this.#tickScheduled = window.cancelAnimationFrame(
            this.#tickScheduled,
          );

          this.#queue = [];
          this.#model = action[0][0];

          const vdom = view(this.#model);
          const dispatch =
            (handler, immediate = false) =>
            (event) => {
              const result = handler(event);

              if (result instanceof Ok) {
                this.send(new Dispatch(result[0], immediate));
              }
            };
          const prev =
            this.shadowRoot.childNodes[this.#adoptedStyleElements.length] ??
            this.shadowRoot.appendChild(document.createTextNode(""));

          morph(prev, vdom, dispatch);
        }
      }

      // The dispatch action is how the outside world can send messages to a user's
      // Lustre application. Normally, messages being dispatched are batched and
      // processed in the next animation frame to allow for multiple messages to
      // be sent at once or in rapid success (capturing mouse movement for example).
      //
      // Some events do require being processed synchronously, however, such as
      // those coming from input events. In those cases a second 'immediate' parameter
      // is used to to flush the message queue and re-render immediately.
      //
      else if (action instanceof Dispatch) {
        const msg = action[0];
        const immediate = action[1] ?? false;

        this.#queue.push(msg);

        if (immediate) {
          this.#tickScheduled = window.cancelAnimationFrame(
            this.#tickScheduled,
          );
          this.#tick();
        } else if (!this.#tickScheduled) {
          this.#tickScheduled = window.requestAnimationFrame(() =>
            this.#tick(),
          );
        }
      }

      //
      else if (action instanceof Emit) {
        const event = action[0];
        const data = action[1];

        this.dispatchEvent(
          new CustomEvent(event, {
            detail: data,
            bubbles: true,
            composed: true,
          }),
        );
      }
    }

    /** @type {Element[]} */
    #adoptedStyleElements = [];
    /** @type {Model} */
    #model = model;
    /** @type {Array<Msg>} */
    #queue = [];
    /** @type {number | undefined} */
    #tickScheduled;
    /** @type {boolean} */
    #connected = true;

    #tick(effects = [], isFirstRender = false) {
      this.#tickScheduled = undefined;

      if (!this.#connected) return;
      if (!this.#flush(isFirstRender, effects)) return;

      const vdom = view(this.#model);
      const dispatch =
        (handler, immediate = false) =>
        (event) => {
          const result = handler(event);

          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
      const prev =
        this.shadowRoot.childNodes[this.#adoptedStyleElements.length] ??
        this.shadowRoot.appendChild(document.createTextNode(""));

      morph(prev, vdom, dispatch);
    }

    #flush(didUpdate = false, effects = []) {
      while (this.#queue.length > 0) {
        const msg = this.#queue.shift();
        const [next, effect] = update(this.#model, msg);

        didUpdate ||= this.#model !== next;
        effects = effects.concat(effect.all.toArray());

        this.#model = next;
      }

      while (effects.length > 0) {
        const effect = effects.shift();
        const dispatch = (msg) => this.send(new Dispatch(msg));
        const emit = (event, data) =>
          this.dispatchEvent(
            new CustomEvent(event, {
              detail: data,
              bubbles: true,
              composed: true,
            }),
          );
        const select = () => {};

        effect({ dispatch, emit, select });
      }

      // If any effects immediately dispatched a message we can process it
      // synchronously before the next render.
      if (this.#queue.length > 0) {
        return this.#flush(didUpdate, effects);
      } else {
        return didUpdate;
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

// SERVER RUNTIME --------------------------------------------------------------

export class LustreServerApplication {
  static start({ init, update, view, on_attribute_change }, flags) {
    const app = new LustreServerApplication(
      init(flags),
      update,
      view,
      on_attribute_change,
    );

    return new Ok((action) => app.send(action));
  }

  constructor([model, effects], update, view, on_attribute_change) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#html = view(model);
    this.#onAttributeChange = on_attribute_change;

    this.#renderers = new Map();
    this.#handlers = handlers(this.#html);

    this.#tick(effects.all.toArray());
  }

  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder = this.#onAttributeChange.get(attr[0]);
        if (!decoder) continue;
        const msg = decoder(attr[1]);
        if (msg instanceof Error) continue;

        this.#queue.push(msg);
      }

      this.#tick();
    } else if (action instanceof Batch) {
      this.#queue = this.#queue.concat(action[0].toArray());
      this.#tick(action[1].all.toArray());
    } else if (action instanceof Debug) {
    } else if (action instanceof Dispatch) {
      this.#queue.push(action[0]);
      this.#tick();
    } else if (action instanceof Emit) {
      const event = new EmitPatch(action[0], action[1]);

      for (const [_, renderer] of this.#renderers) {
        renderer(event);
      }
    } else if (action instanceof Event) {
      const handler = this.#handlers.get(action[0]);
      if (!handler) return;
      const msg = handler(action[1]);
      if (msg instanceof Error) return;

      this.#queue.push(msg[0]);
      this.#tick();
    } else if (action instanceof Subscribe) {
      const attrs = keys(this.#onAttributeChange);
      const patch = new Init(attrs, this.#html);

      this.#renderers = this.#renderers.set(action[0], action[1]);
      action[1](patch);
    } else if (action instanceof Unsubscribe) {
      this.#renderers = this.#renderers.delete(action[0]);
    }
  }

  #model;
  #update;
  #queue;
  #view;
  #html;

  #renderers;
  #handlers;

  #onAttributeChange;

  #tick(effects = []) {
    if (!this.#flush(false, effects)) return;

    const vdom = this.#view(this.#model);
    const diff = elements(this.#html, vdom);

    if (!is_empty_element_diff(diff)) {
      const patch = new Diff(diff);
      for (const [_, renderer] of this.#renderers) {
        renderer(patch);
      }
    }

    this.#html = vdom;
    this.#handlers = diff.handlers;
  }

  #flush(didUpdate = false, effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);

      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());

      this.#model = next;
    }

    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit = (event, data) =>
        this.root.dispatchEvent(
          new CustomEvent(event, {
            detail: data,
            bubbles: true,
            composed: true,
          }),
        );
      const select = () => {};

      effect({ dispatch, emit, select });
    }

    // If any effects immediately dispatched a message we can process it
    // synchronously before the next render.
    if (this.#queue.length > 0) {
      return this.#flush(didUpdate, effects);
    } else {
      return didUpdate;
    }
  }
}

export const start_server_application = LustreServerApplication.start;

// SHARED METHODS --------------------------------------------------------------

// UTILS -----------------------------------------------------------------------

/**
 * @returns {boolean}
 */
export const is_browser = () => globalThis.window && window.document;

/**
 * @param {string} name
 *
 * @returns {boolean}
 */
export const is_registered = (name) =>
  is_browser() && !!window.customElements.get(name);

/**
 * @param {Event} event

 * @returns {void}
 */
export const prevent_default = (event) => event.preventDefault();

/**
 * @param {Event} event

 * @returns {void}
 */
export const stop_propagation = (event) => event.stopPropagation();
