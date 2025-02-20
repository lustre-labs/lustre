import Dict from "../gleam_stdlib/dict.mjs";
import { Dispatch } from "./lustre/internals/runtime.mjs";
import { ElementNotFound, NotABrowser } from "./lustre.mjs";
import { LustreReconciler } from "./reconciler.ffi.mjs";
import { Ok, Error, NonEmpty, isEqual, List, Empty } from "./gleam.mjs";
import { diff } from "./lustre/runtime/vdom.mjs";
import { Some } from "../gleam_stdlib/gleam/option.mjs";



// dumb hack to speed up lists to see more stuff.
List.prototype.hasLength = function(n) {
  let ptr = this;
  while (n-- > 0 && ptr) ptr = ptr.tail;
  return ptr instanceof Empty;
}
List.prototype.atLeastLength = function(n) {
  let ptr = this;
  while (n-- > 0 && ptr) ptr = ptr.tail;
  return !!ptr;
}

export const compare_attributes = (a, b) => {
  if (a.name < b.name) {
    return -1;
  } else if (a.name > b.name) {
    return 1;
  } else {
    return 0;
  }
}

export const sort_attributes = (attributes) =>  {
  let attrs_array = [];
  for (let attr = attributes; attr.tail; attr = attr.tail) {
    attrs_array.push(attr.head)
  }
  attrs_array.sort(compare_attributes);
  return List.fromArray(attrs_array);
}


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
      this.#tick(effects.all);
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
    console.time("tick")
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

    console.time("render")
    console.time("view")
    const next = this.#view(this.#model);
    console.timeEnd("view")
    console.time("diff")
    const { patch, handlers } = diff(
      this.#prev,
      next,
      this.#reconciler_handlers,
    );
    this.#reconciler_handlers = handlers;
    console.timeEnd("diff")
    console.time("reconcile")
    this.#reconciler.push(patch);
    this.#prev = next;
    console.timeEnd("reconcile")
    console.timeEnd("render")
    console.timeEnd("tick")
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
  const initialView = view(model);
  const hasAttributes = on_attribute_change instanceof Some;
  const observedAttributes = hasAttributes
    ? on_attribute_change[0].entries().map(([name]) => name)
    : [];

  const component = class LustreClientComponent extends HTMLElement {
    static get observedAttributes() {
      return observedAttributes;
    }

    #model = model;
    #update = update;
    #view = view;

    #prev = initialView;
    #reconciler;
    #reconciler_handlers = new Dict();

    constructor() {
      super();
      this.attachShadow({ mode: "open" });
      this.internals = this.attachInternals();
      this.root = this.shadowRoot;

      this.#reconciler = new LustreReconciler(
        this.shadowRoot,
        (msg, immediate) => {
          if (msg.constructor === Ok) {
            this.#dispatch(msg[0], immediate);
          }
        },
      );

      this.#reconciler.mount(this.#prev);

      if (effects.all instanceof NonEmpty) {
        this.#tick(effects.all);
      }

      if (hasAttributes) {
        on_attribute_change[0].forEach((decoder, name) => {
          Object.defineProperty(this, name, {
            get() {
              return this[`_${name}`];
            },

            set(value) {
              const prev = this[`_${name}`];
              if (isEqual(prev, value)) return;
              this[`_${name}`] = value;
              const decoded = decoder(value);

              if (decoded.constructor === Ok) {
                this.#dispatch(decoded[0]);
              }
            },
          });
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

      const next = this.#view(this.#model);
      const { patch, handlers } = diff(
        this.#prev,
        next,
        this.#reconciler_handlers,
      );

      this.#reconciler_handlers = handlers;

      this.#reconciler.push(patch);
      this.#prev = next;
    }
  };

  window.customElements.define(name, component);

  return new Ok(undefined);
};
