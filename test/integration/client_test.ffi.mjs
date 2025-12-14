import { Result$isOk, Result$Ok$0 } from "../gleam.mjs";
import { register, unregister } from "./happy-dom.ffi.mjs";
import { virtualise as do_virtualise } from "../lustre/vdom/virtualise_cleaned.ffi.mjs";
import { Reconciler } from "../lustre/vdom/reconciler.ffi.mjs";
import { Runtime } from "../lustre/runtime/client/runtime.ffi.mjs";
import * as effect from "../lustre/effect.mjs";

async function runInBrowserContext(callback) {
  register({
    width: 1920,
    height: 1080,
    url: "https://localhost:1234",
  });

  try {
    return await callback();
  } finally {
    await unregister();
  }
}

export function virtualise(html, callback) {
  return runInBrowserContext(() => {
    document.body.innerHTML = html;
    return callback(do_virtualise(document.body));
  });
}

export function get_html() {
  synchronise_value_attributes();
  return document.body.innerHTML;
}

export function get_vdom() {
  synchronise_value_attributes();
  return do_virtualise(document.body);
}

function synchronise_value_attributes() {
  // make sure the attributes reflect the current state in the DOM, for example
  // if we only updated the property values.
  document.querySelectorAll("input, select, option").forEach((el) => {
    el.setAttribute("value", el.value);
    if (el.checked) {
      el.setAttribute("checked", "");
    } else {
      el.removeAttribute("checked");
    }
    if (el.selected) {
      el.setAttribute("selected", "");
    } else {
      el.removeAttribute("selected");
    }
  });
}

export function with_reconciler(callback) {
  return runInBrowserContext(() => {
    const noop = () => {};
    const reconciler = new Reconciler(document.body, noop, noop, {
      debug: true,
    });

    callback(reconciler);
  });
}

export function mount(reconciler, vdom) {
  // reset the body to allow multiple `mount` tests using a single browser context.
  document.body.innerHTML = "";
  reconciler.mount(vdom);
}

export function push(reconciler, patch) {
  reconciler.push(patch);
}

export function with_client_runtime(initial_html, app, callback) {
  return runInBrowserContext(async () => {
    // Set the pre-rendered HTML
    document.body.innerHTML = initial_html;

    const [init_model, init_effects] = app.init();
    const runtime = {
      model: init_model,
      vdom: null,
      lustre: null,
    };

    const wrappedUpdate = (model, msg) => {
      const [newModel, effects] = app.update(model, msg);
      runtime.model = newModel;
      return [newModel, effects];
    };

    const wrappedView = (model) => {
      runtime.vdom = app.view(model);
      return runtime.vdom;
    };

    runtime.lustre = new Runtime(
      document.body,
      [init_model, init_effects],
      wrappedView,
      wrappedUpdate,
      { debug: true },
    );

    await waitForNextFrame();
    await callback(runtime);
  });
}

export async function send(runtime, msg, callback) {
  runtime.lustre.dispatch(msg, true);
  await waitForNextFrame();
  await callback();
}

export async function emit(selector, event_name, callback) {
  const element = document.querySelector(selector);
  if (!element) {
    throw new Error(`Element not found: ${selector}`);
  }

  element.dispatchEvent(new Event(event_name, { bubbles: true, composed: true }));

  await waitForNextFrame();
  await callback();
}

export async function emit_with_value(selector, event_name, value, callback) {
  const element = document.querySelector(selector);
  if (!element) {
    throw new Error(`Element not found: ${selector}`);
  }

  // Set the value on the element
  if (element.type === "checkbox" || element.type === "radio") {
    element.checked = value === "true" || value === true;
  } else {
    element.value = value;
  }

  // Dispatch the event
  element.dispatchEvent(new Event(event_name, { bubbles: true, composed: true }));

  await waitForNextFrame();
  await callback();
}

export function model(runtime) {
  return runtime.model;
}

function waitForNextFrame() {
  return new Promise((resolve) => {
    window.requestAnimationFrame(() =>
      window.requestAnimationFrame(() => resolve()),
    );
  });
}
