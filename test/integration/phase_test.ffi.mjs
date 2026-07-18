import { register, unregister } from "./happy-dom.ffi.mjs";
import { virtualise as do_virtualise } from "../agnostic/vdom/virtualise.ffi.mjs";
import { Runtime } from "../agnostic/runtime/platform/base.ffi.mjs";
import { new$ as newPlatform, Phase } from "../agnostic/platform.mjs";
import { toList } from "../agnostic/internals/list.ffi.mjs";
import {
  mount_strict,
  create_element,
  create_text_node,
  create_fragment,
  create_comment,
  insert_before,
  move_before,
  remove_child,
  next_sibling,
  get_attribute,
  set_attribute,
  remove_attribute,
  set_property,
  set_text,
  set_raw_content,
  create_raw_node,
  add_event_listener,
  remove_event_listener,
  schedule_render,
  after_render,
} from "../agnostic/platform/dom.ffi.mjs";

// LOG -------------------------------------------------------------------------

// A module-level log shared between the Gleam tests and the fake phase
// schedulers. `take_log` drains it so each test starts from what it observed.
let log = [];

export const log_push = (entry) => {
  log.push(entry);
};

export const take_log = () => {
  const entries = log;
  log = [];
  return toList(entries);
};

export const body_text = () => document.body.textContent ?? "";

export const is_body = (node) => node === document.body;

// CUSTOM PLATFORM -------------------------------------------------------------

// A fake phase scheduler: records that the runtime invoked it, then defers via
// a microtask — satisfying the scheduler-must-defer contract.
const makeScheduler = (name) => (callback) => {
  log.push("schedule:" + name);
  queueMicrotask(callback);
};

// A DOM-backed platform whose deferred phases are the given names, declared in
// the given order, each with a recording scheduler.
const customPlatform = (phaseNames) =>
  newPlatform(
    document.body,
    mount_strict,
    create_element,
    create_text_node,
    create_fragment,
    create_comment,
    insert_before,
    move_before,
    remove_child,
    next_sibling,
    get_attribute,
    set_attribute,
    remove_attribute,
    set_property,
    set_text,
    set_raw_content,
    create_raw_node,
    add_event_listener,
    remove_event_listener,
    schedule_render,
    after_render,
    toList([...phaseNames].map((name) => new Phase(name, makeScheduler(name)))),
  );

// HARNESS ---------------------------------------------------------------------

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

export function with_custom_phase_runtime(
  initial_html,
  make_app,
  phase_names,
  callback,
) {
  return runInBrowserContext(async () => {
    log = [];
    document.body.innerHTML = initial_html;

    const app = make_app();

    const [init_model, init_effects] = app.init();
    const runtime = {
      model: init_model,
      renders: 0,
      lustre: null,
    };

    const wrappedUpdate = (model, message) => {
      const [newModel, effects] = app.update(model, message);
      runtime.model = newModel;
      return [newModel, effects];
    };

    const wrappedView = (model) => {
      runtime.renders += 1;
      return app.view(model);
    };

    const initialVdom = do_virtualise(document.body);
    runtime.lustre = new Runtime(
      document.body,
      initialVdom,
      [init_model, init_effects],
      wrappedView,
      wrappedUpdate,
      customPlatform(phase_names),
      { debug: true },
    );

    await waitForNextFrame();
    await callback(runtime);
  });
}

export function model(runtime) {
  return runtime.model;
}

export function render_count(runtime) {
  return runtime.renders;
}

// Waits for a macrotask, letting any pending microtask-scheduled work (such as
// the headless runtime's effect processing) finish first.
export async function flush_microtasks(callback) {
  await new Promise((resolve) => setTimeout(resolve, 0));
  await callback();
}

function waitForNextFrame() {
  return new Promise((resolve) => {
    window.requestAnimationFrame(() =>
      window.requestAnimationFrame(() => resolve()),
    );
  });
}
