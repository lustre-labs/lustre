import { register, unregister } from "./happy-dom.ffi.mjs";
import { Reconciler } from "../lustre/vdom/reconciler.ffi.mjs";
import { virtualise } from "../lustre/vdom/virtualise.ffi.mjs";

export function use(callback) {
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

export function get_html() {
  synchronise_value_attributes();
  return document.body.innerHTML;
}

export function get_vdom() {
  synchronise_value_attributes();
  return virtualise(document.body);
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
