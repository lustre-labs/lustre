import { register, unregister } from '../happy-dom.ffi.mjs'
import { initialiseMetadata, Reconciler } from '../lustre/vdom/reconciler.ffi.mjs';
import { virtualise } from '../lustre/vdom/virtualise.ffi.mjs';

export function use(callback) {
  return runInBrowserContext(() => {
    const reconciler = new Reconciler(document.body, () => {}, {
      exposeKeys: true
    })

    initialiseMetadata(null, document.body);

    callback(reconciler);
  })
}

export function mount(reconciler, vdom) {
  reconciler.mount(vdom);
}

export function push(reconciler, patch) {
  reconciler.push(patch);
}

export function get_html() {
  return document.body.innerHTML;
}

export function get_vdom() {
  return virtualise(document.body);
}

async function runInBrowserContext(callback) {
  register({
    width: 1920,
    height: 1080,
    url: "https://localhost:1234"
  })

  try {
    return await callback();
  } finally {
    await unregister();
  }
}
