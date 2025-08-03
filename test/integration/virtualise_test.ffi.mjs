import { register, unregister } from './happy-dom.ffi.mjs'
import { virtualise as do_virtualise } from '../lustre/vdom/virtualise.ffi.mjs';

export function virtualise(html, callback) {
  return runInBrowserContext(() => {
    document.body.innerHTML = html;
    return callback(do_virtualise(document.body));
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
