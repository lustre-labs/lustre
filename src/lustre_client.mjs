// This file only exists because we want a nice tree-shaken bundle to ship for
// the server components. I'm using esbuild and this file is the entry.
export { ServerComponent } from "../build/dev/javascript/lustre/lustre.ffi.mjs";
