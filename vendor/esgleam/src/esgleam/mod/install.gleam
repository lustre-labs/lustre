import esgleam/mod/platform
import gleam/io
import gleam/string

/// Should be updated for every esgleam release
const tarball_url = "https://registry.npmjs.org/@esbuild/{#platform}/-/{#platform}-0.27.0.tgz"

@target(erlang)
@external(erlang, "ffi_esgleam", "do_fetch")
fn do_fetch(url: String) -> Nil

@target(javascript)
@external(javascript, "../../ffi_esgleam.mjs", "do_fetch")
fn do_fetch(url: String, then: fn() -> Nil) -> Nil

@target(erlang)
/// installs esbuild and take in a callback to run post install
fn fetch_from_version(platform: String, then callback: fn() -> Nil) {
  let url = string.replace(tarball_url, "{#platform}", platform)
  io.println("Fetching esbuild from: " <> url)
  do_fetch(url)
  callback()
}

@target(javascript)
/// installs esbuild and take in a callback to run post install
fn fetch_from_version(platform: String, then callback: fn() -> Nil) {
  let url = string.replace(tarball_url, "{#platform}", platform)
  io.println("Fetching esbuild from: " <> url)
  do_fetch(url, callback)
}

fn fetch_latest(then callback: fn() -> Nil) {
  fetch_from_version(platform.get_package_name(), callback)
}

@target(erlang)
/// Synchronously install `esbuild` under both targets
pub fn fetch() {
  fetch_latest(then: fn() { Nil })
}

@target(javascript)
/// Syncronously install `esbuild` under both targets
@external(javascript, "../../ffi_esgleam.mjs", "install")
pub fn fetch() -> Nil

/// WARNING: on JavaScript, this is promise based
/// it is perferable to use `install.fetch` or `gleam run -m esgleam/install`
pub fn internal_fetch() {
  fetch_latest(fn() { io.println("Installed esbuild") })
  Nil
}
