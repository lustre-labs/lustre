// IMPORTS ---------------------------------------------------------------------
//
// This test file serves as a smoke test for the JavaScript FFI modules used by
// the Lustre runtime. By calling runtime functions like `lustre.is_browser()`,
// we force Node.js to load the runtime FFI files, which in turn loads all their
// imports (including cache.mjs). This catches FFI-related errors such as:
//
// - Incorrect import paths or missing FFI files
// - Syntax errors in FFI files

@target(javascript)
import lustre
@target(javascript)
import lustre/platform
@target(javascript)
import lustre_test

// RUNTIME FFI SMOKE TESTS -----------------------------------------------------

@target(javascript)
pub fn runtime_is_browser_test() {
  use <- lustre_test.test_filter("runtime_is_browser_test")

  let _ = platform.is_browser()

  Nil
}

@target(javascript)
pub fn runtime_is_registered_test() {
  use <- lustre_test.test_filter("runtime_is_registered_test")

  let _ = lustre.is_registered("test-component")

  Nil
}
