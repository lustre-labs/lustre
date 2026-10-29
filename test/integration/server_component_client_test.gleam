@target(javascript)
import lustre_test

// TYPES -----------------------------------------------------------------------

@target(javascript)
pub type ServerComponents

// TESTS -----------------------------------------------------------------------

@target(javascript)
pub fn server_component_context_subscription_test() {
  use <- lustre_test.test_filter("server_component_context_subscription_test")
  use components <- with_server_components(provides: "theme")

  provide(components, "theme", "dark")
  subscribe(components, "theme")
  provide(components, "theme", "light")

  // A subscribed server component should forward every value to the server,
  // not just the first one.
  assert sent_context_values(components, "theme") == ["dark", "light"]

  unsubscribe(components, "theme")
  provide(components, "theme", "blue")
  assert sent_context_values(components, "theme") == ["dark", "light"]

  // Disconnecting should unsubscribe from every context too.
  subscribe(components, "theme")
  assert sent_context_values(components, "theme") == ["dark", "light", "blue"]

  let callbacks = provider_callbacks(components)
  disconnect(components)
  provide(components, "theme", "green")
  assert provider_callbacks(components) == callbacks
}

// FFI ------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "with_server_components")
fn with_server_components(
  provides provides: String,
  run callback: fn(ServerComponents) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "provide")
fn provide(components: ServerComponents, key: String, value: String) -> Nil

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "subscribe")
fn subscribe(components: ServerComponents, key: String) -> Nil

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "unsubscribe")
fn unsubscribe(components: ServerComponents, key: String) -> Nil

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "disconnect")
fn disconnect(components: ServerComponents) -> Nil

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "sent_context_values")
fn sent_context_values(
  components: ServerComponents,
  key: String,
) -> List(String)

@target(javascript)
@external(javascript, "./server_component_client_test.ffi.mjs", "provider_callbacks")
fn provider_callbacks(components: ServerComponents) -> Int
