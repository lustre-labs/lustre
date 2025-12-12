// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/result
import lustre.{type App, type Runtime}

// EXTERNAL --------------------------------------------------------------------

/// Convert any value to Dynamic (type erasure)
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn unsafe_coerce(a: anything) -> Dynamic

// TYPES -----------------------------------------------------------------------

/// A contract that defines everything needed to run a component over WebSocket.
/// This abstracts over the component's model, msg, and initialization args.
pub type ComponentContract(init_args, model, msg) {
  ComponentContract(
    /// The Lustre App definition
    app: App(init_args, model, msg),
    /// Function to create the init args from connection context
    /// This allows components to receive context (like pub/sub registries)
    make_init_args: fn(ConnectionContext) -> init_args,
    /// Called after the component runtime starts
    on_connect: fn(Runtime(msg)) -> Nil,
    /// Called before the component runtime shuts down
    on_disconnect: fn(Runtime(msg)) -> Nil,
  )
}

/// Context passed to components during WebSocket initialization.
pub type ConnectionContext {
  ConnectionContext(
    /// Unique connection ID for this WebSocket session
    connection_id: String,
    /// Type-erased resources container for dependency injection
    resources: Resources,
  )
}

/// Type-erased container for injecting dependencies into components.
/// Components can extract typed values using `get_resources` with a decoder.
pub opaque type Resources {
  Resources(inner: Dynamic)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create a simple contract for components that don't need special init args.
pub fn simple_contract(
  app: App(Nil, model, msg),
) -> ComponentContract(Nil, model, msg) {
  ComponentContract(
    app: app,
    make_init_args: fn(_ctx) { Nil },
    on_connect: fn(_runtime) { Nil },
    on_disconnect: fn(_runtime) { Nil },
  )
}

/// Create a contract with custom init args derived from context.
pub fn contract(
  app: App(init_args, model, msg),
  make_init_args: fn(ConnectionContext) -> init_args,
) -> ComponentContract(init_args, model, msg) {
  ComponentContract(
    app: app,
    make_init_args: make_init_args,
    on_connect: fn(_runtime) { Nil },
    on_disconnect: fn(_runtime) { Nil },
  )
}

/// Wrap any value as Resources for dependency injection.
pub fn resources(value: a) -> Resources {
  Resources(unsafe_coerce(value))
}

/// Create empty resources when no dependencies are needed.
pub fn empty_resources() -> Resources {
  Resources(unsafe_coerce(Nil))
}

// ACCESSORS -------------------------------------------------------------------

/// Extract a typed value from Resources using a decoder.
/// Returns Error(Nil) if the decoder fails.
pub fn get_resources(
  resources: Resources,
  decoder: Decoder(a),
) -> Result(a, Nil) {
  decode.run(resources.inner, decoder)
  |> result.replace_error(Nil)
}

/// Unsafely extract a value from Resources without type checking.
/// Use this only when you're certain about the type stored in Resources.
pub fn get_resources_unsafe(resources: Resources) -> a {
  unsafe_coerce_to(resources.inner)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn unsafe_coerce_to(a: Dynamic) -> b

// MODIFIERS -------------------------------------------------------------------

/// Add an on_connect hook to a contract.
pub fn with_on_connect(
  contract: ComponentContract(init_args, model, msg),
  hook: fn(Runtime(msg)) -> Nil,
) -> ComponentContract(init_args, model, msg) {
  ComponentContract(..contract, on_connect: hook)
}

/// Add an on_disconnect hook to a contract.
pub fn with_on_disconnect(
  contract: ComponentContract(init_args, model, msg),
  hook: fn(Runtime(msg)) -> Nil,
) -> ComponentContract(init_args, model, msg) {
  ComponentContract(..contract, on_disconnect: hook)
}
