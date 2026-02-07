//// In other frameworks it's common for components to perform side effects
//// whenever the need them. An event handler might make an HTTP request, or a
//// component might reach into the DOM to focus an input.
////
//// In Lustre we try to keep side effects separate from our main program loop.
//// This comes with a whole bunch of benefits like making it easier to test and
//// reason about our code, making it possible to implement time-travel debugging,
//// or even to run our app on the server using Lustre's server components. This
//// is great but we still need to perform side effects at some point, so how do
//// we do that?
////
//// The answer is through the `Effect` type that treats side effects as *data*.
//// This approach is known as having **managed effects**: you pass data that
//// describes a side effect to Lustre's runtime and it takes care of performing
//// that effect and potentially sending messages back to your program for you.
//// By going through this abstraction we discourage side effects from being
//// performed in the middle of our program.
////
//// ## Related packages
////
//// While Lustre doesn't include many built-in effects, there are a number of
//// community packages define useful common effects for your applications.
////
//// - [`rsvp`](https://hexdocs.pm/rsvp) – Send HTTP requests from Lustre
////   applications and server components.
////
//// - [`modem`](https://hexdocs.pm/modem) – A friendly Lustre package to help
////   you build a router, handle links, and manage URLs.
////
////  - [`plinth`](https://hexdocs.pm/plinth) – Bindings to Node.js and browser
////    platform APIs. (This package does not include any effects directly, but
////    it does provide bindings to many APIs that you can use to create your
////    own.)
////
//// ## Examples
////
//// For folks coming from other languages (or other Gleam code!) where side
//// effects are often performed in-place, this can feel a bit strange. We have
//// a category of example apps dedicated to showing various effects in action:
////
//// - [HTTP requests](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/01-http-requests)
////
//// - [Generating random values](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/02-random)
////
//// - [Setting up timers](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/03-timers)
////
//// - [Working with LocalStorage](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/04-local-storage)
////
//// - [Reading from the DOM](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/05-dom-effects)
////
//// - [Optimistic state updates](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/06-optimistic-requests)
////
//// This list of examples is likely to grow over time, so be sure to check back
//// every now and then to see what's new!
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/json.{type Json}
import lustre/runtime/effect

// TYPES -----------------------------------------------------------------------

/// The `Effect` type treats side effects as data and is a way of saying "Hey
/// Lustre, do this thing for me." Each effect specifies two things:
///
/// 1. The side effects for the runtime to perform.
///
/// 2. The type of messages that (might) be sent back to the program in response.
///
///
///
pub type Effect(message) =
  effect.Effect(message)

///
///
pub type Key =
  effect.Key

// CONSTRUCTORS ----------------------------------------------------------------

/// Most Lustre applications need to return a tuple of `#(model, Effect(msg))`
/// from their `init` and `update` functions. If you don't want to perform any
/// side effects, you can use `none` to tell the runtime there's no work to do.
///
pub fn none() -> Effect(msg) {
  effect.none
}

/// Construct your own reusable effect from a custom callback. This callback is
/// called with a `dispatch` function you can use to send messages back to your
/// application's `update` function.
///
/// Example using the `window` module from the `plinth` library to dispatch a
/// message on the browser window object's `"visibilitychange"` event.
///
/// ```gleam
/// import lustre/effect.{type Effect}
/// import plinth/browser/window
///
/// type Model {
///   Model(Int)
/// }
///
/// type Msg {
///   FetchState
/// }
///
/// fn init(_flags) -> #(Model, Effect(Msg)) {
///   #(
///     Model(0),
///     effect.from(fn(dispatch) {
///       window.add_event_listener("visibilitychange", fn(_event) {
///         dispatch(FetchState)
///       })
///     }),
///   )
/// }
/// ```
///
pub fn from(effect: fn(fn(message) -> Nil) -> Nil) -> Effect(message) {
  use actions <- effect.synchronous

  effect(actions.dispatch)
}

/// Schedule a side effect that is guaranteed to run after your `view` function
/// is called and the DOM has been updated, but **before** the browser has
/// painted the screen. This effect is useful when you need to read from the DOM
/// or perform other operations that might affect the layout of your application.
///
/// In addition to the `dispatch` function, your callback will also be provided
/// with root element of your app or component. This is especially useful inside
/// of components, giving you a reference to the [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot).
///
/// Messages dispatched immediately in this effect will trigger a second re-render
/// of your application before the browser paints the screen. This let's you read
/// the state of the DOM, update your model, and then render a second time with
/// the additional information.
///
/// > **Note**: dispatching messages synchronously in this effect can lead to
/// > degraded performance if not used correctly. In the worst case you can lock
/// > up the browser and prevent it from painting the screen _at all_.
///
/// > **Note**: There is no concept of a "paint" for server components. These
/// > effects will be ignored in those contexts and never run.
///
pub fn before_paint(
  effect: fn(fn(message) -> Nil, Dynamic) -> Nil,
) -> Effect(message) {
  use actions <- effect.before_paint
  let root = actions.root()
  let dispatch = actions.dispatch

  effect(dispatch, root)
}

/// Schedule a side effect that is guaranteed to run after the browser has painted
/// the screen.
///
/// In addition to the `dispatch` function, your callback will also be provided
/// with root element of your app or component. This is especially useful inside
/// of components, giving you a reference to the [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot).
///
/// > **Note**: There is no concept of a "paint" for server components. These
/// > effects will be ignored in those contexts and never run.
///
pub fn after_paint(
  effect: fn(fn(message) -> Nil, Dynamic) -> Nil,
) -> Effect(message) {
  use actions <- effect.after_paint
  let root = actions.root()
  let dispatch = actions.dispatch

  effect(dispatch, root)
}

/// Provide a context value to children. After this effect has been called, any
/// child component that requests a context with a matching key will be given the
/// `value` passed here.  Subsequent calls to `provide` will update children with
/// the new value automatically.
///
/// > **Note**: timing is significant for context providers! A context value must
/// > be provided _before_ a child component requests it. It is common for apps
/// > and components to provide all context values at least once in their `init`
/// > function.
///
pub fn provide(key: String, value: Json) -> Effect(msg) {
  use actions <- effect.synchronous

  actions.provide(key, value)
}

/// Construct an effect that has the ability to be cleaned up in the future. The
/// callback you provide is called with two arguments: a unique [`Key`](#Key) that
/// identifies the effect, and a function to call with a callback to run when the
/// effect's cleanup is invoked.
///
/// For example, consider an effect that wraps JavaScript's `window.setInterval`
/// and `window.clearInterval` functions to start a timer that can be stopped in
/// the future:
///
/// ```gleam
/// fn start_clock() {
///   use key, cleanup <- effect.with_cleanup
///   use dispatch <- effect.from
///   let timer_id = set_interval(1000, fn() { dispatch(ClockTicked) })
///
///   cleanup(fn() { clear_interval(timer_id) })
///   dispatch(TimerStarted(key))
/// }
///
/// fn stop_clock(key) {
///   effect.cleanup(key)
/// }
/// ```
///
/// If the `cleanup` callback is called multiple times, every function passed in
/// will be called when the effect's cleanup is invoked.
///
/// > **Note**: there is _no guarantee_ that an effect's cleanup callback(s) will
/// > ever be invoked.
///
pub fn with_cleanup(
  effect: fn(Key, fn(fn() -> Nil) -> Nil) -> Effect(message),
) -> Effect(message) {
  use actions <- effect.deferred
  let key = effect.key()

  effect(key, actions.register(key, _))
}

/// Invoke any cleanup functions associated with the effect by the given [`Key`](#Key).
/// A key can only be acquired by calling [`with_cleanup`](#with_cleanup).
///
pub fn cleanup(key: Key) -> Effect(message) {
  use actions <- effect.synchronous

  actions.cleanup(key)
}

// MANIPULATIONS ---------------------------------------------------------------

/// Batch multiple effects to be performed at the same time.
///
/// > **Note**: The runtime makes no guarantees about the order on which effects
/// > are performed! If you need to chain or sequence effects together, you have
/// > two broad options:
/// >
/// > 1. Create variants of your `msg` type to represent each step in the sequence
/// >    and fire off the next effect in response to the previous one.
/// >
/// > 2. If you're defining effects yourself, consider whether or not you can handle
/// >    the sequencing inside the effect itself.
///
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  effect.batch(effects)
}

/// Transform the result of an effect. This is useful for mapping over effects
/// produced by other libraries or modules.
///
/// > **Note**: Remember that effects are not _required_ to dispatch any messages.
/// > Your mapping function may never be called!
///
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  effect.map(effect, f)
}
