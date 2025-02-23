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
//// ## Examples
////
//// For folks coming from other languages (or other Gleam code!) where side
//// effects are often performed in-place, this can feel a bit strange. A couple
//// of the examples in the repo tackle effects:
////
//// - [`05-http-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests)
//// - [`06-custom-effects`](https://github.com/lustre-labs/lustre/tree/main/examples/06-custom-effects)
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
//// While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
//// is also a great resource for learning about the Model-View-Update architecture
//// and the kinds of patterns that Lustre is built around.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Selector}
import gleam/json.{type Json}
import gleam/list
import lustre/internals/constants

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
pub opaque type Effect(msg) {
  Effect(all: List(fn(Actions(msg)) -> Nil))
}

type Actions(msg) {
  Actions(
    dispatch: fn(msg) -> Nil,
    emit: fn(String, Json) -> Nil,
    select: fn(Selector(msg)) -> Nil,
    root: Dynamic,
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

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
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  use dispatch, _, _, _ <- custom

  effect(dispatch)
}

/// Emit a custom event from a component as an effect. Parents can listen to these
/// events in their `view` function like any other HTML event. Any data you pass
/// to `effect.emit` can be accessed by event listeners through the `detail` property
/// of the event object.
///
@internal
pub fn event(name: String, data: Json) -> Effect(msg) {
  use _, emit, _, _ <- custom

  emit(name, data)
}

///
///
@internal
pub fn custom(
  run: fn(
    fn(msg) -> Nil,
    fn(String, Json) -> Nil,
    fn(Selector(msg)) -> Nil,
    Dynamic,
  ) ->
    Nil,
) -> Effect(msg) {
  Effect([
    fn(actions: Actions(msg)) {
      run(actions.dispatch, actions.emit, actions.select, actions.root)
    },
  ])
}

/// Most Lustre applications need to return a tuple of `#(model, Effect(msg))`
/// from their `init` and `update` functions. If you don't want to perform any
/// side effects, you can use `none` to tell the runtime there's no work to do.
///
pub const none: Effect(msg) = Effect([])

// MANIPULATIONS ---------------------------------------------------------------

/// Batch multiple effects to be performed at the same time.
///
/// **Note**: The runtime makes no guarantees about the order on which effects
/// are performed! If you need to chain or sequence effects together, you have
/// two broad options:
///
/// 1. Create variants of your `msg` type to represent each step in the sequence
///    and fire off the next effect in response to the previous one.
///
/// 2. If you're defining effects yourself, consider whether or not you can handle
///    the sequencing inside the effect itself.
///
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect({
    use b, Effect(a) <- list.fold(effects, constants.empty_list)
    list.append(b, a)
  })
}

@target(erlang)
/// Transform the result of an effect. This is useful for mapping over effects
/// produced by other libraries or modules.
///
/// **Note**: Remember that effects are not _required_ to dispatch any messages.
/// Your mapping function may never be called!
///
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect({
    use eff <- list.map(effect.all)
    fn(actions: Actions(b)) {
      eff(Actions(
        dispatch: fn(msg) { actions.dispatch(f(msg)) },
        emit: actions.emit,
        select: fn(selector) {
          actions.select(process.map_selector(selector, f))
        },
        root: actions.root,
      ))
    }
  })
}

@target(javascript)
/// Transform the result of an effect. This is useful for mapping over effects
/// produced by other libraries or modules.
///
/// **Note**: Remember that effects are not _required_ to dispatch any messages.
/// Your mapping function may never be called!
///
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect({
    use eff <- list.map(effect.all)
    fn(actions: Actions(b)) {
      eff(Actions(
        dispatch: fn(msg) { actions.dispatch(f(msg)) },
        emit: actions.emit,
        // Selectors don't exist for the JavaScript target so there's no way
        // for anyone to legitimately construct one. It's kind of clunky that
        // we have to use `@target` for this and duplicate the function but
        // so be it.
        select: fn(_) { Nil },
        root: actions.root,
      ))
    }
  })
}

/// Perform a side effect by supplying your own `dispatch` and `emit`functions.
/// This is primarily used internally by the server component runtime, but it is
/// may also useful for testing.
///
/// **Note**: For now, you should **not** consider this function a part of the
/// public API. It may be removed in a future minor or patch release. If you have
/// a specific use case for this function, we'd love to hear about it! Please
/// reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
/// [open an issue](https://github.com/lustre-labs/lustre/issues/new)!
///
@internal
pub fn perform(
  effect: Effect(a),
  dispatch: fn(a) -> Nil,
  emit: fn(String, Json) -> Nil,
  select: fn(Selector(a)) -> Nil,
  root: Dynamic,
) -> Nil {
  let actions = Actions(dispatch, emit, select, root)
  use eff <- list.each(effect.all)

  eff(actions)
}
