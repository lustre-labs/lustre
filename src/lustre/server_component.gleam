//// > **Note**: server components are currently only supported on the **erlang**
//// > target. If it's important to you that they work on the javascript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run entire
//// Lustre applications on the server. DOM changes are broadcasted to a small
//// client runtime and browser events are sent back to the server.
////
//// ```text
//// -- SERVER -----------------------------------------------------------------
////
////                  Msg                            Element(Msg)
//// +--------+        v        +----------------+        v        +------+
//// |        | <-------------- |                | <-------------- |      |
//// | update |                 | Lustre runtime |                 | view |
//// |        | --------------> |                | --------------> |      |
//// +--------+        ^        +----------------+        ^        +------+
////         #(model, Effect(msg))  |        ^          Model
////                                |        |
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                        +-----------------------+
////                        |                       |
////                        | Your WebSocket server |
////                        |                       |
////                        +-----------------------+
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
//// -- BROWSER ----------------------------------------------------------------
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                            +----------------+
////                            |                |
////                            | Client runtime |
////                            |                |
////                            +----------------+
//// ```
////
//// **Note**: Lustre's server component runtime is separate from your application's
//// WebSocket server. You're free to bring your own stack, connect multiple
//// clients to the same Lustre instance, or keep the application alive even when
//// no clients are connected.
////
//// Lustre server components run next to the rest of your backend code, your
//// services, your database, etc. Real-time applications like chat services, games,
//// or components that can benefit from direct access to your backend services
//// like an admin dashboard or data table are excellent candidates for server
//// components.
////
//// ## Examples
////
//// Server components are a new feature in Lustre and we're still working on the
//// best ways to use them and show them off. For now, you can find a simple
//// undocumented example in the `examples/` directory:
////
//// - [`99-server-components`](https://github.com/lustre-labs/lustre/tree/main/examples/99-server-components)
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError, dynamic}
import gleam/erlang/process.{type Selector}
import gleam/int
import gleam/json.{type Json}
import gleam/result
import lustre.{type Patch, type ServerComponent}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element, element}
import lustre/internals/constants
@target(erlang)
import lustre/internals/runtime.{type Action, Attrs, Event, SetSelector}
@target(javascript)
import lustre/internals/runtime.{type Action, Attrs, Event}
import lustre/internals/patch

// ELEMENTS --------------------------------------------------------------------

/// Render the Lustre Server Component client runtime. The content of your server
/// component will be rendered inside this element.
///
/// **Note**: you must include the `lustre-server-component.mjs` script found in
/// the `priv/` directory of the Lustre package in your project's HTML.
///
pub fn component(attrs: List(Attribute(msg))) -> Element(msg) {
  element("lustre-server-component", attrs, [])
}

// ATTRIBUTES ------------------------------------------------------------------

/// The `route` attribute tells the client runtime what route it should use to
/// set up the WebSocket connection to the server. Whenever this attribute is
/// changed (by a clientside Lustre app, for example), the client runtime will
/// destroy the current connection and set up a new one.
///
pub fn route(path: String) -> Attribute(msg) {
  attribute("route", path)
}

/// Ocassionally you may want to attach custom data to an event sent to the server.
/// This could be used to include a hash of the current build to detect if the
/// event was sent from a stale client.
///
/// Your event decoders can access this data by decoding `data` property of the
/// event object.
///
pub fn data(json: Json) -> Attribute(msg) {
  json
  |> json.to_string
  |> attribute("data-lustre-data", _)
}

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to pass them to the server we need to copy them into a new
/// object first.
///
/// This attribute tells Lustre what properties to include. Properties can come
/// from nested objects by using dot notation. For example, you could include the
/// `id` of the target `element` by passing `["target.id"]`.
///
/// ```gleam
/// import gleam/dynamic
/// import gleam/result.{try}
/// import lustre/element.{type Element}
/// import lustre/element/html
/// import lustre/event
/// import lustre/server
///
/// pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {
///   let handler = fn(event) {
///     use target <- try(dynamic.field("target", dynamic.dynamic)(event))
///     use id <- try(dynamic.field("id", dynamic.string)(target))
///
///     Ok(on_click(id))
///   }
///
///   html.button([event.on_click(handler), server.include(["target.id"])], [
///     element.text("Click me!")
///   ])
/// }
/// ```
///
pub fn include(properties: List(String)) -> Attribute(msg) {
  properties
  |> json.array(json.string)
  |> json.to_string
  |> attribute("data-lustre-include", _)
}

// ACTIONS ---------------------------------------------------------------------

/// A server component broadcasts patches to be applied to the DOM to any connected
/// clients. This action is used to add a new client to a running server component.
///
pub fn subscribe(
  id: String,
  renderer: fn(Patch(msg)) -> Nil,
) -> Action(msg, ServerComponent) {
  runtime.Subscribe(id, renderer)
}

/// Remove a registered renderer from a server component. If no renderer with the
/// given id is found, this action has no effect.
///
pub fn unsubscribe(id: String) -> Action(msg, ServerComponent) {
  runtime.Unsubscribe(id)
}

// EFFECTS ---------------------------------------------------------------------

/// Instruct any connected clients to emit a DOM event with the given name and
/// data. This lets your server component communicate to frontend the same way
/// any other HTML elements do: you might emit a `"change"` event when some part
/// of the server component's state changes, for example.
///
/// This is a real DOM event and any JavaScript on the page can attach an event
/// listener to the server component element and listen for these events.
///
pub fn emit(event: String, data: Json) -> Effect(msg) {
  effect.event(event, data)
}

///
///
pub fn set_selector(sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  do_set_selector(sel)
}

@target(erlang)
fn do_set_selector(sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  use _ <- effect.from
  let self = process.new_subject()

  process.send(self, SetSelector(sel))
}

@target(javascript)
fn do_set_selector(_sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  effect.none()
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON encoded actions for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
pub fn decode_action(
  dyn: Dynamic,
) -> Result(Action(runtime, ServerComponent), List(DecodeError)) {
  dynamic.any([decode_event, decode_attrs])(dyn)
}

fn decode_event(dyn: Dynamic) -> Result(Action(runtime, msg), List(DecodeError)) {
  use #(kind, name, data) <- result.try(dynamic.tuple3(
    dynamic.int,
    dynamic,
    dynamic,
  )(dyn))
  use <- bool.guard(
    kind != constants.event,
    Error([
      DecodeError(
        path: ["0"],
        found: int.to_string(kind),
        expected: int.to_string(constants.event),
      ),
    ]),
  )
  use name <- result.try(dynamic.string(name))

  Ok(Event(name, data))
}

fn decode_attrs(dyn: Dynamic) -> Result(Action(runtime, msg), List(DecodeError)) {
  use #(kind, attrs) <- result.try(dynamic.tuple2(dynamic.int, dynamic)(dyn))
  use <- bool.guard(
    kind != constants.attrs,
    Error([
      DecodeError(
        path: ["0"],
        found: int.to_string(kind),
        expected: int.to_string(constants.attrs),
      ),
    ]),
  )
  use attrs <- result.try(dynamic.list(decode_attr)(attrs))

  Ok(Attrs(attrs))
}

fn decode_attr(dyn: Dynamic) -> Result(#(String, Dynamic), List(DecodeError)) {
  dynamic.tuple2(dynamic.string, dynamic)(dyn)
}

// ENCODERS --------------------------------------------------------------------

/// Encode a DOM patch as JSON you can send to the client runtime to apply. Whenever
/// the server runtime re-renders, all subscribed clients will receive a patch
/// message they must forward to the client runtime.
///
pub fn encode_patch(patch: Patch(msg)) -> Json {
  patch.patch_to_json(patch)
}
