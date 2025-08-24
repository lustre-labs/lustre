// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/pair
import gleam/result
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import lustre/internals/constants
import lustre/vdom/vattr.{Event, Handler}

// TYPES -----------------------------------------------------------------------

/// A custom event handler that can be used to conditionally stop propagation
/// or prevent the default behaviour of an event. You can construct these handlers
/// with the [`handler`](#handler) function and use them with the [`advanced`](#advanced)
/// event listener.
///
pub type Handler(msg) =
  vattr.Handler(msg)

// EFFECTS ---------------------------------------------------------------------

/// Dispatches a custom message from a Lustre component. This lets components
/// communicate with their parents the same way native DOM elements do.
///
/// Any JSON-serialisable payload can be attached as additional data for any
/// event listeners to decode. This data will be on the event's `detail` property.
///
pub fn emit(event: String, data: Json) -> Effect(msg) {
  effect.event(event, data)
}

// CUSTOM EVENTS ---------------------------------------------------------------

/// Listens for the given event and then runs the given decoder on the event
/// object. If the decoder succeeds, the decoded event is dispatched to your
/// application's `update` function. If it fails, the event is silently ignored.
///
/// The event name is typically an all-lowercase string such as "click" or "mousemove".
/// If you're listening for non-standard events (like those emitted by a custom
/// element) their event names might be slightly different.
///
/// > **Note**: if you are developing a server component, it is important to also
/// > use [`server_component.include`](./server_component.html#include) to state
/// > which properties of the event you need to be sent to the server.
///
pub fn on(name: String, handler: Decoder(msg)) -> Attribute(msg) {
  vattr.event(
    name:,
    handler: decode.map(handler, fn(msg) {
      Handler(prevent_default: False, stop_propagation: False, message: msg)
    }),
    include: constants.empty_list,
    prevent_default: vattr.never,
    stop_propagation: vattr.never,
    immediate: is_immediate_event(name),
    debounce: 0,
    throttle: 0,
  )
}

/// Listens for the given event and then runs the given decoder on the event
/// object. This decoder is capable of _conditionally_ stopping propagation or
/// preventing the default behaviour of the event by returning a `Handler` record
/// with the appropriate flags set. This makes it possible to write event handlers
/// for more-advanced scenarios such as handling specific key presses.
///
/// > **Note**: it is not possible to conditionally stop propagation or prevent
/// > the default behaviour of an event when using _server components_. Your event
/// > handler runs on the server, far away from the browser!
///
/// > **Note**: if you are developing a server component, it is important to also
/// > use [`server_component.include`](./server_component.html#include) to state
/// > which properties of the event you need to be sent to the server.
///
pub fn advanced(name: String, handler: Decoder(Handler(msg))) -> Attribute(msg) {
  vattr.event(
    name:,
    handler:,
    include: constants.empty_list,
    prevent_default: vattr.possible,
    stop_propagation: vattr.possible,
    immediate: is_immediate_event(name),
    debounce: 0,
    throttle: 0,
  )
}

/// Construct a [`Handler`](#Handler) that can be used with [`advanced`](#advanced)
/// to conditionally stop propagation or prevent the default behaviour of an event.
///
pub fn handler(
  dispatch message: msg,
  prevent_default prevent_default: Bool,
  stop_propagation stop_propagation: Bool,
) -> Handler(msg) {
  Handler(prevent_default:, stop_propagation:, message:)
}

fn is_immediate_event(name: String) -> Bool {
  case name {
    "input" | "change" | "focus" | "focusin" | "focusout" | "blur" | "select" ->
      True
    _ -> False
  }
}

/// Indicate that the event should have its default behaviour cancelled. This is
/// equivalent to calling `event.preventDefault()` in JavaScript.
///
/// > **Note**: this will override the conditional behaviour of an event handler
/// > created with [`advanced`](#advanced).
///
pub fn prevent_default(event: Attribute(msg)) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, prevent_default: vattr.always)
    _ -> event
  }
}

/// Indicate that the event should not propagate to parent elements. This is
/// equivalent to calling `event.stopPropagation()` in JavaScript.
///
/// > **Note**: this will override the conditional behaviour of an event handler
/// > created with [`advanced`](#advanced).
///
pub fn stop_propagation(event: Attribute(msg)) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, stop_propagation: vattr.always)
    _ -> event
  }
}

/// Use Lustre's built-in event debouncing to wait a delay after a burst of
/// events before dispatching the most recent one. You can visualise debounced
/// events like so:
///
/// ```
///  original : --a-b-cd--e----------f--------
/// debounced : ---------------e----------f---
/// ```
///
/// This is particularly useful for server components where many events in quick
/// succession can introduce problems because of network latency.
///
/// The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.
///
/// ### Example:
///
/// ```gleam
/// type Msg {
///     UserInputText(String)
/// }
/// 
/// html.input([event.debounce(event.on_input(fn(v) { UserInputText(v) }), 200)])
/// ```
///
/// > **Note**: debounced events inherently introduce latency. Try to consider
/// > typical interaction patterns and experiment with different delays to balance
/// > responsiveness and update frequency.
///
pub fn debounce(event: Attribute(msg), delay: Int) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, debounce: int.max(0, delay))
    _ -> event
  }
}

/// Use Lustre's built-in event throttling to restrict the number of events
/// that can be dispatched in a given time period. You can visualise throttled
/// events like so:
///
/// ```
/// original : --a-b-cd--e----------f--------
/// throttled : -a------ e----------e--------
/// ```
///
/// This is particularly useful for server components where many events in quick
/// succession can introduce problems because of network latency.
///
/// The unit of `delay` is millisecond, same as JavaScript's `setTimeout`.
///
/// > **Note**: throttled events inherently reduce precision. Try to consider
/// > typical interaction patterns and experiment with different delays to balance
/// > responsiveness and update frequency.
///
pub fn throttle(event: Attribute(msg), delay: Int) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, throttle: int.max(0, delay))
    _ -> event
  }
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click(msg: msg) -> Attribute(msg) {
  on("click", decode.success(msg))
}

///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  on("mousedown", decode.success(msg))
}

///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  on("mouseup", decode.success(msg))
}

///
pub fn on_mouse_enter(msg: msg) -> Attribute(msg) {
  on("mouseenter", decode.success(msg))
}

///
pub fn on_mouse_leave(msg: msg) -> Attribute(msg) {
  on("mouseleave", decode.success(msg))
}

///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  on("mouseover", decode.success(msg))
}

///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  on("mouseout", decode.success(msg))
}

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listens for key presses on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keypress", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

/// Listens for key down events on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keydown", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

/// Listens for key up events on an element, and dispatches a message with the
/// current key being released.
///
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keyup", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

// FORM EVENTS -----------------------------------------------------------------

/// Listens for input events on elements such as `<input>`, `<textarea>` and
/// `<select>`. This handler automatically decodes the string value of the input
/// and passes it to the given message function. This is commonly used to
/// implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
///
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
  on("input", {
    use value <- decode.subfield(["target", "value"], decode.string)

    decode.success(msg(value))
  })
}

/// Listens for change events on elements such as `<input>`, `<textarea>` and
/// `<select>`. This handler automatically decodes the string value of the input
/// and passes it to the given message function. This is commonly used to
/// implement [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
///
pub fn on_change(msg: fn(String) -> msg) -> Attribute(msg) {
  on("change", {
    use value <- decode.subfield(["target", "value"], decode.string)

    decode.success(msg(value))
  })
}

/// Listens for change events on `<input type="checkbox">` elements. This handler
/// automatically decodes the boolean value of the checkbox and passes it to
/// the given message function. This is commonly used to implement
/// [controlled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
///
pub fn on_check(msg: fn(Bool) -> msg) -> Attribute(msg) {
  on("change", {
    use checked <- decode.subfield(["target", "checked"], decode.bool)

    decode.success(msg(checked))
  })
}

/// Listens for submit events on a `<form>` element and receives a list of
/// name/value pairs for each field in the form. Files are not included in this
/// list: if you need them, you can write your own handler for the `"submit"`
/// event and decode the non-standard `detail.formData` property manually.
///
/// This handler is best paired with the [`formal`](https://hexdocs.pm/formal/)
/// package which lets you process form submissions in a type-safe way.
///
/// This will automatically call [`prevent_default`](#prevent_default) to stop
/// the browser's native form submission. In a Lustre app you'll want to handle
/// that yourself as an [`Effect`](./effect.html#Effect).
///
pub fn on_submit(msg: fn(List(#(String, String))) -> msg) -> Attribute(msg) {
  on("submit", {
    use formdata <- decode.subfield(["detail", "formData"], formdata_decoder())

    formdata
    |> msg
    |> decode.success
  })
  |> prevent_default
}

fn formdata_decoder() -> Decoder(List(#(String, String))) {
  let string_value_decoder = {
    use key <- decode.field(0, decode.string)
    use value <- decode.field(
      1,
      // Our `formData` entries will include the string values of any fields *and*
      // any `File` objects selected for file inputs. Our built-in `on_submit`
      // handler only supports those string values so we decode into a `Result`
      // so we can filter the files out without failing spectacularly.
      decode.one_of(decode.map(decode.string, Ok), [decode.success(Error(Nil))]),
    )

    value
    |> result.map(pair.new(key, _))
    |> decode.success
  }

  string_value_decoder
  |> decode.list
  |> decode.map(result.values)
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus(msg: msg) -> Attribute(msg) {
  on("focus", decode.success(msg))
}

pub fn on_blur(msg: msg) -> Attribute(msg) {
  on("blur", decode.success(msg))
}
