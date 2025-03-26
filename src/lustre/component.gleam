// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/list
import gleam/option.{Some}
import gleam/string
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/internals/constants
import lustre/runtime/server/runtime

// TYPES -----------------------------------------------------------------------

/// The configuration for a Lustre [`component`](../lustre.html#component). In
/// Lustre, components are real custom elements. You can use this configuration
/// to define what features the component supports and what platform functionality
/// it should have access to.
///
pub opaque type Config(msg) {
  Config(
    //
    open_shadow_root: Bool,
    adopt_styles: Bool,
    //
    attributes: Dict(String, fn(String) -> Result(msg, Nil)),
    properties: Dict(String, Decoder(msg)),
    //
    is_form_associated: Bool,
    on_form_autofill: option.Option(fn(String) -> msg),
    on_form_reset: option.Option(msg),
    on_form_restore: option.Option(fn(String) -> msg),
  )
}

/// Options are used to configure a component's behaviour.
///
/// - [`on_attribute_change`](#on_attribute_change) lets you register a callback
///   that runs whenever an [`attribute`](./attribute.html#attribute) is set on
///   your component in the DOM.
///
/// - [`on_property_change`](#on_property_change) lets you register a decoder to
///   run whenever a [`property`](./attribute.html#property) is set on the
///   component.
///
///   See [this note](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md)
///   on the difference between attributes and properties.
///
/// - [`form_associated`](#form_associated) marks the component as "form-associated",
///   allowing your component to participate in form submission and get accesss
///   to form-specific events.
///
///   - [`on_form_autofill`](#on_form_autofill) lets you register a callback that
///     runs when the browser autofills your component's `"value"` attribute.
///
///   - [`on_form_reset`](#on_form_reset) lets you register a message that runs
///     when a form containing your component is reset.
///
///   - [`on_form_restore`](#on_form_restore) lets you register a callback that
///     runs when the browser restores your component's `"value"` attribute, often
///     after a page reload or the user navigating back or forward in their history.
///
/// - [`open_shadow_root`](#open_shadow_root) lets you control whether the component
///   uses an open or closed [shadow root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/mode).
///
/// - [`adopt_styles`](#adopt_styles) lets you control whether the component should
///   attempt to adopt stylesheets from its parent document. All Lustre components
///   use shadow DOM to get access to certain features like form-associated elements
///   or HTML slots. Unfortunately, this means typically styles in the shadow DOM
///   are isolated from the parent document.
///
///   Setting `adopt_styles` to `True` tells Lustre to attempt to adopt or clone
///   stylesheets from the parent document _into_ the shadow DOM. This can give
///   you an experience similar to components in other frameworks like React or
///   Vue.
///
/// **Note**: Not all options are available for server components. For example
/// server components cannot be form-associated and participate in form submission.
///
pub opaque type Option(msg) {
  Option(apply: fn(Config(msg)) -> Config(msg))
}

/// 🚨 This is an **internal** function and should not be consumed by user code.
/// Internal functions may depend on unstable APIs or require certain usage
/// patterns: no guarantees are made about the stability _or_ reliability of
/// internal functions.
///
/// Construct a new `Config` record and apply a list of `Option`s to it. Options
/// are applied in order and later options may override earlier ones.
///
@internal
pub fn new(options: List(Option(msg))) -> Config(msg) {
  let init =
    Config(
      //
      open_shadow_root: False,
      adopt_styles: True,
      //
      attributes: constants.empty_dict(),
      properties: constants.empty_dict(),
      //
      is_form_associated: False,
      on_form_autofill: constants.option_none,
      on_form_reset: constants.option_none,
      on_form_restore: constants.option_none,
    )

  use config, option <- list.fold(options, init)

  option.apply(config)
}

// BUILDERS --------------------------------------------------------------------

/// Register a decoder to run whenever the named attribute changes. Attributes
/// can be set in Lustre using the [`attribute`](./attribute.html#attribute)
/// function, set directly on the component's HTML tag, or in JavaScript using
/// the [`setAttribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute)
/// method.
///
/// Attributes are always strings, but your decoder is responsible for decoding
/// the string into a message that your component can understand.
///
pub fn on_attribute_change(
  name: String,
  decoder: fn(String) -> Result(msg, Nil),
) -> Option(msg) {
  use config <- Option
  let attributes = dict.insert(config.attributes, name, decoder)

  Config(..config, attributes:)
}

/// Register decoder to run whenever the given property is set on the component.
/// Properties can be set in Lustre using the [`property`](./attribute.html#property)
/// function or in JavaScript by setting a property directly on the component
/// object.
///
/// Properties can be any JavaScript object. For server components properties
/// will be any JSON-serializable value.
///
pub fn on_property_change(name: String, decoder: Decoder(msg)) -> Option(msg) {
  use config <- Option
  let properties = dict.insert(config.properties, name, decoder)

  Config(..config, properties:)
}

/// Mark a component as "form-associated". This lets your component participate
/// in form submission and respond to additional form-specific events such as
/// the form being reset or the browser autofilling this component's value.
///
pub fn form_associated() -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True)
}

///
///
pub fn on_form_autofill(handler: fn(String) -> msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_autofill: Some(handler))
}

///
///
pub fn on_form_reset(msg: msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_reset: Some(msg))
}

///
///
pub fn on_form_restore(handler: fn(String) -> msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_restore: Some(handler))
}

///
///
pub fn open_shadow_root(open: Bool) -> Option(msg) {
  use config <- Option

  Config(..config, open_shadow_root: open)
}

///
///
pub fn adopt_styles(adopt: Bool) -> Option(msg) {
  use config <- Option

  Config(..config, adopt_styles: adopt)
}

// CONVERSIONS -----------------------------------------------------------------

@internal
pub fn to_server_component_config(config: Config(msg)) -> runtime.Config(msg) {
  runtime.Config(
    open_shadow_root: config.open_shadow_root,
    adopt_styles: config.adopt_styles,
    //
    attributes: config.attributes,
    properties: config.properties,
  )
}

// ELEMENTS --------------------------------------------------------------------

/// Create a default slot for a component. Any elements rendered as children of
/// the component will be placed inside the default slot unless explicitly
/// redirected using the [`slot`](#slot) attribute.
///
/// If no children are placed into the slot, the `fallback` elements will be
/// rendered instead.
///
/// To learn more about Shadow DOM and slots, see this excellent guide:
///
///   https://javascript.info/slots-composition
///
pub fn default_slot(
  attributes: List(Attribute(msg)),
  fallback: List(Element(msg)),
) -> Element(msg) {
  html.slot(attributes, fallback)
}

/// Create a named slot for a component. Any elements rendered as children of
/// the component with a [`slot`](#slot) attribute matching the `name` will be
/// rendered inside this slot.
///
/// If no children are placed into the slot, the `fallback` elements will be
/// rendered instead.
///
/// To learn more about Shadow DOM and slots, see this excellent guide:
///
///   https://javascript.info/slots-composition
///
pub fn named_slot(
  name: String,
  attributes: List(Attribute(msg)),
  fallback: List(Element(msg)),
) -> Element(msg) {
  html.slot([attribute("name", name), ..attributes], fallback)
}

// ATTRIBUTES ------------------------------------------------------------------

///
///
pub fn part(name: String) -> Attribute(msg) {
  attribute("part", name)
}

///
///
pub fn exportparts(names: List(String)) -> Attribute(msg) {
  attribute("exportparts", string.join(names, ", "))
}

///
///
pub fn slot(name: String) -> Attribute(msg) {
  attribute("slot", name)
}

// EFFECTS ---------------------------------------------------------------------

/// Set the value of a [form-associated component](#form_associated). If the
/// component is rendered inside a `<form>` element, the value will be
/// automatically included in the form submission and available in the form's
/// `FormData` object.
///
pub fn set_form_value(value: String) -> Effect(msg) {
  use _, internals <- effect.with_element_internals
  do_set_form_value(internals, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "set_form_value")
fn do_set_form_value(_internals: Dynamic, _value: String) -> Nil {
  Nil
}

/// Clear a form value previously set with [`set_form_value`](#set_form_value).
/// When the form is submitted, this component's value will not be included in
/// the form data.
///
pub fn clear_form_value() -> Effect(msg) {
  use _, internals <- effect.with_element_internals
  do_clear_form_value(internals)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "clear_form_value")
fn do_clear_form_value(_internals: Dynamic) -> Nil {
  Nil
}

/// Set a custom state on the component. This state is not reflected in the DOM
/// but can be selected in CSS using the `:state` pseudo-class. For example,
/// calling `set_psuedo_state("checked")` on a component called `"my-checkbox"`
/// means the following CSS will apply:
///
/// ```css
/// my-checkbox:state(checked) {
///   border: solid;
/// }
/// ```
///
/// If you are styling a component by rendering a `<style>` element _inside_ the
/// component, the previous CSS would be rewritten as:
///
/// ```css
/// :host(:state(checked)) {
///   border: solid;
/// }
/// ```
///
pub fn set_psuedo_state(value: String) -> Effect(msg) {
  use _, internals <- effect.with_element_internals
  do_set_psuedo_state(internals, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "set_psuedo_state")
fn do_set_psuedo_state(_internals: Dynamic, _value: String) -> Nil {
  Nil
}

/// Remove a custom state set by [`set_psuedo_state`](#set_psuedo_state).
///
pub fn remove_psuedo_state(value: String) -> Effect(msg) {
  use _, internals <- effect.with_element_internals
  do_remove_psuedo_state(internals, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "remove_psuedo_state")
fn do_remove_psuedo_state(_internals: Dynamic, _value: String) -> Nil {
  Nil
}
