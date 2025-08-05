//// Lustre's component system is built on top of the Custom Elements API and
//// the Shadow DOM API. This module helps you configure new components and
//// interact with existing ones.
////
//// While it's not required, understanding the spec and how it works will help
//// you get the most out of Lustre's component system. The following resources
//// are a great place to start:
////
////   - https://developer.mozilla.org/en-US/docs/Web/Web_Components
////
////   - https://css-tricks.com/web-components-demystified/
////
////   - https://github.com/web-padawan/awesome-web-components
////
//// ## Examples
////
//// We have a small number of examples showing how to set up and use components
//// that are a great place to see some code:
////
//// - [`Basic setup`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/01-basic-setup)
////
//// - [`Custom attributes and events`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/02-attributes-and-events)
////
//// - [`Slots`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/03-slots)
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

import gleam/dict
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
    delegates_focus: Bool,
    //
    attributes: List(#(String, fn(String) -> Result(msg, Nil))),
    properties: List(#(String, Decoder(msg))),
    contexts: List(#(String, Decoder(msg))),
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
/// > **Note**: Not all options are available for server components. For example
/// > server components cannot be form-associated and participate in form submission.
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
      open_shadow_root: True,
      adopt_styles: True,
      delegates_focus: False,
      //
      attributes: constants.empty_list,
      properties: constants.empty_list,
      contexts: constants.empty_list,
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
  let attributes = [#(name, decoder), ..config.attributes]

  Config(..config, attributes:)
}

/// Register decoder to run whenever the given property is set on the component.
/// Properties can be set in Lustre using the [`property`](./attribute.html#property)
/// function or in JavaScript by setting a property directly on the component
/// object.
///
/// Properties can be any JavaScript object. For server components, properties
/// will be any _JSON-serialisable_ value.
///
pub fn on_property_change(name: String, decoder: Decoder(msg)) -> Option(msg) {
  use config <- Option
  let properties = [#(name, decoder), ..config.properties]

  Config(..config, properties:)
}

/// Register a decoder to run whenever a parent component or application
/// [provides](./effect.html#provide) a new context value for the given `key`.
/// Contexts are a powerful feature that allow parents to inject data into
/// child components without knowledge of the DOM structurre, making them great
/// for advanced use-cases like design systems and flexible component hierarchies.
///
/// Contexts can be any JavaScript object. For server components, contexts will
/// be any _JSON-serialisable_ value.
///
pub fn on_context_change(key: String, decoder: Decoder(msg)) -> Option(msg) {
  use config <- Option
  let contexts = [#(key, decoder), ..config.contexts]

  Config(..config, contexts:)
}

/// Mark a component as "form-associated". This lets your component participate
/// in form submission and respond to additional form-specific events such as
/// the form being reset or the browser autofilling this component's value.
///
/// > **Note**: form-associated components are not supported in server components
/// > for both technical and ideological reasons. If you'd like a component that
/// > participates in form submission, you should use a client component!
///
pub fn form_associated() -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True)
}

/// Register a callback that runs when the browser autofills this
/// [form-associated](#form_associated) component's `"value"` attribute. The
/// callback should convert the autofilled value into a message that you handle
/// in your `update` function.
///
/// > **Note**: server components cannot participate in form submission and configuring
/// > this option will do nothing.
///
pub fn on_form_autofill(handler: fn(String) -> msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_autofill: Some(handler))
}

/// Set a message to be dispatched whenever a form containing this
/// [form-associated](#form_associated) component is reset.
///
/// > **Note**: server components cannot participate in form submission and configuring
/// > this option will do nothing.
///
pub fn on_form_reset(msg: msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_reset: Some(msg))
}

/// Set a callback that runs when the browser restores this
/// [form-associated](#form_associated) component's `"value"` attribute. This is
/// often triggered when the user navigates back or forward in their history.
///
/// > **Note**: server components cannot participate in form submission and configuring
/// > this option will do nothing.
///
pub fn on_form_restore(handler: fn(String) -> msg) -> Option(msg) {
  use config <- Option

  Config(..config, is_form_associated: True, on_form_restore: Some(handler))
}

/// Configure whether a component's [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot)
/// is open or closed. A closed shadow root means the elements rendered inside
/// the component are not accessible from JavaScript outside the component.
///
/// By default a component's shadow root is **open**. You may want to configure
/// this option manually if you intend to build a component for use outside of
/// Lustre.
///
pub fn open_shadow_root(open: Bool) -> Option(msg) {
  use config <- Option

  Config(..config, open_shadow_root: open)
}

/// Configure whether a component should attempt to adopt stylesheets from
/// its parent document. Components in Lustre use the shadow DOM to unlock native
/// web component features like slots, but this means elements rendered inside a
/// component are isolated from the document's styles.
///
/// To get around this, Lustre can attempt to adopt all stylesheets from the
/// parent document when the component is first created; meaning in many cases
/// you can use the same CSS to style your components as you do the rest of your
/// application.
///
/// By default, this option is **enabled**. You may want to disable this option
/// if you are building a component for use outside of Lustre and do not want
/// document styles to interfere with your component's styling
///
pub fn adopt_styles(adopt: Bool) -> Option(msg) {
  use config <- Option

  Config(..config, adopt_styles: adopt)
}

/// Indicates whether or not this component should delegate focus to its children.
/// When set to `True`, a number of focus-related features are enabled:
///
/// - Clicking on any non-interactive part of the component will automatically
///   focus the first focusable child element.
///
/// - The component can receive focus through the `.focus()` method or the
///   `autofocus` attribute, and it will automatically focus the first
///   focusable child element.
///
/// - The component receives the `:focus` CSS pseudo-class when any of its
///   focusable children have focus.
///
/// By default this option is **disabled**. You may want to enable this option
/// when creating complex interactive widgets.
///
pub fn delegates_focus(delegates: Bool) -> Option(msg) {
  use config <- Option

  Config(..config, delegates_focus: delegates)
}

// CONVERSIONS -----------------------------------------------------------------

/// 🚨 This is an **internal** function and should not be consumed by user code.
/// Internal functions may depend on unstable APIs or require certain usage
/// patterns: no guarantees are made about the stability _or_ reliability of
/// internal functions.
///
/// The server component runtime has to define its own `Config` type to avoid
/// circular dependencies. This function converts the public-facing component
/// config into an internal server component one: which is handy because not all
/// options are available in server components anyway.
///
@internal
pub fn to_server_component_config(config: Config(msg)) -> runtime.Config(msg) {
  runtime.Config(
    open_shadow_root: config.open_shadow_root,
    adopt_styles: config.adopt_styles,
    // we reverse both lists here such that the last added value takes precedence
    attributes: dict.from_list(list.reverse(config.attributes)),
    properties: dict.from_list(list.reverse(config.properties)),
    contexts: dict.from_list(list.reverse(config.contexts)),
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

/// Lustre's component system is built on top the Custom Elements API and the
/// Shadow DOM API. A component's `view` function is rendered inside a shadow
/// root, which means the component's HTML is isolated from the rest of the
/// document.
///
/// This can make it difficult to style components from CSS outside the component.
/// To help with this, the `part` attribute lets you expose parts of your component
/// by name to be styled by external CSS.
///
/// For example, if the `view` function for a component called `"my-component`"
/// looks like this:
///
/// ```gleam
/// import gleam/int
/// import lustre/component
/// import lustre/element/html
///
/// fn view(model) {
///   html.div([], [
///     html.button([], [html.text("-")]),
///     html.p([component.part("count")], [html.text(int.to_string(model.count))]),
///     html.button([], [html.text("+")]),
///   ])
/// }
/// ```
///
/// Then the following CSS in the **parent** document can be used to style the
/// `<p>` element:
///
/// ```css
/// my-component::part(count) {
///   color: red;
/// }
/// ```
///
/// To learn more about the CSS Shadow Parts specification, see:
///
///   - https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/part
///
///   - https://developer.mozilla.org/en-US/docs/Web/CSS/::part
///
pub fn part(name: String) -> Attribute(msg) {
  attribute("part", name)
}

/// While the [`part`](#part) attribute can be used to expose parts of a component
/// to its parent, these parts will not automatically become available to the
/// _document_ when components are nested inside each other.
///
/// The `exportparts` attribute lets you forward the parts of a nested component
/// to the parent component so they can be styled from the parent document.
///
/// Consider we have two components, `"my-component"` and `"my-nested-component"`
/// with the following `view` functions:
///
/// ```gleam
/// import gleam/int
/// import lustre/attribute.{property}
/// import lustre/component
/// import lustre/element.{element}
/// import lustre/element/html
///
/// fn my_component_view(model) {
///   html.div([], [
///     html.button([], [html.text("-")]),
///     element(
///       "my-nested-component",
///       [
///         property("count", model.count),
///         component.exportparts(["count"]),
///       ],
///       []
///     )
///     html.button([], [html.text("+")]),
///   ])
/// }
///
/// fn my_nested_component_view(model) {
///   html.p([component.part("count")], [html.text(int.to_string(model.count))])
/// }
/// ```
///
/// The `<my-nested-component />` component has a part called `"count"` which the
/// `<my-component />` then forwards to the parent document using the `"exportparts"`
/// attribute. Now the following CSS can be used to style the `<p>` element nested
/// deep inside the `<my-component />`:
///
/// ```css
/// my-component::part(count) {
///   color: red;
/// }
/// ```
///
/// Notice how the styles are applied to the `<my-component />` element, not the
/// `<my-nested-component />` element!
///
/// To learn more about the CSS Shadow Parts specification, see:
///
///   - https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/exportparts
///
///   - https://developer.mozilla.org/en-US/docs/Web/CSS/::part
///
pub fn exportparts(names: List(String)) -> Attribute(msg) {
  attribute("exportparts", string.join(names, ", "))
}

/// Associate an element with a [named slot](#named_slot) in a component. Multiple
/// elements can be associated with the same slot name.
///
/// To learn more about Shadow DOM and slots, see:
///
///   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/slot
///
///   https://javascript.info/slots-composition
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
  use _, root <- effect.before_paint
  do_set_form_value(root, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "set_form_value")
fn do_set_form_value(_root: Dynamic, _value: String) -> Nil {
  Nil
}

/// Clear a form value previously set with [`set_form_value`](#set_form_value).
/// When the form is submitted, this component's value will not be included in
/// the form data.
///
pub fn clear_form_value() -> Effect(msg) {
  use _, root <- effect.before_paint
  do_clear_form_value(root)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "clear_form_value")
fn do_clear_form_value(_root: Dynamic) -> Nil {
  Nil
}

/// Set a custom state on the component. This state is not reflected in the DOM
/// but can be selected in CSS using the `:state` pseudo-class. For example,
/// calling `set_pseudo_state("checked")` on a component called `"my-checkbox"`
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
pub fn set_pseudo_state(value: String) -> Effect(msg) {
  use _, root <- effect.before_paint
  do_set_pseudo_state(root, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "set_pseudo_state")
fn do_set_pseudo_state(_root: Dynamic, _value: String) -> Nil {
  Nil
}

/// Remove a custom state set by [`set_pseudo_state`](#set_pseudo_state).
///
pub fn remove_pseudo_state(value: String) -> Effect(msg) {
  use _, root <- effect.before_paint
  do_remove_pseudo_state(root, value)
}

@external(javascript, "./runtime/client/component.ffi.mjs", "remove_pseudo_state")
fn do_remove_pseudo_state(_root: Dynamic, _value: String) -> Nil {
  Nil
}
