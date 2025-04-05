// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import lustre/internals/constants
import lustre/vdom/vattr.{Attribute, Event, Property}

// TYPES -----------------------------------------------------------------------

/// The `Attribute` type encompasses HTML attributes, DOM properties, and
/// event listeners.
///
pub type Attribute(msg) =
  vattr.Attribute(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an HTML attribute. This is like saying `element.setAttribute("class", "wibble")`
/// in JavaScript. Attributes will be rendered when calling [`element.to_string`](./element.html#to_string).
///
/// **Note**: there is a subtle difference between attributes and properties. You
/// can read more about the implications of this
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn attribute(name: String, value: String) -> Attribute(msg) {
  vattr.attribute(name, value)
}

/// Create a DOM property. This is like saying `element.className = "wibble"` in
/// JavaScript. Properties will be **not** be rendered when calling
/// [`element.to_string`](./element.html#to_string).
///
/// **Note**: there is a subtle difference between attributes and properties. You
/// can read more about the implications of this
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn property(name: String, value: Json) -> Attribute(msg) {
  vattr.property(name, value)
}

fn boolean_attribute(name: String, value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute(name, "")
    False -> property(name, json.bool(False))
  }
}

///
pub fn on(name: String, handler: Decoder(msg)) -> Attribute(msg) {
  vattr.event(
    name:,
    handler:,
    include: constants.empty_list,
    prevent_default: False,
    stop_propagation: False,
    immediate: is_immediate_event(name),
    limit: vattr.NoLimit(kind: 0),
  )
}

fn is_immediate_event(name: String) -> Bool {
  case name {
    "input" | "change" | "focus" | "focusin" | "focusout" | "blur" | "select" ->
      True
    _ -> False
  }
}

/// Create an empty attribute. This is not added to the DOM and not rendered when
/// calling [`element.to_string`](./element.html#to_string), but it is useful for
/// _conditionally_ adding attributes to an element.
///
pub fn none() -> Attribute(msg) {
  class("")
}

// MANIPULATIONS ---------------------------------------------------------------

/// The `Attribute` type is parameterised by the type of messages it can produce
/// from events handlers. Sometimes you might end up with an attribute from a
/// library or module that produces a different type of message: this function lets
/// you map the messages produced from one type to another.
///
pub fn map(attribute: Attribute(a), f: fn(a) -> b) -> Attribute(b) {
  case attribute {
    Attribute(kind:, name:, value:) -> Attribute(kind:, name:, value:)
    Event(handler:, ..) -> Event(..attribute, handler: decode.map(handler, f))
    Property(kind:, name:, value:) -> Property(kind:, name:, value:)
  }
}

// COMMON ATTRIBUTES -----------------------------------------------------------

///
///
/// > **Note**: unlike most attributes, multiple `style` attributes are merged
/// > with any existing other styles on an element. Styles added _later_ in the
/// > list will override styles added earlier.
///
pub fn style(properties: List(#(String, String))) -> Attribute(msg) {
  attribute("style", {
    use styles, #(name, value) <- list.fold(properties, "")
    styles <> name <> ":" <> value <> ";"
  })
}

///
///
/// > **Note**: unlike most attributes, multiple `class` attributes are merged
/// > with any existing other classes on an element.
///
pub fn class(name: String) -> Attribute(msg) {
  attribute("class", name)
}

///
pub fn classes(names: List(#(String, Bool))) -> Attribute(msg) {
  attribute("class", {
    use classes, #(class, active) <- list.fold(names, "")
    case classes {
      "" if active -> class
      _ if active -> classes <> " " <> class
      _ -> classes
    }
  })
}

/// Add a `data-*` attribute to an HTML element. The key will be prefixed by `data-`.
///
pub fn data(key: String, value: String) -> Attribute(msg) {
  attribute("data-" <> key, value)
}

/// Add an `aria-*` attribute to an HTML element. The key will be prefixed by
/// `aria-`.
///
pub fn aria(name: String, value: String) -> Attribute(msg) {
  attribute("aria-" <> name, value)
}

///
pub fn id(name: String) -> Attribute(msg) {
  attribute("id", name)
}

///
pub fn role(name: String) -> Attribute(msg) {
  attribute("role", name)
}

///
pub fn title(name: String) -> Attribute(msg) {
  attribute("title", name)
}

// INPUTS ----------------------------------------------------------------------

///
pub fn type_(name: String) -> Attribute(msg) {
  attribute("type", name)
}

///
pub fn value(val: String) -> Attribute(msg) {
  attribute("value", val)
}

///
pub fn checked(is_checked: Bool) -> Attribute(msg) {
  boolean_attribute("checked", is_checked)
}

///
pub fn placeholder(text: String) -> Attribute(msg) {
  attribute("placeholder", text)
}

///
pub fn selected(is_selected: Bool) -> Attribute(msg) {
  boolean_attribute("selected", is_selected)
}

// INPUT HELPERS ---------------------------------------------------------------

///
pub fn accept(types: List(String)) -> Attribute(msg) {
  attribute("accept", string.join(types, ","))
}

///
pub fn accept_charset(types: List(String)) -> Attribute(msg) {
  attribute("accept-charset", string.join(types, " "))
}

///
pub fn msg(uri: String) -> Attribute(msg) {
  attribute("msg", uri)
}

///
pub fn autocomplete(name: String) -> Attribute(msg) {
  attribute("autocomplete", name)
}

/// Sets the `autofocus` attribute.
///
/// Lustre's runtime augments that native behaviour of this attribute. Whenever
/// it is toggled true, the element will be automatically focused even if it already
/// exists in the DOM.
///
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg) {
  boolean_attribute("autofocus", should_autofocus)
}

///
pub fn disabled(is_disabled: Bool) -> Attribute(msg) {
  boolean_attribute("disabled", is_disabled)
}

///
pub fn name(name: String) -> Attribute(msg) {
  attribute("name", name)
}

///
pub fn pattern(regex: String) -> Attribute(msg) {
  attribute("pattern", regex)
}

///
pub fn readonly(is_readonly: Bool) -> Attribute(msg) {
  boolean_attribute("readonly", is_readonly)
}

///
pub fn required(is_required: Bool) -> Attribute(msg) {
  boolean_attribute("required", is_required)
}

///
pub fn for(id: String) -> Attribute(msg) {
  attribute("for", id)
}

// INPUT RANGES ----------------------------------------------------------------

///
pub fn maxlength(val: String) -> Attribute(msg) {
  attribute("maxlength", val)
}

///
pub fn max(val: String) -> Attribute(msg) {
  attribute("max", val)
}

///
pub fn min(val: String) -> Attribute(msg) {
  attribute("min", val)
}

///
pub fn step(val: String) -> Attribute(msg) {
  attribute("step", val)
}

// INPUT TEXT AREAS ------------------------------------------------------------

///
pub fn cols(val: Int) -> Attribute(msg) {
  attribute("cols", int.to_string(val))
}

///
pub fn rows(val: Int) -> Attribute(msg) {
  attribute("rows", int.to_string(val))
}

///
pub fn wrap(mode: String) -> Attribute(msg) {
  attribute("wrap", mode)
}

// LINKS AND AREAS -------------------------------------------------------------

///
pub fn href(uri: String) -> Attribute(msg) {
  attribute("href", uri)
}

///
pub fn target(target: String) -> Attribute(msg) {
  attribute("target", target)
}

///
pub fn download(filename: String) -> Attribute(msg) {
  attribute("download", filename)
}

///
pub fn rel(relationship: String) -> Attribute(msg) {
  attribute("rel", relationship)
}

///
pub fn crossorigin(origin: String) -> Attribute(msg) {
  attribute("crossorigin", origin)
}

///
pub fn integrity(cryptographic_hash: String) -> Attribute(msg) {
  attribute("integrity", cryptographic_hash)
}

// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn src(uri: String) -> Attribute(msg) {
  attribute("src", uri)
}

/// **Note**: this uses [`property`](#property) to set the value directly on the
/// DOM node, making it **incompatible** with SVG elements. To set the height of
/// an `<svg>` element, use the [`attribute`](#attribute) function directly.
///
/// You can read more about the difference between attributes and properties
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn height(val: Int) -> Attribute(msg) {
  property("height", json.int(val))
}

/// **Note**: this uses [`property`](#property) to set the value directly on the
/// DOM node, making it **incompatible** with SVG elements. To set the width of
/// an `<svg>` element, use the [`attribute`](#attribute) function directly.
///
/// You can read more about the difference between attributes and properties
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn width(val: Int) -> Attribute(msg) {
  property("width", json.int(val))
}

///
pub fn alt(text: String) -> Attribute(msg) {
  attribute("alt", text)
}

///
pub fn content(text: String) -> Attribute(msg) {
  attribute("content", text)
}

// AUDIO AND VIDEO -------------------------------------------------------------

/// Sets the `autofocus` attribute.
///
/// Lustre will start playing every time this attribute switches from `False`
/// to `True`.
pub fn autoplay(should_autoplay: Bool) -> Attribute(msg) {
  boolean_attribute("autoplay", should_autoplay)
}

///
pub fn controls(visible: Bool) -> Attribute(msg) {
  boolean_attribute("controls", visible)
}

///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  boolean_attribute("loop", should_loop)
}

// FORMS -----------------------------------------------------------------------

///
pub fn action(url: String) -> Attribute(msg) {
  attribute("action", url)
}

///
pub fn enctype(value: String) -> Attribute(msg) {
  attribute("enctype", value)
}

///
pub fn method(method: String) -> Attribute(msg) {
  attribute("method", method)
}

///
pub fn novalidate(value: Bool) -> Attribute(msg) {
  boolean_attribute("novalidate", value)
}

///
pub fn form_action(action: String) -> Attribute(msg) {
  attribute("formaction", action)
}

///
pub fn form_enctype(value: String) -> Attribute(msg) {
  attribute("formenctype", value)
}

///
pub fn form_method(method: String) -> Attribute(msg) {
  attribute("formmethod", method)
}

///
pub fn form_novalidate(value: Bool) -> Attribute(msg) {
  boolean_attribute("formnovalidate", value)
}

///
pub fn form_target(target: String) -> Attribute(msg) {
  attribute("formtarget", target)
}

// DIALOGS ---------------------------------------------------------------------

///
pub fn open(is_open: Bool) -> Attribute(msg) {
  boolean_attribute("open", is_open)
}

// WEB COMPONENTS --------------------------------------------------------------

///
pub fn slot(name: String) -> Attribute(msg) {
  attribute("slot", name)
}

///
pub fn shadow_root_mode(is_open: Bool) -> Attribute(msg) {
  attribute("shadowrootmode", case is_open {
    True -> "open"
    False -> "closed"
  })
}

///
pub fn shadow_root_delegates_focus(delegates_focus: Bool) -> Attribute(msg) {
  boolean_attribute("shadowrootdelegatesfocus", delegates_focus)
}

// META ------------------------------------------------------------------------

///
pub fn charset(name: String) -> Attribute(msg) {
  attribute("charset", name)
}

///
pub fn http_equiv(name: String) -> Attribute(msg) {
  attribute("http-equiv", name)
}

// HTML ------------------------------------------------------------------------

///
pub fn lang(name: String) -> Attribute(msg) {
  attribute("lang", name)
}

// ARIA ------------------------------------------------------------------------

/// The aria-activedescendant attribute identifies the currently active element
/// when focus is on a composite widget, combobox, textbox, group, or application.
///
pub fn aria_activedescendant(id: String) -> Attribute(msg) {
  aria("activedescendant", id)
}

/// In ARIA live regions, the global aria-atomic attribute indicates whether
/// assistive technologies such as a screen reader will present all, or only parts
/// of, the changed region based on the change notifications defined by the
/// aria-relevant attribute.
///
pub fn aria_atomic(value: Bool) -> Attribute(msg) {
  aria("atomic", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-autocomplete attribute indicates whether inputting text could trigger
/// display of one or more predictions of the user's intended value for a combobox,
/// searchbox, or textbox and specifies how predictions will be presented if they
/// are made.
///
pub fn aria_autocomplete(value: String) -> Attribute(msg) {
  aria("autocomplete", value)
}

/// The global aria-braillelabel property defines a string value that labels the
/// current element, which is intended to be converted into Braille.
///
pub fn aria_braillelabel(value: String) -> Attribute(msg) {
  aria("braillelabel", value)
}

/// The global aria-brailleroledescription attribute defines a human-readable,
/// author-localized abbreviated description for the role of an element intended
/// to be converted into Braille.
///
pub fn aria_brailleroledescription(value: String) -> Attribute(msg) {
  aria("brailleroledescription", value)
}

/// Used in ARIA live regions, the global aria-busy state indicates an element is
/// being modified and that assistive technologies may want to wait until the
/// changes are complete before informing the user about the update.
///
pub fn aria_busy(value: Bool) -> Attribute(msg) {
  aria("busy", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-checked attribute indicates the current "checked" state of checkboxes,
/// radio buttons, and other widgets.
///
pub fn aria_checked(value: String) -> Attribute(msg) {
  aria("checked", value)
}

/// The aria-colcount attribute defines the total number of columns in a table,
/// grid, or treegrid when not all columns are present in the DOM.
///
pub fn aria_colcount(value: Int) -> Attribute(msg) {
  aria("colcount", int.to_string(value))
}

/// The aria-colindex attribute defines an element's column index or position with
/// respect to the total number of columns within a table, grid, or treegrid.
///
pub fn aria_colindex(value: Int) -> Attribute(msg) {
  aria("colindex", int.to_string(value))
}

/// The aria-colindextext attribute defines a human-readable text alternative of
/// the numeric aria-colindex.
///
pub fn aria_colindextext(value: String) -> Attribute(msg) {
  aria("colindextext", value)
}

/// The aria-colspan attribute defines the number of columns spanned by a cell
/// or gridcell within a table, grid, or treegrid.
///
pub fn aria_colspan(value: Int) -> Attribute(msg) {
  aria("colspan", int.to_string(value))
}

/// The global aria-controls property identifies the element (or elements) whose
/// contents or presence are controlled by the element on which this attribute is
/// set.
///
pub fn aria_controls(value: String) -> Attribute(msg) {
  aria("controls", value)
}

/// A non-null aria-current state on an element indicates that this element represents
/// the current item within a container or set of related elements.
///
pub fn aria_current(value: String) -> Attribute(msg) {
  aria("current", value)
}

/// The global aria-describedby attribute identifies the element (or elements)
/// that describes the element on which the attribute is set.
///
pub fn aria_describedby(value: String) -> Attribute(msg) {
  aria("describedby", value)
}

/// The global aria-description attribute defines a string value that describes
/// or annotates the current element.
///
pub fn aria_description(value: String) -> Attribute(msg) {
  aria("description", value)
}

/// The global aria-details attribute identifies the element (or elements) that
/// provide additional information related to the object.
///
pub fn aria_details(value: String) -> Attribute(msg) {
  aria("details", value)
}

/// The aria-disabled state indicates that the element is perceivable but disabled,
/// so it is not editable or otherwise operable.
///
pub fn aria_disabled(value: Bool) -> Attribute(msg) {
  aria("disabled", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-errormessage attribute on an object identifies the element that
/// provides an error message for that object.
///
pub fn aria_errormessage(value: String) -> Attribute(msg) {
  aria("errormessage", value)
}

/// The aria-expanded attribute is set on an element to indicate if a control is
/// expanded or collapsed, and whether or not the controlled elements are displayed
/// or hidden.
///
pub fn aria_expanded(value: Bool) -> Attribute(msg) {
  aria("expanded", case value {
    True -> "true"
    False -> "false"
  })
}

/// The global aria-flowto attribute identifies the next element (or elements) in
/// an alternate reading order of content. This allows assistive technology to
/// override the general default of reading in document source order at the user's
/// discretion.
///
pub fn aria_flowto(value: String) -> Attribute(msg) {
  aria("flowto", value)
}

/// The aria-haspopup attribute indicates the availability and type of interactive
/// popup element that can be triggered by the element on which the attribute is
/// set.
///
pub fn aria_haspopup(value: String) -> Attribute(msg) {
  aria("haspopup", value)
}

/// The aria-hidden state indicates whether the element is exposed to an accessibility
/// API.
///
pub fn aria_hidden(value: Bool) -> Attribute(msg) {
  aria("hidden", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-invalid state indicates the entered value does not conform to the
/// format expected by the application.
///
pub fn aria_invalid(value: String) -> Attribute(msg) {
  aria("invalid", value)
}

/// The global aria-keyshortcuts attribute indicates keyboard shortcuts that an
/// author has implemented to activate or give focus to an element.
///
pub fn aria_keyshortcuts(value: String) -> Attribute(msg) {
  aria("keyshortcuts", value)
}

/// The aria-label attribute defines a string value that can be used to name an
/// element, as long as the element's role does not prohibit naming.
///
pub fn aria_label(value: String) -> Attribute(msg) {
  aria("label", value)
}

/// The aria-labelledby attribute identifies the element (or elements) that labels
/// the element it is applied to.
///
pub fn aria_labelledby(value: String) -> Attribute(msg) {
  aria("labelledby", value)
}

/// The aria-level attribute defines the hierarchical level of an element within
/// a structure.
///
pub fn aria_level(value: Int) -> Attribute(msg) {
  aria("level", int.to_string(value))
}

/// The global aria-live attribute indicates that an element will be updated, and
/// describes the types of updates the user agents, assistive technologies, and
/// user can expect from the live region.
///
pub fn aria_live(value: String) -> Attribute(msg) {
  aria("live", value)
}

/// The aria-modal attribute indicates whether an element is modal when displayed.
///
pub fn aria_modal(value: Bool) -> Attribute(msg) {
  aria("modal", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-multiline attribute indicates whether a textbox accepts multiple
/// lines of input or only a single line.
///
pub fn aria_multiline(value: Bool) -> Attribute(msg) {
  aria("multiline", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-multiselectable attribute indicates that the user may select more
/// than one item from the current selectable descendants.
///
pub fn aria_multiselectable(value: Bool) -> Attribute(msg) {
  aria("multiselectable", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-orientation attribute indicates whether the element's orientation is
/// horizontal, vertical, or unknown/ambiguous.
///
pub fn aria_orientation(value: String) -> Attribute(msg) {
  aria("orientation", value)
}

/// The aria-owns attribute identifies an element (or elements) in order to define
/// a visual, functional, or contextual relationship between a parent and its
/// child elements when the DOM hierarchy cannot be used to represent the relationship.
///
pub fn aria_owns(value: String) -> Attribute(msg) {
  aria("owns", value)
}

/// The aria-placeholder attribute defines a short hint (a word or short phrase)
/// intended to help the user with data entry when a form control has no value.
/// The hint can be a sample value or a brief description of the expected format.
///
pub fn aria_placeholder(value: String) -> Attribute(msg) {
  aria("placeholder", value)
}

/// The aria-posinset attribute defines an element's number or position in the
/// current set of listitems or treeitems when not all items are present in the
/// DOM.
///
pub fn aria_posinset(value: Int) -> Attribute(msg) {
  aria("posinset", int.to_string(value))
}

/// The aria-pressed attribute indicates the current "pressed" state of a toggle
/// button.
///
pub fn aria_pressed(value: String) -> Attribute(msg) {
  aria("pressed", value)
}

/// The aria-readonly attribute indicates that the element is not editable, but is
/// otherwise operable.
///
pub fn aria_readonly(value: Bool) -> Attribute(msg) {
  aria("readonly", case value {
    True -> "true"
    False -> "false"
  })
}

/// Used in ARIA live regions, the global aria-relevant attribute indicates what
/// notifications the user agent will trigger when the accessibility tree within
/// a live region is modified.
///
pub fn aria_relevant(value: String) -> Attribute(msg) {
  aria("relevant", value)
}

/// The aria-required attribute indicates that user input is required on the element
/// before a form may be submitted.
///
pub fn aria_required(value: Bool) -> Attribute(msg) {
  aria("required", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-roledescription attribute defines a human-readable, author-localised
/// description for the role of an element.
///
pub fn aria_roledescription(value: String) -> Attribute(msg) {
  aria("roledescription", value)
}

/// The aria-rowcount attribute defines the total number of rows in a table,
/// grid, or treegrid.
///
pub fn aria_rowcount(value: Int) -> Attribute(msg) {
  aria("rowcount", int.to_string(value))
}

/// The aria-rowindex attribute defines an element's position with respect to the
/// total number of rows within a table, grid, or treegrid.
///
pub fn aria_rowindex(value: Int) -> Attribute(msg) {
  aria("rowindex", int.to_string(value))
}

/// The aria-rowindextext attribute defines a human-readable text alternative of
/// aria-rowindex.
///
pub fn aria_rowindextext(value: String) -> Attribute(msg) {
  aria("rowindextext", value)
}

/// The aria-rowspan attribute defines the number of rows spanned by a cell or
/// gridcell within a table, grid, or treegrid.
///
pub fn aria_rowspan(value: Int) -> Attribute(msg) {
  aria("rowspan", int.to_string(value))
}

/// The aria-selected attribute indicates the current "selected" state of various
/// widgets.
///
pub fn aria_selected(value: Bool) -> Attribute(msg) {
  aria("selected", case value {
    True -> "true"
    False -> "false"
  })
}

/// The aria-setsize attribute defines the number of items in the current set of
/// listitems or treeitems when not all items in the set are present in the DOM.
///
pub fn aria_setsize(value: Int) -> Attribute(msg) {
  aria("setsize", int.to_string(value))
}

/// The aria-sort attribute indicates if items in a table or grid are sorted in
/// ascending or descending order.
///
pub fn aria_sort(value: String) -> Attribute(msg) {
  aria("sort", value)
}

/// The aria-valuemax attribute defines the maximum allowed value for a range
/// widget.
///
pub fn aria_valuemax(value: String) -> Attribute(msg) {
  aria("valuemax", value)
}

/// The aria-valuemin attribute defines the minimum allowed value for a range
/// widget.
///
pub fn aria_valuemin(value: String) -> Attribute(msg) {
  aria("valuemin", value)
}

/// The aria-valuenow attribute defines the current value for a range widget.
///
pub fn aria_valuenow(value: String) -> Attribute(msg) {
  aria("valuenow", value)
}

/// The aria-valuetext attribute defines the human-readable text alternative of
/// aria-valuenow for a range widget.
///
pub fn aria_valuetext(value: String) -> Attribute(msg) {
  aria("valuetext", value)
}
