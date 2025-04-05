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

fn boolean_attribute(name: String, value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute(name, "")
    False -> property(name, json.bool(False))
  }
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

///
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

// GLOBAL ATTRIBUTES -----------------------------------------------------------

/// Defines a shortcut key to activate or focus the element. Multiple options
/// may be provided as a set of space-separated characters that are exactly one
/// code point each.
///
/// The way to activate the access key depends on the browser and its platform:
///
/// |         | Windows           | Linux               | Mac OS              |
/// |---------|-------------------|---------------------|---------------------|
/// | Firefox | Alt + Shift + key | Alt + Shift + key   | Ctrl + Option + key |
/// | Chrome  | Alt + key         | Ctrl + Option + key | Ctrl + Option + key |
/// | Safari  |                   |                     | Ctrl + Option + key |
///
pub fn accesskey(key: String) -> Attribute(msg) {
  attribute("accesskey", key)
}

/// Controls whether text input is automatically capitalised. The following values
/// are accepted:
///
/// | Value        | Mode       |
/// |--------------|------------|
/// | ""           | default    |
/// | "none"       | none       |
/// | "off"        |            |
/// | "sentences"  | sentences  |
/// | "on"         |            |
/// | "words"      | words      |
/// | "characters" | characters |
///
/// The autocapitalisation processing model is based on the following five modes:
///
/// - **default**: The user agent and input method should make their own determination
///   of whether or not to enable autocapitalization.
///
/// - **none**: No autocapitalisation should be applied (all letters should default
///   to lowercase).
///
/// - **sentences**: The first letter of each sentence should default to a capital
///   letter; all other letters should default to lowercase.
///
/// - **words**: The first letter of each word should default to a capital letter;
///   all other letters should default to lowercase.
///
/// - **characters**: All letters should default to uppercase.
///
pub fn autocapitalize(value: String) -> Attribute(msg) {
  attribute("autocapitalize", value)
}

/// Controls whether the user agent may automatically correct mispelled words
/// while typing. Whether or not spelling is corrected is left up to the user
/// agent and may also depend on the user's settings.
///
/// When disabled the user agent is **never** allowed to correct spelling.
///
pub fn autocorrect(enabled: Bool) -> Attribute(msg) {
  boolean_attribute("autocorrect", enabled)
}

/// For server-rendered HTML, this attribute controls whether an element should
/// be focused when the page first loads. Lustre's runtime augments the native
/// behaviour of this attribute: when enabled, the element will automatically be
/// focused even if it already exists in the DOM.
///
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg) {
  boolean_attribute("autofocus", should_autofocus)
}

///
///
/// > **Note**: unlike most attributes, multiple `class` attributes are merged
/// > with any existing other classes on an element. Classes added _later_ in the
/// > list will override classes added earlier.
///
pub fn class(name: String) -> Attribute(msg) {
  attribute("class", name)
}

///
///
/// > **Note**: unlike most attributes, multiple `class` attributes are merged
/// > with any existing other classes on an element. Classes added _later_ in the
/// > list will override classes added earlier.
///
pub fn classes(names: List(#(String, Bool))) -> Attribute(msg) {
  class(do_classes(names, ""))
}

fn do_classes(names: List(#(String, Bool)), class: String) -> String {
  case names {
    [] -> class
    [#(name, True), ..rest] -> class <> name <> " " <> do_classes(rest, class)
    [#(_, False), ..rest] -> do_classes(rest, class)
  }
}

/// Indicates whether the element's content is editable by the user, allowing them
/// to modify the HTML content directly. The following values are accepted:
///
/// | Value        | Description                                           |
/// |--------------|-------------------------------------------------------|
/// | "true"       | The element is editable.                              |
/// | ""           |                                                       |
/// | "false"      | The element is not editable.                          |
/// | "plain-text" | The element is editable without rich text formatting. |
///
/// > **Note**: setting the value to an empty string does *not* disable this
/// > attribute, and is instead equivalent to setting it to `"true"`!
///
pub fn contenteditable(is_editable: String) -> Attribute(msg) {
  attribute("contenteditable", is_editable)
}

/// Add a `data-*` attribute to an HTML element. The key will be prefixed by `data-`.
///
pub fn data(key: String, value: String) -> Attribute(msg) {
  attribute("data-" <> key, value)
}

/// Specifies the text direction of the element's content. The following values
/// are accepted:
///
/// | Value  | Description                                                          |
/// |--------|----------------------------------------------------------------------|
/// | "ltr"  | The element's content is left-to-right.                              |
/// | "rtl"  | The element's content is right-to-left.                              |
/// | "auto" | The element's content direction is determined by the content itself. |
///
/// > **Note**: the `"auto"` value should only be used as a last resort in cases
/// > where the content's direction is truly unknown. The heuristic used by
/// > browsers is naive and only considers the first character available that
/// > indicates the direction.
///
pub fn dir(direction: String) -> Attribute(msg) {
  attribute("dir", direction)
}

/// Indicates whether the element can be dragged as part of the HTML drag-and-drop
/// API.
///
pub fn draggable(is_draggable: Bool) -> Attribute(msg) {
  attribute("draggable", case is_draggable {
    True -> "true"
    False -> "false"
  })
}

/// Specifies what action label (or potentially icon) to present for the "enter"
/// key on virtual keyboards such as mobile devices. The following values are
/// accepted:
///
/// | Value      | Example        |
/// |------------|----------------|
/// | "enter"    | "return", "↵"  |
/// | "done"     | "done", "✅"   |
/// | "go"       | "go"           |
/// | "next"     | "next"         |
/// | "previous" | "return"       |
/// | "search"   | "search", "🔍" |
/// | "send"     | "send"         |
///
/// The examples listed are demonstrative and may not be the actual labels used
/// by user agents. When unspecified or invalid, the user agent may use contextual
/// information such as the type of an input to determine the label.
///
pub fn enterkeyhint(value: String) -> Attribute(msg) {
  attribute("enterkeyhint", value)
}

/// Indicates whether the element is relevant to the page's current state. A
/// hidden element is not visible to the user and is inaccessible to assistive
/// technologies such as screen readers. This makes it unsuitable for simple
/// presentation purposes, but it can be useful for example to render something
/// that may be made visible later.
///
pub fn hidden(is_hidden: Bool) -> Attribute(msg) {
  boolean_attribute("hidden", is_hidden)
}

///
///
pub fn id(value: String) -> Attribute(msg) {
  attribute("id", value)
}

/// Marks the element as inert, meaning it is not currently interactive and does
/// not receive user input. For sighted users, it's common to style inert elements
/// in a way that makes them visually distinct from active elements, such as by
/// greying them out: this can help avoid confusion for users who may not otherwise
/// know the content they are looking at is inactive.
///
pub fn inert(is_inert: Bool) -> Attribute(msg) {
  boolean_attribute("inert", is_inert)
}

/// Hints to the user agent about what type of virtual keyboard to display when
/// the user interacts with the element. The following values are accepted:
///
/// | Value        | Description                                                   |
/// |--------------|---------------------------------------------------------------|
/// | "none"       | No virtual keyboard should be displayed.                      |
/// | "text"       | A standard text input keyboard.                               |
/// | "decimal"    | A numeric keyboard with locale-appropriate separator.         |
/// | "numeric"    | A numeric keyboard.                                           |
/// | "tel"        | A telephone keypad including "#" and "*".                     |
/// | "email"      | A keyboard for entering email addresses including "@" and "." |
/// | "url"        | A keyboard for entering URLs including "/" and ".".           |
/// | "search"     | A keyboard for entering search queries should be shown.       |
///
/// The `"none"` value should only be used in cases where you are rendering a
/// custom input method, otherwise the user will not be able to enter any text!
///
pub fn inputmode(value: String) -> Attribute(msg) {
  attribute("inputmode", value)
}

/// Specifies the [customised built-in element](https://html.spec.whatwg.org/#customized-built-in-element)
/// to be used in place of the native element this attribute is applied to.
///
pub fn is(value: String) -> Attribute(msg) {
  attribute("is", value)
}

/// Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
/// specify the global unique identifier of an item, for example books that are
/// identifiable by their ISBN.
///
pub fn itemid(id: String) -> Attribute(msg) {
  attribute("itemid", id)
}

/// Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
/// specify that the content of the element is to be treated as a value of the
/// given property name.
///
pub fn itemprop(name: String) -> Attribute(msg) {
  attribute("itemprop", name)
}

/// Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
/// indicate that the element and its descendants form a single item of key-value
/// data.
///
pub fn itemscope(has_scope: Bool) -> Attribute(msg) {
  boolean_attribute("itemscope", has_scope)
}

/// Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
/// specify the type of item being described. This is a URL that points to
/// a schema containing the vocabulary used for an item's key-value pairs, such
/// as a schema.org type.
///
pub fn itemtype(url: String) -> Attribute(msg) {
  attribute("itemtype", url)
}

/// Specifies the language of the element's content and the language of any of
/// this element's attributes that contain text. The `"lang"` attribute applies
/// to the element itself and all of its descendants, unless overridden by
/// another `"lang"` attribute on a descendant element.
///
/// The value must be a valid [BCP 47 language tag](https://tools.ietf.org/html/bcp47).
///
pub fn lang(language: String) -> Attribute(msg) {
  attribute("lang", language)
}

/// A cryptographic nonce used by CSP (Content Security Policy) to allow or
/// deny the fetch of a given resource.
///
pub fn nonce(value: String) -> Attribute(msg) {
  attribute("nonce", value)
}

/// Specifies that the element should be treated as a popover, rendering it in
/// the top-layer above all other content when the popover is active. The following
/// values are accepted:
///
/// | Value        | Description                                    |
/// |--------------|------------------------------------------------|
/// | "auto"       | Closes other popovers when opened.             |
/// | ""           |                                                |
/// | "manual"     | Does not close other popovers when opened.     |
/// | "hint"       | Closes only other "hint" popovers when opened. |
///
/// All modes except `"manual"` support "light dismiss" letting the user close
/// the popover by clicking outside of it, as well as respond to close requests
/// letting the user dismiss a popover by pressing the "escape" key or by using
/// the dismiss gesture on any assistive technology.
///
/// Popovers can be triggered either programmatically through the `showPopover()`
/// method, or by assigning an [`id`](#id) to the element and including the
/// [`popovertarget`](#popovertarget) attribute on the element that should trigger
/// the popover.
///
pub fn popover(value: String) -> Attribute(msg) {
  attribute("popover", value)
}

/// Indicates whether the element's content should be checked for spelling errors.
/// This typically only applies to inputs and textareas, or elements that are
/// [`contenteditable`](#contenteditable).
///
pub fn spellcheck(should_check: Bool) -> Attribute(msg) {
  attribute("spellcheck", case should_check {
    True -> "true"
    False -> "false"
  })
}

/// Provide a single property name and value to be used as inline styles for the
/// element. If either the property name or value is empty, this attribute will
/// be ignored.
///
/// > **Note**: unlike most attributes, multiple `style` attributes are merged
/// > with any existing other styles on an element. Styles added _later_ in the
/// > list will override styles added earlier.
///
pub fn style(property: String, value: String) -> Attribute(msg) {
  case property, value {
    "", _ | _, "" -> class("")
    _, _ -> attribute("style", property <> ":" <> value <> ";")
  }
}

/// Provide a list of property-value pairs to be used as inline styles for the
/// element. Empty properties or values are omitted from the final style string.
///
/// > **Note**: unlike most attributes, multiple `styles` attributes are merged
/// > with any existing other styles on an element. Styles added _later_ in the
/// > list will override styles added earlier.
///
pub fn styles(properties: List(#(String, String))) -> Attribute(msg) {
  attribute("style", do_styles(properties, ""))
}

fn do_styles(properties: List(#(String, String)), styles: String) -> String {
  case properties {
    [] -> styles
    [#("", _), ..rest] | [#(_, ""), ..rest] -> do_styles(rest, styles)
    [#(name, value), ..rest] ->
      do_styles(rest, styles <> name <> ":" <> value <> ";")
  }
}

/// Specifies the tabbing order of the element. If an element is not typically
/// focusable, such as a `<div>`, it will be made focusable when this attribute
/// is set.
///
/// Any integer value is accepted, but the following values are recommended:
///
/// - `-1`: indicates the element may receive focus, but should not be sequentially
///   focusable. The user agent may choose to ignore this preference if, for
///   example, the user agent is a screen reader.
///
/// - `0`: indicates the element may receive focus and should be placed in the
///   sequential focus order in the order it appears in the DOM.
///
/// - any positive integer: indicates the element should be placed in the sequential
///   focus order relative to other elements with a positive tabindex.
///
/// Values other than `0` and `-1` are generally not recommended as managing
/// the relative order of focusable elements can be difficult and error-prone.
///
pub fn tabindex(index: Int) -> Attribute(msg) {
  attribute("tabindex", int.to_string(index))
}

/// Annotate an element with additional information that may be suitable as a
/// tooltip, such as a description of a link or image.
///
/// It is **not** recommended to use the `title` attribute as a way of providing
/// accessibility information to assistive technologies. User agents often do not
/// expose the `title` attribute to keyboard-only users or touch devices, for
/// example.
///
pub fn title(text: String) -> Attribute(msg) {
  attribute("title", text)
}

/// Controls whether an element's content should be translated.
///
pub fn translate(should_translate: Bool) -> Attribute(msg) {
  attribute("translate", case should_translate {
    True -> "yes"
    False -> "no"
  })
}

/// Indicates if writing suggestions should be enabled for this element.
///
pub fn writingsuggestions(enabled: Bool) -> Attribute(msg) {
  attribute("writingsuggestions", case enabled {
    True -> "true"
    False -> "false"
  })
}

// ARIA ------------------------------------------------------------------------

/// Add an `aria-*` attribute to an HTML element. The key will be prefixed by
/// `aria-`.
///
pub fn aria(name: String, value: String) -> Attribute(msg) {
  attribute("aria-" <> name, value)
}

///
///
pub fn role(name: String) -> Attribute(msg) {
  attribute("role", name)
}

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
