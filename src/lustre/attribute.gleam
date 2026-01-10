// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/json.{type Json}
import gleam/string
import lustre/vdom/vattr

// TYPES -----------------------------------------------------------------------

/// The `Attribute` type encompasses HTML attributes, DOM properties, and
/// event listeners.
///
/// - [`attribute`](#attribute) constructs string-based HTML attributes. These
///   exist on the HTML element directly and can be server-rendered.
///
/// - [`property`](#property) constructs JavaScript DOM properties. These exist
///   on the DOM element and can be arbitrary serialisable values such as arrays
///   and objects. These are not server-rendered.
///
/// - [`event.on`](./event.html#on) can be used to construct event listeners.
///
pub type Attribute(msg) =
  vattr.Attribute(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an HTML attribute. This is like saying `element.setAttribute("class", "wibble")`
/// in JavaScript. Attributes will be rendered when calling [`element.to_string`](./element.html#to_string).
///
/// > **Note**: there is a subtle difference between attributes and properties. You
/// > can read more about the implications of this
/// > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
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
/// > **Note**: there is a subtle difference between attributes and properties. You
/// > can read more about the implications of this
/// > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn property(name: String, value: Json) -> Attribute(msg) {
  vattr.property(name, value)
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
/// be focused when the page first loads.
///
/// > **Note**: Lustre's runtime augments that native behaviour of this attribute.
/// > Whenever it is toggled true, the element will be automatically focused even
/// > if it already exists in the DOM.
///
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg) {
  boolean_attribute("autofocus", should_autofocus)
}

/// A class is a non-unique identifier for an element primarily used for styling
/// purposes. You can provide multiple classes as a space-separated list and any
/// style rules that apply to any of the classes will be applied to the element.
///
/// To conditionally toggle classes on and off, you can use the [`classes`](#classes)
/// function instead.
///
/// > **Note**: unlike most attributes, multiple `class` attributes are merged
/// > with any existing other classes on an element. Classes added _later_ in the
/// > list will override classes added earlier.
///
pub fn class(name: String) -> Attribute(msg) {
  attribute("class", name)
}

/// A class is a non-unique identifier for an element primarily used for styling
/// purposes. You can provide multiple classes as a space-separated list and any
/// style rules that apply to any of the classes will be applied to the element.
/// This function allows you to conditionally toggle classes on and off.
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

/// Add a `data-*` attribute to an HTML element. The key will be prefixed by
/// `"data-"`, and accessible from JavaScript or in Gleam decoders under the
/// path `element.dataset.key` where `key` is the key you provide to this
/// function.
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

/// The `"id"` attribute is used to uniquely identify a single element within a
/// document. It can be used to reference the element in CSS with the selector
/// `#id`, in JavaScript with `document.getElementById("id")`, or by anchors on
/// the same page with the URL `"#id"`.
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

/// Controls whether an element's content may be translated by the user agent
/// when the page is localised. This includes both the element's text content
/// and some of its attributes:
///
/// | Attribute   | Element(s)                                 |
/// |-------------|--------------------------------------------|
/// | abbr        | th                                         |
/// | alt         | area, img, input                           |
/// | content     | meta                                       |
/// | download    | a, area                                    |
/// | label       | optgroup, option, track                    |
/// | lang        | *                                          |
/// | placeholder | input, textarea                            |
/// | srcdoc      | iframe                                     |
/// | title       | *                                          |
/// | style       | *                                          |
/// | value       | input (with type="button" or type="reset") |
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

// DETAILS ---------------------------------------------------------------------

/// Indicates whether the details element is open or closed.
///
pub fn open(is_open: Bool) -> Attribute(msg) {
  boolean_attribute("open", is_open)
}

// ANCHOR AND LINK ATTRIBUTES --------------------------------------------------

/// Specifies the URL of a linked resource. This attribute can be used on various
/// elements to create hyperlinks or to load resources.
///
pub fn href(url: String) -> Attribute(msg) {
  attribute("href", url)
}

/// Specifies where to display the linked resource or where to open the link.
/// The following values are accepted:
///
/// | Value     | Description                                             |
/// |-----------|---------------------------------------------------------|
/// | "_self"   | Open in the same frame/window (default)                 |
/// | "_blank"  | Open in a new window or tab                             |
/// | "_parent" | Open in the parent frame                                |
/// | "_top"    | Open in the full body of the window                     |
/// | framename | Open in a named frame                                   |
///
/// > **Note**: consider against using `"_blank"` for links to external sites as it
/// > removes user control over their browsing experience.
///
pub fn target(value: String) -> Attribute(msg) {
  attribute("target", value)
}

/// Indicates that the linked resource should be downloaded rather than displayed.
/// When provided with a value, it suggests a filename for the downloaded file.
///
pub fn download(filename: String) -> Attribute(msg) {
  attribute("download", filename)
}

/// Provides a space-separated list of URLs that will be notified if the user
/// follows the hyperlink. These URLs will receive POST requests with bodies
/// of type `ping/1.0`.
///
pub fn ping(urls: List(String)) -> Attribute(msg) {
  attribute("ping", string.join(urls, " "))
}

/// Specifies the relationship between the current document and the linked resource.
/// Multiple relationship values can be provided as a space-separated list.
///
pub fn rel(value: String) -> Attribute(msg) {
  attribute("rel", value)
}

/// Specifies the language of the linked resource. The value must be a valid
/// [BCP 47 language tag](https://tools.ietf.org/html/bcp47).
///
pub fn hreflang(language: String) -> Attribute(msg) {
  attribute("hreflang", language)
}

/// Specifies the referrer policy for fetches initiated by the element. The
/// following values are accepted:
///
/// | Value                              | Description                                           |
/// |-----------------------------------|--------------------------------------------------------|
/// | "no-referrer"                     | No Referer header is sent                              |
/// | "no-referrer-when-downgrade"      | Only send Referer for same-origin or more secure       |
/// | "origin"                          | Send only the origin part of the URL                   |
/// | "origin-when-cross-origin"        | Full URL for same-origin, origin only for cross-origin |
/// | "same-origin"                     | Only send Referer for same-origin requests             |
/// | "strict-origin"                   | Like origin, but only to equally secure destinations   |
/// | "strict-origin-when-cross-origin" | Default policy with varying levels of restriction      |
/// | "unsafe-url"                      | Always send the full URL                               |
///
pub fn referrerpolicy(value: String) -> Attribute(msg) {
  attribute("referrerpolicy", value)
}

/// Specifies the type of the resource being linked to, which is necessary for
/// request matching, application of correct content security policy, and setting
/// of correct Accept request header.
///
/// > **Note**: this attribute is required when rel="preload" has been set on the
/// > `<link>` element, optional when `rel="modulepreload"` has been set, and
/// > otherwise should not be used.
///
/// | Value      | Applies to                       |
/// |------------|----------------------------------|
/// | "audio"    | `<audio>`                        |
/// | "document" | `<iframe>`, `<frame>`            |
/// | "embed"    | `<embed>`                        |
/// | "fetch"    | fetch, XHR                       |
/// | "font"     | CSS @font-face                   |
/// | "image"    | `<img>`, `<image>`, `<picture>`  |
/// | "object"   | `<object>`                       |
/// | "script"   | `<script>`, Worker importScripts |
/// | "style"    | `<link rel="stylesheet">`        |
/// | "video"    | `<video>`                        |
/// | "worker"   | Worker, SharedWorker             |
///
pub fn as_(value: String) -> Attribute(msg) {
  attribute("as", value)
}

/// This attribute explicitly indicates that certain operations should be blocked
/// until specific conditions are met. It must only be used when the rel attribute
/// contains the expect or stylesheet keywords. With `rel="expect"`, it indicates
/// that operations should be blocked until a specific DOM node has been parsed.
/// With `rel="stylesheet"`, it indicates that operations should be blocked until
/// an external stylesheet and its critical subresources have been fetched and
/// applied to the document.
///
pub fn blocking(value: Bool) -> Attribute(msg) {
  attribute("blocking", case value {
    True -> "render"
    False -> ""
  })
}

/// Provides a base64-encoded hash of the resource being linked to. This is used
/// by the browser to verify that a fetched resource has not been tampered with.
///
/// > **Note**: this attribute is only meaningful on `<link>` elements with either
/// > `rel="stylesheet"`, `rel="preload"`, or `rel="modulepreload"`. It may also
/// > be used on `<script>` elements.
///
pub fn integrity(hash: String) -> Attribute(msg) {
  attribute("integrity", hash)
}

// IMAGE ATTRIBUTES ------------------------------------------------------------

/// Specifies text that should be displayed when the image cannot be rendered.
/// This attribute is essential for accessibility, providing context about the
/// image to users who cannot see it, including those using screen readers.
///
pub fn alt(text: String) -> Attribute(msg) {
  attribute("alt", text)
}

/// Specifies the URL of an image or resource to be used.
///
pub fn src(url: String) -> Attribute(msg) {
  attribute("src", url)
}

/// Specifies a set of image sources for different display scenarios. This allows
/// browsers to choose the most appropriate image based on factors like screen
/// resolution and viewport size.
///
pub fn srcset(sources: String) -> Attribute(msg) {
  attribute("srcset", sources)
}

/// Used with `srcset` to define the size of images in different layout scenarios.
/// Helps the browser select the most appropriate image source.
///
pub fn sizes(value: String) -> Attribute(msg) {
  attribute("sizes", value)
}

/// Configures the CORS (Cross-Origin Resource Sharing) settings for the element.
/// Valid values are "anonymous" and "use-credentials".
///
pub fn crossorigin(value: String) -> Attribute(msg) {
  attribute("crossorigin", value)
}

/// Specifies the name of an image map to be used with the image.
///
pub fn usemap(value: String) -> Attribute(msg) {
  attribute("usemap", value)
}

/// Indicates that the image is a server-side image map. When a user clicks on the
/// image, the coordinates of the click are sent to the server.
///
pub fn ismap(is_map: Bool) -> Attribute(msg) {
  boolean_attribute("ismap", is_map)
}

/// Specifies the width of the element in pixels.
///
pub fn width(value: Int) -> Attribute(msg) {
  attribute("width", int.to_string(value))
}

/// Specifies the height of the element in pixels.
///
pub fn height(value: Int) -> Attribute(msg) {
  attribute("height", int.to_string(value))
}

/// Provides a hint about how the image should be decoded. Valid values are
/// "sync", "async", and "auto".
///
pub fn decoding(value: String) -> Attribute(msg) {
  attribute("decoding", value)
}

/// Indicates how the browser should load the image. Valid values are "eager"
/// (load immediately) and "lazy" (defer loading until needed).
///
pub fn loading(value: String) -> Attribute(msg) {
  attribute("loading", value)
}

/// Sets the priority for fetches initiated by the element. Valid values are
/// "high", "low", and "auto".
///
pub fn fetchpriority(value: String) -> Attribute(msg) {
  attribute("fetchpriority", value)
}

// FORM ATTRIBUTES -------------------------------------------------------------

/// Specifies the character encodings to be used for form submission. This allows
/// servers to know how to interpret the form data. Multiple encodings can be
/// specified as a space-separated list.
///
pub fn accept_charset(charsets: String) -> Attribute(msg) {
  attribute("accept-charset", charsets)
}

/// Specifies the URL to which the form's data should be sent when submitted.
/// This can be overridden by formaction attributes on submit buttons.
///
pub fn action(url: String) -> Attribute(msg) {
  attribute("action", url)
}

/// Specifies how form data should be encoded before sending it to the server.
/// Valid values include:
///
/// | Value                               | Description                           |
/// |-------------------------------------|---------------------------------------|
/// | "application/x-www-form-urlencoded" | Default encoding (spaces as +, etc.)  |
/// | "multipart/form-data"               | Required for file uploads             |
/// | "text/plain"                        | Simple encoding with minimal escaping  |
///
pub fn enctype(encoding_type: String) -> Attribute(msg) {
  attribute("enctype", encoding_type)
}

/// Specifies the HTTP method to use when submitting the form. Common values are:
///
/// | Value    | Description                                              |
/// |----------|----------------------------------------------------------|
/// | "get"    | Appends form data to URL (default)                       |
/// | "post"   | Sends form data in the body of the HTTP request          |
/// | "dialog" | Closes a dialog if the form is inside one                |
///
pub fn method(http_method: String) -> Attribute(msg) {
  attribute("method", http_method)
}

/// When present, indicates that the form should not be validated when submitted.
/// This allows submission of forms with invalid or incomplete data.
///
pub fn novalidate(disable_validation: Bool) -> Attribute(msg) {
  boolean_attribute("novalidate", disable_validation)
}

// INPUT ATTRIBUTES ------------------------------------------------------------

/// A hint for the user agent about what file types are expected to be submitted.
/// The following values are accepted:
///
/// | Value     | Description                                          |
/// |-----------|------------------------------------------------------|
/// | "audio/*" | Any audio file type.                                 |
/// | "video/*" | Any video file type.                                 |
/// | "image/*" | Any image file type.                                 |
/// | mime/type | A complete MIME type, without additional parameters. |
/// | .ext      | Indicates any file with the given extension.         |
///
/// The following input types support the `"accept"` attribute:
///
/// - `"file"`
///
/// > **Note**: the `"accept"` attribute is a *hint* to the user agent and does
/// > not guarantee that the user will only be able to select files of the specified
/// > type.
///
pub fn accept(values: List(String)) -> Attribute(msg) {
  attribute("accept", string.join(values, ","))
}

/// Allow a colour's alpha component to be manipulated, allowing the user to
/// select a colour with transparency.
///
/// The following input types support the `"alpha"` attribute:
///
/// - `"color"`
///
pub fn alpha(allowed: Bool) -> Attribute(msg) {
  boolean_attribute("alpha", allowed)
}

/// A hint for the user agent to automatically fill the value of the input with
/// an appropriate value. The format for the `"autocomplete"` attribute is a
/// space-separated ordered list of optional tokens:
///
///     "section-* (shipping | billing) [...fields] webauthn"
///
/// - `section-*`: used to disambiguate between two fields with otherwise identical
///   autocomplete values. The `*` is replaced with a string that identifies the
///   section of the form.
///
/// - `shipping | billing`: indicates the field is related to shipping or billing
///   address or contact information.
///
/// - `[...fields]`: a space-separated list of field names that are relevant to
///   the input, for example `"email"`, `"name family-name"`, or `"home tel"`.
///
/// - `webauthn`: indicates the field can be automatically filled with a WebAuthn
///   passkey.
///
/// In addition, the value may instead be `"off"` to disable autocomplete for the
/// input, or `"on"` to let the user agent decide based on context what values
/// are appropriate.
///
/// The following input types support the `"autocomplete"` attribute:
///
/// - `"color"`
/// - `"date"`
/// - `"datetime-local"`
/// - `"email"`
/// - `"hidden"`
/// - `"month"`
/// - `"number"`
/// - `"password"`
/// - `"range"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"time"`
/// - `"url"`
/// - `"week"`
///
pub fn autocomplete(value: String) -> Attribute(msg) {
  attribute("autocomplete", value)
}

/// Whether the control is checked or not. When participating in a form, the
/// value of the input is included in the form submission if it is checked. For
/// checkboxes that do not have a value, the value of the input is `"on"` when
/// checked.
///
/// The following input types support the `"checked"` attribute:
///
/// - `"checkbox"`
/// - `"radio"`
///
pub fn checked(is_checked: Bool) -> Attribute(msg) {
  boolean_attribute("checked", is_checked)
}

/// Set the default checked state of a form control. This element will appear
/// checked to users when the input is first rendered and its value will included in the form
/// submission if the user does not change it.
///
/// Just setting a default value and letting the DOM manage the state of an input
/// is known as using [_uncontrolled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
/// Doing this means your application cannot set the value of an input after it
/// is modified without using an effect.
///
pub fn default_checked(is_checked: Bool) -> Attribute(msg) {
  boolean_attribute("virtual:defaultChecked", is_checked)
}

/// The color space of the serialised CSS color. It also hints to user agents
/// about what kind of interface to present to the user for selecting a color.
/// The following values are accepted:
///
/// - `"limited-srgb"`: The CSS color is converted to the 'srgb' color space and
///   limited to 8-bits per component, e.g., `"#123456"` or
///   `"color(srgb 0 1 0 / 0.5)"`.
///
/// - `"display-p3"`: The CSS color is converted to the 'display-p3' color space,
///   e.g., `"color(display-p3 1.84 -0.19 0.72 / 0.6)"`.
///
/// The following input types support the `"colorspace"` attribute:
///
/// - `"color"`
///
pub fn colorspace(value: String) -> Attribute(msg) {
  attribute("colorspace", value)
}

/// A positive integer value indicating how many visible columns the text control
/// will have. The default value is 20.
pub fn cols(value: Int) -> Attribute(msg) {
  attribute("cols", int.to_string(value))
}

/// The name of the field included in a form that indicates the direcionality of
/// the user's input.
///
/// The following input types support the `"dirname"` attribute:
///
/// - `"email"`
/// - `"hidden"`
/// - `"password"`
/// - `"search"`
/// - `"submit"
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn dirname(direction: String) -> Attribute(msg) {
  attribute("dirname", direction)
}

/// Controls whether or not the input is disabled. Disabled inputs are not
/// validated during form submission and are not interactive.
///
pub fn disabled(is_disabled: Bool) -> Attribute(msg) {
  boolean_attribute("disabled", is_disabled)
}

///
///
pub fn for(id: String) -> Attribute(msg) {
  attribute("for", id)
}

/// Associates the input with a form element located elsewhere in the document.
///
pub fn form(id: String) -> Attribute(msg) {
  attribute("form", id)
}

/// The URL to use for form submission. This URL will override the [`"action"`](#action)
/// attribute on the form element itself, if present.
///
/// The following input types support the `"formaction"` attribute:
///
/// - `"image"`
/// - `"submit"`
///
pub fn formaction(url: String) -> Attribute(msg) {
  attribute("formaction", url)
}

/// Entry list encoding type to use for form submission
///
/// - `"image"`
/// - `"submit"`
///
pub fn formenctype(encoding_type: String) -> Attribute(msg) {
  attribute("formenctype", encoding_type)
}

/// Variant to use for form submission
///
/// - `"image"`
/// - `"submit"`
///
pub fn formmethod(method: String) -> Attribute(msg) {
  attribute("formmethod", method)
}

/// Bypass form control validation for form submission
///
/// - `"image"`
/// - `"submit"`
///
pub fn formnovalidate(no_validate: Bool) -> Attribute(msg) {
  boolean_attribute("formnovalidate", no_validate)
}

/// Navigable for form submission
///
/// - `"image"`
/// - `"submit"`
///
pub fn formtarget(target: String) -> Attribute(msg) {
  attribute("formtarget", target)
}

/// List of autocomplete options
///
/// The following input types support the `"list"` attribute:
///
/// - `"color"`
/// - `"date"`
/// - `"datetime-local"`
/// - `"email"`
/// - `"month"`
/// - `"number"`
/// - `"range"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"time"`
/// - `"url"`
/// - `"week"`
///
pub fn list(id: String) -> Attribute(msg) {
  attribute("list", id)
}

/// Constrain the maximum value of a form control. The exact syntax of this value
/// changes depending on the type of input, for example `"1"`, `"1979-12-31"`, and
/// `"06:00"` are all potentially valid values for the `"max"` attribute.
///
/// The following input types support the `"max"` attribute:
///
/// - `"date"`
/// - `"datetime-local"`
/// - `"month"`
/// - `"number"`
/// - `"range"`
/// - `"time"`
/// - `"week"`
///
pub fn max(value: String) -> Attribute(msg) {
  attribute("max", value)
}

/// Maximum length of value
///
/// The following input types support the `"maxlength"` attribute:
///
/// - `"email"`
/// - `"password"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn maxlength(length: Int) -> Attribute(msg) {
  attribute("maxlength", int.to_string(length))
}

/// Minimum value
///
/// The following input types support the `"max"` attribute:
///
/// - `"date"`
/// - `"datetime-local"`
/// - `"month"`
/// - `"number"`
/// - `"range"`
/// - `"time"`
/// - `"week"`
///
pub fn min(value: String) -> Attribute(msg) {
  attribute("min", value)
}

/// Minimum length of value
///
/// - `"email"`
/// - `"password"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn minlength(length: Int) -> Attribute(msg) {
  attribute("minlength", int.to_string(length))
}

/// Whether an input or select may allow multiple values to be selected at once.
///
/// The following input types support the `"multiple"` attribute:
///
/// - `"email"`
/// - `"file"`
///
pub fn multiple(allow_multiple: Bool) -> Attribute(msg) {
  boolean_attribute("multiple", allow_multiple)
}

/// Name of the element to use for form submission and in the form.elements API
///
pub fn name(element_name: String) -> Attribute(msg) {
  attribute("name", element_name)
}

/// Pattern to be matched by the form control's value
///
/// - `"email"`
/// - `"password"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn pattern(regex: String) -> Attribute(msg) {
  attribute("pattern", regex)
}

/// User-visible label to be placed within the form control
///
/// - `"email"`
/// - `"number"`
/// - `"password"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn placeholder(text: String) -> Attribute(msg) {
  attribute("placeholder", text)
}

/// Targets a popover element to toggle, show, or hide
///
/// The following input types support the `"popovertarget"` attribute:
///
/// - `"button"`
/// - `"image"`
/// - `"reset"`
/// - `"submit"`
///
pub fn popovertarget(id: String) -> Attribute(msg) {
  attribute("popovertarget", id)
}

/// Indicates whether a targeted popover element is to be toggled, shown, or hidden
///
/// The following input types support the `"popovertarget"` attribute:
///
/// - `"button"`
/// - `"image"`
/// - `"reset"`
/// - `"submit"`
///
pub fn popovertargetaction(action: String) -> Attribute(msg) {
  attribute("popovertargetaction", action)
}

/// Whether to allow the value to be edited by the user
///
/// - `"date"`
/// - `"datetime-local"`
/// - `"email"`
/// - `"month"`
/// - `"number"`
/// - `"password"`
/// - `"range"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"time"`
/// - `"url"`
/// - `"week"`
///
pub fn readonly(is_readonly: Bool) -> Attribute(msg) {
  boolean_attribute("readonly", is_readonly)
}

/// Whether the control is required for form submission
///
/// - `"checkbox"`
/// - `"date"`
/// - `"datetime-local"`
/// - `"email"`
/// - `"month"`
/// - `"number"`
/// - `"password"`
/// - `"radio"`
/// - `"range"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"time"`
/// - `"url"`
/// - `"week"`
///
pub fn required(is_required: Bool) -> Attribute(msg) {
  boolean_attribute("required", is_required)
}

/// A positive integer value indicating how many visible rows the text control
/// will have. The browsers default value is 2.
pub fn rows(value: Int) -> Attribute(msg) {
  attribute("rows", int.to_string(value))
}

/// Controls whether or not a select's `<option>` is selected or not. Only one
/// option can be selected at a time, unless the [`"multiple"`](#multiple)
/// attribute is set on the select element.
///
pub fn selected(is_selected: Bool) -> Attribute(msg) {
  boolean_attribute("selected", is_selected)
}

/// An `<option>` with this attribute toggled on will be selected when
/// its corresponding select is rendered for the first time. Only one
/// option can be selected at a time, unless the [`"multiple"`](#multiple)
/// attribute is set on the select element.
///
/// Just setting a default value and letting the DOM manage the state of an input
/// is known as using [_uncontrolled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
/// Doing this means your application cannot set the value of an input after it
/// is modified without using an effect.
///
pub fn default_selected(is_selected: Bool) -> Attribute(msg) {
  boolean_attribute("virtual:defaultSelected", is_selected)
}

/// Size of the control
///
/// The following input types support the `size` attribute:
///
/// - `"email"`
/// - `"password"`
/// - `"search"`
/// - `"tel"`
/// - `"text"`
/// - `"url"`
///
pub fn size(value: String) -> Attribute(msg) {
  attribute("size", value)
}

/// Granularity to be matched by the form control's value
///
/// The following input types support the `"step"` attribute:
///
/// - `"date"`
/// - `"datetime-local"`
/// - `"month"`
/// - `"number"`
/// - `"range"`
/// - `"time"`
/// - `"week"`
///
pub fn step(value: String) -> Attribute(msg) {
  attribute("step", value)
}

/// Type of form control
///
pub fn type_(control_type: String) -> Attribute(msg) {
  attribute("type", control_type)
}

/// Specifies the value of an input or form control. Using this attribute will
/// make sure the value is always in sync with your application's modelled, a
/// practice known as [_controlled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
///
/// If you'd like to let the DOM manage the value of an input but still set a
/// default value for users to see, use the [`default_value`](#default_value)
/// attribute instead.
///
pub fn value(control_value: String) -> Attribute(msg) {
  attribute("value", control_value)
}

/// Set the default value of an input or form control. This is the value that will
/// be shown to users when the input is first rendered and included in the form
/// submission if the user does not change it.
///
/// Just setting a default value and letting the DOM manage the state of an input
/// is known as using [_uncontrolled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
/// Doing this means your application cannot set the value of an input after it
/// is modified without using an effect.
///
pub fn default_value(control_value: String) -> Attribute(msg) {
  attribute("virtual:defaultValue", control_value)
}

// META ATTRIBUTES -------------------------------------------------------------

/// Sets a pragma directive for a document. This is used in meta tags to define
/// behaviors the user agent should follow.
///
pub fn http_equiv(value: String) -> Attribute(msg) {
  attribute("http-equiv", value)
}

/// Specifies the value of the meta element, which varies depending on the value
/// of the name or http-equiv attribute.
///
pub fn content(value: String) -> Attribute(msg) {
  attribute("content", value)
}

/// Declares the character encoding used in the document. When used with a meta
/// element, this replaces the need for the `http_equiv("content-type")` attribute.
///
pub fn charset(value: String) -> Attribute(msg) {
  attribute("charset", value)
}

/// Specifies the media types the resource applies to. This is commonly used with
/// link elements for stylesheets to determine when they should be loaded.
///
pub fn media(query: String) -> Attribute(msg) {
  attribute("media", query)
}

// AUDIO AND VIDEO ATTRIBUTES --------------------------------------------------

/// Indicates that the media resource should automatically begin playing as soon
/// as it can do so without stopping. When not present, the media will not
/// automatically play until the user initiates playback.
///
/// > **Note**: Lustre's runtime augments this attribute. Whenever it is toggled
/// > to true, the media will begin playing as if the element's `play()` method
/// > was called.
///
pub fn autoplay(auto_play: Bool) -> Attribute(msg) {
  boolean_attribute("autoplay", auto_play)
}

/// When present, this attribute shows the browser's built-in control panel for the
/// media player, giving users control over playback, volume, seeking, and more.
///
pub fn controls(show_controls: Bool) -> Attribute(msg) {
  boolean_attribute("controls", show_controls)
}

/// When present, this attribute indicates that the media should start over again
/// from the beginning when it reaches the end.
///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  boolean_attribute("loop", should_loop)
}

/// When present, this attribute indicates that the audio output of the media element
/// should be initially silenced.
///
pub fn muted(is_muted: Bool) -> Attribute(msg) {
  boolean_attribute("muted", is_muted)
}

/// Encourages the user agent to display video content within the element's
/// playback area rather than in a separate window or fullscreen, especially on
/// mobile devices.
///
/// This attribute only acts as a *hint* to the user agent, and setting this to
/// false does not imply that the video will be played in fullscreen.
///
pub fn playsinline(play_inline: Bool) -> Attribute(msg) {
  boolean_attribute("playsinline", play_inline)
}

/// Specifies an image to be shown while the video is downloading, or until the
/// user hits the play button.
///
pub fn poster(url: String) -> Attribute(msg) {
  attribute("poster", url)
}

/// Provides a hint to the browser about what the author thinks will lead to the
/// best user experience. The following values are accepted:
///
/// | Value      | Description                                                      |
/// |------------|------------------------------------------------------------------|
/// | "auto"     | Let's the user agent determine the best option                   |
/// | "metadata" | Hints to the user agent that it can fetch the metadata only.     |
/// | "none"     | Hints to the user agent that server traffic should be minimised. |
///
pub fn preload(value: String) -> Attribute(msg) {
  attribute("preload", value)
}

// TEMPLATE ATTRIBUTES ---------------------------------------------------------

/// Specifies the mode for creating a shadow root on a template. Valid values
/// include:
///
/// | Value     | Description                                 |
/// |-----------|---------------------------------------------|
/// | "open"    | Shadow root's contents are accessible       |
/// | "closed"  | Shadow root's contents are not accessible   |
///
/// > **Note**: if you are pre-rendering a Lustre component you must make sure this
/// > attribute matches the [`open_shadow_root`](./component.html#open_shadow_root)
/// > configuration - or `"closed"` if not explicitly set - to ensure the shadow
/// > root is created correctly.
///
pub fn shadowrootmode(mode: String) -> Attribute(msg) {
  attribute("shadowrootmode", mode)
}

/// Indicates whether focus should be delegated to the shadow root when an element
/// in the shadow tree gains focus.
///
pub fn shadowrootdelegatesfocus(delegates: Bool) -> Attribute(msg) {
  boolean_attribute("shadowrootdelegatesfocus", delegates)
}

/// Determines whether the shadow root can be cloned when the host element is
/// cloned.
///
pub fn shadowrootclonable(clonable: Bool) -> Attribute(msg) {
  boolean_attribute("shadowrootclonable", clonable)
}

/// Controls whether the shadow root should be preserved during serialization
/// operations like copying to the clipboard or saving a page.
///
pub fn shadowrootserializable(serializable: Bool) -> Attribute(msg) {
  boolean_attribute("shadowrootserializable", serializable)
}

// TABLE ATTRIBUTES ------------------------------------------------------------

/// A short, abbreviated description of the header cell's content provided as an
/// alternative label to use for the header cell when referencing the cell in other
/// contexts. Some user-agents, such as speech readers, may present this description
/// before the content itself.
///
pub fn abbr(value: String) -> Attribute(msg) {
  attribute("abbr", value)
}

/// A non-negative integer value indicating how many columns the header cell spans
/// or extends. The default value is `1`. User agents dismiss values higher than
/// `1000` as incorrect, defaulting such values to `1`.
///
pub fn colspan(value: Int) -> Attribute(msg) {
  attribute("colspan", int.to_string(value))
}

/// A list of space-separated strings corresponding to the id attributes of the
/// `<th>` elements that provide the headers for this header cell.
///
pub fn headers(ids: List(String)) -> Attribute(msg) {
  attribute("headers", string.join(ids, " "))
}

/// A non-negative integer value indicating how many rows the header cell spans
/// or extends. The default value is `1`; if its value is set to `0`, the header
/// cell will extends to the end of the table grouping section, that the `<th>`
/// belongs to. Values higher than `65534` are clipped at `65534`.
///
pub fn rowspan(value: Int) -> Attribute(msg) {
  attribute("rowspan", {
    value
    |> int.max(0)
    |> int.min(65_534)
    |> int.to_string()
  })
}

/// Specifies the number of consecutive columns a `<colgroup>` element spans. The
/// value must be a positive integer greater than zero.
///
pub fn span(value: Int) -> Attribute(msg) {
  attribute("span", int.to_string(value))
}

/// The `scope` attribute specifies whether a header cell is a header for a row,
/// column, or group of rows or columns. The following values are accepted:
///
/// The `scope` attribute is only valid on `<th>` elements.
///
pub fn scope(value: String) -> Attribute(msg) {
  attribute("scope", value)
}

// TIME ATTRIBUTES -------------------------------------------------------------

/// Indicates the time and/or date of a `<time>` element. Values may be one of
/// the following formats:
///
/// | Description                       | Syntax                                                                                                                                     | Examples                                                                                                                                   |
/// |-----------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
/// | Valid month string                | `YYYY-MM`                                                                                                                                  | `2011-11`, `2013-05`                                                                                                                       |
/// | Valid date string                 | `YYYY-MM-DD`                                                                                                                               | `1887-12-01`                                                                                                                               |
/// | Valid local date and time string  | `YYYY-MM-DD HH:MM`, `YYYY-MM-DD HH:MM:SS`, `YYYY-MM-DD HH:MM:SS.mmm`, `YYYY-MM-DDTHH:MM`, `YYYY-MM-DDTHH:MM:SS`, `YYYY-MM-DDTHH:MM:SS.mmm` | `2013-12-25 11:12`, `1972-07-25 13:43:07`, `1941-03-15 07:06:23.678`, `2013-12-25T11:12`, `1972-07-25T13:43:07`, `1941-03-15T07:06:23.678` |
/// | Valid global date and time string | A valid local date and time string followed by a valid time-zone offset string                                                             | `2013-12-25 11:12+0200`, `1972-07-25 13:43:07+04:30`, `1941-03-15 07:06:23.678Z`, `2013-12-25T11:12-08:00`                                 |
/// | Valid week string                 | `YYYY-WWW`                                                                                                                                 | `2013-W46`                                                                                                                                 |
///
/// A comprehensive list of valid formats can be found on [MDN](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time#valid_datetime_values).
///
pub fn datetime(value: String) -> Attribute(msg) {
  attribute("datetime", value)
}

// ARIA ATTRIBUTES -------------------------------------------------------------

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
