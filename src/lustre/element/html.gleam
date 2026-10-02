// IMPORTS ---------------------------------------------------------------------

import gleam/json
import lustre/attribute.{type Attribute}
import lustre/element.{type Element, element, namespaced}
import lustre/internals/constants

// HTML ELEMENTS: MAIN ROOT ----------------------------------------------------

///
pub fn html(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("html", attrs, children)
}

pub fn text(content: String) -> Element(message) {
  element.text(content)
}

// HTML ELEMENTS: DOCUMENT METADATA --------------------------------------------

///
pub fn base(attrs: List(Attribute(message))) -> Element(message) {
  element("base", attrs, constants.empty_list)
}

///
pub fn head(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("head", attrs, children)
}

///
pub fn link(attrs: List(Attribute(message))) -> Element(message) {
  element("link", attrs, constants.empty_list)
}

///
pub fn meta(attrs: List(Attribute(message))) -> Element(message) {
  element("meta", attrs, constants.empty_list)
}

///
pub fn style(attrs: List(Attribute(message)), css: String) -> Element(message) {
  element.unsafe_raw_html("", "style", attrs, css)
}

///
pub fn title(
  attrs: List(Attribute(message)),
  content: String,
) -> Element(message) {
  element("title", attrs, [text(content)])
}

// HTML ELEMENTS: SECTIONING ROOT -----------------------------------------------

///
pub fn body(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("body", attrs, children)
}

// HTML ELEMENTS: CONTENT SECTIONING -------------------------------------------

///
pub fn address(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("address", attrs, children)
}

///
pub fn article(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("article", attrs, children)
}

///
pub fn aside(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("aside", attrs, children)
}

///
pub fn footer(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("footer", attrs, children)
}

///
pub fn header(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("header", attrs, children)
}

///
pub fn h1(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h1", attrs, children)
}

///
pub fn h2(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h2", attrs, children)
}

///
pub fn h3(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h3", attrs, children)
}

///
pub fn h4(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h4", attrs, children)
}

///
pub fn h5(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h5", attrs, children)
}

///
pub fn h6(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h6", attrs, children)
}

///
pub fn hgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("hgroup", attrs, children)
}

///
pub fn main(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("main", attrs, children)
}

///
pub fn nav(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("nav", attrs, children)
}

///
pub fn section(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("section", attrs, children)
}

///
pub fn search(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("search", attrs, children)
}

// HTML ELEMENTS: TEXT CONTENT -------------------------------------------------

///
pub fn blockquote(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("blockquote", attrs, children)
}

///
pub fn dd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dd", attrs, children)
}

///
pub fn div(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("div", attrs, children)
}

///
pub fn dl(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dl", attrs, children)
}

///
pub fn dt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dt", attrs, children)
}

///
pub fn figcaption(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("figcaption", attrs, children)
}

///
pub fn figure(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("figure", attrs, children)
}

///
pub fn hr(attrs: List(Attribute(message))) -> Element(message) {
  element("hr", attrs, constants.empty_list)
}

///
pub fn li(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("li", attrs, children)
}

///
pub fn menu(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("menu", attrs, children)
}

///
pub fn ol(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ol", attrs, children)
}

///
pub fn p(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("p", attrs, children)
}

///
pub fn pre(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("pre", attrs, children)
}

///
pub fn ul(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ul", attrs, children)
}

// HTML ELEMENTS: INLINE TEXT SEMANTICS ----------------------------------------

/// When paired with the [`href`](../attribute.html#href) attribute, the `<a>`
/// element creates a hyperlink to another page, file, or email address.
/// 
/// The content inside an `<a>` element should accurately describe where the link
/// goes, without relying on any surrounding text or context. For example, a
/// common _misuse_ is using link text link "here" or "read more." For screen readers
/// or users navigating using the keyboard, just having this text announced provides
/// no indication of where the link will take them. 
/// 
/// Common attributes include: [`download`](../attribute.html#download),
/// [`href`](../attribute.html#href), [`hreflang`](../attribute.html#hreflang),
/// [`ping`](../attribute.html#ping), [`referrerpolicy`](../attribute.html#referrerpolicy),
/// [`rel`](../attribute.html#rel), [`target`](../attribute.html#target), and 
/// [`type`](../attribute.html#type).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/a
/// 
pub fn a(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("a", attrs, children)
}

/// The `<abbr>` element makes it possible to provide an abbreviation or acronym
/// for a term in an accessible way. When paired with the [`title`](../attribute.html#title),
/// you can provide the full description of the term that users can reveal by 
/// hovering over the element or be announced by assistive technologies.
/// 
/// Common attributes include: [`title`](../attribute.html#title).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/abbr
/// 
pub fn abbr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("abbr", attrs, children)
}

/// The `<b>` element represents text that should draw the users attention without
/// implying any added importance or emphasis. It is commonly used to highlight
/// keywords or product names. 
/// 
/// The `<b>` element is one of many elements that can be used to attach additional
/// semantics to a piece of text:
/// 
/// - `<cite>` for citing a reference or source.
/// - `<em>` for adding emphasis.
/// - `<i>` to indicate a different voice or mood.
/// - `<mark>` for highlighting text.
/// - `<strong>` to indicate greater importance.
/// - `<u>` for non-textual annotations.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/b
/// 
pub fn b(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("b", attrs, children)
}

/// You can use the `<bdi>` element to embed text that may have a different text
/// directionality than the surrounding content, such as embedding an Arabic
/// quotation within a paragraph of English text.
/// 
/// Common attributes include: [`dir`](../attribute.html#dir).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdi
/// 
pub fn bdi(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdi", attrs, children)
}

/// The `<bdo>` element allows you to override the content's directionality of
/// text, for example to write a paragraph of English text in reverse. 
/// 
/// Common attributes include: [`dir`](../attribute.html#dir).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdo
/// 
pub fn bdo(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdo", attrs, children)
}

/// The `<br>` element is used to insert a line break in a paragraph or block of
/// text for stylistic purposes. 
/// 
/// It is important *not* to use the `<br>` element to (visually) create separate
/// paragraphs of text as this greatly harms accessibility. You should only use
/// the `<br>` element where it is stylistically appropriate, such as in a poem
/// or when writing out an address.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/br
/// 
pub fn br(attrs: List(Attribute(message))) -> Element(message) {
  element("br", attrs, constants.empty_list)
}

/// The `<cite>` element can be used to reference the title of a creative work,
/// such as a book, a film, or a blog post. It is commonly used in tandem with
/// the [`<blockquote>`](#blockquote) or [`<q>`](#q) elements to provide attribution.
/// 
/// The `<cite>` element is one of many elements that can be used to attach
/// additional semantics to a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<em>` for adding emphasis.
/// - `<i>` to indicate a different voice or mood.
/// - `<mark>` for highlighting text.
/// - `<strong>` to indicate greater importance.
/// - `<u>` for non-textual annotations.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/cite
/// 
pub fn cite(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("cite", attrs, children)
}

/// The `<code>` element is used to represent a fragment of computer code, such
/// as a function name or a variable, embedded in a larger block of text. 
/// 
/// To include larger snippets of code or entire files, you can wrap a `<code>`
/// element inside a [`<pre>`](#pre) element.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/code
/// 
pub fn code(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("code", attrs, children)
}

/// You can use the `<data>` element to provide a machine-readable value for a
/// piece of content, such as a product id or numerical value.
/// 
/// Note that when representing a date or time, the [`<time>`](#time) element must
/// be used instead.
/// 
/// Common attributes include: [`value`](../attribute.html#value).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/data
/// 
pub fn data(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("data", attrs, children)
}

/// The `<dfn>` element is used to indicate the term that is being defined in an
/// enclosing paragraph. You should use this when you're introducing a new term
/// with a definition.
/// 
/// Common attributes include: [`title`](../attribute.html#title).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dfn
/// 
pub fn dfn(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dfn", attrs, children)
}

/// The `<em>` element indicates stress or emphasis of the enclosed text. Browsers
/// will typically style `<em>` elements with italicised text, but it's important
/// _not_ to use `<em>` purely for styling purposes. A good rule of thumb is to
/// consider how the text would be read aloud
/// 
/// The `<em>` element is one of many elements that can be used to attach additional
/// semantics to a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<cite>` for citing a reference or source.
/// - `<i>` to indicate a different voice or mood.
/// - `<mark>` for highlighting text.
/// - `<strong>` to indicate greater importance.
/// - `<u>` for non-textual annotations.
/// 
/// `<em>` elements can be nested inside one another, communicating greater levels
/// of emphasis.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/em
/// 
pub fn em(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("em", attrs, children)
}

/// You can use the `<i>` element to represent a range of text that is distinct
/// from the surrounding text, such as prose in an alternative voice, internal
/// thoughts, idiomatic phrases from another language (such as _et cetera_), or
/// technical terms.
/// 
/// The `<i>` element is one of many elements that can be used to attach additional
/// semantics to a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<cite>` for citing a reference or source.
/// - `<em>` for adding emphasis.
/// - `<mark>` for highlighting text.
/// - `<strong>` to indicate greater importance.
/// - `<u>` for non-textual annotations.
/// 
/// By default browsers will style `<i>` elements with italicised text, and in
/// older HTML specifications the `<i>` element was used only for styling purposes.
/// In modern HTML, however, it's important to use this element when its _semantics_
/// are appropriate.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/i
/// 
pub fn i(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("i", attrs, children)
}

/// The `<kbd>` element represents user input, typically from a keyboard. Common
/// use cases include showing keyboard shortcuts or terminal commands.
/// 
/// `<kbd>` elements can be nested inside one another to indicate individual key
/// presses or units of input.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/kbd
/// 
pub fn kbd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("kbd", attrs, children)
}

/// The `<mark>` element represents a fragment of text which is highlighted for
/// reference purposes. You can imagine this element functioning similar to using
/// a highlighter when taking notes or reading a document. The `<mark>` element
/// is one of many elements that can be used to attach additional semantics to
/// a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<cite>` for citing a reference or source.
/// - `<em>` for adding emphasis.
/// - `<i>` to indicate a different voice or mood.
/// - `<strong>` to indicate greater importance.
/// - `<u>` for non-textual annotations.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/mark
/// 
pub fn mark(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("mark", attrs, children)
}

/// The `<q>` element is the inline version of [`<blockquote>`](#blockquote) and
/// is used to embed a short inline quotation in the surrounding text.
/// 
/// Common attributes include: [`cite`](../attribute.html#cite).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/q
/// 
pub fn q(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("q", attrs, children)
}

/// The `<rp>` element is used in combination with the [`<ruby`](#ruby) and
/// [`<rt>`](#rt) elements to render ruby annotations properly. You should use
/// the `<rp>` element to wrap [`<rt>`](#rt) annotations in parentheses.
/// 
/// The browser will correctly omit these parenthesis if it supports ruby annotations,
/// while providing a fallback for browsers that do not.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rp
/// 
pub fn rp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rp", attrs, children)
}

/// You should use the `<rt>` element in combination with the [`<ruby>`](#ruby)
/// at [`<rp>`](#rp) elements to provide ruby annotations for East Asian typography,
/// for example by providing Japanese furigana or Chinese pinyin.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rt
/// 
pub fn rt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rt", attrs, children)
}

/// The `<ruby>` element is used in combination with the [`<rt>`](#rt) and [`<rp>`](#rp)
/// elements to wrap East Asian typography with annotations such as Japanese
/// furigana or Chinese pinyin.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ruby
/// 
pub fn ruby(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ruby", attrs, children)
}

/// The `<s>` element represents text that is no longer accurate or relevant.
/// Browsers typically render this as strikethrough text.
/// 
/// To represent document edits you should use the [`<del>`](#del) and [`<ins>`](#ins)
/// elements instead.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/s
/// 
pub fn s(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("s", attrs, children)
}

/// The `<samp>` element is used to represent example or quoted output from a
/// computer program, such as the output of a command-line tool or a code
/// snippet.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/samp
/// 
pub fn samp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("samp", attrs, children)
}

/// You can use the `<small>` element to represent small print like copyright
/// or legal notices.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/small
/// 
pub fn small(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("small", attrs, children)
}

/// The `<span>` element is a generic container for inline content that does not
/// have any intrinsic semantics. When you want to change the presentation of
/// some inline text and no semantic HTML element is appropriate, you can use a
/// `<span>` to wrap the content and apply CSS styles as appropriate.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/span
/// 
pub fn span(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("span", attrs, children)
}

/// You should use the `<strong>` element to convey importance or urgency. The
/// `<strong>` element is one of many elements that can be used to attach additional
/// semantics to a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<cite>` for citing a reference or source.
/// - `<em>` for adding emphasis.
/// - `<i>` to indicate a different voice or mood.
/// - `<mark>` for highlighting text.
/// - `<u>` for non-textual annotations.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/strong
/// 
pub fn strong(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("strong", attrs, children)
}

/// You can use the `<sub>` element to represent subscript text when marking up
/// footnotes, chemical formulas, or mathematical expressions.
/// 
/// This element should only be used to _typographic_ purposes: in cases where
/// subscript text is desired for purely presentation purposes, CSS should be used
/// instead.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sub
/// 
pub fn sub(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sub", attrs, children)
}

/// You can use the `<sup>` element to represent superscript text such as exponents
/// in mathematical expressions or ordinal numbers.
/// 
/// This element should only be used to _typographic_ purposes: in cases where
/// superscript text is desired for purely presentation purposes, CSS should be used
/// instead.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sup
/// 
pub fn sup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sup", attrs, children)
}

/// The `<time>` element is used to provide a machine-readable date or time for
/// human-readable content such as a dates, times, or durations.
/// 
/// Common attributes include: [`datetime`](../attribute.html#datetime).
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time
/// 
pub fn time(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("time", attrs, children)
}

/// The `<u>` element is used to provide non-textual annotations such as labeling
/// a spelling mistake or providing a Chinese proper name mark. The `<u>` element
/// is one of many elements that can be used to attach additional semantics to
/// a piece of text:
/// 
/// - `<b>` to draw additional attention.
/// - `<cite>` for citing a reference or source.
/// - `<em>` for adding emphasis.
/// - `<i>` to indicate a different voice or mood.
/// - `<mark>` for highlighting text.
/// - `<strong>` to indicate greater importance.
/// 
/// While browsers typically render `<u>` elements with an underline, it is important
/// _not_ to use this element for presentation purposes. 
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/u
/// 
pub fn u(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("u", attrs, children)
}

/// You can use the `<var>` element to indicate the name of a variable when
/// referencing computer code or writing a mathematical expression.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/var
/// 
pub fn var(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("var", attrs, children)
}

/// The `<wbr>` element hints to the browser where it may optionally break up a
/// word to improve line breaking. This is useful in cases where the line breaking
/// rules would otherwise prevent a line break at a desirable location.
/// 
/// See also: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/wbr
/// 
pub fn wbr(attrs: List(Attribute(message))) -> Element(message) {
  element("wbr", attrs, constants.empty_list)
}

// HTML ELEMENTS: IMAGE AND MULTIMEDIA -----------------------------------------

///
pub fn area(attrs: List(Attribute(message))) -> Element(message) {
  element("area", attrs, constants.empty_list)
}

///
pub fn audio(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("audio", attrs, children)
}

///
pub fn img(attrs: List(Attribute(message))) -> Element(message) {
  element("img", attrs, constants.empty_list)
}

/// Used with <area> elements to define an image map (a clickable link area).
///
pub fn map(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("map", attrs, children)
}

///
pub fn track(attrs: List(Attribute(message))) -> Element(message) {
  element("track", attrs, constants.empty_list)
}

///
pub fn video(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("video", attrs, children)
}

// HTML ELEMENTS: EMBEDDED CONTENT ---------------------------------------------

///
pub fn embed(attrs: List(Attribute(message))) -> Element(message) {
  element("embed", attrs, constants.empty_list)
}

///
pub fn iframe(attrs: List(Attribute(message))) -> Element(message) {
  element("iframe", attrs, constants.empty_list)
}

///
pub fn object(attrs: List(Attribute(message))) -> Element(message) {
  element("object", attrs, constants.empty_list)
}

///
pub fn picture(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("picture", attrs, children)
}

///
pub fn portal(attrs: List(Attribute(message))) -> Element(message) {
  element("portal", attrs, constants.empty_list)
}

///
pub fn source(attrs: List(Attribute(message))) -> Element(message) {
  element("source", attrs, constants.empty_list)
}

// HTML ELEMENTS: SVG AND MATHML -----------------------------------------------

///
pub fn math(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced("http://www.w3.org/1998/Math/MathML", "math", attrs, children)
}

///
pub fn svg(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced("http://www.w3.org/2000/svg", "svg", attrs, children)
}

// HTML ELEMENTS: SCRIPTING ----------------------------------------------------

///
pub fn canvas(attrs: List(Attribute(message))) -> Element(message) {
  element("canvas", attrs, constants.empty_list)
}

///
pub fn noscript(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("noscript", attrs, children)
}

///
pub fn script(attrs: List(Attribute(message)), js: String) -> Element(message) {
  element.unsafe_raw_html("", "script", attrs, js)
}

// HTML ELEMENTS: DEMARCATING EDITS ---------------------------------------------

///
pub fn del(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("del", attrs, children)
}

///
pub fn ins(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("ins", attrs, children)
}

// HTML ELEMENTS: TABLE CONTENT ------------------------------------------------

///
pub fn caption(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("caption", attrs, children)
}

///
pub fn col(attrs: List(Attribute(message))) -> Element(message) {
  element.element("col", attrs, constants.empty_list)
}

///
pub fn colgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("colgroup", attrs, children)
}

///
pub fn table(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("table", attrs, children)
}

///
pub fn tbody(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tbody", attrs, children)
}

///
pub fn td(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("td", attrs, children)
}

///
pub fn tfoot(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tfoot", attrs, children)
}

///
pub fn th(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("th", attrs, children)
}

///
pub fn thead(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("thead", attrs, children)
}

///
pub fn tr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tr", attrs, children)
}

// HTML ELEMENTS: FORMS --------------------------------------------------------

///
pub fn button(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("button", attrs, children)
}

///
pub fn datalist(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("datalist", attrs, children)
}

///
pub fn fieldset(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("fieldset", attrs, children)
}

///
pub fn form(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("form", attrs, children)
}

///
pub fn input(attrs: List(Attribute(message))) -> Element(message) {
  element.element("input", attrs, constants.empty_list)
}

///
pub fn label(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("label", attrs, children)
}

///
pub fn legend(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("legend", attrs, children)
}

///
pub fn meter(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("meter", attrs, children)
}

///
pub fn optgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("optgroup", attrs, children)
}

///
pub fn option(
  attrs: List(Attribute(message)),
  label: String,
) -> Element(message) {
  element.element("option", attrs, [element.text(label)])
}

///
pub fn output(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("output", attrs, children)
}

///
pub fn progress(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("progress", attrs, children)
}

///
pub fn select(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("select", attrs, children)
}

///
pub fn textarea(
  attrs: List(Attribute(message)),
  content: String,
) -> Element(message) {
  element.element(
    "textarea",
    [attribute.property("value", json.string(content)), ..attrs],
    [element.text(content)],
  )
}

// HTML ELEMENTS: INTERACTIVE ELEMENTS -----------------------------------------

///
pub fn details(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("details", attrs, children)
}

///
pub fn dialog(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("dialog", attrs, children)
}

///
pub fn summary(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("summary", attrs, children)
}

// HTML ELEMENTS: WEB COMPONENTS -----------------------------------------------

///
pub fn slot(
  attrs: List(Attribute(message)),
  fallback: List(Element(message)),
) -> Element(message) {
  element.element("slot", attrs, fallback)
}

///
pub fn template(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("template", attrs, children)
}
