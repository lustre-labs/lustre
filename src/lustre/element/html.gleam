// IMPORTS ---------------------------------------------------------------------

import gleam/json
import lustre/attribute.{type Attribute}
import lustre/element.{type Element, element, namespaced}
import lustre/internals/constants

// HTML ELEMENTS: MAIN ROOT ----------------------------------------------------

/// The `<html>` element is the root element of an HTML document.
/// Example:
/// ```gleam
/// html([], [head([], []), body([], [])])
/// ```
pub fn html(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("html", attrs, children)
}

/// Create a text node.
///
/// Example:
/// ```gleam
/// text("Hello, world!")
/// ```
pub fn text(content: String) -> Element(msg) {
  element.text(content)
}

// HTML ELEMENTS: DOCUMENT METADATA --------------------------------------------


/// The `<base>` element specifies the base URL to use for all relative URLs in a document.
///
/// Example:
/// ```gleam
/// base([attribute("href", "https://example.com/")])
/// ```
pub fn base(attrs: List(Attribute(msg))) -> Element(msg) {
  element("base", attrs, constants.empty_list)
}

///
/// The `<head>` element contains metadata and links for the document.
///
/// Example:
/// ```gleam
/// head([], [title([], "My Page")])
/// ```
pub fn head(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("head", attrs, children)
}


/// The `<link>` element defines relationships between the current document and external resources (most used for stylesheets).
///
/// Example:
/// ```gleam
/// link([attribute("rel", "stylesheet"), attribute("href", "/style.css")])
/// ```
pub fn link(attrs: List(Attribute(msg))) -> Element(msg) {
  element("link", attrs, constants.empty_list)
}


/// The `<meta>` element provides metadata about the HTML document (such as charset or viewport).
///
/// Example:
/// ```gleam
/// meta([attribute("charset", "utf-8")])
/// ```
pub fn meta(attrs: List(Attribute(msg))) -> Element(msg) {
  element("meta", attrs, constants.empty_list)
}

/// The `<style>` element contains CSS style rules for the document.
///
/// Example:
/// ```gleam
/// style([], "body { background: #fff; }")
/// ```
pub fn style(attrs: List(Attribute(msg)), css: String) -> Element(msg) {
  element.unsafe_raw_html("", "style", attrs, css)
}

///
/// The `<title>` element sets the title of the document (shown in the browser tab).
///
/// Example:
/// ```gleam
/// title([], "My Page")
/// ```
pub fn title(attrs: List(Attribute(msg)), content: String) -> Element(msg) {
  element("title", attrs, [text(content)])
}

// HTML ELEMENTS: SECTIONING ROOT -----------------------------------------------

///
/// The `<body>` element contains the content of the HTML document.
///
/// Example:
/// ```gleam
/// body([], [div([], [text("Hello")])])
/// ```
pub fn body(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("body", attrs, children)
}

// HTML ELEMENTS: CONTENT SECTIONING -------------------------------------------


/// The `<address>` element supplies contact information for its nearest article or body ancestor.
///
/// Example:
/// ```gleam
/// address([], [text("Contact us at info@example.com")])
/// ```
pub fn address(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("address", attrs, children)
}


/// The `<article>` element represents a self-contained composition in a document, page, application, or site.
///
/// Example:
/// ```gleam
/// article([], [h1([], [text("News")]), p([], [text("Some news...")])])
/// ```
pub fn article(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("article", attrs, children)
}

///
/// The `<aside>` element represents content indirectly related to the main content (like a sidebar).
///
/// Example:
/// ```gleam
/// aside([], [p([], [text("Sidebar info")])])
/// ```
pub fn aside(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("aside", attrs, children)
}


/// The `<footer>` element represents a footer for its nearest sectioning content or root element.
///
/// Example:
/// ```gleam
/// footer([], [text("© 2025")])
/// ```
pub fn footer(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("footer", attrs, children)
}


/// The `<header>` element represents introductory content or navigational links.
///
/// Example:
/// ```gleam
/// header([], [h1([], [text("Welcome")])])
/// ```
pub fn header(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("header", attrs, children)
}

///
/// The `<h1>` element represents a top-level heading.
///
/// Example:
/// ```gleam
/// h1([], [text("Main Heading")])
/// ```
pub fn h1(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h1", attrs, children)
}

///
/// The `<h2>` element represents a second-level heading.
///
/// Example:
/// ```gleam
/// h2([], [text("Subheading")])
/// ```
pub fn h2(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h2", attrs, children)
}

///
/// The `<h3>` element represents a third-level heading.
///
/// Example:
/// ```gleam
/// h3([], [text("Section Heading")])
/// ```
pub fn h3(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h3", attrs, children)
}

///
/// The `<h4>` element represents a fourth-level heading.
///
/// Example:
/// ```gleam
/// h4([], [text("Subsection Heading")])
/// ```
pub fn h4(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h4", attrs, children)
}

///
/// The `<h5>` element represents a fifth-level heading.
///
/// Example:
/// ```gleam
/// h5([], [text("Minor Heading")])
/// ```
pub fn h5(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h5", attrs, children)
}

///
/// The `<h6>` element represents a sixth-level heading.
///
/// Example:
/// ```gleam
/// h6([], [text("Lowest Heading")])
/// ```
pub fn h6(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h6", attrs, children)
}

///
/// The `<hgroup>` element groups a set of `<h1>`–`<h6>` elements when a heading has multiple levels.
///
/// Example:
/// ```gleam
/// hgroup([], [h1([], [text("Title")]), h2([], [text("Subtitle")])])
/// ```
pub fn hgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("hgroup", attrs, children)
}

///
/// The `<main>` element represents the dominant content of the `<body>`.
///
/// Example:
/// ```gleam
/// main([], [p([], [text("Main content")])])
/// ```
pub fn main(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("main", attrs, children)
}

///
/// The `<nav>` element represents a section of navigation links.
///
/// Example:
/// ```gleam
/// nav([], [a([attribute("href", "/")], [text("Home")])])
/// ```
pub fn nav(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("nav", attrs, children)
}

///
/// The `<section>` element represents a standalone section of content.
///
/// Example:
/// ```gleam
/// section([], [h2([], [text("Section")]), p([], [text("Details")])])
/// ```
pub fn section(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("section", attrs, children)
}

///
/// The `<search>` element represents a search form or interface.
///
/// Example:
/// ```gleam
/// search([], [input([])])
/// ```
pub fn search(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("search", attrs, children)
}

// HTML ELEMENTS: TEXT CONTENT -------------------------------------------------

/// The `<blockquote>` element represents a section quoted from another source.
///
/// Example:
/// ```gleam
/// blockquote([], [p([], [text("A quoted passage")])])
/// ```
pub fn blockquote(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("blockquote", attrs, children)
}

/// The `<dd>` element provides the description or value for a preceding `<dt>` term in a description list.
///
/// Example:
/// ```gleam
/// dl([], [dt([], [text("Term")]), dd([], [text("Definition")])])
/// ```
pub fn dd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dd", attrs, children)
}

/// The `<div>` element is a generic container for flow content with no special meaning.
///
/// Example:
/// ```gleam
/// div([attribute("class", "container")], [p([], [text("Content")])])
/// ```
pub fn div(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("div", attrs, children)
}

/// The `<dl>` element represents a description list, a list of terms and their descriptions.
///
/// Example:
/// ```gleam
/// dl([], [dt([], [text("Term")]), dd([], [text("Definition")])])
/// ```
pub fn dl(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dl", attrs, children)
}

/// The `<dt>` element represents a term/name in a description list (`<dl>`).
///
/// Example:
/// ```gleam
/// dt([], [text("Term")])
/// ```
pub fn dt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dt", attrs, children)
}

/// The `<figcaption>` element represents a caption or legend for a `<figure>`.
///
/// Example:
/// ```gleam
/// figcaption([], [text("Figure 1: Example")])
/// ```
pub fn figcaption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("figcaption", attrs, children)
}

/// The `<figure>` element represents self-contained content, like illustrations, diagrams, or code listings, optionally with a caption.
///
/// Example:
/// ```gleam
/// figure([], [img([attribute("src", "/img.png")]), figcaption([], [text("An image")])])
/// ```
pub fn figure(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("figure", attrs, children)
}

/// The `<hr>` element represents a thematic break between paragraph-level elements (a horizontal rule).
///
/// Example:
/// ```gleam
/// hr([])
/// ```
pub fn hr(attrs: List(Attribute(msg))) -> Element(msg) {
  element("hr", attrs, constants.empty_list)
}

/// The `<li>` element represents an item in a list (`<ul>`, `<ol>`, or `<menu>`).
///
/// Example:
/// ```gleam
/// li([], [text("List item")])
/// ```
pub fn li(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("li", attrs, children)
}

/// The `<menu>` element represents a list of commands or menu items.
///
/// Example:
/// ```gleam
/// menu([], [li([], [text("Item")])])
/// ```
pub fn menu(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("menu", attrs, children)
}

/// The `<ol>` element represents an ordered list of items.
///
/// Example:
/// ```gleam
/// ol([], [li([], [text("First")]), li([], [text("Second")])])
/// ```
pub fn ol(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ol", attrs, children)
}

/// The `<p>` element represents a paragraph of text.
///
/// Example:
/// ```gleam
/// p([], [text("A paragraph")])
/// ```
pub fn p(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("p", attrs, children)
}

/// The `<pre>` element represents preformatted text where whitespace is preserved.
///
/// Example:
/// ```gleam
/// pre([], [text("  preformatted\n  text")])
/// ```
pub fn pre(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("pre", attrs, children)
}

/// The `<ul>` element represents an unordered list of items.
///
/// Example:
/// ```gleam
/// ul([], [li([], [text("Item")])])
/// ```
pub fn ul(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ul", attrs, children)
}

// HTML ELEMENTS: INLINE TEXT SEMANTICS ----------------------------------------

/// The `<a>` element represents a hyperlink to another resource or location in the document.
///
/// Example:
/// ```gleam
/// a([attribute("href", "/")], [text("Home")])
/// ```
pub fn a(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("a", attrs, children)
}

/// The `<abbr>` element represents an abbreviation or acronym, optionally with an expansion.
///
/// Example:
/// ```gleam
/// abbr([attribute("title", "HyperText Markup Language")], [text("HTML")])
/// ```
pub fn abbr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("abbr", attrs, children)
}

/// The `<b>` element represents text stylistically offset without extra importance (typically bold).
///
/// Example:
/// ```gleam
/// b([], [text("Bold text")])
/// ```
pub fn b(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("b", attrs, children)
}

/// The `<bdi>` element isolates a span of text that may be formatted in a different directionality.
///
/// Example:
/// ```gleam
/// bdi([], [text("Text")])
/// ```
pub fn bdi(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("bdi", attrs, children)
}

/// The `<bdo>` element overrides the current directionality of text (left-to-right or right-to-left).
///
/// Example:
/// ```gleam
/// bdo([attribute("dir", "rtl")], [text("...")])
/// ```
pub fn bdo(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("bdo", attrs, children)
}

/// The `<br>` element produces a line break in text.
///
/// Example:
/// ```gleam
/// br([])
/// ```
pub fn br(attrs: List(Attribute(msg))) -> Element(msg) {
  element("br", attrs, constants.empty_list)
}

/// The `<cite>` element represents the title of a work (e.g., a book, song, or paper).
///
/// Example:
/// ```gleam
/// cite([], [text("Some Book")])
/// ```
pub fn cite(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("cite", attrs, children)
}

/// The `<code>` element represents a fragment of computer code.
///
/// Example:
/// ```gleam
/// code([], [text("let x = 1")])
/// ```
pub fn code(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("code", attrs, children)
}

/// The `<data>` element links a given content with a machine-readable value via the `value` attribute.
///
/// Example:
/// ```gleam
/// data([attribute("value", "42")], [text("42")])
/// ```
pub fn data(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("data", attrs, children)
}

/// The `<dfn>` element represents the defining instance of a term.
///
/// Example:
/// ```gleam
/// dfn([], [text("Term")])
/// ```
pub fn dfn(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dfn", attrs, children)
}

/// The `<em>` element represents emphasized text (typically rendered as italics).
///
/// Example:
/// ```gleam
/// em([], [text("Important")])
/// ```
pub fn em(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("em", attrs, children)
}

/// The `<i>` element represents a span of text in an alternate voice or mood, typically italicized.
///
/// Example:
/// ```gleam
/// i([], [text("Italic text")])
/// ```
pub fn i(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("i", attrs, children)
}

/// The `<kbd>` element represents user input (typically from a keyboard).
///
/// Example:
/// ```gleam
/// kbd([], [text("Ctrl+C")])
/// ```
pub fn kbd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("kbd", attrs, children)
}

/// The `<mark>` element represents text marked or highlighted for reference purposes.
///
/// Example:
/// ```gleam
/// mark([], [text("Highlighted")])
/// ```
pub fn mark(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("mark", attrs, children)
}

/// The `<q>` element represents an inline quotation.
///
/// Example:
/// ```gleam
/// q([], [text("A quote")])
/// ```
pub fn q(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("q", attrs, children)
}

/// The `<rp>` element provides fallback parentheses for browsers that do not support ruby annotations.
///
/// Example:
/// ```gleam
/// rp([], [text("(")])
/// ```
pub fn rp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("rp", attrs, children)
}

/// The `<rt>` element provides the pronunciation or annotation for a ruby base text.
///
/// Example:
/// ```gleam
/// rt([], [text("pronunciation")])
/// ```
pub fn rt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("rt", attrs, children)
}

/// The `<ruby>` element represents ruby annotations (small annotations for East Asian typography).
///
/// Example:
/// ```gleam
/// ruby([], [text("漢"), rt([], [text("kan")])])
/// ```
pub fn ruby(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ruby", attrs, children)
}

/// The `<s>` element represents text that is no longer accurate or relevant (strikethrough).
///
/// Example:
/// ```gleam
/// s([], [text("Old price")])
/// ```
pub fn s(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("s", attrs, children)
}

/// The `<samp>` element represents sample output from a computer program.
///
/// Example:
/// ```gleam
/// samp([], [text("Output: OK")])
/// ```
pub fn samp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("samp", attrs, children)
}

/// The `<small>` element represents side comments or small print.
///
/// Example:
/// ```gleam
/// small([], [text("Terms apply")])
/// ```
pub fn small(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("small", attrs, children)
}

/// The `<span>` element is an inline container for phrasing content with no special meaning.
///
/// Example:
/// ```gleam
/// span([attribute("class", "label")], [text("Label")])
/// ```
pub fn span(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("span", attrs, children)
}

/// The `<strong>` element represents strongly emphasized text (typically rendered as bold).
///
/// Example:
/// ```gleam
/// strong([], [text("Important")])
/// ```
pub fn strong(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("strong", attrs, children)
}

/// The `<sub>` element represents subscript text.
///
/// Example:
/// ```gleam
/// sub([], [text("H2O")])
/// ```
pub fn sub(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("sub", attrs, children)
}

/// The `<sup>` element represents superscript text.
///
/// Example:
/// ```gleam
/// sup([], [text("1st")])
/// ```
pub fn sup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("sup", attrs, children)
}

/// The `<time>` element represents a specific period in time or a datetime.
///
/// Example:
/// ```gleam
/// time([attribute("datetime", "2025-09-02")], [text("2 Sep 2025")])
/// ```
pub fn time(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("time", attrs, children)
}

/// The `<u>` element represents text that should be stylistically different from normal text (typically underlined).
///
/// Example:
/// ```gleam
/// u([], [text("Underlined")])
/// ```
pub fn u(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("u", attrs, children)
}

/// The `<var>` element represents a variable in a mathematical expression or programming context.
///
/// Example:
/// ```gleam
/// var([], [text("x")])
/// ```
pub fn var(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("var", attrs, children)
}

/// The `<wbr>` element represents a possible line break opportunity within text.
///
/// Example:
/// ```gleam
/// wbr([])
/// ```
pub fn wbr(attrs: List(Attribute(msg))) -> Element(msg) {
  element("wbr", attrs, constants.empty_list)
}

// HTML ELEMENTS: IMAGE AND MULTIMEDIA -----------------------------------------

/// The `<area>` element defines an area inside an image map that has clickable regions.
///
/// Example:
/// ```gleam
/// area([attribute("shape", "rect"), attribute("coords", "0,0,100,100"), attribute("href", "/")])
/// ```
pub fn area(attrs: List(Attribute(msg))) -> Element(msg) {
  element("area", attrs, constants.empty_list)
}

/// The `<audio>` element represents sound content and may contain multiple sources.
///
/// Example:
/// ```gleam
/// audio([], [source([attribute("src", "/audio.mp3")])])
/// ```
pub fn audio(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("audio", attrs, children)
}

/// The `<img>` element embeds an image into the document.
///
/// Example:
/// ```gleam
/// img([attribute("src", "/img.png"), attribute("alt", "An image")])
/// ```
pub fn img(attrs: List(Attribute(msg))) -> Element(msg) {
  element("img", attrs, constants.empty_list)
}

/// The `<map>` element is used with `<area>` elements to define an image map (clickable areas within an image).
///
/// Example:
/// ```gleam
/// map([], [area([attribute("coords", "0,0,100,100"), attribute("href", "/")])])
/// ```
pub fn map(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("map", attrs, children)
}

/// The `<track>` element specifies text tracks for media elements like `<video>` and `<audio>` (e.g., subtitles).
///
/// Example:
/// ```gleam
/// track([attribute("kind", "subtitles"), attribute("srclang", "en"), attribute("src", "/subs.vtt")])
/// ```
pub fn track(attrs: List(Attribute(msg))) -> Element(msg) {
  element("track", attrs, constants.empty_list)
}

/// The `<video>` element represents video content and may contain multiple sources and tracks.
///
/// Example:
/// ```gleam
/// video([], [source([attribute("src", "/video.mp4")])])
/// ```
pub fn video(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("video", attrs, children)
}

// HTML ELEMENTS: EMBEDDED CONTENT ---------------------------------------------

/// The `<embed>` element represents an integration point for an external (often interactive) resource.
///
/// Example:
/// ```gleam
/// embed([attribute("src", "/plugin"), attribute("type", "application/x-shockwave-flash")])
/// ```
pub fn embed(attrs: List(Attribute(msg))) -> Element(msg) {
  element("embed", attrs, constants.empty_list)
}

/// The `<iframe>` element embeds another HTML page into the current document.
///
/// Example:
/// ```gleam
/// iframe([attribute("src", "/embedded")])
/// ```
pub fn iframe(attrs: List(Attribute(msg))) -> Element(msg) {
  element("iframe", attrs, constants.empty_list)
}

/// The `<object>` element represents an external resource, which can be treated as an image, nested browsing context, or plugin.
///
/// Example:
/// ```gleam
/// object([attribute("data", "/file.svg"), attribute("type", "image/svg+xml")])
/// ```
pub fn object(attrs: List(Attribute(msg))) -> Element(msg) {
  element("object", attrs, constants.empty_list)
}

/// The `<picture>` element contains zero or more `<source>` elements and an `<img>` element to provide multiple image sources for responsive design.
///
/// Example:
/// ```gleam
/// picture([], [source([attribute("media", "(min-width: 800px)"), attribute("srcset", "/large.jpg")]), img([attribute("src", "/small.jpg")])])
/// ```
pub fn picture(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("picture", attrs, children)
}

/// The `<portal>` element represents an embedding point for another browsing context (experimental API).
///
/// Example:
/// ```gleam
/// portal([attribute("src", "/preview")])
/// ```
pub fn portal(attrs: List(Attribute(msg))) -> Element(msg) {
  element("portal", attrs, constants.empty_list)
}

/// The `<source>` element specifies multiple media resources for `<picture>`, `<audio>`, or `<video>` elements.
///
/// Example:
/// ```gleam
/// source([attribute("src", "/video.mp4"), attribute("type", "video/mp4")])
/// ```
pub fn source(attrs: List(Attribute(msg))) -> Element(msg) {
  element("source", attrs, constants.empty_list)
}

// HTML ELEMENTS: SVG AND MATHML -----------------------------------------------

/// The `<svg>` element is the container for SVG graphics and uses the SVG namespace.
///
/// Example:
/// ```gleam
/// svg([], [circle([attribute("cx", "50"), attribute("cy", "50"), attribute("r", "40")])])
/// ```
pub fn svg(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/2000/svg", "svg", attrs, children)
}

/// The `<math>` element represents a MathML root element for mathematical notation.
///
/// Example:
/// ```gleam
/// math([], [text("x = y")])
/// ```
pub fn math(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("math", attrs, children)
}

// HTML ELEMENTS: SCRIPTING ----------------------------------------------------

/// The `<canvas>` element provides an area for dynamic, scriptable rendering of 2D shapes and bitmap images.
///
/// Example:
/// ```gleam
/// canvas([attribute("width", "300"), attribute("height", "150")])
/// ```
pub fn canvas(attrs: List(Attribute(msg))) -> Element(msg) {
  element("canvas", attrs, constants.empty_list)
}

/// The `<noscript>` element defines alternate content to be shown when scripts are disabled or unsupported.
///
/// Example:
/// ```gleam
/// noscript([], [text("Enable JavaScript to view this content")])
/// ```
pub fn noscript(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("noscript", attrs, children)
}

/// The `<script>` element allows embedding or referencing executable script content (e.g., JavaScript).
///
/// Example:
/// ```gleam
/// script([attribute("type", "application/javascript")], "console.log('hi')")
/// ```
pub fn script(attrs: List(Attribute(msg)), js: String) -> Element(msg) {
  element.unsafe_raw_html("", "script", attrs, js)
}

// HTML ELEMENTS: DEMARCATING EDITS ---------------------------------------------

/// The `<del>` element represents removed text from a document (often rendered as strike-through).
///
/// Example:
/// ```gleam
/// del([], [text("Removed text")])
/// ```
pub fn del(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("del", attrs, children)
}

/// The `<ins>` element represents inserted text into a document.
///
/// Example:
/// ```gleam
/// ins([], [text("Inserted text")])
/// ```
pub fn ins(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("ins", attrs, children)
}

// HTML ELEMENTS: TABLE CONTENT ------------------------------------------------

/// The `<caption>` element represents the title of a table.
///
/// Example:
/// ```gleam
/// caption([], [text("Table Title")])
/// ```
pub fn caption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("caption", attrs, children)
}

/// The `<col>` element defines column properties for each column within a `<colgroup>`.
///
/// Example:
/// ```gleam
/// col([attribute("span", "2")])
/// ```
pub fn col(attrs: List(Attribute(msg))) -> Element(msg) {
  element.element("col", attrs, constants.empty_list)
}

/// The `<colgroup>` element groups one or more `<col>` elements that define column properties for a table.
///
/// Example:
/// ```gleam
/// colgroup([], [col([attribute("span", "2")])])
/// ```
pub fn colgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("colgroup", attrs, children)
}

/// The `<table>` element represents tabular data with rows and columns.
///
/// Example:
/// ```gleam
/// table([], [thead([], [tr([], [th([], [text("Header")])])]), tbody([], [tr([], [td([], [text("Cell")])])])])
/// ```
pub fn table(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("table", attrs, children)
}

/// The `<tbody>` element groups the body content in a table.
///
/// Example:
/// ```gleam
/// tbody([], [tr([], [td([], [text("Cell")])])])
/// ```
pub fn tbody(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tbody", attrs, children)
}

/// The `<td>` element represents a data cell in a table row.
///
/// Example:
/// ```gleam
/// td([], [text("Cell")])
/// ```
pub fn td(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("td", attrs, children)
}

/// The `<tfoot>` element groups the footer rows in a table.
///
/// Example:
/// ```gleam
/// tfoot([], [tr([], [td([], [text("Summary")])])])
/// ```
pub fn tfoot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tfoot", attrs, children)
}

/// The `<th>` element represents a header cell in a table.
///
/// Example:
/// ```gleam
/// th([], [text("Heading")])
/// ```
pub fn th(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("th", attrs, children)
}

/// The `<thead>` element groups the header rows in a table.
///
/// Example:
/// ```gleam
/// thead([], [tr([], [th([], [text("Header")])])])
/// ```
pub fn thead(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("thead", attrs, children)
}

/// The `<tr>` element represents a row of cells in a table.
///
/// Example:
/// ```gleam
/// tr([], [td([], [text("Cell")])])
/// ```
pub fn tr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tr", attrs, children)
}

// HTML ELEMENTS: FORMS --------------------------------------------------------

/// The `<button>` element represents a clickable button.
///
/// Example:
/// ```gleam
/// button([], [text("Click me")])
/// ```
pub fn button(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("button", attrs, children)
}

/// The `<datalist>` element contains a set of `<option>` elements that represent suggested values for an `<input>`.
///
/// Example:
/// ```gleam
/// datalist([], [option([], "Choice")])
/// ```
pub fn datalist(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("datalist", attrs, children)
}

/// The `<fieldset>` element groups related controls and labels within a form.
///
/// Example:
/// ```gleam
/// fieldset([], [legend([], [text("Group")]), input([])])
/// ```
pub fn fieldset(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("fieldset", attrs, children)
}

/// The `<form>` element represents a section of interactive controls for submitting information.
///
/// Example:
/// ```gleam
/// form([attribute("action", "/submit")], [input([]), button([], [text("Send")])])
/// ```
pub fn form(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("form", attrs, children)
}

/// The `<input>` element represents a control for text, numbers, checkboxes, radio buttons, etc.
///
/// Example:
/// ```gleam
/// input([attribute("type", "text"), attribute("value", "Hello")])
/// ```
pub fn input(attrs: List(Attribute(msg))) -> Element(msg) {
  element.element("input", attrs, constants.empty_list)
}

/// The `<label>` element represents a caption for an item in a user interface and can be associated with a form control.
///
/// Example:
/// ```gleam
/// label([attribute("for", "email")], [text("Email")])
/// ```
pub fn label(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("label", attrs, children)
}

/// The `<legend>` element represents a caption for the content of its parent `<fieldset>`.
///
/// Example:
/// ```gleam
/// legend([], [text("Section title")])
/// ```
pub fn legend(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("legend", attrs, children)
}

/// The `<meter>` element represents a scalar measurement within a known range (e.g., disk usage).
///
/// Example:
/// ```gleam
/// meter([attribute("value", "0.7"), attribute("max", "1")], [])
/// ```
pub fn meter(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("meter", attrs, children)
}

/// The `<optgroup>` element groups related `<option>` elements within a `<select>`.
///
/// Example:
/// ```gleam
/// optgroup([attribute("label", "Group")], [option([], "A")])
/// ```
pub fn optgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("optgroup", attrs, children)
}

/// The `<option>` element represents an option in a `<select>`, `<datalist>`, or `<optgroup>`.
///
/// Example:
/// ```gleam
/// option([], "Choice")
/// ```
pub fn option(attrs: List(Attribute(msg)), label: String) -> Element(msg) {
  element.element("option", attrs, [element.text(label)])
}

/// The `<output>` element represents the result of a calculation or user action.
///
/// Example:
/// ```gleam
/// output([], [text("Result")])
/// ```
pub fn output(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("output", attrs, children)
}

/// The `<progress>` element represents the completion progress of a task.
///
/// Example:
/// ```gleam
/// progress([attribute("value", "30"), attribute("max", "100")], [])
/// ```
pub fn progress(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("progress", attrs, children)
}

/// The `<select>` element represents a control that provides a menu of options.
///
/// Example:
/// ```gleam
/// select([], [option([], "One"), option([], "Two")])
/// ```
pub fn select(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("select", attrs, children)
}

///
/// The `<textarea>` element represents a multi-line plain-text editing control.
///
/// Example:
/// ```gleam
/// textarea([attribute("rows", "4")], "Default text")
/// ```
pub fn textarea(attrs: List(Attribute(msg)), content: String) -> Element(msg) {
  element.element(
    "textarea",
    [attribute.property("value", json.string(content)), ..attrs],
    [element.text(content)],
  )
}

// HTML ELEMENTS: INTERACTIVE ELEMENTS -----------------------------------------

/// The `<details>` element represents a disclosure widget from which the user can obtain additional information or controls.
///
/// Example:
/// ```gleam
/// details([], [summary([], [text("More")]), p([], [text("Hidden content")])])
/// ```
pub fn details(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("details", attrs, children)
}

/// The `<dialog>` element represents a dialog box or other interactive component, like a modal.
///
/// Example:
/// ```gleam
/// dialog([], [p([], [text("Dialog content")])])
/// ```
pub fn dialog(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("dialog", attrs, children)
}

/// The `<summary>` element provides a summary or label for the `<details>` element's disclosure widget.
///
/// Example:
/// ```gleam
/// summary([], [text("Details")])
/// ```
pub fn summary(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("summary", attrs, children)
}

// HTML ELEMENTS: WEB COMPONENTS -----------------------------------------------

/// The `<slot>` element is a placeholder inside a web component that you can fill with markup when using the component.
///
/// Example:
/// ```gleam
/// slot([], [text("Fallback")])
/// ```
pub fn slot(
  attrs: List(Attribute(msg)),
  fallback: List(Element(msg)),
) -> Element(msg) {
  element.element("slot", attrs, fallback)
}

/// The `<template>` element holds client-side content that isn't rendered when the page loads but can be instantiated later via script.
///
/// Example:
/// ```gleam
/// template([], [div([], [text("Template content")])])
/// ```
pub fn template(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("template", attrs, children)
}
