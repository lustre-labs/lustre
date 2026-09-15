// IMPORTS ---------------------------------------------------------------------

import gleam/json
import lustre/attribute.{type Attribute}
import lustre/element.{type Element, element, namespaced}
import lustre/internals/constants

// HTML ELEMENTS: MAIN ROOT ----------------------------------------------------

/// Represents the root of an HTML document; every other element in the document
/// is a descendant of it.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-html-element>
pub fn html(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("html", attrs, children)
}

/// Renders a plain text node in the DOM, without any surrounding element or
/// namespace. Text nodes are leaves in the element tree and cannot have
/// attributes or children of their own.
///
/// <https://developer.mozilla.org/en-US/docs/Web/API/Text>
pub fn text(content: String) -> Element(message) {
  element.text(content)
}

// HTML ELEMENTS: DOCUMENT METADATA --------------------------------------------

/// Lets authors specify the document's base URL, and optionally a default
/// navigable target name, for use when resolving relative URLs and following
/// links.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-base-element>
pub fn base(attrs: List(Attribute(message))) -> Element(message) {
  element("base", attrs, constants.empty_list)
}

/// Contains the collection of a document's metadata elements, such as its
/// title, scripts, and style sheets.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-head-element>
pub fn head(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("head", attrs, children)
}

/// Lets authors link their document to other resources, such as style sheets or
/// icons, by declaring one or more relationships to those resources.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-link-element>
pub fn link(attrs: List(Attribute(message))) -> Element(message) {
  element("link", attrs, constants.empty_list)
}

/// Represents document-level metadata, pragma directives, or a character
/// encoding declaration, depending on which attribute is used to specify it.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-meta-element>
pub fn meta(attrs: List(Attribute(message))) -> Element(message) {
  element("meta", attrs, constants.empty_list)
}

/// Allows authors to embed CSS style information directly in the document.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-style-element>
pub fn style(attrs: List(Attribute(message)), css: String) -> Element(message) {
  element.unsafe_raw_html("", "style", attrs, css)
}

/// Represents the document's title or name, which authors should choose so that
/// it identifies the document even out of context, such as in a browser history
/// or search results.
///
/// <https://html.spec.whatwg.org/multipage/semantics.html#the-title-element>
pub fn title(
  attrs: List(Attribute(message)),
  content: String,
) -> Element(message) {
  element("title", attrs, [text(content)])
}

// HTML ELEMENTS: SECTIONING ROOT -----------------------------------------------

/// Represents the contents of the document; a conforming document has exactly
/// one body element.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-body-element>
pub fn body(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("body", attrs, children)
}

// HTML ELEMENTS: CONTENT SECTIONING -------------------------------------------

/// Represents the contact information for its nearest article or body ancestor.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-address-element>
pub fn address(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("address", attrs, children)
}

/// Represents a complete, self-contained composition that is in principle
/// independently distributable or reusable, such as a forum post, magazine
/// article, or blog entry.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-article-element>
pub fn article(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("article", attrs, children)
}

/// Represents a section of a page whose content is only tangentially related to
/// the content around it, and which could be considered separate from that
/// content.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-aside-element>
pub fn aside(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("aside", attrs, children)
}

/// Represents a footer for its nearest ancestor sectioning content, typically
/// containing information about the section such as who wrote it or copyright
/// data.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-footer-element>
pub fn footer(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("footer", attrs, children)
}

/// Represents a group of introductory or navigational aids, such as a heading,
/// a logo, or a search form.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-header-element>
pub fn header(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("header", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h1(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h1", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h2(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h2", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h3(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h3", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h4(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h4", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h5(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h5", attrs, children)
}

/// Represents a heading for its section, with the heading level given by the
/// number in the element's name.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements>
pub fn h6(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("h6", attrs, children)
}

/// Represents a heading together with related content, grouping a heading
/// element with one or more p elements that hold a subheading, alternative
/// title, or tagline.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-hgroup-element>
pub fn hgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("hgroup", attrs, children)
}

/// Represents the dominant contents of the document, excluding content that is
/// repeated across a set of documents such as navigation links, headers, and
/// footers.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-main-element>
pub fn main(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("main", attrs, children)
}

/// Represents a section of a page whose purpose is to provide navigation links,
/// either within the page or to other pages.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-nav-element>
pub fn nav(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("nav", attrs, children)
}

/// Represents a generic section of a document or application, being a thematic
/// grouping of content that typically has a heading.
///
/// <https://html.spec.whatwg.org/multipage/sections.html#the-section-element>
pub fn section(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("section", attrs, children)
}

/// Represents a part of a document containing a form, controls, or other
/// content related to performing a search or filtering operation.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-search-element>
pub fn search(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("search", attrs, children)
}

// HTML ELEMENTS: TEXT CONTENT -------------------------------------------------

/// Represents a section that is quoted from another source, whose address may
/// be given using the cite attribute.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-blockquote-element>
pub fn blockquote(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("blockquote", attrs, children)
}

/// Represents the description, definition, or value part of a name-value group
/// in a description list.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-dd-element>
pub fn dd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dd", attrs, children)
}

/// Has no special meaning of its own; it simply represents its children and is
/// useful for applying styling or scripting hooks to a group of elements.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-div-element>
pub fn div(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("div", attrs, children)
}

/// Represents an association list of zero or more name-value groups, each
/// consisting of terms and their descriptions.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-dl-element>
pub fn dl(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dl", attrs, children)
}

/// Represents the term, or name, part of a name-value group in a description
/// list.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-dt-element>
pub fn dt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dt", attrs, children)
}

/// Represents a caption or legend for the rest of the contents of its parent
/// figure element, if it has one.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-figcaption-element>
pub fn figcaption(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("figcaption", attrs, children)
}

/// Represents some flow content, optionally with a caption, that is self-
/// contained and would typically be referenced as a single unit from the
/// document's main flow.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-figure-element>
pub fn figure(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("figure", attrs, children)
}

/// Represents a paragraph-level thematic break, such as a scene change in a
/// story or a transition to a new topic within a section.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-hr-element>
pub fn hr(attrs: List(Attribute(message))) -> Element(message) {
  element("hr", attrs, constants.empty_list)
}

/// Represents an item in a list; its parent element determines what kind of
/// list it belongs to, such as an ordered list, unordered list, or menu.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-li-element>
pub fn li(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("li", attrs, children)
}

/// A semantic alternative to ul used to represent a toolbar consisting of an
/// unordered list of commands.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-menu-element>
pub fn menu(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("menu", attrs, children)
}

/// Represents a list of items where the order is meaningful, such that
/// rearranging the items would change the meaning of the document.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-ol-element>
pub fn ol(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ol", attrs, children)
}

/// Represents a paragraph, understood as a structural rather than a purely
/// visual unit of content.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-p-element>
pub fn p(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("p", attrs, children)
}

/// Represents a block of preformatted text, in which structure is conveyed
/// through typographic conventions rather than markup.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-pre-element>
pub fn pre(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("pre", attrs, children)
}

/// Represents a list of items where the order is not meaningful, such that
/// rearranging the items would not materially change the document's meaning.
///
/// <https://html.spec.whatwg.org/multipage/grouping-content.html#the-ul-element>
pub fn ul(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ul", attrs, children)
}

// HTML ELEMENTS: INLINE TEXT SEMANTICS ----------------------------------------

/// Together with an href attribute, creates a hyperlink to web pages, files,
/// email addresses, or any other kind of URL; without an href, it represents a
/// placeholder for where a link might otherwise be.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element>
pub fn a(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("a", attrs, children)
}

/// Represents an abbreviation or acronym, optionally with its expansion given
/// by the title attribute.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-abbr-element>
pub fn abbr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("abbr", attrs, children)
}

/// Draws the reader's attention to text without conveying any extra importance,
/// for instance keywords in a summary or product names in a review.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-b-element>
pub fn b(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("b", attrs, children)
}

/// Isolates a run of text that might be formatted in a different direction from
/// the surrounding text, useful when embedding user-generated content of
/// unknown directionality.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-bdi-element>
pub fn bdi(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdi", attrs, children)
}

/// Overrides the current directionality of text, so that its contents are
/// rendered in the explicitly given direction.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-bdo-element>
pub fn bdo(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdo", attrs, children)
}

/// Represents a line break within text content, such as in a poem or a mailing
/// address, where the division between lines is significant.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-br-element>
pub fn br(attrs: List(Attribute(message))) -> Element(message) {
  element("br", attrs, constants.empty_list)
}

/// Represents the title of a work, such as a book, play, song, film, or paper,
/// whether being cited in detail or merely mentioned in passing.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-cite-element>
pub fn cite(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("cite", attrs, children)
}

/// Represents a fragment of computer code, such as an element name, filename,
/// or a piece of a program.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-code-element>
pub fn code(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("code", attrs, children)
}

/// Links a piece of content with a machine-readable form of that content, given
/// by the value attribute.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-data-element>
pub fn data(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("data", attrs, children)
}

/// Represents the defining instance of a term, whose definition should be found
/// among the contents of the nearest enclosing paragraph, description list
/// group, or section.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-dfn-element>
pub fn dfn(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dfn", attrs, children)
}

/// Represents stress emphasis of its contents, where the degree of stress is
/// indicated by the number of ancestor em elements.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-em-element>
pub fn em(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("em", attrs, children)
}

/// Represents a span of text set off from the surrounding text for some reason,
/// such as idiomatic phrases, technical terms, or a shift in tone, without
/// implying extra importance.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-i-element>
pub fn i(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("i", attrs, children)
}

/// Represents user input, typically keyboard input, though it can also
/// represent other input such as voice commands.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-kbd-element>
pub fn kbd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("kbd", attrs, children)
}

/// Represents text that is marked or highlighted because of its relevance in
/// the surrounding context, such as search terms in a quoted passage.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-mark-element>
pub fn mark(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("mark", attrs, children)
}

/// Represents some phrasing content quoted from another source, with quotation
/// punctuation added automatically by the user agent's rendering rather than
/// typed by the author.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-q-element>
pub fn q(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("q", attrs, children)
}

/// Provides fallback parentheses for legacy user agents that do not support
/// ruby annotations, wrapping around an rt element's content.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-rp-element>
pub fn rp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rp", attrs, children)
}

/// Marks the ruby text component of a ruby annotation, used to give the
/// pronunciation, translation, or transliteration of the associated base text.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-rt-element>
pub fn rt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rt", attrs, children)
}

/// Allows one or more spans of text to be marked up with ruby annotations,
/// short runs of text presented alongside the base text, primarily used in East
/// Asian typography.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-ruby-element>
pub fn ruby(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ruby", attrs, children)
}

/// Represents contents that are no longer accurate or no longer relevant, as
/// distinct from document edits, for which the del element is used instead.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-s-element>
pub fn s(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("s", attrs, children)
}

/// Represents sample or quoted output from a computer program or other
/// computing system.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-samp-element>
pub fn samp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("samp", attrs, children)
}

/// Represents side comments such as small print, for instance disclaimers,
/// caveats, or licensing information.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-small-element>
pub fn small(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("small", attrs, children)
}

/// A generic container for phrasing content that does not itself represent
/// anything, useful for grouping elements for styling or scripting purposes.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-span-element>
pub fn span(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("span", attrs, children)
}

/// Represents strong importance, seriousness, or urgency for its contents, with
/// the degree given by the number of ancestor strong elements.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-strong-element>
pub fn strong(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("strong", attrs, children)
}

/// Specifies that its contents should be displayed as subscript, for
/// typographical reasons such as chemical formulas or footnote markers.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-sub-and-sup-elements>
pub fn sub(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sub", attrs, children)
}

/// Specifies that its contents should be displayed as superscript, for
/// typographical reasons such as exponents or ordinal indicators.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-sub-and-sup-elements>
pub fn sup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sup", attrs, children)
}

/// Represents its contents along with a machine-readable equivalent given by
/// the datetime attribute, restricted to dates, times, time zone offsets, and
/// durations.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-time-element>
pub fn time(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("time", attrs, children)
}

/// Represents a span of text with a non-textual annotation, such as marking it
/// as a proper name in a language that conventionally does so, without implying
/// any particular styling.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-u-element>
pub fn u(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("u", attrs, children)
}

/// Represents a variable, whether an actual variable in a mathematical
/// expression or programming context, or simply a placeholder term used in
/// prose.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-var-element>
pub fn var(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("var", attrs, children)
}

/// Represents a word break opportunity, a position at which the user agent may
/// choose to break a line that would otherwise be inconveniently long.
///
/// <https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-wbr-element>
pub fn wbr(attrs: List(Attribute(message))) -> Element(message) {
  element("wbr", attrs, constants.empty_list)
}

// HTML ELEMENTS: IMAGE AND MULTIMEDIA -----------------------------------------

/// Represents either a hyperlink with some text and a corresponding clickable
/// area on an image map, or a dead area that has no associated link.
///
/// <https://html.spec.whatwg.org/multipage/image-maps.html#the-area-element>
pub fn area(attrs: List(Attribute(message))) -> Element(message) {
  element("area", attrs, constants.empty_list)
}

/// Represents a sound or audio stream, given by its source attribute or child
/// source elements.
///
/// <https://html.spec.whatwg.org/multipage/media.html#the-audio-element>
pub fn audio(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("audio", attrs, children)
}

/// Embeds an image into the document, represented either by the image data
/// itself or, when unavailable, by the fallback text given in the alt
/// attribute.
///
/// <https://html.spec.whatwg.org/multipage/embedded-content.html#the-img-element>
pub fn img(attrs: List(Attribute(message))) -> Element(message) {
  element("img", attrs, constants.empty_list)
}

/// Used with <area> elements to define an image map (a clickable link area).
/// Together with any descendant area elements, defines an image map that can be
/// associated with an img element; the map element itself represents its
/// children.
///
/// <https://html.spec.whatwg.org/multipage/image-maps.html#the-map-element>
pub fn map(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("map", attrs, children)
}

/// Lets authors specify explicit external timed text tracks, such as subtitles
/// or captions, for a parent media element; it does not represent anything on
/// its own.
///
/// <https://html.spec.whatwg.org/multipage/media.html#the-track-element>
pub fn track(attrs: List(Attribute(message))) -> Element(message) {
  element("track", attrs, constants.empty_list)
}

/// Used for playing videos or movies, and can also be used for audio content
/// with the video's playback area repurposed to show captions.
///
/// <https://html.spec.whatwg.org/multipage/media.html#the-video-element>
pub fn video(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("video", attrs, children)
}

// HTML ELEMENTS: EMBEDDED CONTENT ---------------------------------------------

/// Provides an integration point for an external application or interactive
/// content, such as a plugin.
///
/// <https://html.spec.whatwg.org/multipage/iframe-embed-object.html#the-embed-element>
pub fn embed(attrs: List(Attribute(message))) -> Element(message) {
  element("embed", attrs, constants.empty_list)
}

/// Represents its nested browsing context, embedding another HTML page within
/// the current one.
///
/// <https://html.spec.whatwg.org/multipage/iframe-embed-object.html#the-iframe-element>
pub fn iframe(attrs: List(Attribute(message))) -> Element(message) {
  element("iframe", attrs, constants.empty_list)
}

/// Represents an external resource, which, depending on its type, is treated as
/// an image or as a nested browsing context.
///
/// <https://html.spec.whatwg.org/multipage/iframe-embed-object.html#the-object-element>
pub fn object(attrs: List(Attribute(message))) -> Element(message) {
  element("object", attrs, constants.empty_list)
}

/// A container providing multiple sources for its contained img element,
/// allowing the user agent to choose the most appropriate image based on screen
/// density, viewport size, or format.
///
/// <https://html.spec.whatwg.org/multipage/embedded-content.html#the-picture-element>
pub fn picture(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("picture", attrs, children)
}

/// Enables the embedding of another HTML page within the current one to allow
/// smoother navigation into new pages; an experimental element not (yet) part
/// of the WHATWG HTML Standard.
///
/// <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/portal>
pub fn portal(attrs: List(Attribute(message))) -> Element(message) {
  element("portal", attrs, constants.empty_list)
}

/// Allows authors to specify multiple alternative source sets for an img
/// element, or multiple alternative media resources for a video or audio
/// element; it does not represent anything on its own.
///
/// <https://html.spec.whatwg.org/multipage/embedded-content.html#the-source-element>
pub fn source(attrs: List(Attribute(message))) -> Element(message) {
  element("source", attrs, constants.empty_list)
}

// HTML ELEMENTS: SVG AND MATHML -----------------------------------------------

/// The top-level element for embedding MathML content into an HTML document.
///
/// <https://developer.mozilla.org/en-US/docs/Web/MathML/Element/math>
pub fn math(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced("http://www.w3.org/1998/Math/MathML", "math", attrs, children)
}

/// The top-level element for embedding SVG content into an HTML document.
///
/// <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/svg>
pub fn svg(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced("http://www.w3.org/2000/svg", "svg", attrs, children)
}

// HTML ELEMENTS: SCRIPTING ----------------------------------------------------

/// Provides scripts with a resolution-dependent bitmap canvas that can be used
/// to render graphs, game graphics, art, or other images dynamically.
///
/// <https://html.spec.whatwg.org/multipage/canvas.html#the-canvas-element>
pub fn canvas(attrs: List(Attribute(message))) -> Element(message) {
  element("canvas", attrs, constants.empty_list)
}

/// Represents nothing if scripting is enabled, but defines alternative content
/// to be used when scripting is disabled in the user's browser.
///
/// <https://html.spec.whatwg.org/multipage/scripting.html#the-noscript-element>
pub fn noscript(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("noscript", attrs, children)
}

/// Used to embed executable code or data, most commonly JavaScript.
///
/// <https://html.spec.whatwg.org/multipage/scripting.html#the-script-element>
pub fn script(attrs: List(Attribute(message)), js: String) -> Element(message) {
  element.unsafe_raw_html("", "script", attrs, js)
}

// HTML ELEMENTS: DEMARCATING EDITS ---------------------------------------------

/// Represents a removal from the document, such as a deleted passage in a
/// tracked-changes view.
///
/// <https://html.spec.whatwg.org/multipage/edits.html#the-del-element>
pub fn del(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("del", attrs, children)
}

/// Represents an addition to the document, such as an inserted passage in a
/// tracked-changes view.
///
/// <https://html.spec.whatwg.org/multipage/edits.html#the-ins-element>
pub fn ins(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("ins", attrs, children)
}

// HTML ELEMENTS: TABLE CONTENT ------------------------------------------------

/// Represents the title of the table that is its parent, if it has one.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-caption-element>
pub fn caption(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("caption", attrs, children)
}

/// Represents one or more columns in the column group represented by its parent
/// colgroup element.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-col-element>
pub fn col(attrs: List(Attribute(message))) -> Element(message) {
  element.element("col", attrs, constants.empty_list)
}

/// Represents a group of one or more columns in its parent table element.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-colgroup-element>
pub fn colgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("colgroup", attrs, children)
}

/// Represents data with more than one dimension, in the form of a table of rows
/// and columns of cells.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-table-element>
pub fn table(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("table", attrs, children)
}

/// Represents a block of rows that make up the body of data for its parent
/// table element.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-tbody-element>
pub fn tbody(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tbody", attrs, children)
}

/// Represents a data cell in a table.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-td-element>
pub fn td(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("td", attrs, children)
}

/// Represents the block of rows consisting of the column summaries for its
/// parent table element.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-tfoot-element>
pub fn tfoot(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tfoot", attrs, children)
}

/// Represents a header cell in a table, applying to a set of cells determined
/// by its scope attribute.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-th-element>
pub fn th(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("th", attrs, children)
}

/// Represents the block of rows consisting of the column labels and any
/// ancillary non-header cells for its parent table element.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-thead-element>
pub fn thead(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("thead", attrs, children)
}

/// Represents a row of cells in a table.
///
/// <https://html.spec.whatwg.org/multipage/tables.html#the-tr-element>
pub fn tr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("tr", attrs, children)
}

// HTML ELEMENTS: FORMS --------------------------------------------------------

/// Represents a button labeled by its contents, whose behavior when activated
/// is controlled by its type attribute, such as submitting or resetting a form.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-button-element>
pub fn button(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("button", attrs, children)
}

/// Represents a set of option elements giving predefined options to suggest to
/// the user for another control, most commonly a text input.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-datalist-element>
pub fn datalist(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("datalist", attrs, children)
}

/// Represents a set of form controls, optionally grouped under a caption given
/// by a child legend element.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-fieldset-element>
pub fn fieldset(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("fieldset", attrs, children)
}

/// Represents a collection of form-associated elements, some of which can
/// represent editable values that can be submitted to a server for processing.
///
/// <https://html.spec.whatwg.org/multipage/forms.html#the-form-element>
pub fn form(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("form", attrs, children)
}

/// Represents a typed data field, usually with a form control to allow the user
/// to edit that data, with the kind of control determined by its type
/// attribute.
///
/// <https://html.spec.whatwg.org/multipage/input.html#the-input-element>
pub fn input(attrs: List(Attribute(message))) -> Element(message) {
  element.element("input", attrs, constants.empty_list)
}

/// Represents a caption for an item in a user interface, associated with a
/// specific form control either through its for attribute or by containing the
/// control directly.
///
/// <https://html.spec.whatwg.org/multipage/forms.html#the-label-element>
pub fn label(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("label", attrs, children)
}

/// Represents a caption for the rest of the contents of its parent fieldset
/// element, if it has one.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-legend-element>
pub fn legend(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("legend", attrs, children)
}

/// Represents a scalar measurement within a known range, or a fractional value,
/// such as disk usage or the relevance of a search result.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-meter-element>
pub fn meter(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("meter", attrs, children)
}

/// Represents a group of option elements with a common label, used to organize
/// the choices offered by a select element.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-optgroup-element>
pub fn optgroup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("optgroup", attrs, children)
}

/// Represents an option in a select element, or as part of a list of
/// suggestions offered by a datalist element.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-option-element>
pub fn option(
  attrs: List(Attribute(message)),
  label: String,
) -> Element(message) {
  element.element("option", attrs, [element.text(label)])
}

/// Represents the result of a calculation performed by the page, or the result
/// of a user action, typically updated via script.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-output-element>
pub fn output(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("output", attrs, children)
}

/// Represents the completion progress of a task, whether a determinate task
/// with a known amount of work still to do, or an indeterminate one.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-progress-element>
pub fn progress(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("progress", attrs, children)
}

/// Represents a control for selecting one or more options from a set, rendered
/// as a drop-down box or a scrolling list box.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-select-element>
pub fn select(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("select", attrs, children)
}

/// Represents a multiline plain-text edit control for the element's raw value,
/// allowing users to enter a sizeable amount of free-form text.
///
/// <https://html.spec.whatwg.org/multipage/form-elements.html#the-textarea-element>
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

/// Represents a disclosure widget from which the user can obtain additional
/// information or controls, whose contents are only shown when the element is
/// toggled into an open state.
///
/// <https://html.spec.whatwg.org/multipage/interactive-elements.html#the-details-element>
pub fn details(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("details", attrs, children)
}

/// Represents a dialog box or other interactive component, such as a modal
/// window, an inspector, or a subwindow.
///
/// <https://html.spec.whatwg.org/multipage/interactive-elements.html#the-dialog-element>
pub fn dialog(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("dialog", attrs, children)
}

/// Represents a summary, caption, or legend for the rest of the contents of its
/// parent details element, if it has one, and acts as the widget's control for
/// toggling the details open or closed.
///
/// <https://html.spec.whatwg.org/multipage/interactive-elements.html#the-summary-element>
pub fn summary(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("summary", attrs, children)
}

// HTML ELEMENTS: WEB COMPONENTS -----------------------------------------------

/// A placeholder inside a web component's shadow tree that can be filled with
/// markup by the component's user, letting separate DOM trees be composed
/// together.
///
/// <https://html.spec.whatwg.org/multipage/scripting.html#the-slot-element>
pub fn slot(
  attrs: List(Attribute(message)),
  fallback: List(Element(message)),
) -> Element(message) {
  element.element("slot", attrs, fallback)
}

/// A mechanism for holding HTML fragments that are not rendered when the page
/// loads but can be cloned and inserted into the document later using script.
///
/// <https://html.spec.whatwg.org/multipage/scripting.html#the-template-element>
pub fn template(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("template", attrs, children)
}
