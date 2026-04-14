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

///
pub fn a(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("a", attrs, children)
}

///
pub fn abbr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("abbr", attrs, children)
}

///
pub fn b(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("b", attrs, children)
}

///
pub fn bdi(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdi", attrs, children)
}

///
pub fn bdo(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("bdo", attrs, children)
}

///
pub fn br(attrs: List(Attribute(message))) -> Element(message) {
  element("br", attrs, constants.empty_list)
}

///
pub fn cite(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("cite", attrs, children)
}

///
pub fn code(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("code", attrs, children)
}

///
pub fn data(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("data", attrs, children)
}

///
pub fn dfn(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("dfn", attrs, children)
}

///
pub fn em(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("em", attrs, children)
}

///
pub fn i(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("i", attrs, children)
}

///
pub fn kbd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("kbd", attrs, children)
}

///
pub fn mark(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("mark", attrs, children)
}

///
pub fn q(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("q", attrs, children)
}

///
pub fn rp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rp", attrs, children)
}

///
pub fn rt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("rt", attrs, children)
}

///
pub fn ruby(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("ruby", attrs, children)
}

///
pub fn s(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("s", attrs, children)
}

///
pub fn samp(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("samp", attrs, children)
}

///
pub fn small(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("small", attrs, children)
}

///
pub fn span(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("span", attrs, children)
}

///
pub fn strong(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("strong", attrs, children)
}

///
pub fn sub(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sub", attrs, children)
}

///
pub fn sup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("sup", attrs, children)
}

///
pub fn time(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("time", attrs, children)
}

///
pub fn u(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("u", attrs, children)
}

///
pub fn var(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element("var", attrs, children)
}

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
