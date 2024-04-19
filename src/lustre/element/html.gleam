// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute}
import lustre/element.{type Element, element, namespaced}

// HTML ELEMENTS: MAIN ROOT ----------------------------------------------------

///
pub fn html(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("html", attrs, children)
}

pub fn text(content: String) -> Element(msg) {
  element.text(content)
}

// HTML ELEMENTS: DOCUMENT METADATA --------------------------------------------

///
pub fn base(attrs: List(Attribute(msg))) -> Element(msg) {
  element("base", attrs, [])
}

///
pub fn head(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("head", attrs, children)
}

///
pub fn link(attrs: List(Attribute(msg))) -> Element(msg) {
  element("link", attrs, [])
}

///
pub fn meta(attrs: List(Attribute(msg))) -> Element(msg) {
  element("meta", attrs, [])
}

///
pub fn style(attrs: List(Attribute(msg)), css: String) -> Element(msg) {
  element("style", attrs, [text(css)])
}

///
pub fn title(attrs: List(Attribute(msg)), content: String) -> Element(msg) {
  element("title", attrs, [text(content)])
}

// HTML ELEMENTS: SECTIONING ROOT -----------------------------------------------

///
pub fn body(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("body", attrs, children)
}

// HTML ELEMENTS: CONTENT SECTIONING -------------------------------------------

///
pub fn address(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("address", attrs, children)
}

///
pub fn article(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("article", attrs, children)
}

///
pub fn aside(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("aside", attrs, children)
}

///
pub fn footer(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("footer", attrs, children)
}

///
pub fn header(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("header", attrs, children)
}

///
pub fn h1(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h1", attrs, children)
}

///
pub fn h2(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h2", attrs, children)
}

///
pub fn h3(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h3", attrs, children)
}

///
pub fn h4(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h4", attrs, children)
}

///
pub fn h5(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h5", attrs, children)
}

///
pub fn h6(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("h6", attrs, children)
}

///
pub fn hgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("hgroup", attrs, children)
}

///
pub fn main(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("main", attrs, children)
}

///
pub fn nav(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("nav", attrs, children)
}

///
pub fn section(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("section", attrs, children)
}

///
pub fn search(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("search", attrs, children)
}

// HTML ELEMENTS: TEXT CONTENT -------------------------------------------------

///
pub fn blockquote(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("blockquote", attrs, children)
}

///
pub fn dd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dd", attrs, children)
}

///
pub fn div(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("div", attrs, children)
}

///
pub fn dl(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dl", attrs, children)
}

///
pub fn dt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dt", attrs, children)
}

///
pub fn figcaption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("figcaption", attrs, children)
}

///
pub fn figure(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("figure", attrs, children)
}

///
pub fn hr(attrs: List(Attribute(msg))) -> Element(msg) {
  element("hr", attrs, [])
}

///
pub fn li(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("li", attrs, children)
}

///
pub fn menu(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("menu", attrs, children)
}

///
pub fn ol(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ol", attrs, children)
}

///
pub fn p(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("p", attrs, children)
}

///
pub fn pre(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("pre", attrs, children)
}

///
pub fn ul(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ul", attrs, children)
}

// HTML ELEMENTS: INLINE TEXT SEMANTICS ----------------------------------------

///
pub fn a(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("a", attrs, children)
}

///
pub fn abbr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("abbr", attrs, children)
}

///
pub fn b(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("b", attrs, children)
}

///
pub fn bdi(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("bdi", attrs, children)
}

///
pub fn bdo(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("bdo", attrs, children)
}

///
pub fn br(attrs: List(Attribute(msg))) -> Element(msg) {
  element("br", attrs, [])
}

///
pub fn cite(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("cite", attrs, children)
}

///
pub fn code(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("code", attrs, children)
}

///
pub fn data(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("data", attrs, children)
}

///
pub fn dfn(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("dfn", attrs, children)
}

///
pub fn em(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("em", attrs, children)
}

///
pub fn i(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("i", attrs, children)
}

///
pub fn kbd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("kbd", attrs, children)
}

///
pub fn mark(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("mark", attrs, children)
}

///
pub fn q(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("q", attrs, children)
}

///
pub fn rp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("rp", attrs, children)
}

///
pub fn rt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("rt", attrs, children)
}

///
pub fn ruby(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("ruby", attrs, children)
}

///
pub fn s(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("s", attrs, children)
}

///
pub fn samp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("samp", attrs, children)
}

///
pub fn small(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("small", attrs, children)
}

///
pub fn span(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("span", attrs, children)
}

///
pub fn strong(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("strong", attrs, children)
}

///
pub fn sub(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("sub", attrs, children)
}

///
pub fn sup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("sup", attrs, children)
}

///
pub fn time(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("time", attrs, children)
}

///
pub fn u(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("u", attrs, children)
}

///
pub fn var(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("var", attrs, children)
}

///
pub fn wbr(attrs: List(Attribute(msg))) -> Element(msg) {
  element("wbr", attrs, [])
}

// HTML ELEMENTS: IMAGE AND MULTIMEDIA -----------------------------------------

///
pub fn area(attrs: List(Attribute(msg))) -> Element(msg) {
  element("area", attrs, [])
}

///
pub fn audio(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("audio", attrs, children)
}

///
pub fn img(attrs: List(Attribute(msg))) -> Element(msg) {
  element("img", attrs, [])
}

/// Used with <area> elements to define an image map (a clickable link area).
///
pub fn map(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("map", attrs, children)
}

///
pub fn track(attrs: List(Attribute(msg))) -> Element(msg) {
  element("track", attrs, [])
}

///
pub fn video(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("video", attrs, children)
}

// HTML ELEMENTS: EMBEDDED CONTENT ---------------------------------------------

///
pub fn embed(attrs: List(Attribute(msg))) -> Element(msg) {
  element("embed", attrs, [])
}

///
pub fn iframe(attrs: List(Attribute(msg))) -> Element(msg) {
  element("iframe", attrs, [])
}

///
pub fn object(attrs: List(Attribute(msg))) -> Element(msg) {
  element("object", attrs, [])
}

///
pub fn picture(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("picture", attrs, children)
}

///
pub fn portal(attrs: List(Attribute(msg))) -> Element(msg) {
  element("portal", attrs, [])
}

///
pub fn source(attrs: List(Attribute(msg))) -> Element(msg) {
  element("source", attrs, [])
}

// HTML ELEMENTS: SVG AND MATHML -----------------------------------------------

///
pub fn svg(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/2000/svg", "svg", attrs, children)
}

///
pub fn math(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("math", attrs, children)
}

// HTML ELEMENTS: SCRIPTING ----------------------------------------------------

///
pub fn canvas(attrs: List(Attribute(msg))) -> Element(msg) {
  element("canvas", attrs, [])
}

///
pub fn noscript(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element("noscript", attrs, children)
}

///
pub fn script(attrs: List(Attribute(msg)), js: String) -> Element(msg) {
  element("script", attrs, [text(js)])
}

// HTML ELEMENTS: DEMARCATING EDITS ---------------------------------------------

///
pub fn del(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("del", attrs, children)
}

///
pub fn ins(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("ins", attrs, children)
}

// HTML ELEMENTS: TABLE CONTENT ------------------------------------------------

///
pub fn caption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("caption", attrs, children)
}

///
pub fn col(attrs: List(Attribute(msg))) -> Element(msg) {
  element.element("col", attrs, [])
}

///
pub fn colgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("colgroup", attrs, children)
}

///
pub fn table(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("table", attrs, children)
}

///
pub fn tbody(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tbody", attrs, children)
}

///
pub fn td(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("td", attrs, children)
}

///
pub fn tfoot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tfoot", attrs, children)
}

///
pub fn th(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("th", attrs, children)
}

///
pub fn thead(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("thead", attrs, children)
}

///
pub fn tr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("tr", attrs, children)
}

// HTML ELEMENTS: FORMS --------------------------------------------------------

///
pub fn button(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("button", attrs, children)
}

///
pub fn datalist(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("datalist", attrs, children)
}

///
pub fn fieldset(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("fieldset", attrs, children)
}

///
pub fn form(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("form", attrs, children)
}

///
pub fn input(attrs: List(Attribute(msg))) -> Element(msg) {
  element.element("input", attrs, [])
}

///
pub fn label(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("label", attrs, children)
}

///
pub fn legend(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("legend", attrs, children)
}

///
pub fn meter(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("meter", attrs, children)
}

///
pub fn optgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("optgroup", attrs, children)
}

///
pub fn option(attrs: List(Attribute(msg)), label: String) -> Element(msg) {
  element.element("option", attrs, [element.text(label)])
}

///
pub fn output(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("output", attrs, children)
}

///
pub fn progress(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("progress", attrs, children)
}

///
pub fn select(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("select", attrs, children)
}

///
pub fn textarea(attrs: List(Attribute(msg)), content: String) -> Element(msg) {
  element.element("textarea", attrs, [element.text(content)])
}

// HTML ELEMENTS: INTERACTIVE ELEMENTS -----------------------------------------

///
pub fn details(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("details", attrs, children)
}

///
pub fn dialog(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("dialog", attrs, children)
}

///
pub fn summary(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("summary", attrs, children)
}

// HTML ELEMENTS: WEB COMPONENTS -----------------------------------------------

///
pub fn slot(attrs: List(Attribute(msg))) -> Element(msg) {
  element.element("slot", attrs, [])
}

///
pub fn template(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("template", attrs, children)
}
