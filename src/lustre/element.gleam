////

// IMPORTS ---------------------------------------------------------------------
import lustre/attribute.{ Attribute, attribute }

// TYPES -----------------------------------------------------------------------

///
pub external type Element(action)

// CONSTRUCTORS ----------------------------------------------------------------

///
pub external fn element (tag: String, attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action)
    = "./ffi.mjs" "element"

pub external fn fragment (children: List(Element(action))) -> Element(action)
    = "./ffi.mjs" "fragment"

pub external fn text (content: String) -> Element(action)
    = "./ffi.mjs" "text"


// CONSTRUCTING NODES ----------------------------------------------------------
// This list and grouping of nodes has been taken from the MDN reference at:
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element

// MAIN ROOT:
pub fn html (attributes: List(Attribute(action)), head: Element(action), body: Element(action)) -> Element(action) {
    element("html", attributes, [ head, body ])
}


// DOCUMENT METADATA:
pub fn base (attributes: List(Attribute(action))) -> Element(action) {
    element("base", attributes, [])
}

pub fn head (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("head", attributes, children)
}

pub fn meta (attributes: List(Attribute(action))) -> Element(action) {
    element("meta", attributes, [])
}

pub fn style (attributes: List(Attribute(action)), css: String) -> Element(action) {
    element("style", attributes, [ text(css) ])
}

pub fn title (attributes: List(Attribute(action)), name: String) -> Element(action) {
    element("title", attributes, [ text(name) ])
}

// SECTIONING ROOT:
pub fn body (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("body", attributes, children)
}


// CONTENT SECTIONING:
pub fn address (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("address", attributes, children)
}

pub fn article (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("article", attributes, children)
}

pub fn aside (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("aside", attributes, children)
}

pub fn footer (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("footer", attributes, children)
}

pub fn header (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("header", attributes, children)
}

pub fn h1 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h1", attributes, children)
}

pub fn h2 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h2", attributes, children)
}

pub fn h3 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h3", attributes, children)
}

pub fn h4 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h4", attributes, children)
}

pub fn h5 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h5", attributes, children)
}

pub fn h6 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("h6", attributes, children)
}

pub fn main (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("main", attributes, children)
}

pub fn nav (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("nav", attributes, children)
}

pub fn section (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("section", attributes, children)
}


// TEXT CONTENT:
pub fn blockquote (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("blockquote", attributes, children)
}

pub fn dd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("dd", attributes, children)
}

pub fn div (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("div", attributes, children)
}

pub fn dl (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("dl", attributes, children)
}

pub fn dt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("dt", attributes, children)
}

pub fn figcaption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("figcaption", attributes, children)
}

pub fn figure (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("figure", attributes, children)
}

pub fn hr (attributes: List(Attribute(action))) -> Element(action) {
    element("hr", attributes, [])
}

pub fn li (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("li", attributes, children)
}

pub fn menu (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("menu", attributes, children)
}

pub fn ol (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("ol", attributes, children)
}

pub fn p (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("p", attributes, children)
}

pub fn pre (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("pre", attributes, children)
}

pub fn ul (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("ul", attributes, children)
}


// INLINE TEXT SEMANTICS:
pub fn a (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("a", attributes, children)
}

pub fn abbr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("abbr", attributes, children)
}

pub fn b (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("b", attributes, children)
}

pub fn bdi (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("bdi", attributes, children)
}

pub fn bdo (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("bdo", attributes, children)
}

pub fn br (attributes: List(Attribute(action))) -> Element(action) {
    element("br", attributes, [])
}

pub fn cite (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("cite", attributes, children)
}

pub fn code (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("code", attributes, children)
}

pub fn dfn (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("dfn", attributes, children)
}

pub fn em (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("em", attributes, children)
}

pub fn i (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("i", attributes, children)
}

pub fn kbd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("kbd", attributes, children)
}

pub fn mark (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("mark", attributes, children)
}

pub fn rp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("rp", attributes, children)
}

pub fn rt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("rt", attributes, children)
}

pub fn ruby (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("ruby", attributes, children)
}

pub fn s (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("s", attributes, children)
}

pub fn samp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("samp", attributes, children)
}

pub fn small (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("small", attributes, children)
}

pub fn span (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("span", attributes, children)
}

pub fn strong (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("strong", attributes, children)
}

pub fn sub (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("sub", attributes, children)
}

pub fn sup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("sup", attributes, children)
}

pub fn time (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("time", attributes, children)
}

pub fn u (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("u", attributes, children)
}

pub fn var_ (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("var", attributes, children)
}

pub fn wbr (attributes: List(Attribute(action))) -> Element(action) {
    element("wbr", attributes, [])
}


// IMAGE AND MULTIMEDIA:
pub fn area (attributes: List(Attribute(action))) -> Element(action) {
    element("area", attributes, [])
}

pub fn audio (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("audio", attributes, children)
}

pub fn img (attributes: List(Attribute(action))) -> Element(action) {
    element("img", attributes, [])
}

pub fn map (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("map", attributes, children)
}

pub fn track (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("track", attributes, children)
}

pub fn video (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("video", attributes, children)
}


// EMBEDDED CONTENT:
pub fn embed (attributes: List(Attribute(action))) -> Element(action) {
    element("embed", attributes, [])
}

pub fn iframe (attributes: List(Attribute(action))) -> Element(action) {
    element("iframe", attributes, [])
}

pub fn object (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("object", attributes, children)
}

pub fn param (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("param", attributes, children)
}

pub fn picture (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("picture", attributes, children)
}

pub fn portal (attributes: List(Attribute(action))) -> Element(action) {
    element("portal", attributes, [])
}

pub fn source (attributes: List(Attribute(action))) -> Element(action) {
    element("source", attributes, [])
}


// SVG AND MATHML:
pub fn svg (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("svg", [attribute("xmlns", "http://www.w3.org/2000/svg"), ..attributes], children)
}

pub fn mathml (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("mathml", [attribute("xmlns", "http://www.w3.org/1998/Math/MathML"), ..attributes], children)
}

// SCRIPTING:
pub fn canvas (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("canvas", attributes, children)
}

pub fn noscript (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("noscript", attributes, children)
}


// DEMARCATING EDITS:
pub fn del (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("del", attributes, children)
}

pub fn ins (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("ins", attributes, children)
}


// TABLE CONTENT:
pub fn caption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("caption", attributes, children)
}

pub fn col (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("col", attributes, children)
}

pub fn colgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("colgroup", attributes, children)
}

pub fn table (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("table", attributes, children)
}

pub fn tbody (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("tbody", attributes, children)
}

pub fn td (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("td", attributes, children)
}

pub fn tfoot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("tfoot", attributes, children)
}

pub fn th (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("th", attributes, children)
}

pub fn thead (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("thead", attributes, children)
}

pub fn tr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("tr", attributes, children)
}


// FORMS:
pub fn button (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("button", attributes, children)
}

pub fn datalist (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("datalist", attributes, children)
}

pub fn fieldset (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("fieldset", attributes, children)
}

pub fn form (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("form", attributes, children)
}

pub fn input (attributes: List(Attribute(action))) -> Element(action) {
    element("input", attributes, [])
}

pub fn label (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("label", attributes, children)
}

pub fn legend (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("legend", attributes, children)
}

pub fn meter (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("meter", attributes, children)
}

pub fn optgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("optgroup", attributes, children)
}

pub fn option (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("option", attributes, children)
}

pub fn output (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("output", attributes, children)
}

pub fn progress (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("progress", attributes, children)
}

pub fn select (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("select", attributes, children)
}

pub fn textarea (attributes: List(Attribute(action))) -> Element(action) {
    element("textarea", attributes, [])
}


// INTERACTIVE ELEMENTS:
pub fn details (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("details", attributes, children)
}

pub fn dialog (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("dialog", attributes, children)
}

pub fn summary (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("summary", attributes, children)
}


// WEB COMPONENTS:
pub fn slot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("slot", attributes, children)
}

pub fn template (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    element("template", attributes, children)
}
