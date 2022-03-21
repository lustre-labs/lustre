////

// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{ Attribute, attribute }

// TYPES -----------------------------------------------------------------------

///
pub external type Element(action)

// CONSTRUCTORS ----------------------------------------------------------------

/// Construct a plain HTML element or registered Web Component by providing the
/// tag name, a list of attributes (including event handlers), and a list of
/// child elements.
///
pub external fn html (tag: String, attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action)
    = "./ffi.mjs" "html"

/// A stateful element is exactly what it sounds like: some element with local
/// encapsulated state! The `render` function we must provide is called with the
/// element's current state as well as a function to set a new state. Whenever
/// that function is called, the element is re-rendered.
///
/// You might be wondering where the `stateless` version of this function is. 
/// Those are just regular Gleam functions that return `Element`s!
///
pub external fn stateful (init: state, render: fn (state, fn (state) -> Nil) -> Element(action)) -> Element(action)
    = "./ffi.mjs" "stateful"

/// A fragment doesn't appear in the DOM, but allows us to treat a list of elements
/// as if it were a single one. 
///
pub external fn fragment (children: List(Element(action))) -> Element(action)
    = "./ffi.mjs" "fragment"

/// Render a Gleam string as an HTML text node.
///
pub external fn text (content: String) -> Element(action)
    = "./ffi.mjs" "text"


// MANIPULATIONS ---------------------------------------------------------------

/// Transforms the actions produced by some element.
///
pub external fn map (element: Element(a), f: fn (a) -> b) -> Element(b)
    = "./ffi.mjs" "map"


// CONSTRUCTING NODES ----------------------------------------------------------
// This list and grouping of nodes has been taken from the MDN reference at:
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element

// MAIN ROOT -------------------------------------------------------------------

///
pub fn html_ (attributes: List(Attribute(action)), head: Element(action), body: Element(action)) -> Element(action) {
    html("html", attributes, [ head, body ])
}


// DOCUMENT METADATA -----------------------------------------------------------

///
pub fn base (attributes: List(Attribute(action))) -> Element(action) {
    html("base", attributes, [])
}

///
pub fn head (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("head", attributes, children)
}

///
pub fn meta (attributes: List(Attribute(action))) -> Element(action) {
    html("meta", attributes, [])
}

///
pub fn style (attributes: List(Attribute(action)), css: String) -> Element(action) {
    html("style", attributes, [ text(css) ])
}

///
pub fn title (attributes: List(Attribute(action)), name: String) -> Element(action) {
    html("title", attributes, [ text(name) ])
}

// SECTIONING ROOT -------------------------------------------------------------

///
pub fn body (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("body", attributes, children)
}


// CONTENT SECTIONING ----------------------------------------------------------

///
pub fn address (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("address", attributes, children)
}

///
pub fn article (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("article", attributes, children)
}

///
pub fn aside (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("aside", attributes, children)
}

///
pub fn footer (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("footer", attributes, children)
}

///
pub fn header (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("header", attributes, children)
}

///
pub fn h1 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h1", attributes, children)
}

///
pub fn h2 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h2", attributes, children)
}

///
pub fn h3 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h3", attributes, children)
}

///
pub fn h4 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h4", attributes, children)
}

///
pub fn h5 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h5", attributes, children)
}

///
pub fn h6 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("h6", attributes, children)
}

///
pub fn main (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("main", attributes, children)
}

///
pub fn nav (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("nav", attributes, children)
}

///
pub fn section (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("section", attributes, children)
}


// TEXT CONTENT ----------------------------------------------------------------

///
pub fn blockquote (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("blockquote", attributes, children)
}

///
pub fn dd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("dd", attributes, children)
}

///
pub fn div (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("div", attributes, children)
}

///
pub fn dl (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("dl", attributes, children)
}

///
pub fn dt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("dt", attributes, children)
}

///
pub fn figcaption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("figcaption", attributes, children)
}

///
pub fn figure (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("figure", attributes, children)
}

///
pub fn hr (attributes: List(Attribute(action))) -> Element(action) {
    html("hr", attributes, [])
}

///
pub fn li (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("li", attributes, children)
}

///
pub fn menu (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("menu", attributes, children)
}

///
pub fn ol (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("ol", attributes, children)
}

///
pub fn p (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("p", attributes, children)
}

///
pub fn pre (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("pre", attributes, children)
}

///
pub fn ul (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("ul", attributes, children)
}


// INLINE TEXT SEMANTICS -------------------------------------------------------

///
pub fn a (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("a", attributes, children)
}

///
pub fn abbr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("abbr", attributes, children)
}

///
pub fn b (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("b", attributes, children)
}

///
pub fn bdi (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("bdi", attributes, children)
}

///
pub fn bdo (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("bdo", attributes, children)
}

///
pub fn br (attributes: List(Attribute(action))) -> Element(action) {
    html("br", attributes, [])
}

///
pub fn cite (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("cite", attributes, children)
}

///
pub fn code (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("code", attributes, children)
}

///
pub fn dfn (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("dfn", attributes, children)
}

///
pub fn em (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("em", attributes, children)
}

///
pub fn i (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("i", attributes, children)
}

///
pub fn kbd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("kbd", attributes, children)
}

///
pub fn mark (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("mark", attributes, children)
}

///
pub fn rp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("rp", attributes, children)
}

///
pub fn rt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("rt", attributes, children)
}

///
pub fn ruby (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("ruby", attributes, children)
}

///
pub fn s (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("s", attributes, children)
}

///
pub fn samp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("samp", attributes, children)
}

///
pub fn small (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("small", attributes, children)
}

///
pub fn span (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("span", attributes, children)
}

///
pub fn strong (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("strong", attributes, children)
}

///
pub fn sub (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("sub", attributes, children)
}

///
pub fn sup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("sup", attributes, children)
}

///
pub fn time (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("time", attributes, children)
}

///
pub fn u (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("u", attributes, children)
}

///
pub fn var_ (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("var", attributes, children)
}

///
pub fn wbr (attributes: List(Attribute(action))) -> Element(action) {
    html("wbr", attributes, [])
}


// IMAGE AND MULTIMEDIA --------------------------------------------------------

///
pub fn area (attributes: List(Attribute(action))) -> Element(action) {
    html("area", attributes, [])
}

///
pub fn audio (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("audio", attributes, children)
}

///
pub fn img (attributes: List(Attribute(action))) -> Element(action) {
    html("img", attributes, [])
}

///
pub fn map_ (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("map", attributes, children)
}

///
pub fn track (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("track", attributes, children)
}

///
pub fn video (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("video", attributes, children)
}


// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn embed (attributes: List(Attribute(action))) -> Element(action) {
    html("embed", attributes, [])
}

///
pub fn iframe (attributes: List(Attribute(action))) -> Element(action) {
    html("iframe", attributes, [])
}

///
pub fn object (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("object", attributes, children)
}

///
pub fn param (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("param", attributes, children)
}

///
pub fn picture (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("picture", attributes, children)
}

///
pub fn portal (attributes: List(Attribute(action))) -> Element(action) {
    html("portal", attributes, [])
}

///
pub fn source (attributes: List(Attribute(action))) -> Element(action) {
    html("source", attributes, [])
}


// SVG AND MATHML --------------------------------------------------------------

///
pub fn svg (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("svg", [attribute("xmlns", "http://www.w3.org/2000/svg"), ..attributes], children)
}

///
pub fn mathml (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("mathml", [attribute("xmlns", "http://www.w3.org/1998/Math/MathML"), ..attributes], children)
}

// SCRIPTING -------------------------------------------------------------------

///
pub fn canvas (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("canvas", attributes, children)
}

///
pub fn noscript (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("noscript", attributes, children)
}


// DEMARCATING EDITS -----------------------------------------------------------

///
pub fn del (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("del", attributes, children)
}

///
pub fn ins (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("ins", attributes, children)
}


// TABLE CONTENT ---------------------------------------------------------------

///
pub fn caption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("caption", attributes, children)
}

///
pub fn col (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("col", attributes, children)
}

///
pub fn colgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("colgroup", attributes, children)
}

///
pub fn table (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("table", attributes, children)
}

///
pub fn tbody (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("tbody", attributes, children)
}

///
pub fn td (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("td", attributes, children)
}

///
pub fn tfoot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("tfoot", attributes, children)
}

///
pub fn th (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("th", attributes, children)
}

///
pub fn thead (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("thead", attributes, children)
}

///
pub fn tr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("tr", attributes, children)
}


// FORMS -----------------------------------------------------------------------

///
pub fn button (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("button", attributes, children)
}

///
pub fn datalist (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("datalist", attributes, children)
}

///
pub fn fieldset (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("fieldset", attributes, children)
}

///
pub fn form (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("form", attributes, children)
}

///
pub fn input (attributes: List(Attribute(action))) -> Element(action) {
    html("input", attributes, [])
}

///
pub fn label (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("label", attributes, children)
}

///
pub fn legend (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("legend", attributes, children)
}

///
pub fn meter (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("meter", attributes, children)
}

///
pub fn optgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("optgroup", attributes, children)
}

///
pub fn option (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("option", attributes, children)
}

///
pub fn output (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("output", attributes, children)
}

///
pub fn progress (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("progress", attributes, children)
}

///
pub fn select (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("select", attributes, children)
}

///
pub fn textarea (attributes: List(Attribute(action))) -> Element(action) {
    html("textarea", attributes, [])
}


// INTERACTIVE ELEMENTS --------------------------------------------------------

///
pub fn details (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("details", attributes, children)
}

///
pub fn dialog (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("dialog", attributes, children)
}

///
pub fn summary (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("summary", attributes, children)
}


// WEB COMPONENTS --------------------------------------------------------------

///
pub fn slot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("slot", attributes, children)
}

///
pub fn template (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    html("template", attributes, children)
}
