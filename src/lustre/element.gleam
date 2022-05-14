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
pub external fn node (tag: String, attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action)
    = "../lustre.mjs" "node"

/// A stateful element is exactly what it sounds like: some element with local
/// encapsulated state! The `render` function we must provide is called with the
/// element's current state as well as a function to set a new state. Whenever
/// that function is called, the element is re-rendered.
///
/// You might be wondering where the `stateless` version of this function is. 
/// Those are just regular Gleam functions that return `Element`s!
///
pub external fn stateful (init: state, render: fn (state, fn (state) -> Nil) -> Element(action)) -> Element(action)
    = "../lustre.mjs" "stateful"

/// A fragment doesn't appear in the DOM, but allows us to treat a list of elements
/// as if it were a single one. 
///
pub external fn fragment (children: List(Element(action))) -> Element(action)
    = "../lustre.mjs" "fragment"

/// Render a Gleam string as an HTML text node.
///
pub external fn text (content: String) -> Element(action)
    = "../lustre.mjs" "text"


// MANIPULATIONS ---------------------------------------------------------------

/// Transforms the actions produced by some element.
///
pub external fn map (element: Element(a), f: fn (a) -> b) -> Element(b)
    = "../lustre.mjs" "map"


// CONSTRUCTING NODES ----------------------------------------------------------
// This list and grouping of nodes has been taken from the MDN reference at:
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element

// MAIN ROOT -------------------------------------------------------------------

///
pub fn html (attributes: List(Attribute(action)), head: Element(action), body: Element(action)) -> Element(action) {
    node("html", attributes, [ head, body ])
}


// DOCUMENT METADATA -----------------------------------------------------------

///
pub fn base (attributes: List(Attribute(action))) -> Element(action) {
    node("base", attributes, [])
}

///
pub fn head (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("head", attributes, children)
}

///
pub fn meta (attributes: List(Attribute(action))) -> Element(action) {
    node("meta", attributes, [])
}

///
pub fn style (attributes: List(Attribute(action)), css: String) -> Element(action) {
    node("style", attributes, [ text(css) ])
}

///
pub fn title (attributes: List(Attribute(action)), name: String) -> Element(action) {
    node("title", attributes, [ text(name) ])
}

// SECTIONING ROOT -------------------------------------------------------------

///
pub fn body (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("body", attributes, children)
}


// CONTENT SECTIONING ----------------------------------------------------------

///
pub fn address (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("address", attributes, children)
}

///
pub fn article (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("article", attributes, children)
}

///
pub fn aside (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("aside", attributes, children)
}

///
pub fn footer (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("footer", attributes, children)
}

///
pub fn header (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("header", attributes, children)
}

///
pub fn h1 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h1", attributes, children)
}

///
pub fn h2 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h2", attributes, children)
}

///
pub fn h3 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h3", attributes, children)
}

///
pub fn h4 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h4", attributes, children)
}

///
pub fn h5 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h5", attributes, children)
}

///
pub fn h6 (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("h6", attributes, children)
}

///
pub fn main (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("main", attributes, children)
}

///
pub fn nav (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("nav", attributes, children)
}

///
pub fn section (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("section", attributes, children)
}


// TEXT CONTENT ----------------------------------------------------------------

///
pub fn blockquote (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("blockquote", attributes, children)
}

///
pub fn dd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("dd", attributes, children)
}

///
pub fn div (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("div", attributes, children)
}

///
pub fn dl (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("dl", attributes, children)
}

///
pub fn dt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("dt", attributes, children)
}

///
pub fn figcaption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("figcaption", attributes, children)
}

///
pub fn figure (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("figure", attributes, children)
}

///
pub fn hr (attributes: List(Attribute(action))) -> Element(action) {
    node("hr", attributes, [])
}

///
pub fn li (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("li", attributes, children)
}

///
pub fn menu (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("menu", attributes, children)
}

///
pub fn ol (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("ol", attributes, children)
}

///
pub fn p (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("p", attributes, children)
}

///
pub fn pre (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("pre", attributes, children)
}

///
pub fn ul (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("ul", attributes, children)
}


// INLINE TEXT SEMANTICS -------------------------------------------------------

///
pub fn a (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("a", attributes, children)
}

///
pub fn abbr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("abbr", attributes, children)
}

///
pub fn b (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("b", attributes, children)
}

///
pub fn bdi (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("bdi", attributes, children)
}

///
pub fn bdo (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("bdo", attributes, children)
}

///
pub fn br (attributes: List(Attribute(action))) -> Element(action) {
    node("br", attributes, [])
}

///
pub fn cite (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("cite", attributes, children)
}

///
pub fn code (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("code", attributes, children)
}

///
pub fn dfn (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("dfn", attributes, children)
}

///
pub fn em (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("em", attributes, children)
}

///
pub fn i (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("i", attributes, children)
}

///
pub fn kbd (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("kbd", attributes, children)
}

///
pub fn mark (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("mark", attributes, children)
}

///
pub fn rp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("rp", attributes, children)
}

///
pub fn rt (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("rt", attributes, children)
}

///
pub fn ruby (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("ruby", attributes, children)
}

///
pub fn s (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("s", attributes, children)
}

///
pub fn samp (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("samp", attributes, children)
}

///
pub fn small (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("small", attributes, children)
}

///
pub fn span (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("span", attributes, children)
}

///
pub fn strong (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("strong", attributes, children)
}

///
pub fn sub (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("sub", attributes, children)
}

///
pub fn sup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("sup", attributes, children)
}

///
pub fn time (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("time", attributes, children)
}

///
pub fn u (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("u", attributes, children)
}

///
pub fn var_ (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("var", attributes, children)
}

///
pub fn wbr (attributes: List(Attribute(action))) -> Element(action) {
    node("wbr", attributes, [])
}


// IMAGE AND MULTIMEDIA --------------------------------------------------------

///
pub fn area (attributes: List(Attribute(action))) -> Element(action) {
    node("area", attributes, [])
}

///
pub fn audio (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("audio", attributes, children)
}

///
pub fn img (attributes: List(Attribute(action))) -> Element(action) {
    node("img", attributes, [])
}

///
pub fn map_ (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("map", attributes, children)
}

///
pub fn track (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("track", attributes, children)
}

///
pub fn video (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("video", attributes, children)
}


// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn embed (attributes: List(Attribute(action))) -> Element(action) {
    node("embed", attributes, [])
}

///
pub fn iframe (attributes: List(Attribute(action))) -> Element(action) {
    node("iframe", attributes, [])
}

///
pub fn object (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("object", attributes, children)
}

///
pub fn param (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("param", attributes, children)
}

///
pub fn picture (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("picture", attributes, children)
}

///
pub fn portal (attributes: List(Attribute(action))) -> Element(action) {
    node("portal", attributes, [])
}

///
pub fn source (attributes: List(Attribute(action))) -> Element(action) {
    node("source", attributes, [])
}


// SVG AND MATHML --------------------------------------------------------------

///
pub fn svg (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("svg", [attribute("xmlns", "http://www.w3.org/2000/svg"), ..attributes], children)
}

///
pub fn mathml (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("mathml", [attribute("xmlns", "http://www.w3.org/1998/Math/MathML"), ..attributes], children)
}

// SCRIPTING -------------------------------------------------------------------

///
pub fn canvas (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("canvas", attributes, children)
}

///
pub fn noscript (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("noscript", attributes, children)
}


// DEMARCATING EDITS -----------------------------------------------------------

///
pub fn del (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("del", attributes, children)
}

///
pub fn ins (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("ins", attributes, children)
}


// TABLE CONTENT ---------------------------------------------------------------

///
pub fn caption (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("caption", attributes, children)
}

///
pub fn col (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("col", attributes, children)
}

///
pub fn colgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("colgroup", attributes, children)
}

///
pub fn table (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("table", attributes, children)
}

///
pub fn tbody (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("tbody", attributes, children)
}

///
pub fn td (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("td", attributes, children)
}

///
pub fn tfoot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("tfoot", attributes, children)
}

///
pub fn th (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("th", attributes, children)
}

///
pub fn thead (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("thead", attributes, children)
}

///
pub fn tr (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("tr", attributes, children)
}


// FORMS -----------------------------------------------------------------------

///
pub fn button (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("button", attributes, children)
}

///
pub fn datalist (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("datalist", attributes, children)
}

///
pub fn fieldset (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("fieldset", attributes, children)
}

///
pub fn form (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("form", attributes, children)
}

///
pub fn input (attributes: List(Attribute(action))) -> Element(action) {
    node("input", attributes, [])
}

///
pub fn label (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("label", attributes, children)
}

///
pub fn legend (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("legend", attributes, children)
}

///
pub fn meter (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("meter", attributes, children)
}

///
pub fn optgroup (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("optgroup", attributes, children)
}

///
pub fn option (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("option", attributes, children)
}

///
pub fn output (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("output", attributes, children)
}

///
pub fn progress (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("progress", attributes, children)
}

///
pub fn select (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("select", attributes, children)
}

///
pub fn textarea (attributes: List(Attribute(action))) -> Element(action) {
    node("textarea", attributes, [])
}


// INTERACTIVE ELEMENTS --------------------------------------------------------

///
pub fn details (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("details", attributes, children)
}

///
pub fn dialog (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("dialog", attributes, children)
}

///
pub fn summary (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("summary", attributes, children)
}


// WEB COMPONENTS --------------------------------------------------------------

///
pub fn slot (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("slot", attributes, children)
}

///
pub fn template (attributes: List(Attribute(action)), children: List(Element(action))) -> Element(action) {
    node("template", attributes, children)
}
