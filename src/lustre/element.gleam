////

// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{Attribute, attribute}

// TYPES -----------------------------------------------------------------------

/// 
///
pub external type Element(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// Construct a plain HTML element or registered Web Component by providing the
/// tag name, a list of attributes (including event handlers), and a list of
/// child elements.
///
pub external fn node(
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) =
  "../ffi.mjs" "node"

/// A stateful element is exactly what it sounds like: an element with local
/// encapsulated state! The `render` function we must provide is called with the
/// element's current state as well as a function to set a new state. Whenever
/// that function is called, the element is re-rendered.
///
/// You might be wondering where the `stateless` version of this function is. 
/// Those are just regular Gleam functions that return `Element`s!
///
/// We can implement a counter whose current value is stored locally like so:
///
/// ```gleam
/// import gleam/int
/// import lustre/attribute.{Attribute}
/// import lustre/element.{Element}
/// import lustre/event
/// 
/// pub fn counter() -> Element(msg) {
///   use state, set_state = lustre.stateful(0)
/// 
///   let decr = event.on("click", fn(_, _) { set_state(state - 1) })
///   let incr = event.on("click", fn(_, _) { set_state(state + 1) })
/// 
///   element.div([], [
///     element.button([decr], [element.text("-")]),
///     element.text(int.to_string(state)),
///     element.button([incr], [element.text("+")]),
///   ])
/// }
/// ```
///
/// We can use this `counter` element anywhere in our app, we don't have to worry
/// about passing state around or managing it in a parent component.
///
/// When attaching event handlers to stateful elements, you'll typically have to
/// use the generic `event.on` function rather than the more specific helpders.
/// The helpers are designed to emit messages for your application, but stateful
/// elements don't necessarily produce messages: calling `set_state` is a side
/// effect.
///
pub external fn stateful(
  init: state,
  render: fn(state, fn(state) -> Nil) -> Element(msg),
) -> Element(msg) =
  "../ffi.mjs" "stateful"

/// A fragment doesn't appear in the DOM, but allows us to treat a list of elements
/// as if it were a single one. 
///
pub external fn fragment(children: List(Element(msg))) -> Element(msg) =
  "../ffi.mjs" "fragment"

/// Render a Gleam string as an HTML text node.
///
pub external fn text(content: String) -> Element(msg) =
  "../ffi.mjs" "text"

// MANIPULATIONS ---------------------------------------------------------------

/// Transforms the actions produced by an element. Our Lustre applications have
/// a single top-level `Msg` type and `update` function, and ultimately all
/// elements we create must produce actions of that type (or not produce any at
/// all).
///
/// To save us having a monolothic `Msg` type with many constructors, we can crete
/// child types and then use `map` to wrap them up in the parent type. A simple
/// counter with its own message type my look like this:
///
/// ```gleam
/// import gleam/int
/// import lustre/element.{Element}
/// import lustre/event
/// 
/// type Msg {
///   Decr
///   Incr
/// }
/// 
/// pub fn update(count: Int, msg: CounterMsg) -> Int {
///   case msg {
///     Decr -> count - 1
///     Incr -> count + 1
///   }
/// }
/// 
/// pub fn render(count: Int) -> Element(CounterMsg) {
///   element.div([], [
///     element.button([event.on_click(Decr)], [element.text("-")]),
///     element.text(int.to_string(count)),
///     element.button([event.on_click(Incr)], [element.text("+")]),
///   ])
/// }
/// ```
///
/// To integrate this into a large Lustre application, we need to do some plumbing.
/// Here's how we might have a parent application that renders multiple counters:
///
/// ```gleam
/// import counter.{Msg as CounterMsg}
/// import gleam/list
/// import gleam/map.{Map}
/// import gleam/result
/// import lustre
/// import lustre/element.{Element}
/// import lustre/event
/// 
/// pub fn main() {
///   let app = lustre.simple(init(), update, render)
///   assert Ok(_) = lustre.start(app, "#root")
/// }
/// 
/// type Model = Map(Int, Int)
/// 
/// fn init() -> Model {
///   map.from_list([
///     #(0, 0),
///     #(1, 0),
///     #(2, 0),
///   ])
/// }
/// 
/// type Msg {
///   UpdateCounter(Int, CounterMsg)
/// }
/// 
/// pub fn update(model: Model, msg: Msg) -> Model {
///   case msg {
///     UpdateCounter(id, counter_msg) -> {
///       map.get(model, id)
///       // Call the counter's update function with the `counter_msg`. This will
///       // either be `counter.Decr` or `counter.Incr`.
///       |> result.map(counter.update(_, counter_msg))
///       |> result.map(map.insert(model, id, _))
///       |> result.unwrap(model)
///     }
///   }
/// }
/// 
/// pub fn render(model: Model) -> Element(Msg) {
///   let counters = {
///     use els, id, count <- map.fold(model, [])
///     let counter = counter.render(counr)
/// 
///     // Right now `counter` produces messages of type `counter.Msg`. Here we're
///     // calling `element.map` to wrap that message in our main app's `Msg` type.
///     [element.map(counter, UpdateCounter(id, _)), ...els]
///   }
/// 
///   element.div([], list.reverse(counters))
/// }
/// ```
///
/// If this feels like a lt of work... sometimes it is! Take a look at the docs
/// for [`stateful`](#stateful) elements to see how all this can be encapsulated.
///
pub external fn map(element: Element(a), f: fn(a) -> b) -> Element(b) =
  "../ffi.mjs" "map"

// CONSTRUCTING NODES ----------------------------------------------------------
// This list and grouping of nodes has been taken from the MDN reference at:
// https://developer.mozilla.org/en-US/docs/Web/HTML/Element

// MAIN ROOT -------------------------------------------------------------------

///
pub fn html(
  attrs: List(Attribute(msg)),
  head: Element(msg),
  body: Element(msg),
) -> Element(msg) {
  node("html", attrs, [head, body])
}

// DOCUMENT METADATA -----------------------------------------------------------

///
pub fn base(attrs: List(Attribute(msg))) -> Element(msg) {
  node("base", attrs, [])
}

///
pub fn head(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("head", attrs, children)
}

///
pub fn meta(attrs: List(Attribute(msg))) -> Element(msg) {
  node("meta", attrs, [])
}

///
pub fn style(attrs: List(Attribute(msg)), css: String) -> Element(msg) {
  node("style", attrs, [text(css)])
}

///
pub fn title(attrs: List(Attribute(msg)), name: String) -> Element(msg) {
  node("title", attrs, [text(name)])
}

// SECTIONING ROOT -------------------------------------------------------------

///
pub fn body(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("body", attrs, children)
}

// CONTENT SECTIONING ----------------------------------------------------------

///
pub fn address(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("address", attrs, children)
}

///
pub fn article(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("article", attrs, children)
}

///
pub fn aside(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("aside", attrs, children)
}

///
pub fn footer(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("footer", attrs, children)
}

///
pub fn header(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("header", attrs, children)
}

///
pub fn h1(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h1", attrs, children)
}

///
pub fn h2(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h2", attrs, children)
}

///
pub fn h3(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h3", attrs, children)
}

///
pub fn h4(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h4", attrs, children)
}

///
pub fn h5(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h5", attrs, children)
}

///
pub fn h6(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("h6", attrs, children)
}

///
pub fn main(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("main", attrs, children)
}

///
pub fn nav(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("nav", attrs, children)
}

///
pub fn section(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("section", attrs, children)
}

// TEXT CONTENT ----------------------------------------------------------------

///
pub fn blockquote(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("blockquote", attrs, children)
}

///
pub fn dd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("dd", attrs, children)
}

///
pub fn div(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("div", attrs, children)
}

///
pub fn dl(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("dl", attrs, children)
}

///
pub fn dt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("dt", attrs, children)
}

///
pub fn figcaption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("figcaption", attrs, children)
}

///
pub fn figure(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("figure", attrs, children)
}

///
pub fn hr(attrs: List(Attribute(msg))) -> Element(msg) {
  node("hr", attrs, [])
}

///
pub fn li(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("li", attrs, children)
}

///
pub fn menu(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("menu", attrs, children)
}

///
pub fn ol(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("ol", attrs, children)
}

///
pub fn p(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("p", attrs, children)
}

///
pub fn pre(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("pre", attrs, children)
}

///
pub fn ul(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("ul", attrs, children)
}

// INLINE TEXT SEMANTICS -------------------------------------------------------

///
pub fn a(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("a", attrs, children)
}

///
pub fn abbr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("abbr", attrs, children)
}

///
pub fn b(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("b", attrs, children)
}

///
pub fn bdi(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("bdi", attrs, children)
}

///
pub fn bdo(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("bdo", attrs, children)
}

///
pub fn br(attrs: List(Attribute(msg))) -> Element(msg) {
  node("br", attrs, [])
}

///
pub fn cite(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("cite", attrs, children)
}

///
pub fn code(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("code", attrs, children)
}

///
pub fn dfn(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("dfn", attrs, children)
}

///
pub fn em(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("em", attrs, children)
}

///
pub fn i(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("i", attrs, children)
}

///
pub fn kbd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("kbd", attrs, children)
}

///
pub fn mark(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("mark", attrs, children)
}

///
pub fn rp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("rp", attrs, children)
}

///
pub fn rt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("rt", attrs, children)
}

///
pub fn ruby(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("ruby", attrs, children)
}

///
pub fn s(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("s", attrs, children)
}

///
pub fn samp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("samp", attrs, children)
}

///
pub fn small(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("small", attrs, children)
}

///
pub fn span(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("span", attrs, children)
}

///
pub fn strong(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("strong", attrs, children)
}

///
pub fn sub(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("sub", attrs, children)
}

///
pub fn sup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("sup", attrs, children)
}

///
pub fn time(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("time", attrs, children)
}

///
pub fn u(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("u", attrs, children)
}

///
pub fn var_(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("var", attrs, children)
}

///
pub fn wbr(attrs: List(Attribute(msg))) -> Element(msg) {
  node("wbr", attrs, [])
}

// IMAGE AND MULTIMEDIA --------------------------------------------------------

///
pub fn area(attrs: List(Attribute(msg))) -> Element(msg) {
  node("area", attrs, [])
}

///
pub fn audio(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("audio", attrs, children)
}

///
pub fn img(attrs: List(Attribute(msg))) -> Element(msg) {
  node("img", attrs, [])
}

///
pub fn map_(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("map", attrs, children)
}

///
pub fn track(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("track", attrs, children)
}

///
pub fn video(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("video", attrs, children)
}

// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn embed(attrs: List(Attribute(msg))) -> Element(msg) {
  node("embed", attrs, [])
}

///
pub fn iframe(attrs: List(Attribute(msg))) -> Element(msg) {
  node("iframe", attrs, [])
}

///
pub fn object(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("object", attrs, children)
}

///
pub fn param(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("param", attrs, children)
}

///
pub fn picture(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("picture", attrs, children)
}

///
pub fn portal(attrs: List(Attribute(msg))) -> Element(msg) {
  node("portal", attrs, [])
}

///
pub fn source(attrs: List(Attribute(msg))) -> Element(msg) {
  node("source", attrs, [])
}

// SVG AND MATHML --------------------------------------------------------------

///
pub fn svg(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node(
    "svg",
    [attribute("xmlns", "http://www.w3.org/2000/svg"), ..attrs],
    children,
  )
}

///
pub fn mathml(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node(
    "mathml",
    [attribute("xmlns", "http://www.w3.org/1998/Math/MathML"), ..attrs],
    children,
  )
}

// SCRIPTING -------------------------------------------------------------------

///
pub fn canvas(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("canvas", attrs, children)
}

///
pub fn noscript(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("noscript", attrs, children)
}

// DEMARCATING EDITS -----------------------------------------------------------

///
pub fn del(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("del", attrs, children)
}

///
pub fn ins(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("ins", attrs, children)
}

// TABLE CONTENT ---------------------------------------------------------------

///
pub fn caption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("caption", attrs, children)
}

///
pub fn col(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("col", attrs, children)
}

///
pub fn colgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("colgroup", attrs, children)
}

///
pub fn table(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("table", attrs, children)
}

///
pub fn tbody(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("tbody", attrs, children)
}

///
pub fn td(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("td", attrs, children)
}

///
pub fn tfoot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("tfoot", attrs, children)
}

///
pub fn th(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("th", attrs, children)
}

///
pub fn thead(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("thead", attrs, children)
}

///
pub fn tr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("tr", attrs, children)
}

// FORMS -----------------------------------------------------------------------

///
pub fn button(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("button", attrs, children)
}

///
pub fn datalist(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("datalist", attrs, children)
}

///
pub fn fieldset(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("fieldset", attrs, children)
}

///
pub fn form(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("form", attrs, children)
}

///
pub fn input(attrs: List(Attribute(msg))) -> Element(msg) {
  node("input", attrs, [])
}

///
pub fn label(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("label", attrs, children)
}

///
pub fn legend(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("legend", attrs, children)
}

///
pub fn meter(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("meter", attrs, children)
}

///
pub fn optgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("optgroup", attrs, children)
}

///
pub fn option(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("option", attrs, children)
}

///
pub fn output(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("output", attrs, children)
}

///
pub fn progress(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("progress", attrs, children)
}

///
pub fn select(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("select", attrs, children)
}

///
pub fn textarea(attrs: List(Attribute(msg))) -> Element(msg) {
  node("textarea", attrs, [])
}

// INTERACTIVE ELEMENTS --------------------------------------------------------

///
pub fn details(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("details", attrs, children)
}

///
pub fn dialog(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("dialog", attrs, children)
}

///
pub fn summary(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("summary", attrs, children)
}

// WEB COMPONENTS --------------------------------------------------------------

///
pub fn slot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("slot", attrs, children)
}

///
pub fn template(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  node("template", attrs, children)
}
