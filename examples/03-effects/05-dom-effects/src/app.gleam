// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/result
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(state: State, height: Int)
}

type State {
  Collapsed
  Collapsing
  Expanded
}

fn init(_) -> #(Model, Effect(Msg)) {
  let model = Model(state: Collapsed, height: 0)

  #(model, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserClickedExpand
  DomReturnedHeight(Int)
  DomTransitionEnded
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserClickedExpand ->
      case model.state {
        Expanded -> #(Model(state: Collapsing, height: 0), effect.none())
        _ -> #(Model(..model, state: Expanded), measure_height())
      }

    DomReturnedHeight(height) -> #(Model(..model, height:), effect.none())

    DomTransitionEnded ->
      case model.state {
        Collapsing -> #(Model(state: Collapsed, height: 0), effect.none())
        _ -> #(model, effect.none())
      }
  }
}

fn measure_height() -> Effect(Msg) {
  // In addition to a `dispatch` function, before_paint and after_paint effects
  // have access to the root element of your Lustre app.
  use dispatch, root_element <- effect.before_paint

  // We can grab the element we're interested in by navigating the DOM from
  // the root element, or we could FFI to `root_element.querySelector` too.
  let img_decoder =
    decode.at(["firstChild", "firstChild", "firstChild"], decode.dynamic)

  case decode.run(root_element, img_decoder) {
    Ok(img_element) ->
      case do_measure_height(img_element) {
        Ok(height) -> dispatch(DomReturnedHeight(height))
        Error(_) -> Nil
      }
    Error(_) -> Nil
  }
}

@external(javascript, "./app.ffi.mjs", "measure_height")
fn do_measure_height(of _element: decode.Dynamic) -> Result(Int, Nil) {
  Error(Nil)
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div(
    [
      attribute.class(
        "h-screen justify-center items-center flex flex-col gap-2",
      ),
    ],
    [
      view_collapsable(
        id: "cat",
        expanded: model.state == Expanded,
        height: model.height,
        content: case model.state {
          Collapsed -> element.none()
          _ ->
            html.img([
              attribute.class("aspect-square w-full max-w-2xl"),
              attribute.src("https://cdn2.thecatapi.com/images/8lg.gif"),
            ])
        },
        on_collapse: DomTransitionEnded,
      ),
      view_button(
        controls: "cat",
        expanded: model.state == Expanded,
        on_click: UserClickedExpand,
      ),
    ],
  )
}

fn view_button(
  controls id: String,
  expanded expanded: Bool,
  on_click handle_click: msg,
) -> Element(msg) {
  html.button(
    [
      attribute.class("px-4 py-1 bg-blue-500 text-white rounded"),
      attribute.aria_controls(id),
      attribute.aria_expanded(expanded),
      event.on_click(handle_click),
    ],
    [html.text("Show/hide")],
  )
}

fn view_collapsable(
  id id: String,
  expanded expanded: Bool,
  height height: Int,
  content child: Element(msg),
  on_collapse handle_collapse: msg,
) -> Element(msg) {
  let styles = [
    #("transition", "height 0.5s ease-in-out"),
    #("height", case expanded {
      True -> int.to_string(height) <> "px"
      False -> "0px"
    }),
  ]

  html.div(
    [
      attribute.id(id),
      attribute.class("overflow-hidden"),
      attribute.styles(styles),
      case expanded {
        True -> attribute.none()
        False -> event.on("transitionend", decode.success(handle_collapse))
      },
    ],
    [html.div([attribute.class("content")], [child])],
  )
}
