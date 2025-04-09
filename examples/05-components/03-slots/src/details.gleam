// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import lustre
import lustre/attribute.{type Attribute, attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

//
pub fn register() -> Result(Nil, lustre.Error) {
  let component =
    lustre.component(init, update, view, [
      component.on_attribute_change("summary", fn(value) {
        value |> ParentChangedSummary |> Ok
      }),
      component.on_property_change("summary", {
        decode.string |> decode.map(ParentChangedSummary)
      }),
    ])

  lustre.register(component, "my-details")
}

pub fn element(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    "my-details",
    attributes,
    // We're not passing children into the component directly. In other frameworks
    // passing children into a component means receiving them as a prop or some
    // data that you can handle. In Lustre, we use native HTML slots to direct
    // the children into the right place. This makes slots *declarative*: as we'll
    // see our `view` function says where the children go, and the component handles
    // the rest.
    children,
  )
}

pub fn summary(value: String) -> Attribute(msg) {
  attribute("summary", value)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(summary: String, state: State, height: Int)
}

type State {
  Collapsed
  Collapsing
  Expanded
}

fn init(_) -> #(Model, Effect(Msg)) {
  let model = Model(summary: "", state: Collapsed, height: 0)

  #(model, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  DomReturnedHeight(Int)
  DomTransitionEnded
  ParentChangedSummary(String)
  UserClickedToggle
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    DomReturnedHeight(height) -> #(
      Model(..model, state: Expanded, height:),
      effect.none(),
    )

    DomTransitionEnded ->
      case model.state {
        Collapsing -> #(
          Model(..model, state: Collapsed, height: 0),
          effect.none(),
        )

        _ -> #(model, effect.none())
      }

    ParentChangedSummary(summary) -> #(Model(..model, summary:), effect.none())

    UserClickedToggle ->
      case model.state {
        Expanded -> #(
          Model(..model, state: Collapsing, height: 0),
          effect.none(),
        )

        _ -> #(
          Model(..model, state: Expanded, height: 0),
          measure_height("slot:not([name])"),
        )
      }
  }
}

fn measure_height(of selector: String) -> Effect(Msg) {
  // When we're interacting with the DOM inside a component, it's important that
  // we do so via the component's shadow root. Calling methods like `document.querySelector`
  // won't work as expected because our component content is isolated from the
  // document. Instead we'll need to be calling `shadow_root.querySelector`
  // instead!
  use dispatch, shadow_root <- effect.before_paint

  case do_measure_height(of: selector, in: shadow_root) {
    Ok(height) -> dispatch(DomReturnedHeight(height))
    Error(_) -> Nil
  }
}

@external(javascript, "./details.ffi.mjs", "measure_height")
fn do_measure_height(
  of _selector: String,
  in _root: Dynamic,
) -> Result(Int, Nil) {
  Error(Nil)
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  element.fragment([
    view_trigger(
      expanded: model.state == Expanded,
      content: model.summary,
      on_click: UserClickedToggle,
    ),
    view_content(
      expanded: model.state != Collapsed,
      height: model.height,
      on_collapse: DomTransitionEnded,
    ),
  ])
}

fn view_content(
  expanded expanded: Bool,
  height height: Int,
  on_collapse handle_collapse: msg,
) -> Element(msg) {
  html.div(
    [
      attribute.class("mt-2 overflow-y-hidden"),
      attribute.style("transition", "height 0.5s ease-in-out"),
      attribute.style("height", case expanded {
        True -> int.to_string(height) <> "px"
        False -> "0px"
      }),
      event.on("transitionend", case expanded {
        True -> decode.success(handle_collapse)
        False -> decode.failure(handle_collapse, "")
      }),
    ],
    [
      case expanded {
        // The default slot is filled with any content that does not have an explicit
        // slot attribute. By conditionally rendering the default slot we can easily
        // show or hide the content of our details element.
        True ->
          component.default_slot(
            [
              // By default, slots have `display: contents` meaning they don't
              // create a new block in the DOM or have any dimensions themselves.
              // Because we want to be able to animate the height of the content
              // container based on the height of this slot, we set its display
              // to `block` so it draws a box in the DOM that we can measure.
              attribute.class("block"),
            ],
            [],
          )
        False -> element.none()
      },
    ],
  )
}

fn view_trigger(
  expanded expanded: Bool,
  content summary: String,
  on_click handle_click: msg,
) -> Element(msg) {
  let handle_keydown = {
    use key <- decode.field("key", decode.string)

    case key {
      "Enter" -> decode.success(handle_click)
      " " -> decode.success(handle_click)
      _ -> decode.failure(handle_click, "")
    }
  }

  html.div(
    [
      attribute.class("flex items-center gap-2 py-2 border-b"),
      attribute.class("group hover:cursor-pointer"),
      attribute.tabindex(0),
      event.on_click(handle_click),
      event.on("keydown", handle_keydown),
    ],
    [
      // This is a named slot: if the parent renders any children that have the
      // "slot" attribute set to "summary" then they will appear here.
      component.named_slot("summary", [attribute.class("block flex-1")], [
        // If the slot is not filled by any content, any children in this list
        // are used as a fallback. We're making use of that functionality to
        // show the content of the summary *attribute* here while still being
        // flexible enough to allow the parent to override it with custom markup
        // if they want to.
        html.text(summary),
      ]),
      html.button(
        [
          attribute.class("size-6 rounded group-hover:bg-blue-50"),
          attribute.class(case expanded {
            True -> "bg-blue-50 text-blue-500 rotate-90"
            False -> ""
          }),
        ],
        [html.text("›")],
      ),
    ],
  )
}
