// IMPORTS ---------------------------------------------------------------------

import gleam/string
import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui
import lustre/platform/opentui/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(name: String)
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(name: "Lucy"),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("name-input"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserUpdatedName(String)
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserUpdatedName(name) ->
      case string.length(name) <= 10 {
        // A "controlled" input has both its `value` attribute set and a handler
        // for `input` events. This way it is always in sync with your model.
        True -> #(Model(name:), effect.none())
        // If we don't update the state, the input won't change even though
        // it's still receiving keyboard events.
        False -> #(model, effect.none())
      }

    KeyPressed(key_event) ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  tui.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      tui.box(
        [
          attribute.flex_direction("column"),
          attribute.border_style("round"),
          attribute.border_color("#444"),
          attribute.padding(2),
          attribute.gap(1),
          attribute.title(" Controlled Inputs "),
          attribute.title_alignment("center"),
        ],
        [
          tui.text_node(
            [attribute.dim(True), attribute.color("#888")],
            [tui.text("Type to enter a name (max 10 chars), Ctrl+Q to quit")],
          ),
          tui.box(
            [attribute.flex_direction("row"), attribute.gap(1), attribute.align_items("center")],
            [
              tui.text_node(
                [attribute.color("#e0e0e0")],
                [tui.text("Enter a name:")],
              ),
              tui.input([
                attribute.id("name-input"),
                attribute.value(model.name),
                event.on_input(UserUpdatedName),
                attribute.width(20),
                attribute.border_style("single"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#69db7c"),
                attribute.placeholder("Your name..."),
              ]),
            ],
          ),
          tui.box(
            [attribute.margin_top(1)],
            [
              tui.text_node(
                [attribute.color("#69db7c"), attribute.bold(True)],
                [tui.text("Hello there, " <> model.name <> "!")],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}
