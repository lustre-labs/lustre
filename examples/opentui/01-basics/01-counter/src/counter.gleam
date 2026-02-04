import gleam/int

import lustre
import lustre/effect
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element
import lustre/platform/opentui/event

pub fn main() {
  let config =
    opentui.default_config()
    |> opentui.use_mouse(False)

  use platform <- opentui.platform(config)
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
  Nil
}

pub type Model {
  Model(count: Int)
}

pub type Msg {
  Increment
  Decrement
  KeyPressed(tui_effect.KeyEvent)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(count: 0),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("btn-plus"),
    ]),
  )
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Increment -> #(Model(count: model.count + 1), effect.none())
    Decrement -> #(Model(count: model.count - 1), effect.none())
    KeyPressed(key_event) ->
      case key_event.key {
        "tab" | "right" -> #(model, tui_effect.focus_next())
        "left" -> #(model, tui_effect.focus_previous())
        "up" -> #(Model(count: model.count + 1), effect.none())
        "down" -> #(Model(count: model.count - 1), effect.none())
        _ -> #(model, effect.none())
      }
  }
}

fn view(model: Model) {
  element.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      element.box(
        [
          attribute.flex_direction("column"),
          attribute.align_items("center"),
          attribute.border_style("round"),
          attribute.border_color("#444"),
          attribute.padding_left(3),
          attribute.padding_right(3),
          attribute.padding_top(1),
          attribute.padding_bottom(1),
          attribute.gap(1),
          attribute.title(" Counter "),
          attribute.title_alignment("center"),
        ],
        [
          element.text_node(
            [
              attribute.bold(True),
              attribute.color("#e0e0e0"),
              attribute.dim(True),
            ],
            [element.text("Arrows to navigate, Enter to activate")],
          ),
          element.text_node([attribute.bold(True), attribute.color("#fff")], [
            model.count
            |> int.to_string()
            |> element.text(),
          ]),
          element.box([attribute.flex_direction("row"), attribute.gap(2)], [
            element.box(
              [
                attribute.focusable(True),
                attribute.border_style("round"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#ff6b6b"),
                attribute.focused_background_color("#2a1a1a"),
                attribute.padding_left(2),
                attribute.padding_right(2),
                event.on_click(Decrement),
                event.on_activate(Decrement),
              ],
              [
                element.text_node(
                  [attribute.bold(True), attribute.color("#ff6b6b")],
                  [element.text(" - ")],
                ),
              ],
            ),
            element.box(
              [
                attribute.id("btn-plus"),
                attribute.focusable(True),
                attribute.border_style("round"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#69db7c"),
                attribute.focused_background_color("#1a2a1a"),
                attribute.padding_left(2),
                attribute.padding_right(2),
                event.on_click(Increment),
                event.on_activate(Increment),
              ],
              [
                element.text_node(
                  [attribute.bold(True), attribute.color("#69db7c")],
                  [element.text(" + ")],
                ),
              ],
            ),
          ]),
        ],
      ),
    ],
  )
}
