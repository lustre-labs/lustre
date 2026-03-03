import gleam/int
import gleam/option.{type Option, None, Some}

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
  let config =
    opentui.default_config()
    |> opentui.use_mouse(False)

  use platform <- opentui.platform(config)
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(selected: Option(Selection), highlighted: Option(Int))
}

type Selection {
  Selection(index: Int, name: String)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserSelectedItem(Int)
  UserChangedSelection(Int)
  KeyPressed(tui_effect.KeyEvent)
}

const menu_options = [
  attribute.SelectOption(name: "Gleam", description: "Type-safe functional language"),
  attribute.SelectOption(name: "Rust", description: "Systems programming language"),
  attribute.SelectOption(name: "TypeScript", description: "Typed superset of JavaScript"),
  attribute.SelectOption(name: "Elixir", description: "Dynamic, functional language for scalable apps"),
  attribute.SelectOption(name: "Go", description: "Simple, fast, compiled language"),
  attribute.SelectOption(name: "Haskell", description: "Purely functional programming language"),
]

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(selected: None, highlighted: None),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("lang-select"),
    ]),
  )
}

fn get_option_name(index: Int) -> String {
  case index {
    0 -> "Gleam"
    1 -> "Rust"
    2 -> "TypeScript"
    3 -> "Elixir"
    4 -> "Go"
    5 -> "Haskell"
    _ -> "Unknown"
  }
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserSelectedItem(index) -> {
      let name = get_option_name(index)
      #(
        Model(..model, selected: Some(Selection(index:, name:))),
        effect.none(),
      )
    }
    UserChangedSelection(index) -> {
      #(Model(..model, highlighted: Some(index)), effect.none())
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
      attribute.gap(1),
    ],
    [
      tui.box(
        [
          attribute.flex_direction("column"),
          attribute.border_style("round"),
          attribute.border_color("#555"),
          attribute.focused_border_color("#7c6ff5"),
          attribute.padding(1),
          attribute.padding_left(2),
          attribute.padding_right(2),
          attribute.gap(1),
          attribute.title(" Pick a Language "),
          attribute.title_alignment("center"),
        ],
        [
          tui.text([
            attribute.content("j/k to navigate, Enter to select, Ctrl+Q to quit"),
            attribute.dim(True),
            attribute.color("#888"),
          ]),
          tui.select(
            [
              attribute.id("lang-select"),
              attribute.width(50),
              attribute.height(12),
              attribute.options(menu_options),
              attribute.show_description(True),
              attribute.wrap_selection(True),
              attribute.selected_background_color("#334466"),
              attribute.selected_text_color("#FFFFFF"),
              attribute.description_color("#888888"),
              attribute.selected_description_color("#CCCCCC"),
              attribute.background_color("#1a1a1a"),
              event.on_select(UserSelectedItem),
              event.on_selection_change(UserChangedSelection),
            ],
            [],
          ),
          case model.highlighted {
            Some(index) ->
              tui.text([
                attribute.content(
                  "Browsing: " <> get_option_name(index),
                ),
                attribute.color("#7c6ff5"),
              ])
            None -> element.none()
          },
          case model.selected {
            Some(selection) ->
              tui.text([
                attribute.content(
                  "Selected: "
                  <> selection.name
                  <> " (index "
                  <> int.to_string(selection.index)
                  <> ")",
                ),
                attribute.color("#69db7c"),
                attribute.bold(True),
              ])
            None ->
              tui.text([
                attribute.content("No selection yet"),
                attribute.color("#666"),
                attribute.italic(True),
              ])
          },
        ],
      ),
    ],
  )
}
