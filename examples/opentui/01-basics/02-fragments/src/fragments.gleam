// IMPORTS ---------------------------------------------------------------------

import gleam/list
import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/element/keyed
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
  Model(open: List(String), entries: List(#(String, String)), focus_index: Int)
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let entries = [
    #(
      "What is Gleam?",
      "Gleam is a friendly language for building type-safe systems that scale! It runs on the Erlang VM (BEAM) and can also compile to JavaScript.",
    ),
    #(
      "What is Lustre?",
      "Lustre is a framework for building Web apps in Gleam! It provides a declarative, functional API for constructing HTML, and is able to run not only in the browser, but on the server as well!",
    ),
    #(
      "How can I get help?",
      "The best place to get help is the Gleam Discord server!",
    ),
  ]

  #(
    Model(open: [], entries:, focus_index: 0),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("faq-0"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserToggledEntry(String)
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserToggledEntry(id) ->
      case list.contains(model.open, id) {
        True -> #(
          Model(..model, open: list.filter(model.open, fn(x) { x != id })),
          effect.none(),
        )
        False -> #(Model(..model, open: [id, ..model.open]), effect.none())
      }

    KeyPressed(key_event) ->
      case key_event.key {
        "tab" | "down" -> {
          let new_index = { model.focus_index + 1 } % list.length(model.entries)
          #(
            Model(..model, focus_index: new_index),
            tui_effect.focus("faq-" <> int_to_string(new_index)),
          )
        }
        "up" -> {
          let len = list.length(model.entries)
          let new_index = { model.focus_index - 1 + len } % len
          #(
            Model(..model, focus_index: new_index),
            tui_effect.focus("faq-" <> int_to_string(new_index)),
          )
        }
        "q" -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }
  }
}

@external(javascript, "../gleam_stdlib/gleam/int.mjs", "to_string")
fn int_to_string(n: Int) -> String

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
          attribute.width(60),
          attribute.border_style("round"),
          attribute.border_color("#444"),
          attribute.padding(1),
          attribute.gap(1),
          attribute.title(" FAQ "),
          attribute.title_alignment("center"),
        ],
        [
          tui.text_node([attribute.dim(True), attribute.color("#888")], [
            tui.text(
              "Use arrows to navigate, Enter to expand/collapse, q to quit",
            ),
          ]),
          // Lists of elements can be grouped using fragments.
          keyed.fragment(
            list.index_map(model.entries, fn(entry, index) {
              view_entry(model.open, entry, index)
            }),
          ),
        ],
      ),
    ],
  )
}

fn view_entry(
  open: List(String),
  entry: #(String, String),
  index: Int,
) -> #(String, Element(Msg)) {
  let #(question, answer) = entry
  let is_open = list.contains(open, question)
  let id = "faq-" <> int_to_string(index)

  let indicator = case is_open {
    True -> "[-]"
    False -> "[+]"
  }

  let html =
    tui.box(
      [
        attribute.id(id),
        attribute.focusable(True),
        attribute.flex_direction("column"),
        attribute.border_style("single"),
        attribute.border_color("#555"),
        attribute.focused_border_color("#69db7c"),
        attribute.focused_background_color("#1a2a1a"),
        attribute.padding_left(1),
        attribute.padding_right(1),
        event.on_click(UserToggledEntry(question)),
        event.on_activate(UserToggledEntry(question)),
      ],
      [
        tui.box(
          [attribute.flex_direction("row")],
          [
            tui.text_node([attribute.color("#69db7c"), attribute.bold(True)], [
              tui.text(indicator <> " "),
            ]),
            tui.text_node([attribute.color("#e0e0e0")], [tui.text(question)]),
          ],
        ),
        case is_open {
          True ->
            tui.box(
              [
                attribute.background_color("#1a1a2e"),
                attribute.padding(1),
              ],
              [
                tui.text_node([attribute.color("#a0a0a0")], [tui.text(answer)]),
              ],
            )
          False -> element.none()
        },
      ],
    )

  #(question, html)
}
