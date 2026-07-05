// IMPORTS ---------------------------------------------------------------------

import agnostic
import agnostic/effect
import agnostic/element.{type Element}
import agnostic/platform/opentui
import agnostic/platform/opentui/attribute
import agnostic/platform/opentui/effect as tui_effect
import agnostic/platform/opentui/element as tui
import agnostic/platform/opentui/event
import gleam/int
import gleam/string

// MAIN ------------------------------------------------------------------------

pub fn main() {
  opentui.platform(opentui.default_config(), fn(platform) {
    let app = agnostic.application(init, update, view)
    let assert Ok(_) = agnostic.start(app, on: platform, with: Nil)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(content: String, cursor_line: Int, cursor_col: Int)
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(content: "", cursor_line: 1, cursor_col: 1),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("notes-textarea"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserUpdatedContent(String)
  CursorMoved(Int, Int)
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserUpdatedContent(content) -> #(
      Model(..model, content: content),
      effect.none(),
    )

    CursorMoved(line, col) -> #(
      Model(..model, cursor_line: line, cursor_col: col),
      effect.none(),
    )

    KeyPressed(key_event) ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let char_count = string.length(model.content)
  let word_count = count_words(model.content)
  let line_count = count_lines(model.content)

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
      // Main editor box
      tui.box(
        [
          attribute.flex_direction("column"),
          attribute.border_style("round"),
          attribute.border_color("#3498db"),
          attribute.padding(1),
          attribute.gap(1),
          attribute.title(" Notes Editor "),
          attribute.title_alignment("center"),
        ],
        [
          // Instructions
          tui.text([
            attribute.content("Type your notes below. Ctrl+Q to quit."),
            attribute.dim(True),
            attribute.color("#888"),
          ]),
          // Textarea
          tui.textarea([
            attribute.id("notes-textarea"),
            attribute.width(50),
            attribute.height(8),
            attribute.initial_value(""),
            attribute.placeholder("Write your notes here..."),
            attribute.placeholder_color("#666666"),
            attribute.background_color("#1a1a2e"),
            attribute.focused_background_color("#16213e"),
            attribute.text_color("#e0e0e0"),
            attribute.cursor_color("#3498db"),
            attribute.wrap_mode("word"),
            event.on_content_change(UserUpdatedContent),
            event.on_cursor_change(CursorMoved),
          ]),
          // Preview
          tui.box(
            [
              attribute.flex_direction("column"),
              attribute.border_style("single"),
              attribute.border_color("#555"),
              attribute.title(" Preview "),
              attribute.width(50),
              attribute.height(4),
              attribute.padding_left(1),
              attribute.padding_right(1),
            ],
            [
              tui.text([
                attribute.content(case string.is_empty(model.content) {
                  True -> "(empty)"
                  False -> model.content
                }),
                attribute.color(case string.is_empty(model.content) {
                  True -> "#555"
                  False -> "#e0e0e0"
                }),
              ]),
            ],
          ),
          // Status bar
          tui.box(
            [
              attribute.flex_direction("row"),
              attribute.justify_content("space-between"),
              attribute.width_("100%"),
            ],
            [
              // Cursor position
              tui.text([
                attribute.content(
                  "Ln "
                  <> int.to_string(model.cursor_line)
                  <> ", Col "
                  <> int.to_string(model.cursor_col),
                ),
                attribute.color("#888"),
              ]),
              // Stats
              tui.text([
                attribute.content(
                  int.to_string(char_count)
                  <> " chars | "
                  <> int.to_string(word_count)
                  <> " words | "
                  <> int.to_string(line_count)
                  <> " lines",
                ),
                attribute.color("#888"),
              ]),
            ],
          ),
        ],
      ),
    ],
  )
}

// HELPERS ---------------------------------------------------------------------

fn count_words(text: String) -> Int {
  case string.is_empty(string.trim(text)) {
    True -> 0
    False ->
      text
      |> string.split(" ")
      |> list_filter(fn(s) { !string.is_empty(string.trim(s)) })
      |> list_length
  }
}

fn count_lines(text: String) -> Int {
  case string.is_empty(text) {
    True -> 1
    False -> {
      let lines = string.split(text, "\n")
      list_length(lines)
    }
  }
}

fn list_filter(list: List(a), predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}

fn do_filter(list: List(a), predicate: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> list_reverse(acc)
    [first, ..rest] ->
      case predicate(first) {
        True -> do_filter(rest, predicate, [first, ..acc])
        False -> do_filter(rest, predicate, acc)
      }
  }
}

fn list_length(list: List(a)) -> Int {
  do_length(list, 0)
}

fn do_length(list: List(a), acc: Int) -> Int {
  case list {
    [] -> acc
    [_, ..rest] -> do_length(rest, acc + 1)
  }
}

fn list_reverse(list: List(a)) -> List(a) {
  do_reverse(list, [])
}

fn do_reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [first, ..rest] -> do_reverse(rest, [first, ..acc])
  }
}
