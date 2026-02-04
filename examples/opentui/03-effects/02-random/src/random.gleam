// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui
import lustre/platform/opentui/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Generate a random number before starting the app
  let initial_target = int.random(10) + 1

  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: initial_target)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(
    target: Int,
    guess_text: String,
    guess: Int,
    result: Option(Order),
    focused: FocusedField,
  )
}

type FocusedField {
  InputField
  GuessButton
}

fn init(initial_target: Int) -> #(Model, Effect(Msg)) {
  #(
    Model(
      target: initial_target,
      guess_text: "",
      guess: 0,
      result: None,
      focused: InputField,
    ),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("guess-input"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ComputerPickedNumber(Int)
  UserUpdatedGuess(String)
  UserSubmittedGuess
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ComputerPickedNumber(target) -> #(
      Model(..model, target:, guess_text: "", guess: 0),
      tui_effect.focus("guess-input"),
    )

    UserUpdatedGuess(text) -> {
      let guess = case int.parse(text) {
        Ok(n) if n >= 0 && n <= 10 -> n
        Ok(_) -> model.guess
        Error(_) -> model.guess
      }
      // Only allow numeric input
      let clean_text = case int.parse(text) {
        Ok(n) if n >= 0 && n <= 10 -> text
        Ok(_) -> model.guess_text
        Error(_) if text == "" -> ""
        Error(_) -> model.guess_text
      }
      #(Model(..model, guess_text: clean_text, guess:), effect.none())
    }

    UserSubmittedGuess -> {
      let result = int.compare(model.guess, model.target)
      let model = Model(..model, result: Some(result))

      #(model, case result {
        Eq -> generate_new_target()
        _ -> effect.none()
      })
    }

    KeyPressed(key_event) ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        "tab" | "down" -> {
          let #(new_focused, focus_id) = case model.focused {
            InputField -> #(GuessButton, "guess-btn")
            GuessButton -> #(InputField, "guess-input")
          }
          #(Model(..model, focused: new_focused), tui_effect.focus(focus_id))
        }
        "up" -> {
          let #(new_focused, focus_id) = case model.focused {
            InputField -> #(GuessButton, "guess-btn")
            GuessButton -> #(InputField, "guess-input")
          }
          #(Model(..model, focused: new_focused), tui_effect.focus(focus_id))
        }
        _ -> #(model, effect.none())
      }
  }
}

fn generate_new_target() -> Effect(Msg) {
  // `effect.from` lets us write custom effects that can send messages back to
  // the runtime using `dispatch`.
  use dispatch <- effect.from
  let value = int.random(10) + 1

  dispatch(ComputerPickedNumber(value))
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
          attribute.title(" Number Guessing Game "),
          attribute.title_alignment("center"),
        ],
        [
          tui.text_node([attribute.dim(True), attribute.color("#888")], [
            tui.text("Tab to switch fields, Enter to guess, Ctrl+Q to quit"),
          ]),
          tui.box(
            [
              attribute.flex_direction("row"),
              attribute.gap(1),
              attribute.align_items("center"),
            ],
            [
              tui.text_node([attribute.color("#e0e0e0")], [
                tui.text("Enter a number from 1-10:"),
              ]),
              tui.input([
                attribute.id("guess-input"),
                attribute.value(model.guess_text),
                event.on_input(UserUpdatedGuess),
                event.on_submit(fn(_) { UserSubmittedGuess }),
                attribute.width(5),
                attribute.border_style("single"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#3498db"),
                attribute.placeholder("?"),
              ]),
              tui.box(
                [
                  attribute.id("guess-btn"),
                  attribute.focusable(True),
                  attribute.border_style("round"),
                  attribute.border_color("#555"),
                  attribute.focused_border_color("#3498db"),
                  attribute.focused_background_color("#1a2a3a"),
                  attribute.padding_left(2),
                  attribute.padding_right(2),
                  event.on_click(UserSubmittedGuess),
                  event.on_activate(UserSubmittedGuess),
                ],
                [
                  tui.text_node(
                    [attribute.color("#3498db"), attribute.bold(True)],
                    [tui.text("Guess!")],
                  ),
                ],
              ),
            ],
          ),
          case model.result {
            Some(result) -> view_result(result)
            None -> element.none()
          },
        ],
      ),
    ],
  )
}

fn view_result(result: Order) -> Element(msg) {
  let #(color, message) = case result {
    Eq -> #("#2ecc71", "You guessed it! But can you do it again?")
    Gt -> #("#e74c3c", "Your guess was too high!")
    Lt -> #("#f39c12", "Your guess was too low!")
  }

  tui.box([attribute.margin_top(1)], [
    tui.text_node([attribute.color(color), attribute.bold(True)], [
      tui.text(message),
    ]),
  ])
}
