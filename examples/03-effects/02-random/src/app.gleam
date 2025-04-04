// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Your `main` function is a good opportunity to perform any side effects whose
  // results you need to start your app. Generating a random number is a side
  // effect that would typically be handled as a Lustre `Effect`, but we run it
  // here *before* starting the app so we can pass the result in as flags.
  let initial_target = int.random(10)

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", initial_target)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(target: Int, guess: Int, result: Option(Order))
}

fn init(initial_target: Int) -> #(Model, Effect(Msg)) {
  let model = Model(target: initial_target, guess: 0, result: None)

  #(model, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ComputerPickedNumber(Int)
  UserUpdatedGuess(Int)
  UserSubmittedGuess
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ComputerPickedNumber(target) -> #(Model(..model, target:), effect.none())

    UserUpdatedGuess(guess) -> #(
      case guess >= 0 && guess <= 10 {
        True -> Model(..model, guess:)
        False -> model
      },
      effect.none(),
    )

    UserSubmittedGuess -> {
      let result = int.compare(model.guess, model.target)
      let model = Model(..model, result: Some(result))

      #(model, case result {
        Eq -> generate_new_target()
        _ -> effect.none()
      })
    }
  }
}

fn generate_new_target() -> Effect(Msg) {
  // `effect.from` lets us write custom effects that can send messages back to
  // the runtime using `dispatch`.
  use dispatch <- effect.from
  // It might seem silly, but even something like generating random numbers is a
  // side effect that should go through Lustre's effect system! Every time
  // `int.random` is called it generates a new random number: if we called this
  // in `update` it would make our app non-deterministic and impossible to test!
  let value = int.random(10)

  dispatch(ComputerPickedNumber(value))
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 mx-auto w-full max-w-3xl space-y-4")], [
    html.div([attribute.class("flex items-center gap-2")], [
      html.label([attribute.class("contents")], [
        html.span([], [html.text("Enter a number from 1-10: ")]),
        view_number_input(value: model.guess, on_input: UserUpdatedGuess),
      ]),
      html.button(
        [
          attribute.class("px-2 py-1 bg-blue-500 text-white rounded"),
          event.on_click(UserSubmittedGuess),
        ],
        [html.text("Guess!")],
      ),
    ]),
    case model.result {
      Some(result) -> view_result(result)
      // `element.none` can be used in cases where you want to conditionally
      // render an element.
      None -> element.none()
    },
  ])
}

fn view_number_input(
  value value: Int,
  on_input handle_input: fn(Int) -> msg,
) -> Element(msg) {
  let on_input =
    event.on("input", {
      use value <- decode.subfield(["target", "value"], decode.string)

      case int.parse(value) {
        Ok(n) -> decode.success(handle_input(n))
        Error(_) -> decode.failure(handle_input(0), "")
      }
    })

  html.input([
    attribute.value(int.to_string(value)),
    attribute.type_("number"),
    on_input,
  ])
}

fn view_result(result: Order) -> Element(msg) {
  let class =
    attribute.class(case result {
      Eq -> "text-green-500"
      _ -> "text-slate-700"
    })

  html.p([class], [
    html.text(case result {
      Eq -> "You guessed it! But can you do it again?"
      Gt -> "Your guess was too high!"
      Lt -> "Your guess was too low!"
    }),
  ])
}
