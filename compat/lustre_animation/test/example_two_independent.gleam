import lustre
import lustre/animation.{Animations}
import lustre/attribute.{style}
import lustre/effect.{Effect}
import lustre/element.{text}
import lustre/element/html.{button, div, h3, span}
import lustre/event.{on_click}
import gleam/float

pub type Msg {
  Left
  Right
  Top
  Bottom
  Tick(time_offset: Float)
}

pub type Model {
  Model(x: Float, y: Float, animations: Animations)
}

pub fn main() {
  lustre.application(init, update, render)
  |> lustre.start("#root", Nil)
}

fn init(_) {
  #(Model(0.5, 0.5, animation.new()), effect.none())
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  let m = case msg {
    Left -> {
      let new_animations =
        animation.add(model.animations, "x", model.x, 0.0, 2.5)
      Model(..model, animations: new_animations)
    }
    Right -> {
      let new_animations =
        animation.add(model.animations, "x", model.x, 1.0, 2.5)
      Model(..model, animations: new_animations)
    }
    Top -> {
      let new_animations =
        animation.add(model.animations, "y", model.y, 0.0, 2.5)
      Model(..model, animations: new_animations)
    }
    Bottom -> {
      let new_animations =
        animation.add(model.animations, "y", model.y, 1.0, 2.5)
      Model(..model, animations: new_animations)
    }
    Tick(time_offset) -> {
      let new_animations = animation.tick(model.animations, time_offset)
      let x = animation.value(new_animations, "x", model.x)
      let y = animation.value(new_animations, "y", model.y)
      Model(x: x, y: y, animations: new_animations)
    }
  }
  #(m, animation.effect(m.animations, Tick))
}

pub fn render(model: Model) {
  let sp = span([], [])
  let to_s = float.to_string
  div(
    [
      style([
        #("display", "grid"),
        #("grid-template-rows", "1fr auto"),
        #("width", "100%"),
        #("height", "100%"),
      ]),
    ],
    [
      div(
        [
          style([
            #("display", "grid"),
            #(
              "grid-template-rows",
              to_s(model.y) <> "fr auto " <> to_s(1.0 -. model.y) <> "fr",
            ),
            #(
              "grid-template-columns",
              to_s(model.x) <> "fr auto " <> to_s(1.0 -. model.x) <> "fr",
            ),
          ]),
        ],
        [sp, sp, sp, sp, h3([], [text("Move me around")]), sp, sp, sp, sp],
      ),
      div(
        [],
        [
          button([on_click(Left)], [text("Move to the Left")]),
          button([on_click(Right)], [text("Move to the Right")]),
          button([on_click(Top)], [text("Move to the Top")]),
          button([on_click(Bottom)], [text("Move to the Bottom")]),
        ],
      ),
    ],
  )
}
