import lustre
import lustre/animation.{Animations}
import lustre/attribute.{id, style}
import lustre/effect.{Effect}
import lustre/element.{Element, text}
import lustre/element/html.{div, h3}
import lustre/event.{on}
import gleam/float
import gleam/int
import gleam/list.{filter, map}
import gleam/dynamic.{Dynamic} as d
import gleam/option.{Some}

pub type Msg {
  Click(x: Float, y: Float)
  Tick(time_offset: Float)
}

pub type Drop {
  Drop(id: String, x: Float, y: Float, r: Float)
}

pub type Model {
  Model(counter: Int, drops: List(Drop), animations: Animations)
}

pub fn main() {
  lustre.application(init, update, render)
  |> lustre.start("#drops", Nil)
}

fn init(_) {
  #(Model(0, [], animation.new()), effect.none())
}

const to_s = int.to_string

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  let m = case msg {
    Click(x, y) -> {
      let id = "drop" <> to_s(model.counter)
      let new_animations = animation.add(model.animations, id, 0.0, 1.0, 1.5)
      let new_drop = Drop(id, x, y, 0.0)
      Model(model.counter + 1, [new_drop, ..model.drops], new_animations)
    }
    Tick(time_offset) -> {
      let new_animations = animation.tick(model.animations, time_offset)
      let new_drops =
        model.drops
        |> filter(fn(drop) { drop.r != 1.0 })
        |> map(fn(drop) {
          let r = animation.value(new_animations, drop.id, drop.r)
          Drop(..drop, r: r)
        })
      Model(..model, drops: new_drops, animations: new_animations)
    }
  }
  #(m, animation.effect(m.animations, Tick))
}

pub fn render(model: Model) -> Element(Msg) {
  div(
    [
      style([
        #("width", "100%"),
        #("height", "100%"),
        #("display", "grid"),
        #("grid-template-rows", "auto 1fr"),
      ]),
    ],
    [
      h3([style([#("text-align", "center")])], [text("Click to make drops")]),
      div(
        [
          id("pond"),
          style([#("position", "relative")]),
          on(
            "mouseDown",
            fn(event) {
              let assert Ok(x) = d.field("clientX", d.float)(event)
              let assert Ok(y) = d.field("clientY", d.float)(event)
              let rect = bounding_client_rect("pond")
              let assert Ok(top) = d.field("top", d.float)(rect)
              let assert Ok(left) = d.field("left", d.float)(rect)

              Some(Click(x -. left, y -. top))
            },
          ),
        ],
        map(model.drops, render_drop),
      ),
    ],
  )
}

fn render_drop(drop: Drop) {
  let r = drop.r *. 50.0
  let rad = float.to_string(r *. 2.0)
  let rw = float.to_string(drop.r *. 2.5)
  let alpha =
    1.0 -. drop.r
    |> float.to_string
  div(
    [
      style([
        #("position", "absolute"),
        #("left", float.to_string(drop.x -. r) <> "px"),
        #("top", float.to_string(drop.y -. r) <> "px"),
        #("width", rad <> "px"),
        #("height", rad <> "px"),
        #("border", rw <> "px solid rgba(0, 0, 0, " <> alpha <> ")"),
        #("border-radius", "50%"),
      ]),
    ],
    [],
  )
}

external fn bounding_client_rect(String) -> Dynamic =
  "./info.mjs" "bounding_client_rect"
