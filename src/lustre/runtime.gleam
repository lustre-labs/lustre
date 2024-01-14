// IMPORTS ---------------------------------------------------------------------

import gleam/function.{identity}
import gleam/dict.{type Dict}
import gleam/set.{type Set}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Diff, type Element}

// TYPES -----------------------------------------------------------------------

///
/// 
type State(runtime, model, msg) {
  State(
    self: Subject(Action(runtime, msg)),
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    html: Element(msg),
    renderers: Set(fn(Diff(msg)) -> Nil),
    handlers: Dict(String, fn(Dynamic) -> Result(msg, Nil)),
  )
}

/// 
/// 
pub type Action(runtime, msg) {
  AddRenderer(fn(Diff(msg)) -> Nil)
  Dispatch(msg)
  Event(String, Dynamic)
  RemoveRenderer(fn(Diff(msg)) -> Nil)
  Shutdown
}

// ACTOR -----------------------------------------------------------------------

///
/// 
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> Result(Subject(Action(runtime, msg)), StartError) {
  let timeout = 1000
  let init = fn() {
    let self = process.new_subject()
    let html = view(init.0)
    let handlers = element.handlers(html)
    let state = State(self, init.0, update, view, html, set.new(), handlers)
    let selector = process.selecting(process.new_selector(), self, identity)

    run_effects(init.1, self)
    actor.Ready(state, selector)
  }

  actor.start_spec(Spec(init, timeout, loop))
}

fn loop(
  message: Action(runtime, msg),
  state: State(runtime, model, msg),
) -> Next(Action(runtime, msg), State(runtime, model, msg)) {
  case message {
    AddRenderer(renderer) -> {
      let renderers = set.insert(state.renderers, renderer)
      let next = State(..state, renderers: renderers)
      let diff = element.diff(element.text(""), state.html)

      renderer(diff)
      actor.continue(next)
    }

    Dispatch(msg) -> {
      let #(model, effects) = state.update(state.model, msg)
      let html = state.view(model)
      let diff = element.diff(state.html, html)
      let next =
        State(..state, model: model, html: html, handlers: diff.handlers)

      run_renderers(state.renderers, diff)
      run_effects(effects, state.self)
      actor.continue(next)
    }

    Event(name, event) -> {
      case dict.get(state.handlers, name) {
        Error(_) -> actor.continue(state)
        Ok(handler) -> {
          handler(event)
          |> result.map(Dispatch)
          |> result.map(actor.send(state.self, _))
          |> result.unwrap(Nil)

          actor.continue(state)
        }
      }
    }

    RemoveRenderer(renderer) -> {
      let renderers = set.delete(state.renderers, renderer)
      let next = State(..state, renderers: renderers)

      actor.continue(next)
    }

    Shutdown -> actor.Stop(process.Killed)
  }
}

// UTILS -----------------------------------------------------------------------

fn run_renderers(renderers: Set(fn(Diff(msg)) -> Nil), diff: Diff(msg)) -> Nil {
  use _, renderer <- set.fold(renderers, Nil)
  renderer(diff)
}

fn run_effects(effects: Effect(msg), self: Subject(Action(runtime, msg))) -> Nil {
  use msg <- effect.perform(effects)
  actor.send(self, Dispatch(msg))
}
