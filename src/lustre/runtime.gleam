// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/function.{identity}
import gleam/json.{type Json}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/result
import gleam/set.{type Set}
import lustre/effect.{type Effect}
import lustre/element.{type Element, type Patch}
import lustre/internals/patch.{Diff, Morph}
import lustre/internals/vdom

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
    renderers: Set(fn(Patch(msg)) -> Nil),
    handlers: Dict(String, fn(Dynamic) -> Result(msg, Nil)),
  )
}

/// 
/// 
pub type Action(runtime, msg) {
  AddRenderer(fn(Patch(msg)) -> Nil)
  Dispatch(msg)
  Emit(String, Json)
  Event(String, Dynamic)
  RemoveRenderer(fn(Patch(msg)) -> Nil)
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
    let handlers = vdom.handlers(html)
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

      renderer(Morph(state.html))
      actor.continue(next)
    }

    Dispatch(msg) -> {
      let #(model, effects) = state.update(state.model, msg)
      let html = state.view(model)
      let diff = patch.elements(state.html, html)
      let next =
        State(..state, model: model, html: html, handlers: diff.handlers)

      run_effects(effects, state.self)

      case patch.is_empty_element_diff(diff) {
        True -> Nil
        False -> run_renderers(state.renderers, Diff(diff))
      }

      actor.continue(next)
    }

    Emit(name, event) -> {
      let patch = patch.Emit(name, event)

      run_renderers(state.renderers, patch)
      actor.continue(state)
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

fn run_renderers(
  renderers: Set(fn(Patch(msg)) -> Nil),
  patch: Patch(msg),
) -> Nil {
  use _, renderer <- set.fold(renderers, Nil)
  renderer(patch)
}

fn run_effects(effects: Effect(msg), self: Subject(Action(runtime, msg))) -> Nil {
  let dispatch = fn(msg) { actor.send(self, Dispatch(msg)) }
  let emit = fn(name, event) { actor.send(self, Emit(name, event)) }

  effect.perform(effects, dispatch, emit)
}
