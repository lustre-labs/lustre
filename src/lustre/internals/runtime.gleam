// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/erlang/process.{type Selector, type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/internals/patch.{type Patch, Diff, Init}
import lustre/internals/vdom

// TYPES -----------------------------------------------------------------------

///
///
type State(model, msg, runtime) {
  State(
    self: Subject(Action(msg, runtime)),
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    html: Element(msg),
    renderers: Dict(String, fn(Patch(msg)) -> Nil),
    handlers: Dict(String, Decoder(msg)),
    on_attribute_change: Dict(String, Decoder(msg)),
  )
}

///
///
pub type Action(msg, runtime) {
  Attrs(List(#(String, Dynamic)))
  Batch(List(msg), Effect(msg))
  Debug(DebugAction)
  Dispatch(msg)
  Emit(String, Json)
  Event(String, Dynamic)
  SetSelector(Selector(Action(msg, runtime)))
  Shutdown
  Subscribe(String, fn(Patch(msg)) -> Nil)
  Unsubscribe(String)
}

pub type DebugAction {
  ForceModel(Dynamic)
  Model(reply: fn(Dynamic) -> Nil)
  View(reply: fn(Element(Dynamic)) -> Nil)
}

// ACTOR -----------------------------------------------------------------------

@target(erlang)
///
///
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> Result(Subject(Action(msg, runtime)), StartError) {
  let timeout = 1000
  let init = fn() {
    let self = process.new_subject()
    let html = view(init.0)
    let handlers = vdom.handlers(html)
    let state =
      State(
        self,
        init.0,
        update,
        view,
        html,
        dict.new(),
        handlers,
        on_attribute_change,
      )
    let selector =
      process.selecting(process.new_selector(), self, fn(msg) { msg })

    run_effects(init.1, self)
    actor.Ready(state, selector)
  }

  actor.start_spec(Spec(init, timeout, loop))
}

@target(erlang)
fn loop(
  message: Action(msg, runtime),
  state: State(model, msg, runtime),
) -> Next(Action(msg, runtime), State(model, msg, runtime)) {
  case message {
    Attrs(attrs) -> {
      list.filter_map(attrs, fn(attr) {
        case dict.get(state.on_attribute_change, attr.0) {
          Error(_) -> Error(Nil)
          Ok(decoder) ->
            decoder(attr.1)
            |> result.replace_error(Nil)
        }
      })
      |> Batch(effect.none())
      |> loop(state)
    }

    Batch([], _) -> actor.continue(state)
    Batch([msg], other_effects) -> {
      let #(model, effects) = state.update(state.model, msg)
      let html = state.view(model)
      let diff = patch.elements(state.html, html)
      let next =
        State(..state, model: model, html: html, handlers: diff.handlers)

      run_effects(effect.batch([effects, other_effects]), state.self)

      case patch.is_empty_element_diff(diff) {
        True -> Nil
        False -> run_renderers(state.renderers, Diff(diff))
      }

      actor.continue(next)
    }
    Batch([msg, ..rest], other_effects) -> {
      let #(model, effects) = state.update(state.model, msg)
      let html = state.view(model)
      let diff = patch.elements(state.html, html)
      let next =
        State(..state, model: model, html: html, handlers: diff.handlers)

      loop(Batch(rest, effect.batch([effects, other_effects])), next)
    }

    Debug(ForceModel(model)) -> {
      let model = dynamic.unsafe_coerce(model)
      let html = state.view(model)
      let diff = patch.elements(state.html, html)
      let next =
        State(..state, model: model, html: html, handlers: diff.handlers)

      case patch.is_empty_element_diff(diff) {
        True -> Nil
        False -> run_renderers(state.renderers, Diff(diff))
      }

      actor.continue(next)
    }

    Debug(Model(reply)) -> {
      reply(dynamic.from(state.model))
      actor.continue(state)
    }

    Debug(View(reply)) -> {
      reply(element.map(state.html, dynamic.from))
      actor.continue(state)
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

    Subscribe(id, renderer) -> {
      let renderers = dict.insert(state.renderers, id, renderer)
      let next = State(..state, renderers: renderers)

      renderer(Init(dict.keys(state.on_attribute_change), state.html))
      actor.continue(next)
    }

    Unsubscribe(id) -> {
      let renderers = dict.delete(state.renderers, id)
      let next = State(..state, renderers: renderers)

      actor.continue(next)
    }

    SetSelector(selector) -> actor.Continue(state, Some(selector))
    Shutdown -> actor.Stop(process.Killed)
  }
}

// UTILS -----------------------------------------------------------------------

@target(erlang)
fn run_renderers(
  renderers: Dict(any, fn(Patch(msg)) -> Nil),
  patch: Patch(msg),
) -> Nil {
  use _, _, renderer <- dict.fold(renderers, Nil)
  renderer(patch)
}

@target(erlang)
fn run_effects(effects: Effect(msg), self: Subject(Action(msg, runtime))) -> Nil {
  let dispatch = fn(msg) { actor.send(self, Dispatch(msg)) }
  let emit = fn(name, event) { actor.send(self, Emit(name, event)) }

  effect.perform(effects, dispatch, emit)
}
