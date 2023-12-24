// IMPORTS ---------------------------------------------------------------------

import gleam/function.{identity}
import gleam/set.{type Set}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{Spec}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}

// TYPES -----------------------------------------------------------------------

///
/// 
pub opaque type State(model, msg) {
  State(
    self: Subject(Message(model, msg)),
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    renderers: Set(fn(Element(msg)) -> Nil),
    on_client_event: Dict(String, Decoder(msg)),
  )
}

///
/// 
pub opaque type Message(model, msg) {
  // Events
  GotClientEvent(String, Dynamic)
  GotClientMsg(msg)
  GotRenderer(fn(Element(msg)) -> Nil)

  // Requests
  GetModel(reply: Subject(model))
  RemoveRenderer(fn(Element(msg)) -> Nil)
  Shutdown
}

///
/// 
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_client_event: Dict(String, Decoder(msg)),
) -> Subject(Message(model, msg)) {
  let init = fn() {
    let self = process.new_subject()
    let state = State(self, init.0, update, view, set.new(), on_client_event)
    let selector = process.selecting(process.new_selector(), self, identity)

    actor.Ready(state, selector)
  }

  let assert Ok(subject) =
    actor.start_spec(
      Spec(init, 1000, fn(message, state) {
        case message {
          GotClientMsg(msg) -> {
            let #(model, effects) = state.update(state.model, msg)
            let html = state.view(model)

            set.fold(state.renderers, Nil, fn(_, renderer) { renderer(html) })
            effect.perform(effects, dispatch(_, state.self))
            actor.continue(State(..state, model: model))
          }

          GotRenderer(renderer) -> {
            renderer(state.view(state.model))
            actor.continue(
              State(..state, renderers: set.insert(state.renderers, renderer)),
            )
          }

          GotClientEvent(name, event) -> {
            case dict.get(state.on_client_event, name) {
              Ok(decoder) ->
                decoder(event)
                |> result.map(GotClientMsg)
                |> result.map(actor.send(state.self, _))
                |> result.unwrap(Nil)
              Error(_) -> Nil
            }

            actor.continue(state)
          }

          GetModel(reply) -> {
            actor.send(reply, state.model)
            actor.continue(state)
          }

          RemoveRenderer(renderer) -> {
            actor.continue(
              State(..state, renderers: set.delete(state.renderers, renderer)),
            )
          }

          Shutdown -> actor.Stop(process.Normal)
        }
      }),
    )

  subject
}

///
/// 
pub fn dispatch(msg: msg, to runtime: Subject(Message(model, msg))) -> Nil {
  actor.send(runtime, GotClientMsg(msg))
}

///
/// 
pub fn shutdown(runtime: Subject(Message(model, msg))) -> Nil {
  actor.send(runtime, Shutdown)
}

///
/// 
pub fn handle_client_event(
  runtime: Subject(Message(model, msg)),
  tag: String,
  event: Dynamic,
) -> Nil {
  actor.send(runtime, GotClientEvent(tag, event))
}

///
/// 
pub fn add_renderer(
  runtime: Subject(Message(model, msg)),
  renderer: fn(Element(msg)) -> Nil,
) -> Nil {
  actor.send(runtime, GotRenderer(renderer))
}

///
/// 
pub fn remove_renderer(
  runtime: Subject(Message(model, msg)),
  renderer: fn(Element(msg)) -> Nil,
) -> Nil {
  actor.send(runtime, RemoveRenderer(renderer))
}
