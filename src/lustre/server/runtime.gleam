// IMPORTS ---------------------------------------------------------------------

import gleam/function.{identity}
import gleam/dict.{type Dict}
import gleam/set.{type Set}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{type StartError, Spec}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}

// TYPES -----------------------------------------------------------------------

/// 
/// 
pub type ComponentRuntime(msg) =
  Subject(Message(msg))

///
/// 
type State(model, msg) {
  State(
    self: ComponentRuntime(msg),
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    html: Element(msg),
    renderers: Set(fn(Element(msg)) -> Nil),
    handlers: Dict(String, fn(Dynamic) -> Result(msg, Nil)),
  )
}

/// 
/// 
pub opaque type Message(msg) {
  // Events
  GotClientEvent(String, Dynamic)
  GotClientMsg(msg)
  GotRenderer(fn(Element(msg)) -> Nil)

  // Requests
  RemoveRenderer(fn(Element(msg)) -> Nil)

  // Internals
  Shutdown
}

///
/// 
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> Result(ComponentRuntime(msg), StartError) {
  let timeout = 1000
  let init = fn() {
    let self = process.new_subject()
    let html = view(init.0)
    let handlers = element.handlers(html)
    let state = State(self, init.0, update, view, html, set.new(), handlers)
    let selector = process.selecting(process.new_selector(), self, identity)

    actor.Ready(state, selector)
  }

  actor.start_spec({
    use message, state <- Spec(init, timeout)
    case message {
      // Events ----------------------------------------------------------------
      GotClientMsg(msg) -> {
        let #(model, effects) = state.update(state.model, msg)
        let html = state.view(model)
        let handlers = element.handlers(html)
        let next = State(..state, model: model, html: html, handlers: handlers)

        set.fold(state.renderers, Nil, fn(_, renderer) { renderer(html) })
        effect.perform(effects, dispatch(_, state.self))
        actor.continue(next)
      }

      GotRenderer(renderer) -> {
        let renderers = set.insert(state.renderers, renderer)
        let next = State(..state, renderers: renderers)

        renderer(state.html)
        actor.continue(next)
      }

      GotClientEvent(name, event) -> {
        case dict.get(state.handlers, name) {
          Error(_) -> actor.continue(state)
          Ok(handler) -> {
            handler(event)
            |> result.map(GotClientMsg)
            |> result.map(actor.send(state.self, _))
            |> result.unwrap(Nil)

            actor.continue(state)
          }
        }
      }

      // Requests --------------------------------------------------------------
      RemoveRenderer(renderer) -> {
        actor.continue(
          State(..state, renderers: set.delete(state.renderers, renderer)),
        )
      }

      // Internals -------------------------------------------------------------
      Shutdown -> actor.Stop(process.Killed)
    }
  })
}

/// Dispatch a message for the server component to handle. This will always cause
/// the component to be re-rendered and any listeners to be notified.
/// 
pub fn dispatch(msg: msg, to runtime: ComponentRuntime(msg)) -> Nil {
  actor.send(runtime, GotClientMsg(msg))
}

/// Instruct the component to shutdown. The 
/// [`ExitReason`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#ExitReason)
/// will be `Killed`.
/// 
pub fn shutdown(runtime: ComponentRuntime(msg)) -> Nil {
  actor.send(runtime, Shutdown)
}

/// Pass a client event to the server component to handle. 
/// 
pub fn handle_client_event(
  runtime: ComponentRuntime(msg),
  tag: String,
  event: Dynamic,
) -> Nil {
  actor.send(runtime, GotClientEvent(tag, event))
}

///
/// 
pub fn add_renderer(
  runtime: ComponentRuntime(msg),
  renderer: fn(Element(msg)) -> Nil,
) -> Nil {
  actor.send(runtime, GotRenderer(renderer))
}

///
/// 
pub fn remove_renderer(
  runtime: ComponentRuntime(msg),
  renderer: fn(Element(msg)) -> Nil,
) -> Nil {
  actor.send(runtime, RemoveRenderer(renderer))
}
