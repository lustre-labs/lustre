import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type ProcessMonitor, type Selector, type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/result
import lustre/effect.{type Effect}
import lustre/vdom/events.{type Events}
import lustre/vdom/node.{type Node}

// STATE -----------------------------------------------------------------------

pub type State(model, msg) {
  State(
    //
    self: Subject(Msg(msg)),
    selector: Selector(Msg(msg)),
    base_selector: Selector(Msg(msg)),
    //
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Node(msg),
    on_attribute_change: Dict(String, Decoder(msg)),
    //
    vdom: Node(msg),
    events: Events(msg),
    //
    subscribers: Dict(Subject(Nil), ProcessMonitor),
  )
}

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Node(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> Result(Subject(Msg(msg)), StartError) {
  todo
}

// UPDATE ----------------------------------------------------------------------

pub type Msg(msg) {
  ClientChangedAttributes(attributes: List(#(String, Dynamic)))
  ClientDispatchedEvent(path: String, name: String, event: Dynamic)
  EffectEmitEvent(name: String, data: Json)
  EffectDispatchedMessage(message: msg)
  SelfDispatchedMessages(messages: List(msg), effect: Effect(msg))
  SubjectSubscribed(subject: Subject(Nil))
  SubjectUnsubscribed(subject: Subject(Nil))
}

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
fn update(
  message: Msg(msg),
  state: State(model, msg),
) -> Next(Msg(msg), State(model, msg)) {
  case message {
    ClientChangedAttributes(attributes: []) -> actor.continue(state)
    ClientChangedAttributes(attributes: [#(name, value), ..attributes]) -> {
      todo
    }

    ClientDispatchedEvent(path:, name:, event:) ->
      case events.handle(state.events, path, name, event) {
        Ok(message) -> todo
        Error(_) -> todo
      }

    EffectEmitEvent(name:, data:) -> {
      todo
    }

    EffectDispatchedMessage(message:) -> {
      todo
    }

    SelfDispatchedMessages(messages: [], effect:) -> {
      todo
    }

    SelfDispatchedMessages(messages: [message, ..messages], effect:) -> {
      let #(model, more_effects) = state.update(state.model, message)
      let vdom = state.view(model)
      let effect = effect.batch([effect, more_effects])
      let state = State(..state, model:, vdom:)

      update(SelfDispatchedMessages(messages, effect), state)
    }

    SubjectSubscribed(subject:) ->
      case dict.has_key(state.subscribers, subject) {
        True -> actor.continue(state)
        False -> {
          todo
        }
      }

    SubjectUnsubscribed(subject:) -> {
      todo
    }
  }
}
