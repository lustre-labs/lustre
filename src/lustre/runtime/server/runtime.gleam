// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type ProcessMonitor, type Selector, type Subject}
import gleam/function
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/result
import gleam/set.{type Set}
import lustre/effect.{type Effect}
import lustre/runtime/transport.{type ClientMessage, type ServerMessage}
import lustre/vdom/diff.{Diff, diff}
import lustre/vdom/events.{type Events}
import lustre/vdom/vnode.{type Element}

// STATE -----------------------------------------------------------------------

pub type ServerComponent(msg) =
  Subject(Message(msg))

pub type State(model, msg) {
  State(
    //
    self: Subject(Message(msg)),
    selector: Selector(Message(msg)),
    base_selector: Selector(Message(msg)),
    //
    model: model,
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    config: Config(msg),
    //
    vdom: Element(msg),
    events: Events(msg),
    //
    subscribers: Dict(Subject(ClientMessage(msg)), ProcessMonitor),
    callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  )
}

pub type Config(msg) {
  Config(
    open_shadow_root: Bool,
    adopt_styles: Bool,
    attributes: Dict(String, fn(String) -> Result(msg, Nil)),
    properties: Dict(String, Decoder(msg)),
  )
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  config: Config(msg),
) -> Result(Subject(Message(msg)), StartError) {
  actor.start_spec({
    use <- Spec(init: _, init_timeout: 1000, loop:)
    let self = process.new_subject()
    let base_selector =
      process.new_selector()
      |> process.selecting(self, function.identity)

    let vdom = view(init.0)
    let events = events.from_node(vdom)

    let state =
      State(
        self:,
        selector: base_selector,
        base_selector:,
        //
        model: init.0,
        update:,
        view:,
        config:,
        //
        vdom:,
        events:,
        //
        subscribers: dict.new(),
        callbacks: set.new(),
      )

    handle_effect(self, init.1)

    actor.Ready(state, base_selector)
  })
}

// UPDATE ----------------------------------------------------------------------

pub type Message(msg) {
  ClientDispatchedMessage(message: ServerMessage)
  ClientRegisteredSubject(client: Subject(ClientMessage(msg)))
  ClientDeregisteredSubject(client: Subject(ClientMessage(msg)))
  ClientRegisteredCallback(callback: fn(ClientMessage(msg)) -> Nil)
  ClientDeregisteredCallback(callback: fn(ClientMessage(msg)) -> Nil)
  //
  EffectAddedSelector(selector: Selector(Message(msg)))
  EffectDispatchedMessage(message: msg)
  EffectEmitEvent(name: String, data: Json)
  //
  SelfDispatchedMessages(messages: List(msg), effect: Effect(msg))
  //
  SystemRequestedShutdown
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
fn loop(
  message: Message(msg),
  state: State(model, msg),
) -> Next(Message(msg), State(model, msg)) {
  case echo message {
    ClientDispatchedMessage(message:) -> {
      let next = handle_client_message(state, message)
      let Diff(patch:, events:) = diff(state.events, state.vdom, next.vdom)

      broadcast(state.subscribers, state.callbacks, transport.reconcile(patch))

      actor.continue(State(..next, events:))
    }

    ClientRegisteredSubject(client:) ->
      case dict.has_key(state.subscribers, client) {
        True -> actor.continue(state)
        False -> {
          let monitor = process.monitor_process(process.subject_owner(client))
          let subscribers = dict.insert(state.subscribers, client, monitor)
          let selector = {
            use selector, client, monitor <- dict.fold(
              subscribers,
              state.base_selector,
            )

            process.selecting_process_down(selector, monitor, fn(_) {
              ClientDeregisteredSubject(client:)
            })
          }

          process.send(
            client,
            transport.mount(
              state.config.open_shadow_root,
              state.config.adopt_styles,
              dict.keys(state.config.attributes),
              dict.keys(state.config.properties),
              state.vdom,
            ),
          )

          actor.Continue(
            State(..state, subscribers:, selector:),
            Some(selector),
          )
        }
      }

    ClientDeregisteredSubject(client:) ->
      case dict.get(state.subscribers, client) {
        Error(_) -> actor.continue(state)
        Ok(monitor) -> {
          let _ = process.demonitor_process(monitor)
          let subscribers = dict.delete(state.subscribers, client)
          let selector = {
            use selector, client, monitor <- dict.fold(
              subscribers,
              state.base_selector,
            )
            let unsubscribe = fn(_) { ClientDeregisteredSubject(client:) }

            process.selecting_process_down(selector, monitor, unsubscribe)
          }

          actor.Continue(
            State(..state, subscribers:, selector:),
            Some(selector),
          )
        }
      }

    ClientRegisteredCallback(callback:) ->
      case set.contains(state.callbacks, callback) {
        True -> actor.continue(state)
        False -> {
          let callbacks = set.insert(state.callbacks, callback)

          callback(transport.mount(
            state.config.open_shadow_root,
            state.config.adopt_styles,
            dict.keys(state.config.attributes),
            dict.keys(state.config.properties),
            state.vdom,
          ))

          actor.continue(State(..state, callbacks:))
        }
      }

    ClientDeregisteredCallback(callback:) ->
      case set.contains(state.callbacks, callback) {
        False -> actor.continue(state)
        True -> {
          let callbacks = set.delete(state.callbacks, callback)

          actor.continue(State(..state, callbacks:))
        }
      }

    EffectAddedSelector(selector:) -> {
      let base_selector = process.merge_selector(state.base_selector, selector)
      let selector = process.merge_selector(state.selector, selector)

      actor.Continue(State(..state, base_selector:, selector:), Some(selector))
    }

    EffectDispatchedMessage(message:) -> {
      let #(model, effect) = state.update(state.model, message)
      let vdom = state.view(model)
      let Diff(patch:, events:) = diff(state.events, state.vdom, vdom)

      handle_effect(state.self, effect)
      broadcast(state.subscribers, state.callbacks, transport.reconcile(patch))

      actor.continue(State(..state, model:, vdom:, events:))
    }

    EffectEmitEvent(name:, data:) -> {
      broadcast(state.subscribers, state.callbacks, transport.emit(name, data))

      actor.continue(state)
    }

    SelfDispatchedMessages(messages: [], effect:) -> {
      let vdom = state.view(state.model)
      let Diff(patch:, events:) = diff(state.events, state.vdom, vdom)

      handle_effect(state.self, effect)
      broadcast(state.subscribers, state.callbacks, transport.reconcile(patch))

      actor.continue(State(..state, vdom:, events:))
    }

    SelfDispatchedMessages(messages: [message, ..messages], effect:) -> {
      let #(model, more_effects) = state.update(state.model, message)
      let effect = effect.batch([effect, more_effects])
      let state = State(..state, model:)

      loop(SelfDispatchedMessages(messages, effect), state)
    }

    SystemRequestedShutdown -> {
      dict.each(state.subscribers, fn(_, monitor) {
        process.demonitor_process(monitor)
      })

      actor.Stop(process.Normal)
    }
  }
}

fn handle_client_message(
  state: State(model, msg),
  message: ServerMessage,
) -> State(model, msg) {
  case message {
    transport.Batch(messages:, ..) ->
      list.fold(messages, state, handle_client_message)

    transport.AttributeChanged(name:, value:, ..) ->
      case handle_attribute_change(state.config.attributes, name, value) {
        Error(_) -> state
        Ok(msg) -> {
          let #(model, effect) = state.update(state.model, msg)
          let vdom = state.view(model)

          handle_effect(state.self, effect)

          State(..state, model:, vdom:)
        }
      }

    transport.PropertyChanged(name:, value:, ..) ->
      case handle_property_change(state.config.properties, name, value) {
        Error(_) -> state
        Ok(msg) -> {
          let #(model, effect) = state.update(state.model, msg)
          let vdom = state.view(model)

          handle_effect(state.self, effect)

          State(..state, model:, vdom:)
        }
      }

    transport.EventFired(path:, name:, event:, ..) ->
      case events.handle(state.events, path, name, event) {
        #(events, Error(_)) -> State(..state, events:)
        #(events, Ok(message)) -> {
          let #(model, effect) = state.update(state.model, message)
          let vdom = state.view(model)

          handle_effect(state.self, effect)

          State(..state, model:, vdom:, events:)
        }
      }
  }
}

fn handle_attribute_change(
  attributes: Dict(String, fn(String) -> Result(msg, Nil)),
  name: String,
  value: String,
) -> Result(msg, Nil) {
  case dict.get(attributes, name) {
    Error(_) -> Error(Nil)
    Ok(handler) -> handler(value)
  }
}

fn handle_property_change(
  properties: Dict(String, Decoder(msg)),
  name: String,
  value: Dynamic,
) -> Result(msg, Nil) {
  case dict.get(properties, name) {
    Error(_) -> Error(Nil)
    Ok(decoder) -> decode.run(value, decoder) |> result.replace_error(Nil)
  }
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
fn handle_effect(self: Subject(Message(msg)), effect: Effect(msg)) -> Nil {
  let send = process.send(self, _)
  let dispatch = fn(message) { send(EffectDispatchedMessage(message:)) }
  let emit = fn(name, data) { send(EffectEmitEvent(name:, data:)) }

  let select = fn(selector) {
    selector
    |> process.map_selector(EffectDispatchedMessage)
    |> EffectAddedSelector
    |> send
  }

  let internals = fn() { dynamic.from(Nil) }

  effect.perform(effect, dispatch, emit, select, internals)
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
fn broadcast(
  clients: Dict(Subject(ClientMessage(msg)), ProcessMonitor),
  callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  message: ClientMessage(msg),
) -> Nil {
  dict.each(clients, fn(client, _) { process.send(client, message) })
  set.each(callbacks, fn(callback) { callback(message) })
}
