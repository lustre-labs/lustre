// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Monitor, type Selector, type Subject}
import gleam/json.{type Json}
import gleam/list
import gleam/otp/actor.{type Next, type StartError}
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
    providers: Dict(String, Json),
    //
    subscribers: Dict(Subject(ClientMessage(msg)), Monitor),
    callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  )
}

pub type Config(msg) {
  Config(
    open_shadow_root: Bool,
    adopt_styles: Bool,
    attributes: Dict(String, fn(String) -> Result(msg, Nil)),
    properties: Dict(String, Decoder(msg)),
    contexts: Dict(String, Decoder(msg)),
  )
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  config: Config(msg),
) -> Result(Subject(Message(msg)), StartError) {
  let result =
    actor.new_with_initialiser(1000, fn(self) {
      let vdom = view(init.0)
      let events = events.from_node(vdom)
      let base_selector =
        process.new_selector()
        |> process.select(self)
        |> process.select_monitors(fn(down) {
          MonitorReportedDown(down.monitor)
        })

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
          providers: dict.new(),
          //
          subscribers: dict.new(),
          callbacks: set.new(),
        )

      handle_effect(self, init.1)

      actor.initialised(state)
      |> actor.selecting(base_selector)
      |> actor.returning(self)
      |> Ok
    })
    |> actor.on_message(loop)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(error) -> Error(error)
  }
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
  EffectProvidedValue(key: String, value: Json)
  //
  MonitorReportedDown(monitor: Monitor)
  //
  SystemRequestedShutdown
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
fn loop(
  state: State(model, msg),
  message: Message(msg),
) -> Next(State(model, msg), Message(msg)) {
  case message {
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
          case process.subject_owner(client) {
            Error(_) -> actor.continue(state)
            Ok(pid) -> {
              let monitor = process.monitor(pid)
              let subscribers = dict.insert(state.subscribers, client, monitor)

              process.send(
                client,
                transport.mount(
                  state.config.open_shadow_root,
                  state.config.adopt_styles,
                  dict.keys(state.config.attributes),
                  dict.keys(state.config.properties),
                  dict.keys(state.config.contexts),
                  state.providers,
                  state.vdom,
                ),
              )

              actor.continue(State(..state, subscribers:))
            }
          }
        }
      }

    ClientDeregisteredSubject(client:) -> {
      let subscribers = dict.delete(state.subscribers, client)

      actor.continue(State(..state, subscribers:))
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
            dict.keys(state.config.contexts),
            state.providers,
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

      actor.continue(State(..state, base_selector:, selector:))
      |> actor.with_selector(selector)
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

    EffectProvidedValue(key:, value:) -> {
      let providers = dict.insert(state.providers, key, value)

      broadcast(
        state.subscribers,
        state.callbacks,
        transport.provide(key, value),
      )

      actor.continue(State(..state, providers:))
    }

    MonitorReportedDown(monitor:) -> {
      let subscribers =
        dict.filter(state.subscribers, fn(_, m) { m != monitor })

      actor.continue(State(..state, subscribers:))
    }

    SystemRequestedShutdown -> {
      dict.each(state.subscribers, fn(_, monitor) {
        process.demonitor_process(monitor)
      })

      actor.stop()
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
        #(events, Ok(handler)) -> {
          let #(model, effect) = state.update(state.model, handler.message)
          let vdom = state.view(model)

          handle_effect(state.self, effect)

          State(..state, model:, vdom:, events:)
        }
      }

    transport.ContextProvided(key:, value:, ..) -> {
      case dict.get(state.config.contexts, key) {
        Error(_) -> state
        Ok(decoder) -> {
          case decode.run(value, decoder) {
            Error(_) -> state
            Ok(context) -> {
              let #(model, effect) = state.update(state.model, context)
              let vdom = state.view(model)

              handle_effect(state.self, effect)

              State(..state, model:, vdom:)
            }
          }
        }
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
  let provide = fn(key, value) { send(EffectProvidedValue(key:, value:)) }

  let select = fn(selector) {
    selector
    |> process.map_selector(EffectDispatchedMessage)
    |> EffectAddedSelector
    |> send
  }

  let internals = fn() { dynamic.nil() }

  effect.perform(effect, dispatch, emit, select, internals, provide)
}

@external(javascript, "../client/runtime.ffi.mjs", "throw_server_component_error")
fn broadcast(
  clients: Dict(Subject(ClientMessage(msg)), Monitor),
  callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  message: ClientMessage(msg),
) -> Nil {
  dict.each(clients, fn(client, _) { process.send(client, message) })
  set.each(callbacks, fn(callback) { callback(message) })
}
