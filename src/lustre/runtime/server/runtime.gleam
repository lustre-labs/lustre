// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type ProcessMonitor, type Selector, type Subject}
import gleam/function
import gleam/json.{type Json}
import gleam/option.{Some}
import gleam/otp/actor.{type Next, type StartError, Spec}
import gleam/set.{type Set}
import lustre/effect.{type Effect}
import lustre/runtime/transport.{type ClientMessage, type ServerMessage}
import lustre/vdom/diff.{Diff, diff}
import lustre/vdom/events.{type Events}
import lustre/vdom/node.{type Node}

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
    view: fn(model) -> Node(msg),
    on_attribute_change: Dict(String, Decoder(msg)),
    //
    vdom: Node(msg),
    events: Events(msg),
    //
    subscribers: Dict(Subject(ClientMessage(msg)), ProcessMonitor),
    callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  )
}

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
pub fn start(
  init: #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Node(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> Result(Subject(Message(msg)), StartError) {
  actor.start_spec({
    use <- Spec(init: _, init_timeout: 1000, loop:)
    let self = process.new_subject()
    let base_selector =
      process.new_selector()
      |> process.selecting(self, function.identity)

    let vdom = view(init.0)
    let events =
      events.new(function.identity)
      |> events.add_child(function.identity, 0, vdom)

    let state =
      State(
        self:,
        selector: base_selector,
        base_selector:,
        //
        model: init.0,
        update:,
        view:,
        on_attribute_change:,
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

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
fn loop(
  message: Message(msg),
  state: State(model, msg),
) -> Next(Message(msg), State(model, msg)) {
  case message {
    ClientDispatchedMessage(message: transport.AttributesChanged(..) as message) -> {
      let #(model, effect, did_update) =
        handle_attribute_changes(
          message.attributes,
          state.on_attribute_change,
          state.update,
          False,
          #(state.model, effect.none()),
        )

      use <- bool.lazy_guard(!did_update, fn() { actor.continue(state) })
      let vdom = state.view(model)
      let Diff(patch:, events:) = diff(state.vdom, vdom)

      handle_effect(state.self, effect)
      broadcast(state.subscribers, state.callbacks, transport.reconcile(patch))

      actor.continue(State(..state, model:, vdom:, events:))
    }

    ClientDispatchedMessage(message: transport.EventFired(..) as message) ->
      case
        events.handle(state.events, message.path, message.name, message.event)
      {
        Error(_) -> actor.continue(state)
        Ok(message) -> {
          let #(model, effect) = state.update(state.model, message)
          let vdom = state.view(model)
          let Diff(patch:, events:) = diff(state.vdom, vdom)

          handle_effect(state.self, effect)
          broadcast(
            state.subscribers,
            state.callbacks,
            transport.reconcile(patch),
          )

          actor.continue(State(..state, model:, vdom:, events:))
        }
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

          process.send(client, transport.mount(state.vdom))

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

          callback(transport.mount(state.vdom))
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
      let vdom = state.view(state.model)
      let Diff(patch:, events:) = diff(state.vdom, vdom)

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
      let Diff(patch:, events:) = diff(state.vdom, vdom)

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

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
fn handle_attribute_changes(
  attributes: List(#(String, Dynamic)),
  on_attribute_change: Dict(String, Decoder(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  did_update: Bool,
  model: #(model, Effect(msg)),
) -> #(model, Effect(msg), Bool) {
  case attributes {
    [] -> #(model.0, model.1, did_update)

    [#(name, value), ..attributes] ->
      case dict.get(on_attribute_change, name) {
        Error(_) ->
          handle_attribute_changes(
            attributes,
            on_attribute_change,
            update,
            did_update,
            model,
          )

        Ok(decoder) ->
          case decode.run(value, decoder) {
            Error(_) ->
              handle_attribute_changes(
                attributes,
                on_attribute_change,
                update,
                did_update,
                model,
              )

            Ok(message) -> {
              let #(new_model, effect) = update(model.0, message)

              handle_attribute_changes(
                attributes,
                on_attribute_change,
                update,
                True,
                #(new_model, effect.batch([effect, model.1])),
              )
            }
          }
      }
  }
}

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
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

  let root = dynamic.from(Nil)

  effect.perform(effect, dispatch, emit, select, root)
}

@external(javascript, "../client/core.ffi.mjs", "throw_server_component_error")
fn broadcast(
  clients: Dict(Subject(ClientMessage(msg)), ProcessMonitor),
  callbacks: Set(fn(ClientMessage(msg)) -> Nil),
  message: ClientMessage(msg),
) -> Nil {
  dict.each(clients, fn(client, _) { process.send(client, message) })
  set.each(callbacks, fn(callback) { callback(message) })
}
