// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import group_registry.{type GroupRegistry}
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/server_component
import server/component_contract.{
  type ComponentContract, type ConnectionContext, type Resources,
}

// TYPES -----------------------------------------------------------------------

/// A declarative definition for a pub/sub enabled component.
///
/// This separates domain logic from pub/sub infrastructure by:
/// - Keeping the model free of registry references
/// - Having `update` return an optional shared message to broadcast
/// - Handling incoming shared messages in a separate `on_shared` function
pub type PubSubComponent(model, local_msg, shared_msg) {
  PubSubComponent(
    /// The topic name to subscribe to in the registry
    topic: String,
    /// Initial model state
    init_model: model,
    /// Initial effects to run (mapped to local messages)
    init_effect: Effect(local_msg),
    /// Update function for local messages.
    /// Returns the new model, effects, and optionally a message to broadcast.
    update: fn(model, local_msg) -> #(model, Effect(local_msg), Option(shared_msg)),
    /// Handle incoming shared messages from other instances.
    on_shared: fn(model, shared_msg) -> #(model, Effect(local_msg)),
    /// View function rendering the model to HTML.
    view: fn(model) -> Element(local_msg),
  )
}

/// Internal state wrapping the user's model plus registry info.
pub opaque type PubSubState(model, shared_msg) {
  PubSubState(
    model: model,
    registry: GroupRegistry(shared_msg),
    topic: String,
  )
}

/// Internal message type distinguishing shared vs local messages.
pub opaque type PubSubMsg(local_msg, shared_msg) {
  ReceivedShared(shared_msg)
  Local(local_msg)
}

// PUBLIC API ------------------------------------------------------------------

/// Build a Lustre App from a PubSubComponent definition.
///
/// The resulting App expects a `GroupRegistry(shared_msg)` as its init argument.
pub fn build(
  component: PubSubComponent(model, local_msg, shared_msg),
) -> lustre.App(
  GroupRegistry(shared_msg),
  PubSubState(model, shared_msg),
  PubSubMsg(local_msg, shared_msg),
) {
  lustre.application(
    fn(registry) { init(component, registry) },
    fn(state, msg) { update(component, state, msg) },
    fn(state) { view(component, state) },
  )
}

/// Create a ComponentContract from a PubSubComponent.
///
/// The `registry_decoder` extracts the GroupRegistry from the Resources
/// in the ConnectionContext.
pub fn to_contract(
  component: PubSubComponent(model, local_msg, shared_msg),
  registry_decoder: Decoder(GroupRegistry(shared_msg)),
) -> ComponentContract(
  GroupRegistry(shared_msg),
  PubSubState(model, shared_msg),
  PubSubMsg(local_msg, shared_msg),
) {
  let app = build(component)

  component_contract.contract(app, fn(ctx: ConnectionContext) {
    let assert Ok(registry) =
      component_contract.get_resources(ctx.resources, registry_decoder)
    registry
  })
}

/// Create a ComponentContract using a function to extract the registry.
///
/// This is an alternative to `to_contract` when you need more control
/// over how the registry is obtained from resources.
pub fn to_contract_with(
  component: PubSubComponent(model, local_msg, shared_msg),
  get_registry: fn(Resources) -> GroupRegistry(shared_msg),
) -> ComponentContract(
  GroupRegistry(shared_msg),
  PubSubState(model, shared_msg),
  PubSubMsg(local_msg, shared_msg),
) {
  let app = build(component)

  component_contract.contract(app, fn(ctx: ConnectionContext) {
    get_registry(ctx.resources)
  })
}

// INTERNAL - LUSTRE APP FUNCTIONS ---------------------------------------------

fn init(
  component: PubSubComponent(model, local_msg, shared_msg),
  registry: GroupRegistry(shared_msg),
) -> #(PubSubState(model, shared_msg), Effect(PubSubMsg(local_msg, shared_msg))) {
  echo "pubsub init called"
  let state =
    PubSubState(model: component.init_model, registry: registry, topic: component.topic)

  // Subscribe to the topic and map the component's init effect
  let subscribe_effect = subscribe(registry, component.topic, ReceivedShared)
  let init_effect = effect.map(component.init_effect, Local)

  #(state, effect.batch([subscribe_effect, init_effect]))
}

fn update(
  component: PubSubComponent(model, local_msg, shared_msg),
  state: PubSubState(model, shared_msg),
  msg: PubSubMsg(local_msg, shared_msg),
) -> #(PubSubState(model, shared_msg), Effect(PubSubMsg(local_msg, shared_msg))) {
  case msg {
    ReceivedShared(shared) -> {
      let #(model, eff) = component.on_shared(state.model, shared)
      #(PubSubState(..state, model: model), effect.map(eff, Local))
    }

    Local(local) -> {
      let #(model, eff, maybe_broadcast) = component.update(state.model, local)
      let local_effect = effect.map(eff, Local)

      let broadcast_effect = case maybe_broadcast {
        None -> effect.none()
        Some(shared) -> broadcast(state.registry, state.topic, shared)
      }

      #(
        PubSubState(..state, model: model),
        effect.batch([local_effect, broadcast_effect]),
      )
    }
  }
}

fn view(
  component: PubSubComponent(model, local_msg, shared_msg),
  state: PubSubState(model, shared_msg),
) -> Element(PubSubMsg(local_msg, shared_msg)) {
  component.view(state.model)
  |> element.map(Local)
}

// INTERNAL - EFFECTS ----------------------------------------------------------

/// Subscribe to a topic in the registry.
/// Uses server_component.select to register a selector with the runtime.
fn subscribe(
  registry: GroupRegistry(msg),
  topic: String,
  wrap: fn(msg) -> wrapped_msg,
) -> Effect(wrapped_msg) {
  use _dispatch, _self <- server_component.select

  // Join the topic - this returns a Subject we need to select on
  let subject = group_registry.join(registry, topic, process.self())

  // Create a selector that maps incoming messages through the wrapper
  process.new_selector()
  |> process.select_map(subject, wrap)
}

/// Broadcast a message to all subscribers of a topic.
fn broadcast(
  registry: GroupRegistry(msg),
  topic: String,
  msg: msg,
) -> Effect(any) {
  use _ <- effect.from

  group_registry.members(registry, topic)
  |> list.each(fn(member) { process.send(member, msg) })
}
