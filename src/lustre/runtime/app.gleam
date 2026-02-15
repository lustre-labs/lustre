// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Name}
import gleam/list
import gleam/option
import lustre/effect.{type Effect}
import lustre/internals/constants
import lustre/runtime/server/runtime
import lustre/vdom/vnode.{type Element}

// TYPES -----------------------------------------------------------------------

pub type App(arguments, model, message) {
  App(
    name: option.Option(Name(runtime.Message(message))),
    init: fn(arguments) -> #(model, Effect(message)),
    update: fn(model, message) -> #(model, Effect(message)),
    view: fn(model) -> Element(message),
    config: Config(message),
  )
}

pub type Config(message) {
  Config(
    //
    open_shadow_root: Bool,
    adopt_styles: Bool,
    delegates_focus: Bool,
    //
    attributes: List(#(String, fn(String) -> Result(message, Nil))),
    properties: List(#(String, Decoder(message))),
    contexts: List(#(String, Decoder(message))),
    //
    is_form_associated: Bool,
    on_form_autofill: option.Option(fn(String) -> message),
    on_form_reset: option.Option(message),
    on_form_restore: option.Option(fn(String) -> message),
  )
}

pub type Option(message) {
  Option(apply: fn(Config(message)) -> Config(message))
}

// CONSTANTS -------------------------------------------------------------------

pub const default_config: Config(message) = Config(
  open_shadow_root: True,
  adopt_styles: True,
  delegates_focus: False,
  attributes: constants.empty_list,
  properties: constants.empty_list,
  contexts: constants.empty_list,
  is_form_associated: False,
  on_form_autofill: option.None,
  on_form_reset: option.None,
  on_form_restore: option.None,
)

// MANIPULATIONS ---------------------------------------------------------------

pub fn configure(options: List(Option(message))) -> Config(message) {
  list.fold(options, default_config, fn(config, option) { option.apply(config) })
}

pub fn configure_server_component(
  config: Config(message),
) -> runtime.Config(message) {
  runtime.Config(
    open_shadow_root: config.open_shadow_root,
    adopt_styles: config.adopt_styles,
    // we reverse both lists here such that the last added value takes precedence
    attributes: dict.from_list(list.reverse(config.attributes)),
    properties: dict.from_list(list.reverse(config.properties)),
    contexts: dict.from_list(list.reverse(config.contexts)),
  )
}
