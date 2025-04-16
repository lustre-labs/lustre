# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- [lustre] Added a `send` function for sending messages to an app's runtime.
- [lustre] Defines new `Runtime` and `RuntimeMessage` types to model communication with running apps.
- [lustre/attribute] Added many many attributes to `lustre/attribute`.
- [lustre/component] Added a `lustre/component` module with attributes and effects to access Custom Element features such as slots, parts, and CSS custom states.
- [lustre/component] Component's can be notified of browser autofill requests using the `on_form_autofill` configuration option.
- [lustre/component] Component's can be notified of browser restore requests using the `on_form_restore` configuration option.
- [lustre/component] Component's can be notified of form reset events using the `on_form_reset` configuration option.
- [lustre/component] Component's can listen to attribute value changes using the `on_attribute_change` configuration option.
- [lustre/component] Component's can listen to property value changes using the `on_property_change` configuration option.
- [lustre/component] Component's can now be made form-associated custom elements using the `form_associated` configuration option.
- [lustre/component] Component's can now control whether their shadow dom is open or closed using the `open_shadow_root` configuration option.
- [lustre/effect] A new `after_paint` effect allows you to run an effect after the browser paints.
- [lustre/effect] A new `before_paint` effect allows you to run an effect before the browser paints.
- [lustre/element] Added a new `unsafe_raw_html` function allows you to render raw unescaped HTML.
- [lustre/element/keyed] Added a `lustre/element/keyed` module with convenience functions for common keyed elements.
- [lustre/server_component] Defines a new `ClientMessage` type to model messages sent from the server to the client.
- [lustre/server_component] Added a `TransportMethod` type and `method` attribute to choose between websocket, sse, and http methods for server component clients.
- [lustre/server_component] Adds an `element` function to more-conveniently create the `<lustre-server-component>` HTML element.
- [lustre/server_component] Adds a `subject` function to recover the `process.Subject` of a server component's runtime.
- [lustre/server_component] Adds `register_subject` and `deregister_subject` functions for Erlang-based server components.
- [lustre/server_component] Adds `register_callback` and `deregister_callback` functions for JavaScript-based server components.

### Changed

- [lustre] `component` no longer takes a `Dict` of attribute change callbacks and now takes a list of configuration options.
- [lustre] `start` and `start_server_component` now return the new `Runtime` type.
- [lustre/attribute] `property` now requires a `Json` value instead of any `dynamic.Dynamic` value.
- [lustre/element] `fragment` no longer renders a wrapper div around its children.
- [lustre/event] Event handlers written using `on` now use the new `gleam/dynamic/decode` API.
- [lustre/event] The `submit` event now includes a list for form data entries for processing.
- [lustre/event] `prevent_default` and `stop_propagation` are now modifiers of the event attribute itself.
- [lustre/server_component] `decode_action` has been replaced with `runtime_message_decoder`.
- [lustre/server_component] `encode_patch` has been replaced with `client_message_to_json`.

### Removed

- [lustre] The `Action`, `ClientSpa`, `Patch`, and `ServerComponent` types have been removed from the `lustre` module.
- [lustre] `start_actor` has been removed in favour of a unified `start_server_component` for both targets.
- [lustre/attribute] The `on` function has been removed in favour of the `event.on` function.
- [lustre/element] The `keyed` function has been removed and replaced by a dedicated `lustre/element/keyed` module.
- [lustre/event] The `checked`, `mouse_position`, and `value` decoders have been removed.
- [lustre/server_component] The deprecated `set_selector` effect has been removed.
- [lustre/server_component] `subscribe` and `unsubscribe` have been removed.
- [lustre/server_component] The `data` attribute has been removed.
