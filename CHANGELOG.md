# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Changelogs can change! If you spot something is wrong or missing, please consider
[opening an issue](https://github.com/lustre-labs/lustre/issues/new) or a pull
request to fix it.

## [Unreleased]

### Added

- [lustre] The `name` function can now be used to give a `Name` to server components so they can be communicated with even if restarted by a supervisor.
- [lustre] The `supervised` function has been added to create a child specification suitable for gleam/otp's static supervisor.
- [lustre] Added the `factory` function to create gleam/otp factory supervisors capable of dynamically starting many instances of a server component.
- [lustre/attribute] Added the `datetime` attribute for specifying the value for a `<time>` element.
- [lustre/attribute] Added the `cols` and `rows` attributes for use with `<textarea>` elements.
- [lustre/element] Added the new `memo` element function for producing "memoised" or lazy elements.
- [lustre/element] Added a new `Ref` type and the `ref` function for producing dependencies to be used in `memo`.
- [lustre/element/mathml] Added element functions for constructing MathML elements.

### Changed

- [lustre] Internal JavaScript FFI use has been migrated to Gleam's stable FFI API. 
- [lustre/component] Fixed a bug where the `clear_form_value` effect referenced an undefined variable.
- [lustre/element] Rendering an element to a string no longer emits `xmlns` attributes on children that match their parent's namespace.
- [lustre/element] HTML content inside an SVG `<foreignObject>` element will correctly include the XHTML namespace when rendered to a string. 
- [lustre/runtime] Add the ability to virtualise server-rendered fragments, map, and memo nodes.

## [v5.4.0] - 2025-11-01

### Added

- [lustre/attribute] Added the `as_` attribute for specifying the type of a `<link>` element.
- [lustre/attribute] Added the `blocking` attribute for controlling whether a `<link>` or `<script>` element is blocking or non-blocking.
- [lustre/attribute] Added the `default_checked` attribute for specifying the default checked state of checkboxes and radio buttons.
- [lustre/attribute] Added the `default_selected` attribute for specifying the default selected state of options.
- [lustre/attribute] Added the `integrity` attribute for specifying Subresource Integrity hashes.
- [lustre/attribute] Added the `open` attribute for controlling whether details elements are open or closed.

### Changed

- [lustre/element] `element.advanced` correctly respects the `self_closing` and `void` options when generating HTML strings.
- [lustre/event] Fixed a bug where debounced events inside components decoded using the wrong event data.
- [lustre/runtime] Fixed a bug where nested compoents would sometimes render one animation frame later.
- [lustre/runtime] Fixed a bug where providing the same context multiple times would re-render all subscribed components.

## [v5.3.5] - 2025-09-03

### Changed

- [lustre/event] Correctly document the unit used for `debounce` and `throttle`.
- [lustre/runtime] Fix a bug where holding a `WeakRef` to a context subscription function would cause it to GC unpredictably.
- [lustre/server_component] Fixed a bug where listening to attribute changes would crash the JavaScript server component runtime.

## [v5.3.4] - 2025-08-23

### Changed

- [lustre/server_component] Fixed even more bugs in the JavaScript server component runtime.

## [v5.3.3] - 2025-08-22

### Changed

- [lustre/runtime] Fixed a bug where form submitter was not included when constructing form data for decoding.
- [lustre/server_component] Fixed numerous bugs in the JavaScript server component runtime.

## [v5.3.2] - 2025-08-09

### Changed

- [lustre/runtime] Fixed a bug where the server component runtime was actually an outdated bundle.

## [v5.3.1] - 2025-08-09

### Changed

- [lustre/runtime] Fixed a bug where using `element.unsafe_raw_html` would not correctly create the DOM node.

## [v5.3.0] - 2025-08-08

### Added

- [lustre/component] Added a `delegates_focus` config option to control how focus is handled in components.
- [lustre/component] Added a `on_context_change` config option to listen for changes in a parent's provided context.
- [lustre/component] Added a `parts` attribute for toggling independent CSS parts in component elements.
- [lustre/effect] Added a `provide` effect for providing context values to components.
- [lustre/element/svg] Add the `svg.namespace` constant.
- [lustre/element/svg] Add missing `svg.filter`, and `svg.view` elements.

### Changed

- [lustre] Fixed a bug where the internal `isLustreNode` function would incorrectly identify nodes as Lustre nodes when they were not in some cases.
- [lustre/component] Fixed a bug where removing an attribute a component was listening to would cause a runtime error.
- [lustre/component] Fixed a bug where a component's Shadow Root was incorrectly closed by default.
- [lustre/element] Fixed a bug where a top-level fragment would not be hydrated correctly.
- [lustre/element/keyed] Fixed a bug where keyed elements were not virtualised correctly.
- [lustre/event] Fixed a bug where debounced events of child elements would still fire after the node was removed.
- [lustre/event] Fixed a bug where events would not fire correctly after elements where added to or removed from a preceeding fragment.
- [lustre/runtime] Lustre is now more resilient against other scripts or browser plugins modifying the DOM.
- [lustre/server_component] Fixed a bug where empty `value` attributes would result in a value of `undefined`
- [lustre/server_component] Fixed a bug where events inside fragments would not work.

## [v5.2.1] - 2025-06-23

### Changed

- [lustre/server_component] Fixed a bug where WebSocket connections were initialised twice when the component was mounted.
- [lustre/server_component] Fixed a bug where the event paths sent by the client erroneously included the shadowRoot itself.

## [v5.2.0] - 2025-06-23

### Added

- [lustre/dev/query] Added a `matches` function to check if an element matches a given selector.
- [lustre/dev/query] Added a `has` function to check if an element or its descendants match a given selector.
- [lustre/event] Added an `advanced` function to create event listeners that can conditionally prevent default actions and stop propagation.
- [lustre/event] Added a `handler` function to create event handlers to be used with `advanced`.

### Changed

- [lustre] Lustre now requires `gleam_erlang` version `>= 1.0.0 and < 2.0.0`.
- [lustre] Lustre now requires `gleam_otp` version `>= 1.0.0 and < 2.0.0`.
- [lustre] Fix a bug where events can not send the correct message if the browser or external Javascript moved the element.
- [lustre] Fixed a bug where changing the object passed down as a property would not correctly update the property on the DOM element.

## [v5.1.1] - 2025-06-06

### Changed

- [lustre/event] Fixed a bug where setting `prevent_default` on an existing event listener would not remove the passive flag.
- [lustre/event] Fixed a bug where `throttle` would only work when used in combination with `debounce`.
- [lustre/event] Fixed a bug where `throttle` would incorrectly call `.preventDefault` on all discarded events.
- [lustre/element] Fixed a bug where nested fragments would sometimes not be properly replaced

## [v5.1.0] - 2025-05-18

### Added

- [lustre/attribute] Added a `attribute.default_value` function to set the `defaultValue` property of an input.
- [lustre/attribute] Added multple attributes for working with HTML tables: `attribute.abbr`, `attribute.colspan, `attribute.headers, `attribute.rowspan, `attribute.span, `attribute.scope`.
- [lustre/dev/query] Created a module for querying a view for test purposes.
- [lustre/dev/simulate] Created a module for simulating a running application for test purposes.
- [lustre/event] Added support for `debounce` and `throttle` on the same event, at the same time.

### Changed

- [lustre] Change `gleam_stdlib` constraint to `>= 0.60.0 and < 2.0.0`.
- [lustre/event] Fixed a bug where updating the delay for `debounce` or `throttle` would have no effect.
- [lustre/runtime] Fixed a bug where keyed elements where no fully virtualised.

## [5.0.3] - 2025-05-09

### Changed

- [lustre/attribute] The `width` and `height` functions now set the respective attributes instead of properties.
- [lustre/element] Fixed a bug in `element.map` where the mapper functions were composed in reversed order.
- [lustre/element/html] Fixed a bug where `html.textarea` would not always update when the `content` changes.
- [lustre/server_component] Fixed a bug where `server_component.script` contained invalid characters.

## [5.0.2] - 2025-04-20

### Changed

- [lustre/runtime] Fixed a bug where stale app state was used to compute patches, leading to empty diffs.

## [5.0.1] - 2025-04-19

### Changed

- [lustre/event] Fixed a bug with some internal decoders incorrectly using `decode.field` instead of `decode.subfield`.

## [5.0.0] - 2025-04-19

### Added

- [lustre] Added a `send` function for sending messages to an app's runtime.
- [lustre] Defines new `Runtime` and `RuntimeMessage` types to model communication with running apps.
- [lustre/attribute] Added many many attributes to `lustre/attribute`.
- [lustre/attribute] Added `style` as an alternative way to `styles` to set a single CSS property.
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
- [lustre/server_component] Adds a `pid` function to recover the `process.Pid` of a server component's runtime.
- [lustre/server_component] Adds `register_subject` and `deregister_subject` functions for Erlang-based server components.
- [lustre/server_component] Adds `register_callback` and `deregister_callback` functions for JavaScript-based server components.

### Changed

- [lustre] App constructors such as `simple` and `application` now have labelled arguments.
- [lustre] `component` no longer takes a `Dict` of attribute change callbacks and now takes a list of configuration options.
- [lustre] `start` and `start_server_component` now return the new `Runtime` type.
- [lustre/attribute] `property` now requires a `Json` value instead of any `dynamic.Dynamic` value.
- [lustre/attribute] `style` has been renamed to `styles`.
- [lustre/element] `fragment` no longer renders a wrapper div around its children.
- [lustre/element] `to_string_builder` and `to_document_string_builder` have been renamed to `to_string_tree` and `to_document_string_tree` respectively.
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
