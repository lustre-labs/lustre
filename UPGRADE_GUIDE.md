# v5 Breaking Changes


### Event listeners use the `gleam/dynamic/decode` API

All uses of the deprecated `gleam/dynamic` module have been replaced with the
new `gleam/dynamic/decode` module.

**Caution:** If you used to trigger side effects directly from your event
decoders, be aware that the new decode API will always run your decoder functions
fully, regardless of failure of a previous step!

We recommend to remove all side-effects from decoders, and instead return them
as effects from your `update` function.

### `stop_propagation` and `prevent_default` now have to be set on the event attribute

`stop_propagation` and `prevent_default` can no longer be called dynamically
based on data decoded from the event - instead, they are properties of the event
attribute itself, which makes them compatible with server components.

### `element.keyed` replaced by `lustre/element/keyed` module (#253)

The previous keyed api was cute but also slightly odd and clunky.
This introduces a new `lustre/element/keyed` module that provides a similar API
to the element module but takes lists of string/element pairs.

- `keyed.element`
- `keyed.namespaced`
- `keyed.fragment`

In addition, the module provides functions for constructing the most common kinds
of keyed elements:

- `keyed.ul`
- `keyed.ol`
- `keyed.div`
- `keyed.tbody`


### `attribute.property` values need to be `Json`

Instead of allowing for arbitrary Gleam values to be passed around, property
values now are required to be serialisable to a `json.Json` value.

This change makes it possible to use `attribute.property` with server components.


### Wrapper elements around fragments removed.

Fragments are now rendered without the wrapping div/custom element and are
completely invisible in the final rendered HTML.

They also interact slightly differently with keyed elements now. The fragment as
a whole can now have a `key` associated with it, and children of the fragment
can either be keyed or not separately from the fragment itself.


### `effect.none()` becomes `effect.none`

A small change that makes the common case slightly shorter and more efficient.
