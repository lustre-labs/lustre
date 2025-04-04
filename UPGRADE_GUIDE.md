# v5.0.0 upgrade guide

## New features

### Deferred renders, `effect.before_paint`, and `effect.after_paint`

Lustre now defers renders and makes sure to only call `view` and update the DOM
once per frame. To help manage side effects that need to read or wait for the DOM,
two new effects have been added:

- `effect.before_paint` runs after the DOM has been modified but **before** the
  browser paints the changes. Any messages dispatched synchronously from these
  effects will be processed and rendered in the same frame.

- `effect.after_paint` runs after the browser has painted the changes to the DOM.
  This is useful for side effects that need to know the screen has been updated
  before doing their work.

## Breaking changes

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

### `autoplay` and `autofocus` attributes now play and focus the element

These attributes usually only work on page load. Lustre now special cases them
to also trigger their associated action every time their value switches from
`False` to `True`.

### Wrapper elements around fragments removed.

Fragments are now rendered without the wrapping div/custom element and are
completely invisible in the final rendered HTML.

They also interact slightly differently with keyed elements now. The fragment as
a whole can now have a `key` associated with it, and children of the fragment
can either be keyed or not separately from the fragment itself.
