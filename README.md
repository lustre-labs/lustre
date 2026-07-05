# agnostic

This is a soft fork of [Lustre](https://github.com/lustre-labs/lustre) that
decouples the reconciler and runtime from the browser DOM through a composable
`Platform` abstraction, so the same MVU application can render to arbitrary
targets — the browser, headless HTML serialization, or custom platforms such
as the OpenTUI terminal renderer included in this repository.

- Based on upstream Lustre v5.7.0. How the fork tracks upstream — branch
  scheme, versioning, rebase procedure — is documented in
  [FORKING.md](./FORKING.md).
- This fork's changes are in [CHANGELOG.md](./CHANGELOG.md); upstream's
  changelog is preserved in [CHANGELOG_UPSTREAM.md](./CHANGELOG_UPSTREAM.md).
- Upstream's documentation applies to everything not touched by the platform
  abstraction: <https://hexdocs.pm/lustre>.

The OpenTUI platform below was merged from the former
`lustre_platform_opentui` package and lives in `agnostic/platform/opentui`.

# agnostic/platform/opentui

## Develop TUI apps with Lustre and OpenTUI

[![Package Version](https://img.shields.io/hexpm/v/agnostic)](https://hex.pm/packages/agnostic)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/agnostic/)

This is an early stage OpenTUI platform for [agnostic](https://hexdocs.pm/agnostic).

It allows you to develop apps using Lustre on the Bun runtime and target OpenTUI
as a renderer.

### Status & Limitations

- Must use Bun as runtime
- Code is early stage, expect bugs and breaking changes
- Read the [OpenTUI docs](https://opentui.com/docs/getting-started/) to know
  which attribute fit which elements


### Examples

There are several examples in the `examples/` folder, but usage is essentially
identical to regular Lustre, just use the elements and attributes supplied by
`agnostic/platform/opentui`:


```sh
gleam add agnostic
```
```gleam
import gleam/int

import agnostic
import agnostic/effect
import agnostic/platform/opentui
import agnostic/platform/opentui/attribute
import agnostic/platform/opentui/effect as tui_effect
import agnostic/platform/opentui/element
import agnostic/platform/opentui/event

pub fn main() {
  let config =
    opentui.default_config()
    |> opentui.use_mouse(False)

  use platform <- opentui.platform(config)
  let app = agnostic.application(init, update, view)
  let assert Ok(_) = agnostic.start(app, on: platform, with: Nil)
  Nil
}

pub type Model {
  Model(count: Int)
}

pub type Msg {
  Increment
  Decrement
  KeyPressed(tui_effect.KeyEvent)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(count: 0),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("btn-plus"),
    ]),
  )
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Increment -> #(Model(count: model.count + 1), effect.none())
    Decrement -> #(Model(count: model.count - 1), effect.none())
    KeyPressed(key_event) ->
      case key_event.key {
        "tab" | "right" -> #(model, tui_effect.focus_next())
        "left" -> #(model, tui_effect.focus_previous())
        "up" -> #(Model(count: model.count + 1), effect.none())
        "down" -> #(Model(count: model.count - 1), effect.none())
        _ -> #(model, effect.none())
      }
  }
}

fn view(model: Model) {
  element.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      element.box(
        [
          attribute.flex_direction("column"),
          attribute.align_items("center"),
          attribute.border_style("round"),
          attribute.border_color("#444"),
          attribute.padding_left(3),
          attribute.padding_right(3),
          attribute.padding_top(1),
          attribute.padding_bottom(1),
          attribute.gap(1),
          attribute.title(" Counter "),
          attribute.title_alignment("center"),
        ],
        [
          element.text_node(
            [
              attribute.bold("true"),
              attribute.color("#e0e0e0"),
              attribute.dim("true"),
            ],
            [element.text("Arrows to navigate, Enter to activate")],
          ),
          element.text_node([attribute.bold("true"), attribute.color("#fff")], [
            model.count
            |> int.to_string()
            |> element.text(),
          ]),
          element.box([attribute.flex_direction("row"), attribute.gap(2)], [
            element.box(
              [
                attribute.focusable(True),
                attribute.border_style("round"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#ff6b6b"),
                attribute.focused_background_color("#2a1a1a"),
                attribute.padding_left(2),
                attribute.padding_right(2),
                event.on_click(Decrement),
                event.on_activate(Decrement),
              ],
              [
                element.text_node(
                  [attribute.bold("true"), attribute.color("#ff6b6b")],
                  [element.text(" - ")],
                ),
              ],
            ),
            element.box(
              [
                attribute.id("btn-plus"),
                attribute.focusable(True),
                attribute.border_style("round"),
                attribute.border_color("#555"),
                attribute.focused_border_color("#69db7c"),
                attribute.focused_background_color("#1a2a1a"),
                attribute.padding_left(2),
                attribute.padding_right(2),
                event.on_click(Increment),
                event.on_activate(Increment),
              ],
              [
                element.text_node(
                  [attribute.bold("true"), attribute.color("#69db7c")],
                  [element.text(" + ")],
                ),
              ],
            ),
          ]),
        ],
      ),
    ],
  )
}
```

Further documentation can be found at <https://hexdocs.pm/agnostic>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
