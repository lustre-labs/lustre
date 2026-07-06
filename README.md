# agnostic

agnostic is a soft fork of [Lustre](https://github.com/lustre-labs/lustre) that
decouples the reconciler and runtime from the browser DOM through a composable
`Platform` abstraction, so the framework's ergonomics can be used for apps on
arbitrary targets.

Currently we ship a DOM platform for the browser and an OpenTUI terminal
platform. While the DOM platform is usable, the intention behind including it
was reusing libraries from the lustre ecosystem across platforms, which was
possible due to a Gleam bug. Since that bug has now been fixed, libraries cannot
be shared anymore, and it is recommended to use upstream Lustre for webapps on 
both client and server.

Use agnostic for the OpenTUI platform, or for building your own. A NativeScript
one, for example, has been proven possible (code unavailable).

The aim is for the ideas in this repo (or parts of them) to be implemented
upstream, and therefore we try to keep things up to date with it. To see how we
do that, please refer to [FORKING.md](./FORKING.md).

Agnostic is used in production by [Bliss Writer](https://blisswriter.app). 

## Docs

Familiarity with Lustre is assumed throughout this repository. Please refer to
their documentation at <https://hexdocs.pm/lustre> to learn more about general
framework use and the browser target (client or server side).

When familiar, the differences are easily bridged by going over doc-comments and
function signatures in agnostic.


Examples are also available in the `examples/` folder.

### agnostic/platform/opentui

Allows you to develop apps targeting OpenTUI as a renderer.

### Status & Limitations

- Must use Bun as runtime
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
          element.text([
            attribute.content("Arrows to navigate, Enter to activate"),
            attribute.bold(True),
            attribute.color("#e0e0e0"),
            attribute.dim(True),
          ]),
          element.text([
            attribute.content(int.to_string(model.count)),
            attribute.bold(True),
            attribute.color("#fff"),
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
                element.text([
                  attribute.content(" - "),
                  attribute.bold(True),
                  attribute.color("#ff6b6b"),
                ]),
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
                element.text([
                  attribute.content(" + "),
                  attribute.bold(True),
                  attribute.color("#69db7c"),
                ]),
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
gleam test --target erlang      # Run the tests on Erlang
gleam test --target javascript  # Run the tests on JavaScript
bunx tsc --noEmit               # Typecheck the TypeScript FFI
gleam run -m build              # Regenerate the server-component bundle
```
