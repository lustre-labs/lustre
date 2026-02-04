<h1 align="center">Lustre Platform</h1>

<div align="center">
  <a href="https://hex.pm/packages/lustre_platform">
    <img src="https://img.shields.io/hexpm/v/lustre_platform"
      alt="Available on Hex" />
  </a>
  <a href="https://hexdocs.pm/lustre_platform">
    <img src="https://img.shields.io/badge/hex-docs-ffaff3"
      alt="Documentation" />
  </a>
</div>

---

Lustre platform is a **fork** of [Lustre](https://hexdocs.pm/lustre), an MVU web
framework in Gleam. This fork adds the ability to define different rendering
targets ("platforms") for Lustre, thus being able to use Lustre for applications
outside the web, like TUIs or mobile apps.

## Status and Limitations

- The code structure and design are very young
- Except this to change often, including breaking changes
- This repo diverges non-trivially from upstream, expect it to lag behind
- Due to the immature architecture, some features are not available. For example,
  the Erlang target only supports Headless platforms at the moment, and it's
  impossible to build more Headless platforms without editing Lustre source,
  since Element is opaque
- Server component were _not_ tested

## Known Platforms

If you built a platform for Lustre Platform, please name your package with a
`lustre_platform_` prefix, and open a PR to edit this list.

- [lustre_platform_opentui](https://hexdocs.pm/lustre_platform_opentui)

## Example {#example}

If you've used Lustre before, the main difference will be defining a platform
to start your app with. Platform specific utilieis functions, like printing
the view to string (for SSR), are also now the job of the platform.

The web app platform is called dom:

```gleam
import gleam/int
import lustre
import lustre/element.{text}
import lustre/element/html.{div, button, p}
import lustre/event.{on_click}
import lustre/platform/dom

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

fn init(_flags) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model, msg) {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

fn view(model) {
  let count = int.to_string(model)

  div([], [
    button([on_click(Incr)], [text(" + ")]),
    p([], [text(count)]),
    button([on_click(Decr)], [text(" - ")])
  ])
}
```
