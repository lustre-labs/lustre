<h1 align="center">Lustre</h1>

<div align="center">
  ✨ <strong>Make your frontend shine</strong> ✨
</div>

<div align="center">
  A framework for building Web apps in Gleam!
</div>

<br />

<div align="center">
  <a href="https://hex.pm/packages/lustre">
    <img src="https://img.shields.io/hexpm/v/lustre"
      alt="Available on Hex" />
  </a>
  <a href="https://hexdocs.pm/lustre">
    <img src="https://img.shields.io/badge/hex-docs-ffaff3"
      alt="Documentation" />
  </a>
</div>

<div align="center">
  <h3>
    <!--
    <a href="https://lustre.build">
      Website
    </a>
    <span> | </span>
    -->
    <a href="https://hexdocs.pm/lustre/guide/01-quickstart.html">
      Quickstart
    </a>
    <span> | </span>
    <a href="https://hexdocs.pm/lustre">
      Reference
    </a>
    <span> | </span>
    <a href="https://discord.gg/Fm8Pwmy">
      Discord
    </a>
  </h3>
</div>

<div align="center">
  <sub>Built with ❤︎ by
  <a href="https://twitter.com/hayleighdotdev">Hayleigh Thompson</a> and
  <a href="https://github.com/lustre-labs/lustre/graphs/contributors">
    contributors
  </a>
</div>

---

## Table of contents

- [Features](#features)
- [Example](#example)
- [Philosophy](#philosophy)
- [Installation](#installation)
- [Where next](#where-next)
- [Support](#support)

## Features

- A **declarative**, functional API for constructing HTML. No templates, no macros,
  just Gleam.

- An Erlang and Elm-inspired architecture for **managing state**.

- **Managed side effects** for predictable, testable code.

- Universal components. **Write once, run anywhere**. Elm meets Phoenix LiveView.

- A **batteries-included CLI** that makes scaffolding and building apps a breeze.

- **Server-side rendering** for static HTML templating.

## Example

```gleam
import gleam/int
import lustre
import lustre/element.{text}
import lustre/element/html.{div, button, p}
import lustre/event.{on_click}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

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

## Philosophy

Lustre is an _opinionated_ framework for building small-to-medium-sized Web
applications. Modern frontend development is hard and complex. Some of that
complexity is necessary, but a lot of it is accidental or comes from having far
too many options. Lustre has the same design philosophy as Gleam: where possible,
there should be only one way to do things.

That means shipping with a single state management system out of the box, modelled
after Elm and Erlang/OTP. Open any Lustre application and you should feel
right at home.

It also means we encourage simple approaches to constructing views over complex
ones. Lustre _does_ have a way to create encapsulated stateful components (something
we sorely missed in Elm) but it shouldn't be the default. Prefer simple functions
to stateful components.

Where components _are_ necessary, lean into the fact that Lustre components can
run _anywhere_. Lustre gives you the tools to write components that can run inside
an existing Lustre application, export them as a standalone Web Component, or run
them on the server with a minimal runtime for patching the DOM. Lustre calls these
**universal components** and they're written with Gleam's multiple targets in mind.

## Installation

Lustre is published on [Hex](https://hex.pm/packages/lustre)! You can add it to
your Gleam projects from the command line:

```sh
gleam add lustre
```

Lustre also has a companion package containing development tooling that you might
like to install:

```sh
gleam add --dev lustre_dev_tools
```

If you're using a different build tool, like Rebar3 or Mix, you can add Lustre
to your `rebar.config` or `mix.exs` file respectively.

```erlang
{deps, [
  {lustre, "4.0.0"}
]}
```

```elixir
defp deps do
  [
    {:lustre, "~> 4.0"}
  ]
end
```

## Where next

To get up to speed with Lustre, check out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).
If you prefer to see some code, the [examples](https://github.com/lustre-labs/lustre/tree/main/examples)
directory contains a handful of small applications that demonstrate different
aspects of the framework.

You can also read through the documentation and API reference on
[HexDocs](https://hexdocs.pm/lustre).

## Support

Lustre is mostly built by just me, [Hayleigh](https://github.com/hayleigh-dot-dev),
around two jobs. If you'd like to support my work, you can [sponsor me on GitHub](https://github.com/sponsors/hayleigh-dot-dev).

Contributions are also very welcome! If you've spotted a bug, or would like to
suggest a feature, please open an issue or a pull request.
