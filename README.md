# Lustre

[![Package Version](https://img.shields.io/hexpm/v/lustre)](https://hex.pm/packages/lustre)

An Elm-inspired framework for building web apps in Gleam!

```gleam
import gleam/int
import lustre
import lustre/element.{text}
import lustre/element/html.{div, button, p}
import lustre/event.{on_click}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start("[data-lustre-app]", Nil)

  Nil
}

fn init(_) {
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
    button([on_click(Decr)], [text(" + ")]),
    p([], [text(count)]),
    button([on_click(Incr)], [text(" - ")])
  ])
}
```

## Documentation

You can find the official documentation over at [pkg.hayleigh.dev/lustre](https://pkg.hayleigh.dev/lustre).
Note that if you're viewing the documentation published on Hexdocs, you may find
that things are missing! Because of the way Gleam's documentation is generated,
packages and functions that target JavaScript don't get documented.

## Installation

Lustre is available on [Hex](https://hex.pm/packages/lustre). You can install
it like any other Hex package:

```sh
$ gleam add lustre
```
