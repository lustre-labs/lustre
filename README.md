# Lustre

An Elm-inspired framework for building web apps in Gleam!

---

[![Package Version](https://img.shields.io/hexpm/v/lustre)](https://hex.pm/packages/lustre)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lustre/)

```gleam
import gleam/int
import lustre
import lustre/element.{button, div, p, text}
import lustre/event.{on_click}
import lustre/cmd

pub fn main() {
  let app = lustre.simple(init, update, render)
  let assert Ok(_) = lustre.start(app, "#app")

  Nil
}

fn init() {
  0
}

type Msg {
  Incr
  Decr
}

fn update(state, msg) {
  case msg {
    Incr -> state + 1
    Decr -> state - 1
  }
}

fn render(state) {
  div(
    [],
    [
      button([on_click(Decr)], [text("-")]),
      p([], [text(int.to_string(state))]),
      button([on_click(Incr)], [text("+")]),
    ],
  )
}
```

---

❗️ This package relies on Gleam's JavaScript FFI and is intended to be run in
the browser. **It will not work if your are targetting Node.js or Erlang.**

---

## Installation

Lustre is available on [Hex](https://hex.pm/packages/lustre). You can install
it like any other Hex package:

```sh
$ gleam add lustre
```
