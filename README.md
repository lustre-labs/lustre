# Lustre

A framework for building create web apps – powered by Gleam and React!

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
  let app = lustre.application(#(0, cmd.none()), update, render)
  lustre.start(app, "#app")
}

pub type Action {
  Incr
  Decr
}

fn update(state, action) {
  case action {
    Incr -> #(state + 1, cmd.none())
    Decr -> #(state - 1, cmd.none())
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

If available on Hex, this package can be added to your Gleam project:

```sh
gleam add lustre
```

and its documentation can be found at <https://hexdocs.pm/lustre>. You will also
need to install `react` and `react-dom` from npm:

```sh
npm i react react-dom
```

---

## Development

First, make sure you have both Gleam and Node.js installed, then:

```bash
npm i
npm start
```

This sets up `chokidar` to watch our gleam source code and runs the compiler
whenever we make a change. It also starts a server that will serve the examples
located in `test/example/`.
