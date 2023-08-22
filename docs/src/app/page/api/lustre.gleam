// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [title, applications, components, utilities]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# lustre
"

const applications: String = "
## Applications

### App | erlang javascript

```gleam
pub type App(flags, model, msg)
```

### Error | erlang javascript

```gleam
pub type Error {
  AppAlreadyStarted
  AppNotYetStarted
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
}
```

### element | javascript

```gleam
pub fn element(el: Element(msg)) -> App(Nil, Nil, msg)
```

### simple | javascript

```gleam
pub fn simple(
  init: fn(flags) -> model,
  update: fn(model, msg) -> model,
  view: fn(model) -> Element(msg)
) -> App(flags, model, msg)
```

### application | javascript

```gleam
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg)
) -> App(flags, model, msg)
```

### start | javascript

```gleam
pub fn start(
  app: App(flags, model, msg),
  selector: String,
  flags: flags,
) -> Result(fn(msg) -> Nil, Error)
```

### destroy | javascript

```gleam
pub fn destroy(app: App(flags, model, msg)) -> Nil
```
"

const components: String = "
## Components

### component | javascript

```gleam
pub fn component(
  name: String,
  init: fn() -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Map(String, Decoder(msg)),
) -> Result(Nil, Error)
```
"

const utilities: String = "
## Utilities

### is_browser | erlang javascript

```gleam
pub fn is_browser() -> Bool
```

### is_registered | erlang javascript

```gleam
pub fn is_registered(_name: String) -> Bool
```

"
