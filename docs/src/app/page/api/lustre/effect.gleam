// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [title, constructing_effects, manipulating_effects]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# lustre/effect
"

// CONTENT: CONSTRUCTING EFFECTS ------------------------------------------------

const constructing_effects: String = "
## Constructing Effects

### Effect | erlang javascript

```gleam
pub opaque type Effect(action) 
```

### from | erlang javascript

```gleam
pub fn from(effect: fn(fn(action) -> Nil) -> Nil) -> Effect(action)
```

### none | erlang javascript

```gleam
pub fn none() -> Effect(action)
```

### batch | erlang javascript

```gleam
pub fn batch(effects: List(Effect(action))) -> Effect(action)
```
"

// CONTENT: MANIPULATING EFFECTS -----------------------------------------------

const manipulating_effects: String = "
## Manipulating Effects

### map | erlang javascript

```gleam
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b)
```
"
