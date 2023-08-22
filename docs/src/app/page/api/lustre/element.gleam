// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [title, constructing_elements, mapping_elements, conversions]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# lustre/element
"

// CONTENT: CONSTRUCTING ELEMENTS ----------------------------------------------

const constructing_elements: String = "
## Constructing elements

### Element | erlang javascript

```gleam
pub opaque type Element(msg)
```

### element | erlang javascript

```gleam
pub fn element(
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### namespaced | erlang javascript

```gleam
pub fn namespaced(
  namespace: String,
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### text | erlang javascript

```gleam
pub fn text(content: String) -> Element(msg) 
```
"

// CONTENT: MAPPING ELEMENTS ---------------------------------------------------

const mapping_elements: String = "
## Mapping elements

### map | erlang javascript

```gleam
pub fn map(element: Element(a), f: fn(a) -> b) -> Element(b)
```
"

// CONTENT: CONVERSIONS --------------------------------------------------------

const conversions: String = "
## Conversions

### to_string | erlang javascript

```gleam
pub fn to_string(element: Element(msg)) -> String
```

### to_string_builder | erlang javascript

```gleam
pub fn to_string_builder(element: Element(msg)) -> StringBuilder
```
"
