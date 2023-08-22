// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [
    title,
    constructing_attributes,
    mapping_attributes,
    conversions,
    common_attributes,
    input_attributes,
    more_input_attributes,
    range_attributes,
    textarea_attributes,
    link_attributes,
    embedded_content,
    audio_and_video,
  ]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# lustre/attribute
"

// CONTENT: CONSTRUCTING ATTRIBUTES --------------------------------------------

const constructing_attributes: String = "
## Constructing attributes

### Attribute | erlang javascript

```gleam
pub opaque type Attribute(msg)
```

### attribute | erlang javascript

```gleam
pub fn attribute(name: String, value: String) -> Attribute(msg)
```

### property | erlang javascript

```gleam
pub fn property(name: String, value: any) -> Attribute(msg)
```

### on | erlang javascript

```gleam
pub fn on(name: String, handler: fn(Dynamic) -> Option(msg)) -> Attribute(msg)
```
"

// CONTENT: MAPPING ATTRIBUTES -------------------------------------------------

const mapping_attributes: String = "
## Mapping attributes

### map | erlang javascript

```gleam
pub fn map(attr: Attribute(a), f: fn(a) -> b) -> Attribute(b)
```
"

// CONTENT: CONVERSIONS --------------------------------------------------------

const conversions: String = "
## Conversions

### to_string | erlang javascript

```gleam
pub fn to_string(attr: Attribute(msg)) -> String
```

### to_string_builder | erlang javascript

```gleam
pub fn to_string_builder(attr: Attribute(msg)) -> StringBuilder
```
"

// CONTENT: COMMON ATTRIBUTES --------------------------------------------------

const common_attributes: String = "
## Common attributes

### style | erlang javascript

```gleam
pub fn style(properties: List(#(String, String))) -> Attribute(msg)
```

### class | erlang javascript

```gleam
pub fn class(name: String) -> Attribute(msg)
```

### classes | erlang javascript

```gleam
pub fn classes(names: List(#(String, Bool))) -> Attribute(msg) 
```

### id | erlang javascript

```gleam
pub fn id(name: String) -> Attribute(msg)
```
"

// CONTENT: INPUT ATTRIBUTES ---------------------------------------------------

const input_attributes: String = "
## Input attributes

### type_ | erlang javascript

```gleam
pub fn type_(name: String) -> Attribute(msg)
```

### value | erlang javascript

```gleam
pub fn value(val: Dynamic) -> Attribute(msg)
```

### checked | erlang javascript

```gleam
pub fn checked(is_checked: Bool) -> Attribute(msg)
```

### placeholder | erlang javascript

```gleam
pub fn placeholder(text: String) -> Attribute(msg)
```

### selected | erlang javascript

```gleam
pub fn selected(is_selected: Bool) -> Attribute(msg
```
"

// CONTENT: MORE INPUT ATTRIBUTES ----------------------------------------------

const more_input_attributes: String = "
## More input attributes

### accept | erlang javascript

```gleam
pub fn accept(types: List(String)) -> Attribute(msg)
```

### accept_charset | erlang javascript

```gleam
pub fn accept_charset(types: List(String)) -> Attribute(msg)
```

### msg | erlang javascript

```gleam
pub fn msg(uri: String) -> Attribute(msg)
```

### autocomplete | erlang javascript

```gleam
pub fn autocomplete(name: String) -> Attribute(msg)
```

### autofocus | erlang javascript

```gleam
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg)
```

### disabled | erlang javascript

```gleam
pub fn disabled(is_disabled: Bool) -> Attribute(msg)
```

### name | erlang javascript

```gleam
pub fn name(name: String) -> Attribute(msg)
```

### pattern | erlang javascript

```gleam
pub fn pattern(regex: String) -> Attribute(msg)
```

### readonly | erlang javascript

```gleam
pub fn readonly(is_readonly: Bool) -> Attribute(msg)
```

### required | erlang javascript

```gleam
pub fn required(is_required: Bool) -> Attribute(msg)
```

### for | erlang javascript

```gleam
pub fn for(id: String) -> Attribute(msg)
```
"

// CONTENT: RANGE ATTRIBUTES ---------------------------------------------------

const range_attributes: String = "
## Range attributes

### max | erlang javascript

```gleam
pub fn max(val: String) -> Attribute(msg)
```

### min | erlang javascript

```gleam
pub fn min(val: String) -> Attribute(msg)
```

### step | erlang javascript

```gleam
pub fn step(val: String) -> Attribute(msg)
```
"

// CONTENT: TEXTAREA ATTRIBUTES ------------------------------------------------

const textarea_attributes: String = "
## Textarea attributes

### cols | erlang javascript

```gleam
pub fn cols(val: Int) -> Attribute(msg)
```

### rows | erlang javascript

```gleam
pub fn rows(val: Int) -> Attribute(msg)
```

### wrap | erlang javascript

```gleam
pub fn wrap(mode: String) -> Attribute(msg)
```
"

// CONTENT: LINK ATTRIBUTES ----------------------------------------------------

const link_attributes: String = "
## Link attributes

### href | erlang javascript

```gleam
pub fn href(uri: String) -> Attribute(msg)
```

### target | erlang javascript

```gleam
pub fn target(target: String) -> Attribute(msg)
```

### download | erlang javascript

```gleam
pub fn download(filename: String) -> Attribute(msg)
```

### rel | erlang javascript

```gleam
pub fn rel(relationship: String) -> Attribute(msg)
```
"

// CONTENT: EMBEDDED CONTENT ---------------------------------------------------

const embedded_content: String = "
## Embedded content

### gleam | erlang javascript

```gleam
pub fn src(uri: String) -> Attribute(msg)
```

### gleam | erlang javascript

```gleam
pub fn height(val: Int) -> Attribute(msg)
```

### gleam | erlang javascript

```gleam
pub fn width(val: Int) -> Attribute(msg)
```

### gleam | erlang javascript

```gleam
pub fn alt(text: String) -> Attribute(msg)
```
"

// CONTENT: AUDIO AND VIDEO ATTRIBUTES -----------------------------------------

const audio_and_video: String = "
## Audio and video attributes


### autoplay | erlang javascript

```gleam
pub fn autoplay(should_autoplay: Bool) -> Attribute(msg)
```


### controls | erlang javascript

```gleam
pub fn controls(visible: Bool) -> Attribute(msg)
```


### loop | erlang javascript

```gleam
pub fn loop(should_loop: Bool) -> Attribute(msg)
```
"
