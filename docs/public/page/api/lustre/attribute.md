# lustre/attribute

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
pub fn on(
  name: String,
  handler: fn(Dynamic) -> Result(msg, error)
) -> Attribute(msg)
```

## Mapping attributes

### map | erlang javascript

```gleam
pub fn map(attr: Attribute(a), f: fn(a) -> b) -> Attribute(b)
```

## Conversions

### to_string | erlang javascript

```gleam
pub fn to_string(attr: Attribute(msg)) -> String
```

### to_string_builder | erlang javascript

```gleam
pub fn to_string_builder(attr: Attribute(msg)) -> StringBuilder
```

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

## Input attributes

### type\_ | erlang javascript

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
