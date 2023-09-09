# lustre/element

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

## Mapping elements

### map | erlang javascript

```gleam
pub fn map(element: Element(a), f: fn(a) -> b) -> Element(b)
```

## Conversions

### to_string | erlang javascript

```gleam
pub fn to_string(element: Element(msg)) -> String
```

### to_string_builder | erlang javascript

```gleam
pub fn to_string_builder(element: Element(msg)) -> StringBuilder
```
