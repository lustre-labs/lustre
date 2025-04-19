# 01-bassics/02-attributes

This example demonstrates how to work with attributes and the `Attribute` type
in Lustre.

## What are attributes?

In Lustre, most elements receive two arguments:

1. A list of **attributes** (HTML attributes, DOM properties, and event handlers)
2. A list of **children** to render inside the element

The example demonstrates several important attribute concepts:

### Basic Attributes

```gleam
html.button([event.on_click(UserClickedDecrement)], [html.text("-")])
```

Here, we add a click event handler to a button element.

### Multiple Attributes

```gleam
html.p(
  [
    attribute.class("my-4"),
    // More attributes
  ],
  [html.text("Count: "), html.text(int.to_string(model))]
)
```

Elements can have multiple attributes passed as a list.

### Conditional Attributes

```gleam
case model > 10 {
  True -> attribute.class("text-red-500 font-bold")
  False -> attribute.none()
}
```

You can conditionally apply attributes based on the model state. When the counter exceeds 10, the text turns red and bold.

### Attribute Merging

When adding the same attribute multiple times (like `class` or `style`), Lustre will intelligently merge the values rather than overwriting them.

For example:
```gleam
[
  attribute.class("my-4"),
  attribute.class("text-red-500 font-bold")
]
```

These class values will be merged together rather than having one replace the other.

### Empty Attributes

Use `attribute.none()` when you want to conditionally exclude an attribute.
