> **Note**: this guide is a work in progress and is not currently complete. Content
> here will change and be added over time. In the meantime, you can check out the
> [Gleam Discord server](https://discord.gg/Fm8Pwmy) if you have any questions about
> creating components with Lustre.


# 06 Components

In the previous chapters of this guide we have explored the Model-View-Update
architecture and stressed the importance of having a _single source of truth_ in
your applications. Things are not always as simple in the real world, and there
is a time for _encapsulated_ state. For those times, Lustre has components that
allow you to nest self-contained MVU applications without leaking their implementation.

## The best component is no component

Before we dive into how components work in Lustre, it's important to explore the
options we have to _avoid_ using them. Components require a little bit of set up
and are conceptually "heavier" than components in frameworks like React or Svelte.
If you can avoid using components, you should, so let's take a look at how.

### View functions

In Lustre, we call any function that returns an `Element` a "view function". This
might seem obvious, but you can get quite far only using view functions and passing
in state and update functions where needed. For the simplest cases, this could be
direct arguments to the view function:

```gleam
pub fn my_button(colour: Colour, label: String, msg: msg) -> Element(msg) {
  html.button([colour.to_style(colour), event.on_click(msg)], [
    html.text(label)
  ])
}
```

As you build more complex UIs, you might find the number of arguments your view
functions accept grows. A popular approach is to group those arguments into a
record so that all the arguments to a view function can be treated as a single
piece of data:

```gleam
pub type MyButtonProps(msg) {
  MyButtonProps(label: String, colour: Colour, msg: msg)
}

pub fn my_button(props: MyButtonProps) -> Element(msg) {
  html.button([colour.to_style(props.colour), event.on_click(props.msg)], [
    html.text(props.label)
  ])
}
```

This approach lends itself well to the _builder pattern_. When we have many props
that are optional, or a highly customisable element, we can create builder functions
to construct the props sequentially:

```gleam
pub type MyButtonProps(msg) {
  MyButtonProps(
    label: String,
    msg: msg,
    colour: Colour,
    variant: Variant,
    icon: Option(Icon)
  )
}

pub type MyButtonVariant {
  Solid
  Outline
}

pub fn new(label: String, msg: msg) -> MyButtonProps(msg) {
  MyButtonProps(label, msg, Primary, Solid,  None)
}

pub fn with_variant(
  variant: MyButtonVariant,
  props: MyButtonProps(msg)
) -> MyButtonProps(msg) {
  MyButtonProps(props.label, props.msg, props.colour, variant, props.icon)
}

pub fn with_icon(
  icon: Icon,
  props: MyButtonProps(msg)
) -> MyButtonProps(msg) {
  MyButtonProps(props.label, props.msg, props.colour, props.variant, Some(icon))
}

// ...

pub fn view(props: MyButtonProps(msg)) -> Element(msg) {
  let variant = case props.variant {
    Solid -> attribute.class("solid")
    Outline -> attribute.class("outline")
  }

  let icon = case props.icon {
    Some(icon) -> icon.view(icon)
    None -> html.none()
  }

  html.button(
    [
      colour.to_style(props.colour),
      variant,
      event.on_click(props.msg)
    ],
    [
      icon,
      html.text(props.label)
    ]
  )
}
```

Some folks like to _really_ lean into the builder pattern. If you want to try
something fancy, you can look into the [phantom builder pattern](https://www.youtube.com/watch?v=3lYHFctx2Ks)
to see how you can use the type system to enforce things like required props or
prevent setting the same prop twice. We use this approach in [lustre/ssg](https://hexdocs.pm/lustre_ssg)!

### Nested MVU
