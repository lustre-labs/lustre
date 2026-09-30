# 01-basics/03-view-functions

This example demonstrates how to work with view functions as their own beast
in Lustre.

## What are view functions?

View functions are functions that return the same Lustre elements,
that you have become accustomed to while using the lustre framework.

View functions become a key part of breaking up functionality into composable
bits of code. 

Similar to react or any js based framework, you can use view functions to
create reusable components.


### A Basic View Function

In this example, we extract out the `button` component from `01-basics/02-attributes` into
its own `view_button` component. 


```gleam
fn view_button(
  on_click handle_click: Message,
  label text: String,
) -> Element(message) {
  html.button([event.on_click(handle_click)], [html.text(text)])
}
```

These are just gleam functions, that just return an `Element()` and can be passed back into
our view functions as so 

```gleam
fn view(model: Model) -> Element(Message) {
  html.div([], [
    // Labelled view functions give a similar experience to props in other
    // frontend libraries, while still just being functions!
    view_button(on_click: UserClickedDecrement, label: "-"),
    view_count(model),
    view_button(on_click: UserClickedIncrement, label: "+"),
  ])
}
```

See here as we are able to pass the correct messeage as we need it?

### Logic in View Functions

Because these **View Functions** are just gleam functions, they allow us to
preform logic in the view just as we would before with conditional Attributes:

```gleam
fn view_count(count: Int) -> Element(message) {
  html.p(
    [
      attribute.class(case count > 10 {
        True -> "text-red-500 font-bold"
        False -> ""
      }),
    ],
    [html.text("Count: "), html.text(int.to_string(count))],
  )
}
```

If you understand this, then it should be easy to create a component where instead of just
making the count red if `count` is over `10`. Try making the component red if below 
`0` and green if above `10` instead.
