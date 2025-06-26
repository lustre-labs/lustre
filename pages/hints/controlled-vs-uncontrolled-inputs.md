# Controlled vs Uncontrolled Inputs

In Lustre, there are two approaches to handling form inputs: controlled and
uncontrolled. Each has its own use cases and advantages.

## Controlled Inputs

Controlled inputs are fully managed by your Lustre application. They have two key
characteristics:

1. The input's `value` attribute is set explicitly from your model state.

2. An event handler such as `on_input` or `on_change` updates your model when
   the input changes.

This creates a complete cycle: your model controls what's displayed in the input,
changes to that value are reported to your `update` function, and then the value
is re-synced with your `view` function. In controlled inputs, the DOM is a direct
reflection of your application's state!

In Lustre, a controlled input looks like this:

```gleam
html.input([
  // The value comes from your model
  attribute.value(model),
  // Changes update your model via a message
  event.on_input(UserUpdatedName),
  // Other attributes...
])
```

Controlled inputs give you fine-grained control over user input. You can:

- Validate input on every keystroke.

- Format inputs as the user types.

- Conditionally disable submission until valid.

- Restrict input length or content.

## Uncontrolled Inputs

Uncontrolled inputs rely on the browser to manage their state. In this approach:

1. The browser maintains the input's current value

2. Your application only reads the value when the user is "finished" - typically
   on form submission.

In Lustre, uncontrolled inputs are often used within forms:

```gleam
html.form(
  [event.on_submit(UserSubmittedForm)],
  [
    html.input([
      attribute.type_("text"),
      attribute.name("username"),
      // Optional default value
      attribute.default_value(default_value),
      // But no on_input handler!
    ])
  ]
)
```

Uncontrolled inputs are advantageous when:

- You have many form fields and don't want to manage state for each one.

- You only need the values when the form is submitted.

- You want to leverage native browser form functionality like validation attributes.

- You're using server components and want to reduce the number of messages send
  to and from the component.

### Clearing uncontrolled inputs

In many cases, we might want to use uncontrolled inputs to reduce boilerplate or
handle fewer messages but we still want to clear the input after submission or
some other event.

This presents a problem: if we explicitly don't control the state of the input
anymore how do we clear it? We have two options:

1. Write an effect that queries the DOM and imperatively clears the input and
   return that effect from your `update` function whenever you need it. This
   matches Lustre's mental model around side effects affecting external state.
   **This approach is not possible when using server components** as server
   components do not run in the browser and cannot query or manipulate the DOM.

2. Render the input inside a _keyed_ container such as a `keyed.div` or a
   `keyed.fragment` and change the key every time you want to clear the input.
   This takes advantage of how keyed diffs work in Lustre: when the key changes
   the element is reconstructed rather than updated.

Each approach has different tradeoffs. Using an effect means you cannot use
server components, and you must add an `id` or some other way to locate the input
element in the DOM. If you use a keyed container, you must keep track of the key
and make sure it changes whenever you want to clear the input.
