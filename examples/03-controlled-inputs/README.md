![](./header.png)

# 03 Controlled Inputs

> **Note**: this guide is written for Lustre v4. The latest stable release of
> Lustre is v3. To follow along with this guide, you need to _manually_ edit your
> `gleam.toml` and change the required version of lustre to `"4.0.0-rc.2"`.

The most common way to handle inputs and other state-holding elements is in a
_controlled_ way. This means your app's model is the source of truth for that
element's state, and you update that state based on user input or other events.

This example shows what that means in practice. For any controlled input we need
two things:

- A field in our model (or a function to derive a value from the model) to use
  as the input's `value` attribute.

- A message variant to handle input events and update the model.

```gleam
ui.input([
  // Input's value is fixed to the model's `value` field
  attribute.value(model.value),
  // Whenever the input changes, we send a `GotInput` message with the new value
  event.on_input(GotInput)
])
```

## Why is this beneficial?

Central to Lustre's architecture is the idea that your model is the single source
of truth for your application's UI. This opens up the door to things like serialising
progam state to load in the future, time-travel debugging, and rehydrating your
app's state from a server.

It also gives you tighter control of when and how to update your UI in response
to user input. In this example, we only update the model when the new input
value is less than 10 characters long.

```gleam
case msg {
  GotInput(value) -> {
    let length = string.length(value)

    case length <= model.max {
      True -> Model(..model, value: value, length: length)
      False -> model
    }
  }

  ...
```

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
