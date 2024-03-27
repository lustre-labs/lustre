![](./header.png)

# 05 HTTP Requests

In this example, we will focus on how to send HTTP requests in a Lustre application: a pretty important thing to know!

Up until now, none of our examples have had to perform _side effects_. They've run neatly in a self-contained `Init -> Update 🔁 View` loop without interacting with any external systems.

As a result, we've been able to construct our applications with the [`lustre.simple`](https://hexdocs.pm/lustre/lustre.html#simple) constructor. These kinds of applications are great for introducing the Model-View-Update (MVU) pattern, but for most real-world applications we'll need a way to talk to the outside world.

For this, Lustre provides a system of _managed effects_ through the [lustre/effect module](https://hexdocs.pm/lustre/lustre/effect.html). This allows us to _describe_ the side effects we want the runtime perform for us, rather than performing them ourselves.

Lustre's Effects interface is the same whether you are performing synchronous or asynchronous effects. You don't need to worry about [function colouring](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/) and keywords like `async` / `await`.

## Moving on from `lustre.simple`

From now on, the rest of our examples will use a different application constructor:
[`lustre.application`](https://hexdocs.pm/lustre/lustre.html#application). Let's compare the type of both constructors:

```gleam
pub fn simple(
  init: fn(flags) -> model,
  update: fn(model, msg) -> model,
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg)

pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg)
```

All that's changed is the return type of our `init` and `update` functions. Instead of returning just a new model, they now return a tuple containing both a model and any side effects we want the runtime to perform.

Because the Lustre runtime handles effects _for_ us, our `update` and `view` functions can remain [pure functions](https://en.wikipedia.org/wiki/Pure_function) no matter how many effects we need.

## HTTP requests as side effects

The community library [`lustre_http`](https://hexdocs.pm/lustre_http/) gives us
a way to model HTTP requests as Lustre `Effect`s. Crucially, when we call `lustre_http.get` we are _not_ performing the request! We're constructing a description of the side effect that we can hand off to the Lustre runtime to perform.

```gleam
fn get_quote() -> Effect(Msg) {
  let url = "https://api.quotable.io/random"
  let decoder =
    dynamic.decode2(
      Quote,
      dynamic.field("author", dynamic.string),
      dynamic.field("content", dynamic.string),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, GotQuote))
}
```

To construct HTTP requests, we need a few different things:

- The `url` to send the request to.

- A description of what we _expect_ the result to be. There are a few options:
  `expect_anything`, `expect_text`, `expect_json`. In this example we say we're
  expecting a JSON response and provide a decoder function.

- Along with what we expect the response to be, we also need to provide a way
  to turn that response into a `Msg` value that our `update` function can handle.

The same applies for post requests too, but in that case you also need to provide the JSON body of the request.

## Tying it All Together

Now that we have a `get_quote` function to provide the Lustre runtime with the necessary `Effect`, we can invoke it from `view` and handle it in `update`.

```gleam
fn view(model: Model) -> Element(Msg) {
  ui.centre(
    ...
    ui.button([event.on_click(Refresh)], [element.text("New quote")]),
  )
```

```gleam
fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Refresh -> #(model, get_quote())
    GotQuote(Ok(quote)) -> #(Model(quote: Some(quote)), effect.none())
    GotQuote(Error(_)) -> #(model, effect.none())
  }
}
```

Let's break down what happens when a user clicks the "New Quote" button:

- The click handler emits a `Refresh` message.
- This tells `update` to call `get_quote`, which returns an `Effect` telling the runtime to perform our HTTP request as we've described it.
- The Lustre runtime does ✨magic✨
- The runtime calls `update` with our `GotQuote` message containing the result of our request
- We re-render our `view` with the fresh `model.quote` value (or the unchanged model in case of an error).

Any time the `update` function returns `effect.none()` (an empty `Effect`), it tells the runtime "we're ready to re-render the view!" When it returns a non-empty `Effect`, it means there's work to do first.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
