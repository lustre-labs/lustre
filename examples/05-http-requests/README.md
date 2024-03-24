![](./header.png)

# 05 HTTP Requests

Up until now, all the logic in our examples has run neatly in a self-contained `Init -> Update 🔁 View` loop. But our applications often need to interact with the outside world, whether through browser APIs or HTTP requests.

Up until now, we've seen Lustre applications constructed with the `lustre.simple`
constructor. These kinds of applications are great for introducing the Model-View-Update
(MVU) pattern, but for most real-world applications we'll need a way to talk to
the outside world.

Lustre's runtime includes _managed effects_, which allow us to perform side effects
like HTTP requests and communicate the results back to our application's `update`
function. To learn more about Lustre's effect system and why it's useful, check
out the [side effects guide](https://hexdocs.pm/lustre/guide/side-effects.html),
or the docs for the [lustre/effect module](https://hexdocs.pm/lustre/lustre/effect.html)
For now, we will focus on how to send HTTP requests in a Lustre application: a
pretty important thing to know!

## Moving on from `lustre.simple`

From now on, the rest of these examples will use a different application constructor:
[`lustre.application`]. Let's compare the type of both functions:

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

All that's changed is the return type of our `init` and `update` functions. Instead
of returning just a new model, they now return a tuple containing both a model and
any side effects we want the runtime to perform.

You'll notice that running a Lustre app with side effects _changes the signature_
of our [`init`](src/app.gleam#L43) and [`update`](src/app.gleam#L54) functions.
Instead of returning just a model, we return a tuple containing both a model an
an `Effect(Msg)` value. The effect value specifies any further updates we might
want the Lustre runtime to execute before the next invocation of the `view`
function.

> **Note**: notice how the type of `view` remains the same. In Lustre, your `view`
> is always a [_pure function_](https://en.wikipedia.org/wiki/Pure_function) that
> takes a model and returns the UI to be rendered: we never perform side effects
> in the `view` function itself.

## HTTP requests as side effects

The community library [`lustre_http`](https://hexdocs.pm/lustre_http/) gives us
a way to model HTTP requests as Lustre `Effect`s. Crucially, when we call
`lustre_http.get` we are _not_ performing the request! We're constructing a
description of the side effect that we can hand off to the Lustre runtime to
perform.

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
  expecting a JSON response and provide a decode.

- A long with what we expect the response to be, we also need to provide a way
  to turn that response into a `Msg` value that our `update` function can handle.

The same applies for post requests too, but there you also need to provide the
JSON body of the request.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
