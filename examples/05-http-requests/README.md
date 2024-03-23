![](./header.png)

# 05 HTTP Requests

> **Note**: this guide is written for Lustre v4. The latest stable release of
> Lustre is v3. To follow along with this guide, you need to _manually_ edit your
> `gleam.toml` and change the required version of lustre to `"4.0.0-rc.2"`.

Up until now, all the logic in our examples has run neatly in a self-contained `Init -> Update 🔁 View` loop. But our applications often need to interact with the outside world, whether through browser APIs or HTTP requests. 

For this, Lustre provides an `Effect` system. You should [read the docs on Lustre Effects](https://hexdocs.pm/lustre/4.0.0-rc1/lustre/effect.html) before continuing here. 

... did you read the docs? Ok, let's move on!

## The #(model, effect) Tuple
You'll notice that running a Lustre app with side effects _changes the signature_ of our [`init`](src/app.gleam#L43) and [`update`](src/app.gleam#L54) functions. Instead of returning just a model, we return a tuple containing both a model and an `Effect(Msg)` value. The effect value specifies any further updates we might want the Lustre runtime to execute before the next invocation of the `view` function.

```gleam
fn init(_) -> #(Model, Effect(Msg)) {
  #(Model(quote: None), effect.none())
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Refresh -> #(model, get_quote())
    GotQuote(Ok(quote)) -> #(Model(quote: Some(quote)), effect.none())
    GotQuote(Error(_)) -> #(model, effect.none())
  }
}

fn get_quote() -> Effect(Msg) { ... }
```

> **Note:** Even in an app with side effects, the `view` function signature does not change. In Lustre, the view function is always [pure](https://en.wikipedia.org/wiki/Pure_function), accepting only a `Model` and returning only an `Element` without triggering any effects itself.

## HTTP Requests are Effects

As we touched on before, an HTTP Request is a form of effect, because it means our program needs to interact with the outside world. 

In this example, we use the great community library `lustre_http` to fetch a random quote whenever the user clicks the "New Quote" button. [Check out the lustre_http docs here.](https://hexdocs.pm/lustre_http/lustre_http.html)

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

The `lustre_http.get(...)` function does a _lot_. That second parameter specifies not only what type of response to expect, but also how to decode it, what type to decode it _into_, as well as the `Msg` type that should be sent back to the Lustre runtime after it successfully executes.


## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
