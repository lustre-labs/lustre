![](./header.png)

# 07 Routing

In this example, we demonstrate basic routing using the community library [modem](https://hexdocs.pm/modem/). Modem's quickstart docs should be all you should need to get up to speed, so that's the best place to start.

Of course, it's not much fun routing without something to route _to_. This example lets users create new pages on the fly - a guest book for your next house party! Hospitality is very important, and guests will be sure to feel welcome when they see their name in the navigation with a special greeting page just for them.

## Using Modem

Modem uses [custom side effects](../06-custom-effects/) and external Javascript to translate browser click and navigation events into `update` messages for the Lustre runtime. All we need to use it is a [route change handler function](./src/app.gleam#L59) that we can pass to `modem.init` in our app's `init` function.

> _Note:_ See [`modem.advanced`](https://hexdocs.pm/modem/modem.html#advanced) to configure more options.

Inside our `on_route_change` function, we match URI path segment patterns to our routes:

```rust
  fn on_route_change(uri: Uri) -> Msg {
    case uri.path_segments(uri.path) {
      ["welcome", guest] -> OnRouteChange(WelcomeGuest(guest))
      _ -> OnRouteChange(Home)
    }
  }
```

In our `update` function, we assign the matched route to `model.current_route`:

```rust
fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route) -> #(
      Model(..model, current_route: route),
      effect.none(),
    )
  ...
  }
```

And in our `view` function, we use `model.current_route` to determine what to display to the user:

```rust
fn view(model: Model) -> Element(Msg) {
  let page = case model.current_route {
    Home -> render_home(model)
    WelcomeGuest(name) -> render_welcome(model, name)
  }
  ...
}
```

## Views: They're just functions!

In keeping with the Gleam philosophy of avoiding unecessary complexity, Lustre doesn't provide a traditional HTML or JSX-style templating engine.

Since the `view` portion of this example is a bit more involved than our previous ones have been, it should start to give more of a feel for how views in Lustre truly are _just functions_.

Nav? It's a function. Nav _items_? Those are functions too. The "body" portion of our layout? Also a function. Individual pages are functions as well. The `view` function defines our layout by passing a list of function outputs to the `ui.stack` function.

In this way, Lustre lets you build a type safe, unbreakable UI made entirely of [pure functions](https://github.com/lustre-labs/lustre/blob/main/pages/hints/pure-functions.md) written in Gleam.

## Getting help

If you're having trouble with Lustre or are not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
