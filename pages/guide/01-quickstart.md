# 01 Quickstart guide

Welcome to the Lustre quickstart guide! If you want to hit the ground running,
get something on the screen, and see most of what Lustre has to offer, this is
the place to start.

Lustre can be used to create HTML in many different contexts: static HTML file
generation, dynamic server-side rendering, client-side single-page applications,
web components, and real-time server components. This guide will focus on the
most common use case: single-page applications, or SPAs.

SPAs are JavaScript applications that handle rendering in the browser rather than
requesting HTML from the server. To help you build them, Lustre comes with an
opinionated runtime designed to help you build robust scalable applications:

- **Declarative rendering**: Your app's ui is constructed using a declarative api
  that treats the current HTML as a function of your application's state. This is
  in contrast to more traditional imperative approaches to direct DOM mutation
  like jQuery.

- **State management**: If your ui is a function of your application's state, then
  we'll need a good way to manage that state! Lustre provides a simple message-based
  state management system modelled after Erlang's [gen_servers](https://www.erlang.org/doc/design_principles/gen_server_concepts),
  Gleam's [actors](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html), and the
  [Elm Architecture](https://guide.elm-lang.org/architecture/).

- **Managed side effects**: Managing asynchronous operations like HTTP requests
  or timers can be tricky when JavaScript is single-threaded. Lustre provides a
  runtime to manage these side effects and feed their result back into its state
  management system.

## Your first Lustre program

To get started, let's create a new Gleam application and add Lustre as a
dependency.

```bash
gleam new app && cd app && gleam add lustre
```

Unless you say otherwise, Gleam will check and build projects for the **Erlang**
target. We want to build an app we can run in the browser, so first let's add a
`target` field to the generated `gleam.toml`.

```diff
  name = "app"
  version = "1.0.0"
+ target = "javascript"

  ...
```

The simplest type of Lustre application is the `element`: this function renders
a Lustre `Element` but without any of runtime or event handling support. Let's
start as all good programs do: with "Hello, world!"

```gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.text("Hello, world!"))
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

In order to see the fruits of our labour, we'll need to build the project and
serve it. Lustre has official development tooling published under the
[`lustre_dev_tools`](https://hexdocs.pm/lustre_dev_tools/) package. Experienced
frontend devs might prefer to dive straight into something like Vite, but most
Lustre projects will probably want to use the official tools.

```bash
gleam add --dev lustre_dev_tools
```

Lustre's dev tools include a development server that will serve your app and
rebuild it as you make changes. It's important to make sure you add this package
as a `--dev` dependency for two reasons:

- The dev tools are not needed in production, so we want to be sure we don't
  accidentally ship them.

- The dev tools are an **Erlang** app designed to be run through `gleam run` rather
  than a package you import in your own code.

> **Note**: the lustre_dev_tools development server watches your filesystem for
> changes to your gleam code and can automatically reload the browser. For Linux
> users, this requires [inotify-tools](https://github.com/inotify-tools/inotify-tools)
> be installed. If you do not or cannot install this, the development server will
> still run but it will not watch your files for changes.

To start the development server, run:

```bash
gleam run -m lustre/dev start
```

Once the server is up and running, head on over to your browser and visit
[http://localhost:1234](http://localhost:1234) to be greeted with your first
Lustre application: "Hello, world!"

## Rendering HTML

Of course, there's not much we can do if we can only render text. We mentioned
that Lustre has a _declarative_ API for rendering HTML: instead of a sequence of
`document.createElement` calls, we describe the structure of our HTML using
functions! Let's see what that looks like by rendering something slightly more
complex.

```gleam
import lustre
import lustre/attribute
import lustre/element/html

pub fn main() {
  let app =
    lustre.element(
      html.div([], [
        html.h1([], [html.text("Hello, world!")]),
        html.figure([], [
          html.img([attribute.src("https://cdn2.thecatapi.com/images/b7k.jpg")]),
          html.figcaption([], [html.text("A cat!")])
        ])
      ])
    )
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

You might be wondering **"where are the templates?"** Lustre doesn't have a
separate templating syntax like JSX or HEEx for a few reasons:

- Gleam does not have a macro system or a way to define new syntax, so a templating
  language would exist _outside_ of Gleam. Gleam's language server and compiler
  errors are some of the best around, and adding a templating language on top
  would mean attempting to map those to a different language while maintaining a
  great developer experience.

- Templating languages hide the fact that your ui is made of _just functions_.
  This is a powerful realization, because all of the same patterns you might use
  to abstract or organize your code _also apply to your UI_: pass UI-generating
  functions as arguments, return them from other functions, partial apply them,
  and so on!

If you're initially put off by the lack of templating syntax, we encourage you to
stick with it for a while and see how you get on. Gleam is at its best when you're
writing simple, functional code, and that carries over to your ui as well.

## Adding interactivity

Rendering static HTML is great, but Lustre is intended to be used to build rich
interactive applications, so let's add some interactivity by building a classic
"counter" application.

The first step is to graduate our `lustre.element` app into an interactive one
with an update loop using `lustre.simple`.

```gleam
import gleam/int
import lustre
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

No matter how complex your applications get, all Lustre applications are built
on the same three building blocks:

- A `Model` type that represents the entire state of your application and an
  `init` function to construct the first model to render.

- A `Msg` type that represents all the different ways the outside world can
  communicate with your application and an `update` function to receive those
  messages and update the model.

- A `view` function that takes your model and renders it to HTML. This function
  gets called any time the model changes.

The state of our counter app can just be a single `Int`, and the `init` function
will return `0`.

```gleam
type Model =
  Int

fn init(_args) -> Model {
  0
}
```

> **Note**: the `init` function always takes a single argument representing any
> start arguments or external data passed to the application when you call
> `lustre.start`. In this case, we don't need to worry about them, but in later
> guides we'll see how this can be used to pass in configuration or hydration
> data when your app starts.

The core of every Lustre application is its `Msg` type and update function. By
looking at these, we can get a holistic understanding of how our application
works by seeing up-front all the different ways the outside world can affect
changes to our model.

For our counter app we need two messages, one to handle the user clicking an
"increment" button and another to handle "decrement".

```gleam
type Msg {
  UserClickedIncrement
  UserClickedDecrement
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserClickedIncrement -> model + 1
    UserClickedDecrement -> model - 1
  }
}
```

We strongly encourage naming your messages in a subject-verb-object style such as
`UserClickedIncrement`, `ApiReturnedPosts`, or `DomMeasuredElement`. This makes
messages about _what happened_ rather than _what to do_ and makes it easy to get
a sense of the scope of your application.

To produce these messages, we'll construct a view with some buttons with event
handlers attached. The `lustre/event` module defines an `on` function that lets
you write custom event handlers for any event, but it also includes some of the
most common events like `on_click` out of the box.

```gleam
fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)

  html.div([], [
    html.button([event.on_click(UserClickedIncrement)], [
      html.text("+")
    ]),
    html.p([], [html.text(count)]),
    html.button([event.on_click(UserClickedDecrement)], [
      html.text("-")
    ])
  ])
}
```

Notice how the return type is `Element(Msg)` instead of just `Element`. This means
for any fragment of HTML, the type system knows what kinds of messages it can
dispatch and will error if we try to put things together that don't fit!

Head back to your browser to see we have a fully functional counter app! This is
the foundation for every Lustre application:

- A `Model` is rendered to HTML using a `view` function.

- That view may produce a `Msg` in response to user interaction.

- That `Msg` is passed to an `update` function that returns a new `Model`.

- ... and the cycle continues!

## Talking to the outside world

This closed loop of user interaction and state management is great for systems
where the _only_ kinds of messages we want to handle come from user interaction.
But for most real-world applications, we'll also want to handle messages from
other sources, like HTTP requests.

Lustre manages side effects through an abstraction called an `Effect`. Effects
are functions that interact with state outside of our application or are
non-deterministic in some way. For example, an HTTP request is non-deterministic
because we don't know when it will finish or what the result will be!

It's possible to write your own effects but for now, we'll use another package
- [rsvp](https://hexdocs.pm/rsvp/) - to handle HTTP requests and modify our
counter app to fetch a random cat image from the [cat api](https://thecatapi.com/)
when the count is incremented.

Just as we upgraded our `lustre.element` app to `lustre.simple`, we can do the
same again to a full `lustre.application` to gain access to Lustre's managed
effect system.

```gleam
import gleam/dynamic/decode
import gleam/int
import gleam/list
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import rsvp

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

If you've modified the counter app you'll notice the program no longer compiles!
The type of our `init` and `update` functions don't match what Lustre expects
anymore.

In order to tell Lustre what effects we want it to perform, both these functions
now return a tuple of both the updated model and any effects to perform. We can
update our `init` function like so:

```gleam
type Model {
  Model(total: Int, cats: List(Cat))
}

type Cat {
  Cat(id: String, url: String)
}

fn init(_args) -> #(Model, Effect(Msg)) {
  let model = Model(total: 0, cats: [])

  #(model, effect.none())
}
```

Returning `effect.none()` tells the runtime there's no effects to perform when
the app starts. On the other hand, we _do_ want to perform an effect in our
update function to fetch a new cat image.

```gleam
type Msg {
  UserClickedAddCat
  UserClickedRemoveCat
  ApiReturnedCats(Result(List(Cat), rsvp.Error))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserClickedAddCat -> #(
      Model(..model, total: model.total + 1),
      get_cat()
    )

    UserClickedRemoveCat -> #(
      Model(..model, cats: list.drop(model.cats, 1)),
      effect.none()
    )

    ApiReturnedCats(Ok(cats)) -> #(
      Model(..model, cats: list.append(model.cats, cats)),
      effect.none()
    )

    ApiReturnedCats(Error(_)) -> #(model, effect.none())
  }
}
```

Let's take a look at how the `get_cat` effect is implemented...

```gleam
fn get_cat() -> Effect(Msg) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use url <- decode.field("url", decode.string)

    decode.success(Cat(id:, url:))
  }
  let url = "https://api.thecatapi.com/v1/images/search"
  let handler = rsvp.expect_json(decode.list(decoder), ApiReturnedCats)

  rsvp.get(url, handler)
}
```

Notice how this function returns an `Effect(Msg)` rather than a `Msg` or the result
of the API call. Instead, we added the `ApiReturnedCats` message variant and
passed this to `rsvp`. The library will dispatch this message to our `update`
function when the request completes.

This model of managed effects can feel cumbersome at first, but it comes with some
benefits. Forcing side effects to produce a message means our message type naturally
describes all the ways the world can communicate with our application. As an app
grows, being able to get this kind of overview is invaluable! It also means we can
test our update loop in isolation from the runtime and side effects: we could write
tests that verify a particular sequence of messages produces an expected model
without needing to mock out HTTP requests or timers.

Before we forget, let's also update our `view` function to actually display the
cat images we're fetching:

```gleam
fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.div([], [
      html.button([event.on_click(UserClickedAddCat)], [
        html.text("Add cat")
      ]),
      html.p([], [html.text(int.to_string(model.total))]),
      html.button([event.on_click(UserClickedRemoveCat)], [
        html.text("Remove cat")
      ]),
    ]),
    html.div([], {
      list.map(model.cats, fn(cat) {
        html.img([
          attribute.src(cat.url),
          attribute.width(400),
          attribute.height(400),
        ])
      })
    }),
  ])
}
```

Depending on your network speed, you might notice that when you add a new cat
the last cat image is duplicated for a moment before the new one appears. While
we won't touch on it in this guide, you can learn more about why this happens and
how to prevent it in the [rendering lists](https://github.com/lustre-labs/lustre/blob/main/pages/hints/rendering-lists.md)
hint.

## What next

Believe it or not, we've seen a large part of what Lustre has to offer. From these
core concepts you can build rich interactive applications that are robust and
maintainable. As your application grows you can introduce server-side rendering,
hydration, components, and even interactive server components.

Where to go from here depends on what you want to build and how you like to
learn:

- We have a large number of [examples](https://github.com/lustre-labs/lustre/tree/main/examples)
  that cover a wide range of features and scenarios. If you learn best by seeing
  and copying, this is a great place to start.

- If you learn best by reading, you can follow along with the [rest of this guide](./02-state-management.html) to learn more about Lustre's core concepts and
  patterns.

- Of course, if you want to dive right in and start making things, the
  [API documentation](https://hexdocs.pm/lustre/lustre.html) is always handy to
  keep open.

---

<small>
  Lustre is mostly built by just me, [Hayleigh](https://github.com/hayleigh-dot-dev),
  around two jobs. If you'd like to support my work, [you can sponsor me on GitHub](https://github.com/sponsors/hayleigh-dot-dev).
  Contributions are also very welcome! If you've spotted a bug, or would like to
  suggest a feature, please open an issue or a pull request.
</small>
