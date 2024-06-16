# 01 Quickstart guide

Welcome to the Lustre quickstart guide! This document should get you up to speed
with the core ideas that underpin every Lustre application as well as how to get
something on the screen.

## What is a SPA?

Lustre can be used to create HTML in many different contexts, but it is primarily
designed to be used to build Single-Page Applications – or SPAs. SPAs are a type
of Web application that render content primarily in the browser (rather than on
the server) and, crucially, do not require a full page load when navigating
between pages or loading new content.

To help build these kinds of applications, Lustre comes with an opinionated
runtime. Some of Lustre's core features include:

- **Declarative rendering**: User interfaces are constructed using a declarative
  API that describes HTML as a function of your application's state. This is in
  contrast to more traditional imperative approaches to direct DOM mutation like
  jQuery.

- **State management**: If UIs are a function of state, then orchestrating state
  changes is crucial! Lustre provides a simple message-based state management
  system modelled after OTP [gen_servers](https://www.erlang.org/doc/design_principles/gen_server_concepts),
  Gleam's [actors](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html), and the
  [Elm Architecture](https://guide.elm-lang.org/architecture/).

- **Managed side effects**: Managing asynchronous operations like HTTP requests
  and timers can be tricky when JavaScript is single-threaded. Lustre provides a
  runtime to manage these side effects and let them communicate with your application
  using the same messages as your update loop.

## Your first Lustre program

To get started, let's create a new Gleam application and add Lustre as a dependency.

```sh
gleam new app && cd app && gleam add lustre
```

By default, Gleam builds projects for the Erlang target unless told otherwise. We
can change this by adding a `target` field to the `gleam.toml` file generated in
the root of the project.

```diff
  name = "app"
+ target = "javascript"
  version = "1.0.0"

  ...
```

The simplest type of Lustre application is constructed with the `element` function.
This produces an application that renders a static piece of content without the
typical update loop.

We can start by importing `lustre` and `lustre/element` and just rendering some
text:

```gleam
import lustre
import lustre/element

pub fn main() {
  let app = lustre.element(element.text("Hello, world!"))
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

Lustre has some official development tooling published in the
[`lustre_dev_tools`](https://hexdocs.pm/lustre_dev_tools/) package. Most projects
will probably want to add those too!

> **Note**: the lustre_dev_tools development server watches your filesystem for
> changes to your gleam code and can automatically reload the browser. For linux
> users this requires [inotify-tools](https://github.com/inotify-tools/inotify-tools)
> be installed. If you do not or cannot install this, the development server will
> still run but it will not watch your files for changes.

> **Note**: currently one of lustre_dev_tools' dependencies is not compatible with
> the most recent version of `gleam_json`, making it impossible to install. To fix
> this, add `gleam_json = "1.0.1"` as a dependency in your `gleam.toml` file.

```sh
gleam add --dev lustre_dev_tools
```

It's important to make sure the development tooling is added as a `--dev`
dependency. This ensures they're never included in production builds of your app.

To start a development server, we can run:

```sh
gleam run -m lustre/dev start
```

The first time you run this command might take a little while, but subsequent runs
should be much faster!

> **Note**: Lustre uses esbuild under the hood, and attempts to download the [right
> binary for your platform](https://esbuild.github.io/getting-started/#download-a-build).
> If you're not connected to the internet, on an unsupported platform, or don't
> want Lustre to download the binary you can grab or build it yourself and place it
> in `build/.lustre/bin/esbuild`.

Once the server is up and running you should be able to visit http://localhost:1234
and be greeted with your "Hello, world!" message.

We mentioned Lustre has a declarative API for constructing HTML. Let's see what
that looks like by building something slightly more complex.

```gleam
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html

pub fn main() {
  let app =
    lustre.element(
      html.div([], [
        html.h1([], [element.text("Hello, world!")]),
        html.figure([], [
          html.img([attribute.src("https://cataas.com/cat")]),
          html.figcaption([], [element.text("A cat!")])
        ])
      ])
    )
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

Here we _describe_ the structure of the HTML we want to render, and leave the
busywork to Lustre's runtime: that's what makes it declarative!

"**Where are the templates?**" we hear you cry. Lustre doesn't have a separate
templating syntax like JSX or HEEx for a few reasons (lack of metaprogramming
built into Gleam, for one). Some folks might find this a bit odd at first, but
we encourage you to give it a try. Realising that your UI is _just functions_
can be a bit of a lightbulb moment as you build more complex applications.

## Adding interactivity

Rendering static HTML is great, but we said at the beginning Lustre was designed
primarily for building SPAs – and SPAs are interactive! To do that we'll need
to move on from `lustre.element` to the first of Lustre's application constructors
that includes an update loop: `lustre.simple`.

```gleam
import gleam/int
import lustre
import lustre/element
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

There are three main building blocks to every interactive Lustre application:

- A `Model` that represents your application's state and an `init` function
  to create it.

- A `Msg` type that represents all the different ways the outside world can
  communicate with your application and an `update` function that modifies
  your model in response to those messages.

- A `view` function that renders your model to HTML.

We'll build a simple counter application to demonstrate these concepts. Our
model can be an `Int` and our `init` function will initialise it to `0`:

```gleam
pub type Model = Int

fn init(_flags) -> Model {
  0
}
```

> **Note**: The `init` function always takes a single argument! These are the "flags"
> or start arguments you can pass in when your application is started with
> `lustre.start`. For the time being, we can ignore them, but they're useful for
> passing in configuration or other data when your application starts.

The main update loop in a Lustre application revolves around messages passed in
from the outside world. For our counter application, we'll have two messages to
increment and decrement the counter:

```gleam
pub type Msg {
  Increment
  Decrement
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}
```

Each time a message is produced from an event listener, Lustre will call your
`update` function with the current model and the incoming message. The result
will be the new application state that is then passed to the `view` function:

```gleam
pub fn view(model: Model) -> element.Element(Msg) {
  let count = int.to_string(model)

  html.div([], [
    html.button([event.on_click(Increment)], [
      element.text("+")
    ]),
    element.text(count),
    html.button([event.on_click(Decrement)], [
      element.text("-")
    ])
  ])
}
```

The above snippet attaches two click event listeners that produce an `Increment`
or `Decrement` message when clicked. The Lustre runtime is responsible for
attaching these event listeners and calling your `update` function with the
resulting message.

> **Note**: notice that the return type of `view` is `element.Element(Msg)`. The
> type parameter `Msg` tells us the kinds of messages this element might produce
> from events: type safety to the rescue!

This forms the core of every Lustre application:

- A model produces some view.
- The view can produce messages in response to user interaction.
- Those messages are passed to the update function to produce a new model.
- ... and the cycle continues.

## Talking to the outside world

This "closed loop" of messages and updates works well if all we need is an
interactive document, but many applications will also need to talk to the outside
world – whether that's fetching data from an API, setting up a WebSocket connection,
or even just setting a timer.

Lustre manages these side effects through an abstraction called an `Effect`. In
essence, effects are any functions that talk with the outside world and might
want to send messages back to your application. Lustre lets you write your own
effects, but for now we'll use a community package called
[`lustre_http`](https://hexdocs.pm/lustre_http/index.html) to fetch a new cat image
every time the counter is incremented.

Because this is a separate package, make sure to add it to your project first.
While we're here, we'll also add `gleam_json` so we can decode the response from
the cat API:

```sh
$ gleam add lustre_http
```

Now we are introducing side effects, we need to graduate from `lustre.simple` to
the more powerful `lustre.application` constructor.

```gleam
import gleam/dynamic
import gleam/int
import gleam/list
import lustre
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import lustre_http

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

If you edited your previous counter app, you'll notice the program no longer
compiles. Specifically, the type of our `init` and `update` functions are wrong
for the new `lustre.application` constructor!

In order to tell Lustre about what effects it should perform, these functions now
need to return a _tuple_ of the new model and any effects. We can amend our `init`
function like so:

```gleam
pub type Model {
  Model(count: Int, cats: List(String))
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  #(Model(0, []), effect.none())
}
```

The `effect.none` function is a way of saying "no effects" – we don't need to do
anything when the application starts. We've also changed our `Model` type from a
simple type alias to a Gleam [record](https://tour.gleam.run/data-types/records/)
that holds both the current count and a list of cat image URLs.

In our `update` function, we want to fetch a new cat image every time the counter
is incremented. To do this we need two things:

- An `Effect` to describe the request the runtime should perform.
- A variant of our `Msg` to handle the response.

The `lustre_http` package has the effect side of things handled, so we just need
to modify our `Msg` type to include a new variant for the response:

```gleam
pub type Msg {
  UserIncrementedCount
  UserDecrementedCount
  ApiReturnedCat(Result(String, lustre_http.HttpError))
}
```

> **Note**: Concerned your message type is too verbose? Read our thoughts on why
> this is a good thing in our [state management guide](./02-state-management.html).

Finally, we can modify our `update` function to also fetch a cat image when the
counter is incremented and handle the response:

```gleam
pub fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserIncrementedCount -> #(Model(..model, count: model.count + 1), get_cat())
    UserDecrementedCount -> #(Model(..model, count: model.count - 1), effect.none())
    ApiReturnedCat(Ok(cat)) -> #(Model(..model, cats: [cat, ..model.cats]), effect.none())
    ApiReturnedCat(Error(_)) -> #(model, effect.none())
  }
}

fn get_cat() -> effect.Effect(Msg) {
  let decoder = dynamic.field("_id", dynamic.string)
  let expect = lustre_http.expect_json(decoder, ApiReturnedCat)

  lustre_http.get("https://cataas.com/cat?json=true", expect)
}
```

> **Note**: The `get_cat` function returns an `Effect` that tells the runtime how
> to fetch a cat image. It's important to know that the `get_cat` function doesn't
> perform the request directly! This is why we need to add the `ApiReturnedCat` message
> variant: the runtime needs to know what to do with the response when it arrives.

This model of managed effects can feel cumbersome at first, but it comes with some
benefits. Forcing side effects to produce a message means our message type naturally
describes all the ways the world can communicate with our application; as an app
grows being able to get this kind of overview is invaluable! It also means we can
test our update loop in isolation from the runtime and side effects: we could write
tests that verify a particular sequence of messages produces an expected model
without needing to mock out HTTP requests or timers.

Before we forget, let's also update our `view` function to actually display the
cat images we're fetching:

```gleam
pub fn view(model: Model) -> element.Element(Msg) {
  let count = int.to_string(model.count)

  html.div([], [
    html.button([event.on_click(UserIncrementedCount)], [
      element.text("+")
    ]),
    element.text(count),
    html.button([event.on_click(UserDecrementedCount)], [
      element.text("-")
    ]),
    html.div(
      [],
      list.map(model.cats, fn(cat) {
        html.img([attribute.src("https://cataas.com/cat/" <> cat)])
      }),
    ),
  ])
}
```

## Where to go from here

Believe it or not, you've already seen about 80% of what Lustre has to offer! From
these core concepts, you can build rich interactive applications that are predictable
and maintainable. Where to go from here depends on what you want to build, and
how you like to learn:

- There are a number of [examples](https://github.com/lustre-labs/lustre/tree/main/examples)
  if the Lustre repository that gradually introduce more complex applications
  and ideas.

- The [rest of this guide](./02-state-management.html) also continues to teach
  Lustre's high-level concepts and best-practices.

- Of course, if you want to dive in and start making things straight away, the
  [API documentation](https://hexdocs.pm/lustre/lustre.html) is always handy to keep open.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
