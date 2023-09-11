# lustre

## Applications

On the client, Lustre applications are built on the Model-View-Update architecture.
This pattern was popularised by the Elm programming language before being adopted
by other state mangement libraries like Redux and Vuex.

Your applications will be made up of three fundamental parts:

- A `Model` that represents the entire state of your application and an `init`
  function to create it.
- A `Msg` type that represents all the ways the outside world can communicate
  with your application and an `update` function that that modifies the model
  in response to these messages.
- A `view` function that renders the current state of your application to the
  DOM.

```
                                         ┌--------+
                                         |        |
                                         | update |
                                         |        |
                                         +--------+
                                           ^    |
                                           |    |
                                       Msg |    | #(Model, Effect(Msg))
                                           |    |
                                           |    v
┌------+                         ┌------------------------+
|      |  #(Model, Effect(Msg))  |                        |
| init |------------------------>|     Lustre Runtime     |
|      |                         |                        |
+------+                         +------------------------+
                                           ^    |
                                           |    |
                                       Msg |    | Model
                                           |    |
                                           |    v
                                         ┌--------+
                                         |        |
                                         |  view  |
                                         |        |
                                         +--------+
```

### App | erlang javascript

```gleam
pub type App(flags, model, msg)
```

The `App` type represents all the parts that make up a Lustre program in the
Model-View-Update architecture along with the runtime necessary to run it.

Although the type itself is exposed to both the Erlang and JavaScript targets,
the functions in this module to construct an `App` are only available in the
JavaScript target, and `start` will only succeed when ran in the browser.

In the future we may have a way to run Lustre applications on the backend, if
you have any ideas on how to achieve this I'd love to hear about them!

### Error | erlang javascript

```gleam
pub type Error {
  AppAlreadyStarted
  AppNotYetStarted
  BadComponentName
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
}
```

The `Error` type represents all the ways that a Lustre program can fail. These
include things like trying to start an application that has already been started,
registering a component with a name that is not valid, or trying to start an
application in a context that is not a browser.

Often you will want to perform a couple of these actions together, and unifying
the error type makes this easy. In many of the examples we `let assert` that the
result is `Ok` but if you wanted to be a bit more dilligent you might use
`result.try` instead:

```gleam
import gleam/result
import lustre

pub fn main () {
  use _ <- result.try(lustre.component("my-component", ...))
  let app = lustre.application(...)
  use dispatch <- result.try(lustre.start(app, "[data-lustre-app]", Nil))

  ...
}
```

### element | javascript

```gleam
pub fn element(el: Element(msg)) -> App(Nil, Nil, msg)
```

An `element` application is the simplest kind of Lustre program. It takes an
`Element` to render and renders it to the DOM. These applications hold no state
and do not respond to messages, but that doesn't mean they are not interactive!

It is possible for [`components`](#component) to be rendered inside an
`element` application, and these components can be interactive with their own
contained state and update loops.

### simple | javascript

```gleam
pub fn simple(
  init: fn(flags) -> model,
  update: fn(model, msg) -> model,
  view: fn(model) -> Element(msg)
) -> App(flags, model, msg)
```

A `simple` program introduces the Model-View-Update architecture but leaves out
the ability to dispatch side effects. This means your programs are interactive
but cannot talk to the outside world.

### application | javascript

```gleam
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg)
) -> App(flags, model, msg)
```

The `application` constructor is the most complete way to build a Lustre app. As
with [`simple`](#simple) it uses the Model-View-Update architecture, but now your
init and update functions can return side effects to be performed by the runtime
in the form of an [`Effect`](/api/lustre/effect#effect-type).

### start | javascript

```gleam
pub fn start(
  app: App(flags, model, msg),
  selector: String,
  flags: flags,
) -> Result(fn(msg) -> Nil, Error)
```

Start an application by providing a CSS selector to find the element to mount the
application onto and any flags to pass to the application on first init. This
function returns a `Result` and may fail for a number of reasons. Check out the
[`Error`](#error-type) type for more information.

### destroy | javascript

```gleam
pub fn destroy(app: App(flags, model, msg)) -> Result(Nil, Error)
```

Tear down a running application and remove it from the DOM. This can fail if the
application has not yet been started.

## Components

Components take the same Model-View-Update building blocks used to create Lustre
applications and allow them to be used as reusable stateful components. This is
slightly different to how components are used in other frameworks like React
where "component" refers more generally to any reusable piece of UI.

In Lustre, functions that return an `Element` are known as "view functions" and
components are more specific abstractions that encapsulate state and behaviour
you might not want to deal with in your top-level application.

Resist the urge to reach for components too early. The Elm community has managed
to make do without components at all: you can get surprisingly far storing state
in your top level application and passing it down to different view functions.
This comes with the added benefit of it being much easier to reason about your
UI as a whole.

### component | javascript

```gleam
pub fn component(
  name: String,
  init: fn() -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Map(String, Decoder(msg)),
) -> Result(Nil, Error)
```

Register a component with the runtime from the familiar Model-View-Update building
blocks. Compared to an application, we have two additional arguments:

- A name for the component. This name must follow the same rules laid out in the
  [custom element spec](https://html.spec.whatwg.org/multipage/custom-elements.html#valid-custom-element-name)
  and should contain a hyphen (`-`) to avoid clashes with built-in HTML elements.
- A map of attribute names to listen for changes to and a decoder for each to
  decode those attributes into messages to send to your component's `update`
  function.

If it feels like the API for registering components is a little more verbose than
you're used to, that's because it is! You can get surprisingly far storing state
in your top level application and passing it down to different view functions
without needing to use components at all. In fact, for communities like Elm this
is the _only_ way to do things.

## Utilities

### is_browser | erlang javascript

```gleam
pub fn is_browser() -> Bool
```

Gleam has conditional compilation depending on whether you are targetting Erlang
or JavaScript, but sometimes you want to be a bit more specific than that and
check if you're running in the browser.

This is a runtime check that will tell you just that. You could use this to create
a view function that renders something simple on the backend but more complex or
interactive on the frontend.

### is_registered | erlang javascript

```gleam
pub fn is_registered(name: String) -> Bool
```

Lustre's components are built directly on the
[custom element spec](https://html.spec.whatwg.org/multipage/custom-elements.html)
which means they share the same global registery as other custom elements. This
function can tell you if the name you want to use is already registered, by another
Lustre component or otherwise.
