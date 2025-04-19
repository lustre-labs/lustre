# 03 Side effects

Lustre's implementation of the Model-View-Update architecture includes one
additional piece of the puzzle: managed side effects. If we take the MVU diagram
from the previous guide and upgrade it to include managed effects, it looks like
this:

```text
                                       +--------+
                                       |        |
                                       | update |
                                       |        |
                                       +--------+
                                         ^    |
                                         |    |
                                     Msg |    | #(Model, Effect(msg))
                                         |    |
                                         |    v
+------+                         +------------------------+
|      |  #(Model, Effect(msg))  |                        |
| init |------------------------>|     Lustre Runtime     |
|      |                         |                        |
+------+                         +------------------------+
                                         ^    |
                                         |    |
                                     Msg |    | Model
                                         |    |
                                         |    v
                                       +--------+
                                       |        |
                                       |  view  |
                                       |        |
                                       +--------+
```

Well what does managed effects mean, exactly? In Lustre, we expect your `init`,
`update`, and `view` functions to be [_pure_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/pure-functions.md).
That means they shouldn't perform side effects like making an HTTP request or writing
to local storage: we should be able to run your functions 100 times with the same
input and get the same output every time!

Of course, in real applications performing HTTP requests and writing to local
storage turn out to be quite useful things to do. If we shouldn't perform side
effects in our code how do we do them then? Lustre has an [`Effect`](https://hexdocs.pm/lustre/lustre/effect.html)
type that _tells the runtime what side effects to perform_. So we say "Hey, I
want to make an HTTP request to this URL and when you get the response, dispatch
this message to me". The runtime takes care of performing the side effect and
turning the result into something our `update` function understands.

## Why managed effects?

This can feel like a lot of ceremony to go through just to make an HTTP request.
The natural question is: why not just let us make these requests ourselves?

Managed effects have a number of benefits that come from _separating our programs
from the outside world_:

1. **Predictability**: by keeping side effects out of our `update` function, we
   can be confident that our application's state is only ever changed in one
   place. This makes it easier to reason about our code and track down bugs.

2. **Testability**: because our application code is pure, we can test it without
   needing to mock out HTTP services or browser APIs. We can test our `update`
   function, for example, by passing in a sequence of messages: no network mocks
   required!

3. **Reusability**: Lustre applications can run in a variety of environments and
   contexts. The more we push platform-specific code into managed effects, the
   easier time we'll have running our application as a [server component](https://hexdocs.pm/lustre/lustre/server_component.html)
   or as a static site.

## Packages for common effects

The community has developed packages that cover common side effects. For many
applications it's enough to use these packages without writing any custom effects.

> **Note**: _all_ of these packages are community maintained and unrelated to the
> core Lustre organisation. If you run into issues please open an issue on the
> package's repository!

- [`rsvp`](https://hexdocs.pm/rsvp/) lets you make HTTP requests
  and describe what responses to expect from them.

- [`modem`](https://hexdocs.pm/modem/) is a package that helps you manage navigation
  and routing.

- [`plinth`](https://hexdocs.pm/plinth/) provides bindings to Node.js and browser
  platform APIs for creating your own effects.

## Running effects

We know that effects need to be performed by the runtime, but how does the runtime
know when we want it to run an effect? If you have been using the `lustre.simple`
application constructor until now, it is time to upgrade to
[`lustre.application`](https://hexdocs.pm/lustre/lustre.html#application)!

Full Lustre applications differ from simple applications in one important way by
returning a tuple of `#(Model, Effect(Msg))` from your `init` and `update`
functions:

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

We can, for example, launch an HTTP request on application start by using `rsvp.get`
in our `init` function:

```gleam
fn init(_flags) {
  let model = Model(loading: True, ip: None)
  let get_ip = rsvp.get(
    "https://api.ipify.org",
    rsvp.expect_text(ApiReturnedIpAddress)
  )

  #(model, get_ip)
}
```

> **Note**: to tell the runtime we _don't_ want to perform any side effects this
> time, we can use [`effect.none()`](https://hexdocs.pm/lustre/lustre/effect.html#none).

## Writing your own effects

When you need to do something the existing packages don't cover, you can write
your own effect. You can do that by passing a callback to
[`effect.from`](https://hexdocs.pm/lustre/lustre/effect.html#from). Custom effects
are called with an argument – commonly called `dispatch` – that you can use to
send messages back to your application's `update` function.

Below is an example of a custom effect that reads a value from local storage:

```js
// ffi.mjs
import { Ok, Error } from "./gleam.mjs";

export function read(key) {
  const value = window.localStorage.getItem(key);
  return value ? new Ok(value) : new Error(undefined);
}
```

```gleam
fn read(key: String, to_msg: fn(Result(String, Nil)) -> msg) -> Effect(msg) {
  effect.from(fn(dispatch) {
    do_read(key)
    |> to_msg
    |> dispatch
  })
}

@external(javascript, "ffi.mjs", "read")
fn do_read(key: String) -> Result(String, Nil) {
  Error(Nil)
}
```

> **Note**: we provide a default implementation of the `do_read` function that
> always fails. Where possible it's good to provide an implementation for all of
> Gleam's targets. This makes it much easier to run your code as a
> [server component](https://hexdocs.pm/lustre/lustre/server_component.html) in
> the future.

### Effects that touch the DOM

Effects made using `effect.from` always run after your `update` function returns
but _before_ your `view` function is called. A common source of problems is folks
trying to interact with a particular element in the DOM before it's had a chance
to render!

Lustre provides two special effect constructors for working with the DOM's
rendering lifecycle:

1. `effect.before_paint` - Runs after your view function but before the browser
   paints the screen

2. `effect.after_paint` - Runs after the browser has painted the screen

These are useful when you need to measure or manipulate DOM elements at specific
points in the rendering cycle:

```gleam
import lustre/effect

// Measure the width of an element after it's been rendered but before paint
fn measure_element(id: String, to_msg: fn(Int) -> msg) -> Effect(msg) {
  effect.before_paint(fn(dispatch, _) {
    let element = document.get_element_by_id(id)
    let width = element.client_width
    dispatch(to_msg(width))
  })
}
```

### Effects without dispatch

So far, we have seen side effects that are expected to _return something_ to our
program. If we fire an HTTP request, it wouldn't be much use if we couldn't get
the response back! However, effects don't always need to dispatch messages.

It's perfectly valid to write effects that don't dispatch any messages. Earlier
we saw an example of how to read from local storage, we might also want an effect
to _write_ to local storage where there's no response needed:

```js
// ffi.mjs
export function write(key, value) {
  window.localStorage.setItem(key, value);
}
```

```gleam
// app.gleam
fn write(key: String, value: String) -> Effect(msg) {
  effect.from(fn(_) {
    do_write(key, value)
  })
}

@external(javascript, "ffi.mjs", "write")
fn do_write(key: String, value: String) -> Nil {
  Nil
}
```

### Effects with multiple dispatch

Effects can also dispatch _multiple_ messages over time. This is particularly
useful for subscription-based functionality like WebSockets, timers, or event
listeners. [`Modem`](https://hexdocs.pm/modem/) for example, calls the dispatch
function any time a link is clicked on the page.

Here's a simple timer effect that dispatches a message at regular intervals:

```js
// app.ffi.mjs
export function every(interval, cb) {
  window.setInterval(cb, interval);
}
```

```gleam
// app.gleam
fn every(interval: Int, tick: msg) -> Effect(msg) {
  effect.from(fn(dispatch) {
    do_every(interval, fn() {
      dispatch(tick)
    })
  })
}

@external(javascript, "./app.ffi.mjs", "every")
fn do_every(interval: Int, cb: fn() -> Nil) -> Nil {
  Nil
}
```

### Batching effects

If you need to perform multiple effects at once, you can use `effect.batch` to
combine them:

```gleam
fn update(model, msg) {
  case msg {
    UserClickedRefresh -> #(
      Model(..model, loading: True),
      effect.batch([
        fetch_user_profile(model.user_id),
        fetch_user_settings(model.user_id),
      ]),
    )
    // Other cases...
  }
}
```

## Related examples

If you'd like to see some of the ideas in action, we have a number of examples
that demonstrate how Lustre's effects system works in practice:

- [HTTP Requests](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/01-http-requests)
- [Random values](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/02-random)
- [Timers](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/03-timers)
- [LocalStorage](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/04-local-storage)
- [DOM effects](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/05-dom-effects)

To get an overview of every example, check out the [examples directory](../reference/examples.html).

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
