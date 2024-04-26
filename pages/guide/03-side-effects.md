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

The community has started to build packages that cover common side effects. For
many applications it's enough to drop these packages in and start using them
without needing to write any custom effects.

> **Note**: _all_ of these packages are community maintained and unrelated to the
> core Lustre organisation. If you run into issues please open an issue on the
> package's repository!

- [`lustre_http`](https://hexdocs.pm/lustre_http/) lets you make HTTP requests
  and describe what responses to expect from them.

- [`lustre_websocket`](https://hexdocs.pm/lustre_websocket/) handles WebSocket
  connections and messages.

- [`modem`](https://hexdocs.pm/modem/) and [`lustre_routed`](https://hexdocs.pm/lustre_routed/)
  are two packages that help you manage navigation and routing.

- [`lustre_animation`](https://hexdocs.pm/lustre_animation/) is a simple package
  for interpolating between values over time.

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

We can, for example, launch an HTTP request on application start by using `lustre_http.get`
in our `init` function:

```gleam
fn init(_flags) {
  let model = Model(...)
  let get_ip = lustre_http.get(
    "https://api.ipify.org",
    ApiReturnedIpAddress
  )

  #(model, get_ip)
}
```

> **Note**: to tell the runtime we _don't_ want to perform any side effects this
> time, we can use [`effect.none()`](https://hexdocs.pm/lustre/lustre/effect.html#none).

## Writing your own effects

When you need to do something one of the existing packages doesn't cover, you need
to write your own effect. You can do that by passing a callback to
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

Lustre runs all your side effects after your `update` function returns but _before_
your `view` function is called. A common bug folks run into is trying to interact
with a particular element in the DOM before it's had a chance to render. As a
rule of thumb, you should _always_ wrap custom effects that interact with the DOM
in a [`requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame)
call to ensure the DOM has had a chance to update first.

### Effects without dispatch

So far, we have seen side effects that are expected to _return something_ to our
program. If we fire an HTTP request, it wouldn't be much use if we couldn't get
the response back! Sometimes folks wrongly assume effects _must_ use the `dispatch`
function they're given, but this isn't true!

It's also totally valid to write effects that don't dispatch any messages. Earlier
we saw an example of how to read from local storage, we might also want an effect
to _write_ to local storage and there's not much to dispatch in that case!

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

Similar to effects that don't dispatch any messages, some folks skip over the fact
effects can dispatch _multiple_ messages. Packages like [`lustre_websocket`](https://hexdocs.pm/lustre_websocket/)
and [`modem`](https://hexdocs.pm/modem/) set up effects that will dispatch many
messages over the lifetime of your program.

Once you have a reference to that `dispatch` function, you're free to call it as
many times as you want!

```js
// ffi.mjs
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

@external(javascript, "ffi.mjs", "every")
fn do_every(interval: Int, cb: fn() -> Nil) -> Nil {
  Nil
}
```

Here we set up an effect that will continuously dispatch a `tick` message at a
fixed interval.

## Related examples

If you'd like to see some of the ideas in action, we have a number of examples
that demonstrate how Lustre's effects system works in practice:

- [`05-http-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests)
- [`06-custom-effects`](https://github.com/lustre-labs/lustre/tree/main/examples/06-custom-effects)
- [`07-routing`](https://github.com/lustre-labs/lustre/tree/main/examples/07-routing)

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
