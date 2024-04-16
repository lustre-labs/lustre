# Lustre examples directory

Each of the examples in this directory is a self-contained, complete Gleam app
that demonstrates a particular feature or concept of the library. For newcomers,
we recommend looking through them in order, as each example tends to build on
the previous ones. Feel free to jump in to any example that interests you, though!

> **Note**: these examples all use [`lustre/ui`](https://github.com/lustre-labs/ui)
> to show off something a little more visually interesting than unstyled HTML. None
> of the ideas in these examples are specific to `lustre/ui`, though, and you should
> know that you can follow along with any of these examples using only the standard
> `lustre/element/html` module.

## Examples

- [`01-hello-world`](https://github.com/lustre-labs/lustre/tree/main/examples/01-hello-world)
  is a simple example to just get something on the screen.

- [`02-interactivity`](https://github.com/lustre-labs/lustre/tree/main/examples/02-interactivity)
  introduces the core Model-View-Update loop that underpins every Lustre application.

- [`03-controlled-inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/03-controlled-inputs)
  demonstrates the most common way to handle `<input />` elements in Lustre.

- [`04-custom-event-handlers`](https://github.com/lustre-labs/lustre/tree/main/examples/04-custom-event-handlers)
  shows you how to write your own event handlers and decoders, instead of relying
  on the ones provided by `lustre/event`.

- [`05-http-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests)
  demonstrates how side effects are handled in Lustre, using the third-party
  [`lustre_http`](https://hexdocs.pm/lustre_http/) package.

- [`06-custom-effects`](https://github.com/lustre-labs/lustre/tree/main/examples/06-custom-effects)
  builds on the previous example and shows you how to write your own side effects
  for Lustre to perform.

- [`07-routing`](https://github.com/lustre-labs/lustre/tree/main/examples/07-routing)
  shows how to use [`modem`](https://hexdocs.pm/modem/) to set up routing and
  navigating between pages in a Lustre app.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
