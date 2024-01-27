# 01 Hello World

This hello world example is a tiny example of what you need to put together to
get a Lustre application running. In later examples we'll touch on server-side
rendering and Lustre Universal Components but for these first examples we'll
be looking at rendering on the client _only_.

## Configuring the Gleam project

It's important to remember to add `target = "javascript"` to your `gleam.toml`!
If you forget to do this you might end up confused when it looks like your project
is successfully building but you have no JavaScript output!

## Creating a `lustre.element` application

The simplest kind of Lustre application is the `element`. This sets up a static
application that does not have its own update loop and cannot dynamically render
any content. Instead, we provide a static Lustre `Element` to render once.

### HTML attributes and inline styles

In Lustre, HTML attributes are modelled as a `List` of attributes. This is a bit
different from many other frameworks that use an object or record for attributes.
Lustre takes the list-of-attributes approach for a couple of reasons:

- Gleam doesn't have a way to construct an anonymous record: we'd have to have
  an infinite number of types to cover every possible varation!

- Working with lists makes it convenient to merge different sets of attributes
  together (like an element that defines some local attributes and merges them
  with any passed in as an argument).

In a similar fashion, inline styles are lists of property/value tuples. In this
example we're setting inline styles for the `width` and `height` properties.

### Why `element.text`?

In frameworks like React, it's enough to just return a `String` if you want to
render some text. Gleam's type system works a little differently though, a string
literal isn't compatible with Lustre's `Element` type on its own, so we need to
wrap any text to render in `element.text`.

You won't see us do it in any of the examples we share, but it's common for folks
to import `text` and any html elements they're using unqualified to cut down on
some of the noise:

```gleam
import lustre/element.{text}
import lustre/element/html.{div, p}
...
```

## Starting a Lustre application

Starting a Lustre application with `lustre.start` requires three things:

- A configured `Application` (that's what we used `lustre.element` for).

- A [CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document_object_model/Locating_DOM_elements_using_selectors)
  to locate the DOM node to mount the application on to. As in other frameworks,
  it's common to use an element with the id "app": for that you'd write the
  selector as `#app`.

- Some initial data to pass to the application's `init` function. Because applications
  constructed with `lustre.element` are not dynamic there's nothing meaningful
  to pass in here, so we just use `Nil`.

Starting an application could fail for a number of reasons, so this function
returns a `Result`. The `Ok` value is a function you can use to send messages to
your running application from the outside world: we'll see more of that in later
examples!

## Seeing the result

Lustre ships with a very simple development server to help you look through these
examples. You can run `gleam run -m lustre/try` in any of these examples to start
this development server and head over to `localhost:1234` to see what it produces.

If you're coming from a more mature Web development setup, you should know that
this preview server is _not_ a replacement for a more robust development setup!
While we work on building this into Lustre we recommend using [vite](https://vitejs.dev)
with the [vite-gleam](https://www.npmjs.com/package/vite-gleam) plugin.

### Enabling lustre_ui

[Lustre_ui](https://hexdocs.pm/lustre_ui/) is a separate package published by us
to provide a collection of robust styled elements for folks that want to get working
with Lustre ASAP. Each of these examples have been written to use elements from
that package.

The lustre/try preview server can be configured to include the lustre_ui stylesheet
by passing the `--include-styles` flag:

```sh
$ gleam run -m lustre/try -- --include-styles
```

Note that the first `--` is necessary so the Gleam binary knows this is a flag
that should be passed to lustre/try!

It's not necessary to use lustre_ui to use Lustre or to check out any of these
examples, but the option is there if you want it.
