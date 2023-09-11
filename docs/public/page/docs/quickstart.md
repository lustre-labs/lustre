# Quickstart

Lustre is a frontend web framework for Gleam. It is primarily focused on helping
you build robust single-page applications (SPAs), but it can also be used on the
server to render static HTML. To get an idea of what it's all about, here's a
quick overview of Lustre's key features:

- Elm-inspired runtime with state management and controlled side effects out of
  the box.
- A simple, declarative API for building type-safe user interfaces.
- Stateful components built as custom elements and useable just like any other
  HTML element.
- Static HTML rendering anywhere Gleam can run: the BEAM, Node.js, Deno, or the
  browser.

In this quickstart guide we'll take a look at how to get up and running with
Lustre in both the browser and on the server.

## In the browser | javascript

To get started, we'll scaffold a new Gleam project using `gleam new`. If you've
found your way to this guide but don't already know what Gleam is you can read
about it over at [gleam.run](https://gleam.run).

```shell
$ gleam new lustre_quickstart && cd lustre_quickstart && gleam add lustre
```

In a real project you probably want to use a build tool like [vite](https://vitejs.dev)
along with the [vite-gleam](https://github.com/Enderchief/vite-gleam) plugin, but
to keep this guide simple we'll just show you what code you need to write and leave
the details on serving the app up to you. MDN have a handy guide covering some
different options to [set up a local web server for development](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Tools_and_setup/set_up_a_local_testing_server)
if you need some ideas.

### Basic HTML setup

With our Gleam project scaffolded, go ahead and create an `index.html` in the root
of the project. This is the minimal code you'll typically want to get started:

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Lustre Quickstart</title>

    <script type="module">
      import { main } from "./build/dev/javascript/lustre_quickstart/app.mjs";

      document.addEventListener("DOMContentLoaded", () => {
        main();
      });
    </script>
  </head>

  <body>
    <div data-lustre-app></div>
  </body>
</html>
```

We wait until the DOM has loaded before calling the our app's `main` function.
This will mount the Lustre app and start rendering. We also add the `data-lustre-app`
attribute to the element we want to mount the app to. You could use a class or an
id instead, or none of that: [`lustre.start`](/api/lustre#start) takes a CSS
selector so go wild!

### Hello, world!

Go ahead and rename the generated `lustre_quickstart.gleam` file to `app.gleam`
and replace the contents with the following:

```gleam
import lustre
import lustre/element.{text}

pub fn main() {
  let app = lustre.element(text("Hello, world!"))
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)

  Nil
}
```

This will create a static Lustre app and mount it onto the element that matches
the CSS selector. While we're asserting everything is OK here, it is possible
for `lustre.start` to fail in a couple of ways. Check out the docs for the
[`lustre.Error`](/api/lustre#error-type) type if you want to know more.

Run `gleam build` and serve the HTML with your preferred static file server (this
step is necessary: JavaScript modules can't be imported when just opening a HTML
file) and admire your handiwork.

### Adding interactivity

Now that we know how to get things up and running, let's try something a little
more exciting and add some interactivity. Replace the contents of your `app.gleam`
file with the code below and rebuild the project.

```gleam
import gleam/int
import lustre
import lustre/element.{text}
import lustre/element/html.{div, button, p}
import lustre/event.{on_click}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)

  Nil
}

fn init(_) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model, msg) {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

fn view(model) {
  let count = int.to_string(model)

  div([], [
    button([on_click(Decr)], [text(" - ")]),
    p([], [text(count)]),
    button([on_click(Incr)], [text(" + ")])
  ])
}
```

You should now have a very exciting counter app! Almost every Lustre app will
boil down to the same three parts:

- A `Model` type that represents your application's state and a function to
  `init` it.
- A `Msg` type and an `update` function to update that state based on incoming
  messages.
- A `view` function that takes the current state and renders some HTML.

This architecture is not unique to Lustre. It was introduced by the Elm community
and known as the [Elm Architecture](https://guide.elm-lang.org/architecture/)
before making its way to React as [Redux](https://redux.js.org) and beyond, known
more generally as the Model-View-Update architecture. If you work through the
rest of our guides you'll see how this architecture helps keep side effects out
of our view code and how to create components that can encapsulate their own state
and update logic.

For now though, we'll leave things here. If you're interested in seeing how Lustre
can be used to render static HTML on the server, read on! Otherwise, you can take
this counter application as a base and start building something of your own.

## On the server | erlang javascript

As we've seen, Lustre is primarily meant to be used in the browser to build
interactive SPAs. It is possible to render Lustre elements to static HTML and
simply use Lustre as a templating DSL. As before, we'll start by scaffolding a
new Gleam project and adding Lustre as a dependency:

```shell
$ gleam new lustre_quickstart && cd lustre_quickstart && gleam add lustre
```

The [`lustre/element`](/api/lustre/element) module contains functions to render
an element as either a `String` or `StringBuilder`. Copy the following code into
`lustre_quickstart.gleam`:

```gleam
import gleam/io
import lustre/attribute.{attribute}
import lustre/element.{text}
import lustre/element/html.{html, head, title, body, div, h1}

pub fn main() {
  html([attribute("lang", "en")], [
    head([], [
     title([], [text("Lustre Quickstart")])
    ]),
    body([], [
      h1([], [text("Hello, world!")])
    ])
  ])
  |> element.to_string
  |> io.println
}
```

We can test this out by running `gleam run` and seeing the HTML printed to the
console. From here we could set up a web server using [Mist](/guides/mist) or
[Wisp](/guides/wisp) to serve the HTML to the browser or write it to a file using
[simplifile](https://hexdocs.pm/simplifile/). Because the API is the same for
both client and server rendering, it is easy to create reusable components that
can be rendered anywhere Gleam can run!

### An example with Wisp

Before we go, let's just take a quick look at what it would look like to use
Lustre in a [Wisp](https://hexdocs.pm/wisp) application. We won't scaffold out a
real app in this example, but we'll adapt one of the examples from Wisp's own
documentation.

Specifically, we'll take a look at the `show_form` function from the
["working with form data"](https://github.com/lpil/wisp/blob/ea8a40bc20745f172695c8cc2dc0a63769f890a7/examples/2-working-with-form-data/src/app/router.gleam#L20)
example:

```gleam
...

pub fn show_form() {
  // In a larger application a template library or HTML form library might
  // be used here instead of a string literal.
  let html =
    string_builder.from_string(
      "<form method='post'>
        <label>Title:
          <input type='text' name='title'>
        </label>
        <label>Name:
          <input type='text' name='name'>
        </label>
        <input type='submit' value='Submit'>
      </form>",
    )
  wisp.ok()
  |> wisp.html_body(html)
}
```

They've helpfully left a comment telling us that in a larger application we might
want to use a template library, and Lustre is up to the task! Let's refactor this
using Lustre:

```gleam
import gleam/string
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html
...

pub fn show_form() {
  html.form([attribute("method", "post")], [
    labelled_input("Title"),
    labelled_input("Name"),
    html.input([attribute("type", "submit"), attribute("value", "Submit")])
  ])
  |> element.to_string_builder
  |> wisp.html_body
  |> wisp.ok
}

fn labelled_input(name) {
  html.label([], [
    element.text(name <> ": "),
    html.input([
      attribute("type", "text"),
      attribute("name", string.lowercase(name))
    ])
  ])
}
```
