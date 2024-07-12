> **Note**: this guide is a work in progress and is not currently complete. Content
> here will change and be added over time. In the meantime, you can check out the
> [Gleam Discord server](https://discord.gg/Fm8Pwmy) if you have any questions about
> full stack applications with Lustre.

# 05 Full stack applications

We've now seen how Lustre can render single-page applications in the browser,
static HTML templates, and we've seen how hydration can be implemented. In this
guide we'll look at how to put these pieces together into a single application.

To create a full stack Web application in Gleam you will need to adopt a
_monorepo_. Although Gleam supports multiple targets, and has conditional
compilation features, the language isn't designed to support a single codebase
with different applications for different targets. Instead, we will create
_three separate Gleam projects_:

```sh
mkdir lustre-fullstack-guide \
  && gleam new client --name app \
  && gleam new server --name app
```

We have one project for the frontend SPA, one project for the server, and a third
project called `shared` that we will use to share types and code across the stack.

Full stack applications can get quite complex quite quickly, so this guide will
focus on putting together a simple grocery list with pre-rendering and hydration,
as well as a single API endpoint to save the items to a SQLite database.

## The client

The application we're building will eventually need to make requests to our backend
API, which means we need an application capable of performing effects. To start,
let's scaffold out all the types and functions we need:

```sh
gleam add decipher lustre lustre_http gleam_json
```

```sh
gleam add lustre_dev_tools --dev
```

```gleam
import gleam/dynamic
import gleam/int
import gleam/result
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model

fn init(_) -> #(Model, Effect(Msg)) {
  todo
}

type Msg

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  todo
}

fn view(model: Model) -> Element(Msg) {
  todo
}
```

The `Model` for our grocery list will be a list of tuples containing the name
of some produce and a quantity to purchase. For now, we'll initialise that with
just an empty list:

```gleam
type Model =
  List(#(String, Int))

fn init(_) -> #(Model, Effect(Msg)) {
  let model = []
  let effect = effect.none()

  #(model, effect)
}
```

Our `Msg` type needs to cover changes to the grocery list, the user requesting
to save the list, and the response from the server:

```gleam
type Msg {
  ServerSavedList(Result(Nil, String))
  UserAddedProduct(name: String)
  UserSavedList
  UserUpdatedQuantity(name: String, amount: Int)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ServerSavedList(_) -> todo
    UserAddedProduct(_) -> todo
    UserSavedList -> todo
    UserUpdatedQuantity(_, _) -> todo
  }
}
```

Finally, our `view` function will render the grocery list as an unordered list:
each item will have an input field to update the quantity. Below the list will
be an input field to add a new item, and a button to save the list:

```gleam
fn view(model: Model) -> Element(Msg) {
  let styles = [
    #("max-width", "30ch"),
    #("margin", "0 auto"),
    #("display", "flex"),
    #("flex-direction", "column"),
    #("gap", "1em"),
  ]

  html.div([attribute.style(styles)], [
    view_grocery_list(model),
    view_new_item(),
    html.div([], [html.button([], [html.text("Sync")])]),
  ])
}

fn view_new_item() -> Element(Msg) {
  let handle_click = fn(event) {
    let path = ["target", "previousElementSibling", "value"]

    event
    |> decipher.at(path, dynamic.string)
    |> result.map(UserAddedProduct)
  }

  html.div([], [
    html.input([]),
    html.button([event.on("click", handle_click)], [html.text("Add")]),
  ])
}

fn view_grocery_list(model: Model) -> Element(Msg) {
  let styles = [#("display", "flex"), #("flex-direction", "column-reverse")]

  element.keyed(html.div([attribute.style(styles)], _), {
    use #(name, quantity) <- list.map(model)
    let item = view_grocery_item(name, quantity)

    #(name, item)
  })
}

fn view_grocery_item(name: String, quantity: Int) -> Element(Msg) {
  let handle_input = fn(e) {
    event.value(e)
    |> result.nil_error
    |> result.then(int.parse)
    |> result.map(UserUpdatedQuantity(name, _))
    |> result.replace_error([])
  }

  html.div([attribute.style([#("display", "flex"), #("gap", "1em")])], [
    html.span([attribute.style([#("flex", "1")])], [html.text(name)]),
    html.input([
      attribute.style([#("width", "4em")]),
      attribute.type_("number"),
      attribute.value(int.to_string(quantity)),
      attribute.min("0"),
      event.on("input", handle_input),
    ]),
  ])
}
```

## Building for production

When we're ready to deploy our application we will want to make sure our client
app is _minified_. Minification is a process JavaScript build tools go through to
rename variables, strip whitespace, and transform the code in other ways to make
the file smaller.

Lustre's build tools can produce minified JavaScript (and CSS, if you're using
Tailwind) bundles by providing the `--minify` flag:

```sh
gleam run -m lustre/dev build --minify --outdir=../server/priv/static
```

Typically, building a Lustre application will place the bundled JavaScript in the
project's `priv/static` directory. Because we're building a full stack application
we want our server to serve the JavaScript itself. The `--outdir` flag tells the
build tool to place the output in our _server's_ priv directory instead.

Next, in our server we can use Wisp's [`server_static`](https://hexdocs.pm/wisp/wisp.html#serve_static)
middleware to automatically serve files from our server's `priv/static` directory.
To do that we'll modify our `handler` function:

```gleam
fn handler(req: wisp.Request) -> wisp.Response {
  let assert Ok(priv) = wisp.priv_directory("app")
  let static_dir = priv <> "/static"
  use <- wisp.serve_static(req, under: "/static", from: static_dir)
  ...
}
```
