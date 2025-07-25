# 06 Full stack applications

We've now seen how Lustre can render single-page applications in the browser,
static HTML templates, and we've seen how hydration can be implemented. In this
guide we'll look at how to put these pieces together into a single application.

## Project structure

To create a full stack Web application in Gleam you will need to adopt a
_monorepo_. Although Gleam supports multiple targets, and has conditional
compilation features, the language isn't designed to support a single codebase
with different applications for different targets. Instead, we will create
_three separate Gleam projects_:

```sh
mkdir lustre-fullstack-guide \
  && cd lustre-fullstack-guide \
  && gleam new client \
  && gleam new server \
  && gleam new shared
```

We have one project for the frontend SPA, one project for the server, and a third
project called `shared` that we will use to share types and code across the stack.

Full stack applications can get complex quickly, so this guide will focus on putting
together a simple grocery list with pre-rendering and hydration, as well as a single
API endpoint to save the items to a file-based database.

## The shared package

Let's start by defining the shared types and functions that will be used by both
the client and server applications. This way, we can ensure that our data structures
remain consistent across the stack.

```sh
cd shared
gleam add gleam_json
```

In `src/shared/groceries.gleam`, let's define our core types. The `_to_json`
and `_decoder`  functions are largely auto-generated by using the respective
code actions:

```gleam
import gleam/dynamic/decode
import gleam/json

pub type GroceryItem {
  GroceryItem(name: String, quantity: Int)
}

fn grocery_item_decoder() -> decode.Decoder(GroceryItem) {
  use name <- decode.field("name", decode.string)
  use quantity <- decode.field("quantity", decode.int)
  decode.success(GroceryItem(name:, quantity:))
}

pub fn grocery_list_decoder() -> decode.Decoder(List(GroceryItem)) {
  decode.list(grocery_item_decoder())
}

fn grocery_item_to_json(grocery_item: GroceryItem) -> json.Json {
  let GroceryItem(name:, quantity:) = grocery_item
  json.object([#("name", json.string(name)), #("quantity", json.int(quantity))])
}

pub fn grocery_list_to_json(items: List(GroceryItem)) -> json.Json {
  json.array(items, grocery_item_to_json)
}
```

## The client application

The client application will be a Lustre SPA that can communicate with our backend API.
Let's set up the necessary dependencies and scaffolding:

```sh
cd ../client
gleam add lustre rsvp gleam_json gleam_http plinth
gleam add lustre_dev_tools --dev
```

We also need to add our shared package as a local dependency. In `gleam.toml`, add:

```toml
[dependencies]
shared = { path = "../shared" }
```

Since the client will be a JavaScript SPA, we also set that as the target such
that we can use client-specific code:

```toml
name = "client"
version = "1.0.0"
target = "javascript"

# ...
```

After editing the `gleam.toml` file, we need to tell gleam about this change to
make sure our dependencies are all up-to-date:

```sh
gleam deps update
```

Now, let's implement our client application in `src/client.gleam`:

```gleam
import gleam/http/response.{type Response}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import rsvp
import shared/groceries.{type GroceryItem, GroceryItem}

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", [])

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(
    items: List(GroceryItem),
    new_item: String,
    saving: Bool,
    error: Option(String),
  )
}

fn init(items: List(GroceryItem)) -> #(Model, Effect(Msg)) {
  let model =
    Model(items: items, new_item: "", saving: False, error: option.None)

  #(model, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ServerSavedList(Result(Response(String), rsvp.Error))
  UserAddedItem
  UserTypedNewItem(String)
  UserSavedList
  UserUpdatedQuantity(index: Int, quantity: Int)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ServerSavedList(Ok(_)) -> #(
      Model(..model, saving: False, error: option.None),
      effect.none(),
    )

    ServerSavedList(Error(_)) -> #(
      Model(..model, saving: False, error: option.Some("Failed to save list")),
      effect.none(),
    )

    UserAddedItem -> {
      case model.new_item {
        "" -> #(model, effect.none())
        name -> {
          let item = GroceryItem(name: name, quantity: 1)
          let updated_items = list.append(model.items, [item])

          #(Model(..model, items: updated_items, new_item: ""), effect.none())
        }
      }
    }

    UserTypedNewItem(text) -> #(Model(..model, new_item: text), effect.none())

    UserSavedList -> #(Model(..model, saving: True), save_list(model.items))

    UserUpdatedQuantity(index:, quantity:) -> {
      let updated_items =
        list.index_map(model.items, fn(item, item_index) {
          case item_index == index {
            True -> GroceryItem(..item, quantity:)
            False -> item
          }
        })

      #(Model(..model, items: updated_items), effect.none())
    }
  }
}

fn save_list(items: List(GroceryItem)) -> Effect(Msg) {
  let body = groceries.grocery_list_to_json(items)
  let url = "/api/groceries"

  rsvp.post(url, body, rsvp.expect_ok_response(ServerSavedList))
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [
    #("max-width", "30ch"),
    #("margin", "0 auto"),
    #("display", "flex"),
    #("flex-direction", "column"),
    #("gap", "1em"),
  ]

  html.div([attribute.styles(styles)], [
    html.h1([], [html.text("Grocery List")]),
    view_grocery_list(model.items),
    view_new_item(model.new_item),
    html.div([], [
      html.button(
        [event.on_click(UserSavedList), attribute.disabled(model.saving)],
        [
          html.text(case model.saving {
            True -> "Saving..."
            False -> "Save List"
          }),
        ],
      ),
    ]),
    case model.error {
      option.None -> element.none()
      option.Some(error) ->
        html.div([attribute.style("color", "red")], [html.text(error)])
    },
  ])
}

fn view_new_item(new_item: String) -> Element(Msg) {
  html.div([], [
    html.input([
      attribute.placeholder("Enter item name"),
      attribute.value(new_item),
      event.on_input(UserTypedNewItem),
    ]),
    html.button([event.on_click(UserAddedItem)], [html.text("Add")]),
  ])
}

fn view_grocery_list(items: List(GroceryItem)) -> Element(Msg) {
  case items {
    [] -> html.p([], [html.text("No items in your list yet.")])
    _ -> {
      html.ul(
        [],
        list.index_map(items, fn(item, index) {
          html.li([], [view_grocery_item(item, index)])
        }),
      )
    }
  }
}

fn view_grocery_item(item: GroceryItem, index: Int) -> Element(Msg) {
  html.div([attribute.styles([#("display", "flex"), #("gap", "1em")])], [
    html.span([attribute.style("flex", "1")], [html.text(item.name)]),
    html.input([
      attribute.style("width", "4em"),
      attribute.type_("number"),
      attribute.value(int.to_string(item.quantity)),
      attribute.min("0"),
      event.on_input(fn(value) {
        result.unwrap(int.parse(value), 0)
        |> UserUpdatedQuantity(index, quantity: _)
      }),
    ]),
  ])
}
```

## The server application

Now let's implement our server, which will handle API requests, serve static
assets, and perform server-side rendering. We will use the popular [Wisp](https://hexdocs.pm/wisp/index.html)
web framework as well as [storail](https://hexdocs.pm/storail/index.html)
as a simple database.

```sh
cd ../server
gleam add gleam_erlang gleam_http gleam_json wisp mist lustre storail
```

Again, we will add our shared package as a dependency to `gleam.toml`:

```toml
[dependencies]
shared = { path = "../shared" }
```

Let's implement the server in `src/server.gleam`:

```gleam
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http.{Get, Post}
import gleam/json
import gleam/result
import lustre/attribute
import lustre/element
import lustre/element/html
import mist
import storail
import wisp.{type Request, type Response}
import wisp/wisp_mist

import shared/groceries.{type GroceryItem}

pub fn main() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  // Set up our database
  let assert Ok(db) = setup_database()

  let assert Ok(priv_directory) = wisp.priv_directory("server")
  let static_directory = priv_directory <> "/static"

  let assert Ok(_) =
    handle_request(db, static_directory, _)
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(3000)
    |> mist.start

  process.sleep_forever()
}

// REQUEST HANDLERS ------------------------------------------------------------

fn app_middleware(
  req: Request,
  static_directory: String,
  next: fn(Request) -> Response,
) -> Response {
  let req = wisp.method_override(req)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)
  use <- wisp.serve_static(req, under: "/static", from: static_directory)

  next(req)
}

fn handle_request(
  db: storail.Collection(List(GroceryItem)),
  static_directory: String,
  req: Request,
) -> Response {
  use req <- app_middleware(req, static_directory)

  case req.method, wisp.path_segments(req) {
    // API endpoint for saving grocery lists
    Post, ["api", "groceries"] -> handle_save_groceries(db, req)

    // Everything else gets our HTML with hydration data
    Get, _ -> serve_index(db)

    // Fallback for other methods/paths
    _, _ -> wisp.not_found()
  }
}

fn serve_index(db: storail.Collection(List(GroceryItem))) -> Response {
  let html =
    html.html([], [
      html.head([], [
        html.title([], "Grocery List"),
        html.script(
          [attribute.type_("module"), attribute.src("/static/client.mjs")],
          "",
        ),
      ]),
      html.body([], [html.div([attribute.id("app")], [])]),
    ])

  html
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}

fn handle_save_groceries(
  db: storail.Collection(List(GroceryItem)),
  req: Request,
) -> Response {
  use json <- wisp.require_json(req)

  case decode.run(json, groceries.grocery_list_decoder()) {
    Ok(items) ->
      case save_items_to_db(db, items) {
        Ok(_) -> wisp.ok()
        Error(_) -> wisp.internal_server_error()
      }
    Error(_) -> wisp.bad_request()
  }
}

// DATABASE --------------------------------------------------------------------

fn setup_database() -> Result(storail.Collection(List(GroceryItem)), Nil) {
  let config = storail.Config(storage_path: "./data")

  let items =
    storail.Collection(
      name: "grocery_list",
      to_json: groceries.grocery_list_to_json,
      decoder: groceries.grocery_list_decoder(),
      config:,
    )

  Ok(items)
}

fn grocery_list_key(
  db: storail.Collection(List(GroceryItem)),
) -> storail.Key(List(GroceryItem)) {
  // In a real application, you would probably store items as individual
  // documents, or use a database like PostgreSQL instead.
  storail.key(db, "grocery_list")
}

fn save_items_to_db(
  db: storail.Collection(List(GroceryItem)),
  items: List(GroceryItem),
) -> Result(Nil, storail.StorailError) {
  storail.write(grocery_list_key(db), items)
}
```

## Running our app

To run our new full-stack app, we first have to bundle the client app into a JavaScript file:

```sh
cd ../client
gleam run -m lustre/dev build --outdir=../server/priv/static
```

Afterwards, we can run our server:

```sh
cd ../server
gleam run
```

## Adding hydration

Unfortunately, even after we saved our grocery list, all our items are lost after
we reload!

To fix this, we need to serialise the initial state we want to render instead
in our server, and then load that state from our client, effectively pre-populating
our grocery list with data from the database.

This is often called _hydration_ in other frameworks. The previous guide on
[server-side rendering](https://hexdocs.pm/lustre/guide/05-server-side-rendering.html)
gives a more in-depth introduction to this process.

First, let's extend our server to include the grocery items from our database:

```gleam
// server.gleam

fn serve_index(db: storail.Collection(List(GroceryItem))) -> Response {
  // NEW: Fetch grocery items from database
  let items = fetch_items_from_db(db)

  let html =
    html.html([], [
      html.head([], [
        html.title([], "Grocery List"),
        html.script(
          [attribute.type_("module"), attribute.src("/static/client.mjs")],
          "",
        ),
      ]),
      // NEW: include a script tag with our initial grocery list
      html.script(
        [attribute.type_("application/json"), attribute.id("model")],
        json.to_string(groceries.grocery_list_to_json(items))
      ),
      html.body([], [html.div([attribute.id("app")], [])]),
    ])

  html
  |> element.to_document_string_tree
  |> wisp.html_response(200)
}


fn fetch_items_from_db(
  db: storail.Collection(List(GroceryItem)),
) -> List(GroceryItem) {
  storail.read(grocery_list_key(db))
  |> result.unwrap([])
}
```

Next, we can use this data to initialise our client app state:

```gleam
// client.gleam
import gleam/json
import plinth/browser/document
import plinth/browser/element as plinth_element

pub fn main() {
  let initial_items =
    document.query_selector("#model")
    |> result.map(plinth_element.inner_text)
    |> result.try(fn(json) {
      json.parse(json, groceries.grocery_list_decoder())
      |> result.replace_error(Nil)
    })
    |> result.unwrap([])

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", initial_items)

  Nil
}
```

After that, your app state will persist after reloading.


## Building for production

When preparing for production, you'll want to minify your client application and
ensure it's placed somewhere the server can serve it. Minification is a process
that reduces the size of your JavaScript files by removing whitespace and comments,
renaming variables, and other transformations.

Lustre's build tool can produce minified JavaScript bundles by providing the
`--minify` flag.

```sh
cd ../client
gleam run -m lustre/dev build --minify --outdir=../server/priv/static
```

Typically, building a Lustre application will place the bundled JavaScript in the
project's `priv/static` directory. Because we're building a full stack application
we want our server to serve the JavaScript itself. The `--outdir` flag tells the
build tool to place the output in our _server's_ priv directory instead.

You will also need to change the `script` tag from `/static/client.mjs` to =
`/static/client.min.mjs` to load your production bundle.

## Next steps

To learn how to deploy your full-stack application, check out the [full-stack deployments guide](./07-full-stack-deployments.html).
