# 06 Full stack applications

We've now seen how Lustre can render single-page applications in the browser and
generate static HTML templates. In this guide, we'll look at how to put these pieces
together into a complete full-stack application with server-side rendering and
hydration.

## Project structure

To create a full stack Web application in Gleam you will need to adopt a
_monorepo_. Although Gleam supports multiple targets, and has conditional
compilation features, the language isn't designed to support a single codebase
with different applications for different targets. Instead, we will create
_three separate Gleam projects_:

```sh
mkdir lustre-fullstack-guide \
  && cd lustre-fullstack-guide \
  && gleam new client --name app \
  && gleam new server --name app \
  && gleam new shared --name app
```

We have one project for the frontend SPA, one project for the server, and a third
project called `shared` that we will use to share types and code across the stack.

Full stack applications can get complex quickly, so this guide will focus on putting
together a simple grocery list with pre-rendering and hydration, as well as a single
API endpoint to save the items to a SQLite database.

## The shared package

Let's start by defining the shared types and functions that will be used by both
the client and server applications. This way, we can ensure that our data structures
remain consistent across the stack.

```sh
cd shared
gleam add gleam_json
```

In `src/groceries.gleam`, let's define our core types:

```gleam
import gleam/json
import gleam/list

pub type GroceryItem {
  GroceryItem(name: String, quantity: Int)
}

pub fn serialize_grocery_list(items: List(GroceryItem)) -> String {
  items
  |> list.map(fn(item) {
    json.object([
      #("name", json.string(item.name)),
      #("quantity", json.int(item.quantity)),
    ])
  })
  |> json.array
  |> json.to_string
}

pub fn view_functions() {
  // We'll put shared view functions here later
}
```

## The client application

The client application will be a Lustre SPA that can communicate with our backend API.
Let's set up the necessary dependencies and scaffolding:

```sh
cd ../client
gleam add lustre rsvp gleam_json
gleam add lustre_dev_tools --dev
```

We also need to add our shared package as a local dependency. In `gleam.toml`, add:

```toml
[dependencies]
shared = { path = "../shared" }
```

Now, let's implement our client application in `src/client.gleam`:

```gleam
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
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
  // For hydration, we'll read the initial model from the window
  let flags = get_initial_state()

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", flags)

  Nil
}

fn get_initial_state() -> List(GroceryItem) {
  // In a real application, you would read this from a script tag
  // that was rendered by the server (as described in the SSR guide)
  []
}

type Model {
  Model(
    items: List(GroceryItem),
    new_item: String,
    saving: Bool,
    error: Maybe(String),
  )
}

fn init(items: List(GroceryItem)) -> #(Model, Effect(Msg)) {
  let model = Model(
    items: items,
    new_item: "",
    saving: False,
    error: Nothing,
  )

  #(model, effect.none())
}

type Msg {
  ServerSavedList(Result(Nil, rsvp.Error))
  UserAddedItem
  UserTypedNewItem(String)
  UserSavedList
  UserUpdatedQuantity(name: String, amount: Int)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ServerSavedList(Ok(_)) -> #(
      Model(..model, saving: False, error: Nothing),
      effect.none(),
    )

    ServerSavedList(Error(_)) -> #(
      Model(..model, saving: False, error: Just("Failed to save list")),
      effect.none(),
    )

    UserAddedItem -> {
      case model.new_item {
        "" -> #(model, effect.none())
        name -> {
          let item = GroceryItem(name: name, quantity: 1)
          let updated_items = [item, ..model.items]

          #(
            Model(..model, items: updated_items, new_item: ""),
            effect.none(),
          )
        }
      }
    }

    UserTypedNewItem(text) -> #(
      Model(..model, new_item: text),
      effect.none(),
    )

    UserSavedList -> #(
      Model(..model, saving: True),
      save_list(model.items),
    )

    UserUpdatedQuantity(name, amount) -> {
      let updated_items =
        list.map(model.items, fn(item) {
          case item.name == name {
            True -> GroceryItem(..item, quantity: amount)
            False -> item
          }
        })

      #(Model(..model, items: updated_items), effect.none())
    }
  }
}

fn save_list(items: List(GroceryItem)) -> Effect(Msg) {
  let body = groceries.serialize_grocery_list(items)
  let url = "/api/groceries"

  rsvp.post(url, rsvp.json_body(body), rsvp.expect_nothing(ServerSavedList))
}

fn view(model: Model) -> Element(Msg) {
  let styles = [
    #("max-width", "30ch"),
    #("margin", "0 auto"),
    #("display", "flex"),
    #("flex-direction", "column"),
    #("gap", "1em"),
  ]

  html.div([attribute.style(styles)], [
    html.h1([], [html.text("Grocery List")]),
    view_grocery_list(model.items),
    view_new_item(model.new_item),
    html.div([], [
      html.button(
        [event.on_click(UserSavedList), attribute.disabled(model.saving)],
        [html.text(case model.saving {
          True -> "Saving..."
          False -> "Save List"
        })],
      ),
    ]),
    case model.error {
      Nothing -> html.text("")
      Just(error) -> html.div(
        [attribute.style([#("color", "red")])],
        [html.text(error)],
      )
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
      html.ul([],
        list.map(items, fn(item) {
          html.li([], [view_grocery_item(item)])
        })
      )
    }
  }
}

fn view_grocery_item(item: GroceryItem) -> Element(Msg) {
  html.div([attribute.style([#("display", "flex"), #("gap", "1em")])], [
    html.span([attribute.style([#("flex", "1")])], [html.text(item.name)]),
    html.input([
      attribute.style([#("width", "4em")]),
      attribute.type_("number"),
      attribute.value(int.to_string(item.quantity)),
      attribute.min("0"),
      event.on_input(fn(value) {
        result.unwrap(int.parse(value), 0)
        |> UserUpdatedQuantity(item.name, _)
      }),
    ]),
  ])
}
```

## The server application

Now let's implement our server, which will handle API requests, serve static assets, and perform server-side rendering:

```sh
cd ../server
gleam add gleam_erlang gleam_http mist lustre gleam_json sqlight
gleam add lustre_dev_tools --dev
```

In `gleam.toml`, add our shared package as a dependency:

```toml
[dependencies]
shared = { path = "../shared" }
```

Let's implement the server in `src/server.gleam`:

```gleam
import gleam/bytes_builder
import gleam/erlang/process
import gleam/http.{Get, Post}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lustre/element
import lustre/element/html
import lustre/attribute
import mist.{type Connection, type ResponseData}
import shared/groceries.{type GroceryItem, GroceryItem}
import sqlight

pub fn main() {
  // Set up our database
  let assert Ok(db) = setup_database()

  let handler = fn(req: Request(Connection)) -> Response(ResponseData) {
    let path = request.path_segments(req)
    let method = request.method(req)

    case method, path {
      // API endpoint for saving grocery lists
      Post, ["api", "groceries"] -> handle_save_groceries(req, db)

      // Static files
      Get, ["static", ..rest] -> serve_static(rest)

      // Everything else gets our HTML with hydration data
      Get, _ -> serve_app(db)

      // Fallback for other methods/paths
      _, _ -> response.new(404)
        |> response.set_body(mist.Bytes(bytes_builder.new()))
    }
  }

  let assert Ok(_) =
    mist.new(handler)
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}

fn setup_database() -> Result(sqlight.Connection, sqlight.Error) {
  use db <- result.try(sqlight.open("groceries.db"))

  let create_table = """
  CREATE TABLE IF NOT EXISTS groceries (
    name TEXT PRIMARY KEY,
    quantity INTEGER NOT NULL
  )
  """

  use _ <- result.try(sqlight.exec(db, create_table))

  Ok(db)
}

fn handle_save_groceries(
  req: Request(Connection),
  db: sqlight.Connection
) -> Response(ResponseData) {
  use body <- result.try(
    request.body(req),
    fn(_) {
      response.new(400)
      |> response.set_body(mist.Bytes(bytes_builder.from_string("Bad request")))
    },
  )

  use items <- result.try(
    parse_grocery_items(body),
    fn(_) {
      response.new(400)
      |> response.set_body(mist.Bytes(bytes_builder.from_string("Invalid JSON")))
    },
  )

  // Save items to database
  use _ <- result.try(
    save_items_to_db(items, db),
    fn(_) {
      response.new(500)
      |> response.set_body(mist.Bytes(bytes_builder.from_string("Database error")))
    },
  )

  response.new(200)
  |> response.set_body(mist.Bytes(bytes_builder.from_string("OK")))
}

fn parse_grocery_items(body: String) -> Result(List(GroceryItem), Nil) {
  // In a real application, you would parse the JSON into GroceryItems
  Ok([])
}

fn save_items_to_db(
  items: List(GroceryItem),
  db: sqlight.Connection
) -> Result(Nil, sqlight.Error) {
  // First, delete all existing items
  use _ <- result.try(sqlight.exec(db, "DELETE FROM groceries"))

  // Then insert the new items
  use item <- list.try_each(items)

  let query = """
  INSERT INTO groceries (name, quantity)
  VALUES (?, ?)
  """

  let args = [
    sqlight.text(item.name),
    sqlight.int(item.quantity),
  ]

  sqlight.query(db, query, args, fn(_) { Ok(Nil) })
}

fn serve_static(path_segments: List(String)) -> Response(ResponseData) {
  // In a real application, you would handle serving static files
  // This is just a placeholder
  response.new(404)
  |> response.set_body(mist.Bytes(bytes_builder.from_string("File not found")))
}

fn serve_app(db: sqlight.Connection) -> Response(ResponseData) {
  // Fetch grocery items from database
  let items = fetch_items_from_db(db)

  let html =
    html.html([], [
      html.head([], [
        html.title([], [html.text("Grocery List")]),
        html.script([attribute.type_("module"), attribute.src("/static/client.js")], []),
        html.script(
          [attribute.type_("application/json"), attribute.id("initial-data")],
          [html.text(groceries.serialize_grocery_list(items))],
        ),
      ]),
      html.body([], [
        html.div([attribute.id("app")], [
          // Render the initial view
          // In a real application, you would render the same view as the client
          html.h1([], [html.text("Grocery List")]),
          html.ul([],
            list.map(items, fn(item) {
              html.li([], [
                html.div([], [
                  html.span([], [html.text(item.name)]),
                  html.span([], [html.text(" x " <> string.inspect(item.quantity))])
                ])
              ])
            })
          ),
        ]),
      ]),
    ])

  response.new(200)
  |> response.set_body(
    html
    |> element.to_document_string
    |> bytes_builder.from_string
    |> mist.Bytes
  )
}

fn fetch_items_from_db(db: sqlight.Connection) -> List(GroceryItem) {
  let query = "SELECT name, quantity FROM groceries"

  case sqlight.query(
    db,
    query,
    [],
    fn(row) {
      use name <- result.try(sqlight.column(row, 0, sqlight.as_text))
      use quantity <- result.try(sqlight.column(row, 1, sqlight.as_int))

      Ok(GroceryItem(name: name, quantity: quantity))
    },
  ) {
    Ok(items) -> items
    Error(_) -> []
  }
}
```

## Building for production

When preparing for production, you'll want to minify your client application and
ensure it's placed somewhere the server can serve it. Minification is a process
that reduces the size of your JavaScript files by removing whitespace and comments,
renaming variables, and other transformations.

Lustre's build tool can produce minified JavaScript bundles by providing the
`--minify` flag.

### Building the client

```sh
cd ../client
gleam run -m lustre/dev build --minify --outdir=../server/priv/static
```

Typically, building a Lustre application will place the bundled JavaScript in the
project's `priv/static` directory. Because we're building a full stack application
we want our server to serve the JavaScript itself. The `--outdir` flag tells the
build tool to place the output in our _server's_ priv directory instead.

For the server code to properly serve these static files, update the `serve_static` function in `server.gleam`:

```gleam
fn serve_static(path_segments: List(String)) -> Response(ResponseData) {
  let file_path = string.join(path_segments, "/")
  let priv_dir =
    case gleam_erlang.priv_directory("app") {
      Ok(dir) -> dir
      Error(_) -> "./priv"
    }

  let full_path = string.concat([priv_dir, "/static/", file_path])

  case gleam_erlang.read_file(full_path) {
    Ok(content) -> {
      let content_type = determine_content_type(file_path)

      response.new(200)
      |> response.set_header("content-type", content_type)
      |> response.set_body(mist.Bytes(bytes_builder.from_string(content)))
    }
    Error(_) -> {
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_builder.from_string("File not found")))
    }
  }
}

fn determine_content_type(file_path: String) -> String {
  case string.ends_with(file_path, ".js") {
    True -> "application/javascript"
    False -> case string.ends_with(file_path, ".css") {
      True -> "text/css"
      False -> "text/plain"
    }
  }
}
```

### Enhancing client hydration

Update the `get_initial_state` function in the client to properly read the server's initial state:

```gleam
fn get_initial_state() -> List(GroceryItem) {
  // Access the script tag with initial data
  let script = dom.get_element_by_id("initial-data")

  case script {
    Ok(el) -> {
      let json_string = dom.inner_text(el)

      case parse_grocery_items(json_string) {
        Ok(items) -> items
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

fn parse_grocery_items(json_string: String) -> Result(List(GroceryItem), Nil) {
  // Parse the JSON string into GroceryItems
  // This would be a real implementation in your application
  Ok([])
}
```

## Running the application

To run your full-stack application:

1. Build the client and output to the server's static directory:
   ```sh
   cd client
   gleam run -m lustre/dev build --outdir=../server/priv/static
   ```

2. Run the server:
   ```sh
   cd ../server
   gleam run
   ```

3. Visit `http://localhost:3000` in your browser to see your application with server-side rendering and client-side hydration.

## Next steps

This guide has provided a foundation for building full-stack applications with Lustre. Some areas you might want to explore next:

- Adding authentication
- Implementing more complex API endpoints
- Improving the development workflow with hot reloading
- Setting up deployment for your application

For deployment strategies, check out the [full-stack deployments guide](./07-full-stack-deployments.html).
