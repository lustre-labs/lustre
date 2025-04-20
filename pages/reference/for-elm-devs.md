# Lustre for Elm developers

Lustre has been directly inspired by Elm and shares many of the same architectural
features, particularly the Model-View-Update (MVU) pattern. This guide is for Elm
developers who are new to Lustre and want to get up to speed quickly.

## How do I...?

### Setup a new project

**In Elm**, all you need to get started is to install the `elm` binary.
Running `elm make` against an Elm file will transpile your code to either JavaScript,
or HTML with the JavaScript output inlined. Most people build their own toolchain
to support build-on-save and hot-reload, using tools like Vite or Webpack with
appropriate plugins. A simple hello world looks like this:

```elm
module Main exposing (main)

import Html

main =
    Html.text "Hello, world"
```

**In Lustre**, you need to install the `lustre` package with `gleam add lustre`.
Most Lustre projects will also add the dev tools with `gleam add --dev lustre_dev_tools`.
A simple hello world looks like this:

```gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.h1([], [html.text("Hello, world")]))
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

### Render some HTML

**In Elm**, you call functions in the `elm/html` package to render HTML elements.
The `Html` module contains functions for most standard HTML tags; these functions
take parameters including a list of attributes from `Html.Attributes` or events
from `Html.Events`, as well as a list of child elements:

```elm
Html.button
  [ Html.Attributes.class "primary"
  , Html.Events.onClick ButtonClicked
  ]
  [ Html.text "Click me" ]
```

**In Lustre**, HTML is rendered similarly by calling functions. Functions in
`lustre/element/html` represent HTML tags, and most accept a list of `lustre/attribute`
or `lustre/event` values, as well as a list of child elements:

```gleam
html.button([attribute.class("primary"), event.on_click(ButtonClicked)], [
  html.text("Click me")
])
```

### Render some text

**In Elm**, text is rendered by passing a `String` to the `Html.text` function:

```elm
Html.span [] [ Html.text <| "Hello, " ++ name ]
```

**In Lustre**, text is rendered by passing a `String` to the `html.text` function:

```gleam
html.span([], [
  html.text("Hello, " <> name),
])
```

### Manage state

**In Elm**, all state is stored in a single `Model` type and updates happen through
a central `update` function:

```elm
type alias Model = Int

init : Model
init = 0

type Msg
    = Incr
    | Decr

update : Msg -> Model -> Model
update msg model =
    case msg of
        Incr ->
            model + 1

        Decr ->
            model - 1
```

**In Lustre**, state management works almost identically to Elm with a central
`Model` type and an `update` function:

```gleam
type Model =
  Int

fn init(_) -> Model {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}
```

### Handling side effects

**In Elm**, side effects like HTTP requests are handled with commands. The
`update` function returns both a new model and a command to perform:

```elm
type Msg
  = ApiReturnedBookResponse (Result Http.Error String)

getBook : Cmd Msg
getBook =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString ApiReturnedBookResponse
    }

type alias Model = { bookResponse : Result Http.Error String }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ApiReturnedBookResponse response ->
      ( { model | bookResponse = response }, Cmd.none )
```

**In Lustre**, the approach is similar using the `Effect` type. The recommendation
is to use the `rsvp` package for HTTP requests:

```gleam
type Msg {
  ApiReturnedBookResponse(Result(String, rsvp.Error))
}

fn get_book() -> Effect(Msg) {
  rsvp.get(
    "https://elm-lang.org/assets/public-opinion.txt",
    rsvp.expect_text(ApiReturnedBookResponse)
  )
}

type Model {
  Model(book_response: Result(String, rsvp.Error))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ApiReturnedBookResponse(response) -> #(
      Model(..model, book_response: response),
      effect.none()
    )
  }
}
```

### Create a component

**In Elm**, there's no built-in concept of components. Instead, you create functions
that return HTML elements:

```elm
viewButton : String -> msg -> Html msg
viewButton label msg =
  Html.button [ Html.Events.onClick msg ] [ Html.text label ]

-- In your view
view model =
  Html.div []
    [ viewButton "Increment" Increment
    , viewButton "Decrement" Decrement
    ]
```

**In Lustre**, the primary approach is similar with view functions, but Lustre also
supports stateful components:

```gleam
// Simple view function (like Elm)
fn view_button(label: String, msg: msg) -> Element(msg) {
  html.button([event.on_click(msg)], [html.text(label)])
}

// In your view
fn view(model: Model) -> Element(Msg) {
  html.div([], [
    view_button("Increment", Incr),
    view_button("Decrement", Decr)
  ])
}

// Lustre also supports stateful components with their own MVU cycle
pub fn counter_component() -> App(Nil, CounterModel, CounterMsg) {
  lustre.component(counter_init, counter_update, counter_view, [])
}
```

### Work with lists

**In Elm**, you typically use `List.map` to render lists of items:

```elm
view : Model -> Html Msg
view model =
  Html.ul []
    (List.map viewItem model.items)

viewItem : String -> Html Msg
viewItem item =
  Html.li [] [ Html.text item ]
```

**In Lustre**, it works the same way using `list.map`:

```gleam
fn view(model: Model) -> Element(Msg) {
  html.ul([],
    list.map(model.items, view_item)
  )
}

fn view_item(item: String) -> Element(Msg) {
  html.li([], [html.text(item)])
}
```

For better performance with large lists, you can use `keyed.ul` to provide a
unique key for each item:

```gleam
fn view(model: Model) -> Element(Msg) {
  keyed.ul([],
    list.map(model.items, fn(item) {
      #(item.id, html.li([], [html.text(item.text)]))
    })
  )
}
```

### Handle form inputs

**In Elm**, you typically use event handlers like `onInput` to capture form changes:

```elm
type Msg = UpdateName String

view : Model -> Html Msg
view model =
  Html.input
    [ Html.Attributes.value model.name
    , Html.Events.onInput UpdateName
    ]
    []
```

**In Lustre**, this works similarly:

```gleam
type Msg {
  UpdateName(String)
}

fn view(model: Model) -> Element(Msg) {
  html.input([
    attribute.value(model.name),
    event.on_input(UpdateName)
  ], [])
}
```

## Differences to be aware of

1. **Effect handling** - While both Elm and Lustre use a similar approach, Lustre's
   effect system provides more flexibility with `before_paint` and `after_paint`
   effects.

2. **Components** - Lustre has first-class support for stateful components, which
   Elm doesn't provide.

3. **Syntax** - Gleam's syntax is more similar to Rust or OCaml than Elm's
   Haskell-inspired syntax.

5. **JavaScript interop** - Lustre provides easier JavaScript interop through
   Gleam's FFI system compared to Elm's more restricted ports.

## Where to go next

To walk through setting up a new Lustre project and building your first app, check
out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).

If you prefer to learn by example, we have a collection of examples that show
off specific features and patterns in Lustre. You can find them in the
[examples directory](https://hexdocs.pm/lustre/reference/examples.html).

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
