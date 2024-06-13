# Lustre for Elm developers

Lustre has been directly inspired by Elm and shares some of the primary architectural
features of "The Elm Architecture", otherwise known as Model-View-Update. This
guide is for Elm developers who are new to Lustreand want to get up to speed quickly.

## How do I...?

### Setup a new project

**In Elm**, all you really need to get started is to install the `elm` binary.
Running `elm make` against an Elm file will transpile your code to either Javascript,
or HTML with the Javascript output inlined. Most people will build out their own
toolchain to support build-on-save and hot-reload, with tools like Vite or Webpack
with the appropriate plugins. A simple hello world might look like this:

```elm
module Main exposing (main)

import Html

main =
    Html.text "Hello, world"

```

**In Lustre** you need to install the `lustre` package with `gleam add lustre`.
Most Lustre projects will add the dev tools too with `gleam add --dev lustre_dev_tools`.
A simple hello world might look like this:

```gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.h1([], [html.text("Hello, world")]))
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}
```

### Render some HTML

**In Elm**, you can call functions in the `elm/html` package to render HTML
elements. The `Html` module in `elm/html` contains functions for most standard
HTML tags; these functions take as parameters a list of attributes from
`Html.Attributes`, or events from `Html.Events` - as well as a list of child
elements:

```elm
Html.button
  [ Html.Attributes.class "primary"
  , Html.Events.onClick ButtonClicked
  ]
  [ Html.text "Click me" ]
```

---

**In Lustre**, HTML is rendered by calling functions, many of whom share the same
signature - functions in `lustre/element/html` represent HTML tags, and most
functions accept a list of `lustre/attribute` or `lustre/event` values, as well
as a list of child elements.

```lustre
html.button([attribute.class("primary"), event.on_click(ButtonClicked)], [
  html.text("Click me")
])
```

### Render some text

**In Elm**, text is rendered by passing a `String` to the `Html.text` function:

```elm
Html.span [] [ Html.text <| "Hello, " ++ name ]
```

---

**In Lustre**, text is rendered by passing a `String` to the `html.text` or
`element.text` functions:

```gleam
html.span([], [
  html.text("Hello, " <> name),
])
```

### Manage state

**In Elm** all state is stored in a single `Model` type and updates happen through
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

---

**In Lustre** all state is stored in a single `Model` type and updates happen
through a central `update` function, just like in Elm:

```gleam
fn init(_) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> Msg {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}
```

### Handle events

**In Elm** event handlers are decoders for event objects. When the decoder succeeds,
that value is dispatched as a message to your `update` function.

```elm
Html.input [ Html.Events.onInput UserUpdatedNameField ] []

type Msg
  = UserUpdatedNameField String

type alias Model = { name : String }

update : Msg -> Model -> Model
update msg model =
  case msg of
    UserUpdatedNameField name
      { model | name = name }
```

---

**In Lustre** event handlers work in the same way. Lustre provides functions to
handle most common events, in [`lustre/effect`](https://hexdocs.pm/lustre/lustre/effect.html):

```gleam
button([on_click(Decr)], [text("-")])
```

```gleam
input([on_input(UpdateInput)])
```

```gleam
div([on("mousemove", fn(event) {
  ...
}], [...])
```

### Fetch data

**In Elm** you can fetch data by making a HTTP request. HTTP request functions
both return a value of type `Cmd msg`, and are handled by the application's `update`
function; the payload from the response is available within the `update` function
and can be used to update the `Model`, or call other functions that return a value
of type `Cmd msg`.

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
      { model | bookResponse = response }
```

---

**In Lustre**, the approach is similar, using types and functions from the
[`lustre_http` package](https://hexdocs.pm/lustre_http/lustre_http.html):

```gleam
pub type Msg {
  ApiReturnedBookResponse(Result(String, lustre_http.HttpError))
}

fn get_book() -> effect.Effect(Msg) {
  lustre_http.get(
    "https://elm-lang.org/assets/public-opinion.txt",
    lustre_http.expect_text(ApiReturnedBookResponse)
  )
}

pub type Model {
  Model(book_response: Result(String, HttpError))
}


pub fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    ApiReturnedBookResponse(response) -> #(Model(..model, book_response: response), effect.none())
  }
}

```

## Where to go next

To walk through setting up a new Lustre project and building your first app, check
out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).

If you prefer to learn by example, we have a collection of examples that show
off specific features and patterns in Lustre. You can find them in the
[examples directory](https://hexdocs.pm/lustre/reference/examples.html)

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
