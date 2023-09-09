// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/function
import gleam/map.{Map}
import lustre
import lustre/attribute
import lustre/effect.{Effect}
import lustre/element.{Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main(route: Route) -> fn(Msg) -> Nil {
  let app = lustre.application(init, update, view)
  let assert Ok(dispatch) = lustre.start(app, "body", route)

  dispatch
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(route: Route, content: String, history: Map(String, String))
}

fn init(route: Route) -> #(Model, Effect(Msg)) {
  let content = ""
  let history = map.new()
  let model = Model(route, content, history)
  let effects = case route.path {
    "/" -> fetch_post("/docs/quickstart", history)
    _ -> fetch_post(route.path, history)
  }

  #(model, effects)
}

// UPDATE ----------------------------------------------------------------------

pub type Msg {
  OnRouteChange(Route)
  GotPost(String, String)
}

pub type Route {
  Route(path: String, hash: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  // We need to do this because Gleam doesn't support record field access in
  // guards just yet.
  let current_path = model.route.path

  case msg {
    OnRouteChange(Route(path: "/", hash: _) as route) -> #(
      Model(..model, route: route),
      fetch_post("/docs/quickstart", model.history),
    )

    // Only fetch the markdown 
    OnRouteChange(Route(path: path, hash: _) as route) if path != current_path -> #(
      Model(..model, route: route),
      fetch_post(path, model.history),
    )

    GotPost(path, content) -> #(
      Model(
        ..model,
        content: content,
        history: map.insert(model.history, path, content),
      ),
      effect.none(),
    )
  }
}

fn fetch_post(path: String, history: Map(String, String)) -> Effect(Msg) {
  use dispatch <- effect.from

  case map.get(history, path) {
    Ok(content) -> dispatch(GotPost(path, content))
    Error(_) ->
      fetch_post_content(path, function.compose(GotPost(path, _), dispatch))
  }
}

@external(javascript, "./app.ffi.mjs", "fetch_post")
fn fetch_post_content(path: String, dispatch: fn(String) -> Nil) -> Nil

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  case model.route.path {
    "/" ->
      html.body(
        [],
        [
          html.div(
            [
              attribute.class(
                "w-screen h-screen flex justify-center items-center",
              ),
              attribute.style([
                #("background-color", "hsla(226,0%,100%,1)"),
                #(
                  "background-image",
                  " radial-gradient(at 62% 13%, hsla(170,76%,60%,1) 0px, transparent 65%),
                radial-gradient(at 67% 42%, hsla(234,89%,70%,1) 0px, transparent 65%),
                radial-gradient(at 10% 7%, hsla(213,93%,57%,1) 0px, transparent 65%),
                radial-gradient(at 32% 46%, hsla(291,93%,80%,1) 0px, transparent 65%)",
                ),
              ]),
            ],
            [
              html.hgroup(
                [],
                [
                  html.h1(
                    [attribute.class("text-8xl")],
                    [element.text("Lustre.")],
                  ),
                  html.p(
                    [attribute.class("pl-1")],
                    [element.text("Web apps from space!")],
                  ),
                ],
              ),
            ],
          ),
          layout.docs_section(model.content),
        ],
      )

    _ -> layout.docs_page(model.content)
  }
}
