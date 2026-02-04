// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/set.{type Set}
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/platform/dom

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(open: Set(String), entries: List(#(String, String)))
}

fn init(_) -> Model {
  let entries = [
    #(
      "What is Gleam?",
      "Gleam is a friendly language for building type-safe systems that scale!
       It runs on the Erlang VM (BEAM) and can also compile to JavaScript.
      ",
    ),
    #(
      "What is Lustre?",
      "Lustre is a framework for building Web apps in Gleam! It provides a
       declarative, functional API for constructing HTML, and is able to run
       not only in the browser, but on the server as well!",
    ),
    #(
      "How can I get help?",
      "The best place to get help is the Gleam Discord server!",
    ),
  ]

  Model(open: set.new(), entries:)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserToggledEntry(String)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserToggledEntry(id) ->
      case set.contains(model.open, id) {
        True -> Model(..model, open: set.delete(model.open, id))
        False -> Model(..model, open: set.insert(model.open, id))
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 w-full max-w-2xl mx-auto space-y-4")], [
    html.h1([attribute.class("text-2xl font-semibold")], [html.text("FAQ")]),
    // Lists of elements can be grouped using fragments. Fragments only exist
    // in the virtual DOM, but do not affect the structure of the final HTML.
    // Open the development tools of your browser to see this in action!
    //
    // Fragments can sometimes have advantages over using a list:
    //
    // - You can return a fragment from your top-level view function, allowing
    //   you to render multiple elements inside your top-level root node.
    // - You can give a group of elements a key, which means the entire group
    //   can be moved efficiently without replacing elements.
    // - You can add and remove elements from a fragment without potentially
    //   triggering a re-render of all other siblings.
    // - You can add keys to a subset of a nodes' children.
    // - It can be more performant to add multiple fragments instead of
    //   flattening/concatenating a list.
    // 
    // Here, we add keys to our FAQ entries, but leave the other nodes unkeyed.
    // Since we want to render some extra content after the FAQ entries, the
    // fragment here also avoids a list.flatten.
    keyed.fragment(list.map(model.entries, view_entry(model.open, _))),
    html.p([], [
      html.text(
        "Open your browser dev tools now to see how the final HTML looks like!",
      ),
    ]),
  ])
}

fn view_entry(
  open: Set(String),
  entry: #(String, String),
) -> #(String, Element(Msg)) {
  let #(question, answer) = entry
  let is_open = set.contains(open, question)

  let html =
    // We use another nested fragment to give a single key to both the FAQ
    // header and the (optional) content.
    //
    // Since the content is only rendered conditionally, this structure also
    // helps Lustre figure out how to update your HTML more efficiently!
    element.fragment([
      html.button(
        [
          attribute.class("block w-full px-4 py-2 text-lg border rounded-lg"),
          event.on_click(UserToggledEntry(question)),
        ],
        [
          html.text(question),
          html.span(
            [
              attribute.class(
                "inline-block ml-4 text-2xl leading-none align-middle transform transition-transform",
              ),
              case is_open {
                True -> attribute.class("rotate-90")
                False -> attribute.class("-rotate-90")
              },
            ],
            [html.text("›")],
          ),
        ],
      ),
      case is_open {
        True ->
          html.p([attribute.class("bg-gray-100 p-4 rounded-lg shadow-inset")], [
            html.text(answer),
          ])
        False -> element.none()
      },
    ])

  #(question, html)
}
