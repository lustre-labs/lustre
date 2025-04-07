// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(posts: Dict(Int, Post), route: Route)
}

// we will build a simple Blog site here, so we need a way to store our posts!
// In this demo, the posts are just stored as a constant list, at the end of
// this file.
type Post {
  Post(id: Int, title: String, summary: String, text: String)
}

// We want to show different views, depending on which URL we are on:
//
// - /      - show the home page
// - /posts - show a list of posts
// - /about - show an about page
//
// and so on.
//
// We could keep around the URL type or even the path as a String just like that
// but this makes it hard to work with, change, and error prone.
// 
// Instead, we _parse_ the URL into a nice Gleam custom type with just the
// variants we need! Every part of the application can then only deal with those
// values. As a nice bonus, we get type and exhaustiveness checking on all our
// internal links, so we know they can never be wrong and that we handled them
// all, given we implemented the route type correctly.
type Route {
  Index
  Posts
  PostById(id: Int)
  About
  NotFound
}

// This is where we parse a URL value into a Route value for our application.
// If we could'nt parse the value correctly, we fall back to a `NotFound` variant.
fn parse_route(uri: Uri) -> Route {
  case uri.path_segments(uri.path) {
    [] | [""] -> Index

    ["posts"] -> Posts

    ["post", post_id] ->
      case int.parse(post_id) {
        Ok(post_id) -> PostById(post_id)
        Error(_) -> NotFound
      }

    ["about"] -> About

    _ -> NotFound
  }
}

// We also need a way to turn a Route back into a an `href` attribute that we
// can then use on `html.a` elements. It is important to keep this function in
// sync with the parsing, but once you do, all links are guaranteed to work!
fn href(route: Route) -> Attribute(msg) {
  let url = case route {
    Index -> "/"
    About -> "/about"
    Posts -> "/posts"
    PostById(post_id) -> "/post/" <> int.to_string(post_id)
    NotFound -> "/404"
  }

  attribute.href(url)
}

fn init(_) {
  // "Routing" in an SPA consists of 2 parts:
  //
  // - Getting the initial URL and showing the appropriate view for it.
  //   This is what we do here by parsing the initial uri into an initial route.
  // - Dynamically intercepting and reacting to URL changes after a link has
  //   been clicked by the user.
  //
  // We will use the `modem` library for both of these. Modem allows us to use
  // normal html.a tags, and will automatically intercept all link clicks to
  // internal links for us and send a message to our update function.
  let route = case modem.initial_uri() {
    Ok(uri) -> parse_route(uri)
    Error(_) -> Index
  }

  let posts = posts |> list.map(fn(post) { #(post.id, post) }) |> dict.from_list

  let model = Model(route:, posts:)

  #(model, modem.init(on_uri_change))
}

// Whenever the URI changes, we parse it into a route and send that to our
// update function.
fn on_uri_change(uri: Uri) -> Msg {
  let route = parse_route(uri)
  OnRouteChange(route)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  OnRouteChange(route: Route)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route:) -> #(Model(..model, route:), effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  element.fragment([
    html.nav(
      [
        attribute.class(
          "flex justify-between items-center px-32 my-16 mx-auto w-full max-w-2xl",
        ),
      ],
      [
        html.h1([attribute.class("text-purple-600 font-medium text-xl")], [
          html.a([href(Index)], [html.text("My little Blog")]),
        ]),
        html.ul([attribute.class("flex space-x-8")], [
          view_header_link(model.route, Posts, [html.text("Posts")]),
          view_header_link(model.route, About, [html.text("About")]),
        ]),
      ],
    ),
    html.main(
      [attribute.class("px-32 my-16 mt mx-auto w-full max-w-2xl")],
      // Just like we would show different HTML based on some other state in the
      // model, we can also pattern match on our Route value to show different
      // views based on the current page!
      case model.route {
        Index -> view_index()
        Posts -> view_posts(model)
        PostById(post_id) -> view_post(model, post_id)
        About -> view_about()
        NotFound -> view_not_found()
      },
    ),
  ])
}

fn view_header_link(
  current: Route,
  target: Route,
  children: List(Element(msg)),
) -> Element(msg) {
  // The posts link is also considered active if we are _currently_ on a post
  // detail page.
  let is_active = case current, target {
    PostById(_), Posts -> True
    _, _ -> current == target
  }

  html.li(
    [
      attribute.classes([
        #("border-transparent border-b-2 hover:border-purple-600", True),
        #("text-purple-600", is_active),
      ]),
    ],
    [html.a([href(target)], children)],
  )
}

// VIEW PAGES ------------------------------------------------------------------

fn view_index() -> List(Element(Msg)) {
  [
    title("Hello, Joe"),
    leading(
      "Or whoever you may be! This is were I will share random ramblings
       and thoughts about life.",
    ),
    html.p([attribute.class("mt-14")], [
      html.text("There is not much going on at the moment, but you can still "),
      link(Posts, "read my ramblings ->"),
    ]),
    paragraph("If you like <3"),
  ]
}

fn view_posts(model: Model) -> List(Element(Msg)) {
  [
    title("Posts"),
    ..model.posts
    |> dict.values
    |> list.sort(fn(a, b) { int.compare(a.id, b.id) })
    |> list.map(fn(post) {
      html.article([attribute.class("mt-14")], [
        html.h3([attribute.class("text-xl text-purple-600 font-light")], [
          html.a([attribute.class("hover:underline"), href(PostById(post.id))], [
            html.text(post.title),
          ]),
        ]),
        html.p([attribute.class("mt-1")], [html.text(post.summary)]),
      ])
    })
  ]
}

fn view_post(model: Model, post_id: Int) -> List(Element(Msg)) {
  case dict.get(model.posts, post_id) {
    Error(_) -> view_not_found()
    Ok(post) -> [
      html.article([], [
        title(post.title),
        leading(post.summary),
        paragraph(post.text),
      ]),
      html.p([attribute.class("mt-14")], [link(Posts, "<- Go back?")]),
    ]
  }
}

fn view_about() -> List(Element(Msg)) {
  [
    title("Me"),
    paragraph(
      "I document the odd occurrences that catch my attention and rewrite my own
       narrative along the way. I'm fine being referred to with pronouns.",
    ),
    paragraph(
      "If you enjoy these glimpses into my mind, feel free to come back
       semi-regularly. But not too regularly, you creep.",
    ),
  ]
}

fn view_not_found() -> List(Element(Msg)) {
  [
    title("Not found"),
    paragraph(
      "You climpse into the void and see -- nothing?
       Well that was somewhat expected.",
    ),
  ]
}

// VIEW HELPERS ----------------------------------------------------------------

fn title(title) {
  html.h2([attribute.class("text-3xl text-purple-800 font-light")], [
    html.text(title),
  ])
}

fn leading(text) {
  html.p([attribute.class("mt-8 text-lg")], [html.text(text)])
}

fn paragraph(text) {
  html.p([attribute.class("mt-14")], [html.text(text)])
}

fn link(target: Route, title: String) -> Element(msg) {
  html.a(
    [
      href(target),
      attribute.class("text-purple-600 hover:underline cursor-pointer"),
    ],
    [html.text(title)],
  )
}

// DATA ------------------------------------------------------------------------

const posts: List(Post) = [
  Post(
    id: 1,
    title: "The Empty Chair",
    summary: "A guide to uninvited furniture and its temporal implications",
    text: "
      There's an empty chair in my home that wasn't there yesterday. When I sit
      in it, I start to remember things that haven't happened yet. The chair is
      getting closer to my bedroom each night, though I never caught it move.
      Last night, I dreamt it was watching me sleep. This morning, it offered
      me coffee.
    ",
  ),
  Post(
    id: 2,
    title: "The Library of Unwritten Books",
    summary: "Warning: Reading this may shorten your narrative arc",
    text: "
      Between the shelves in the public library exists a thin space where
      books that were never written somehow exist. Their pages change when you
      blink. Forms shifting to match the souls blueprint. Librarians warn
      against reading the final chapter of any unwritten book – those who do
      find their own stories mysteriously concluding. Yourself is just another
      draft to be rewritten.
    ",
  ),
  Post(
    id: 3,
    title: "The Hum",
    summary: "A frequency analysis of the collective forgetting",
    text: "
      The citywide hum started Tuesday. Not everyone can hear it, but those who
      can't are slowly being replaced by perfect copies who smile too widely.
      The hum isn't sound – it's the universe forgetting our coordinates.
      Reports suggest humming back in harmony might postpone whatever comes
      next. Or perhaps accelerate it.
    ",
  ),
]
