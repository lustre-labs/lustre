import gleam/uri.{type Uri}
import gleam/list
import gleam/string
import gleam/result
import gleam/dynamic
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import modem
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-example-styles` flag:
//
//   $ gleam run -m lustre/dev start --use-example-styles
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(current_route: Route, guests: List(Guest), new_guest_name: String)
}

type Route {
  Home
  WelcomeGuest(value: String)
}

type Guest {
  Guest(slug: String, name: String)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(
      current_route: Home,
      guests: [
        Guest(slug: "chihiro", name: "Chihiro"),
        Guest(slug: "totoro", name: "Totoro"),
      ],
      new_guest_name: "",
    ),
    modem.init(on_route_change),
  )
}

fn on_route_change(uri: Uri) -> Msg {
  case uri.path_segments(uri.path) {
    ["welcome", guest] -> OnRouteChange(WelcomeGuest(guest))
    _ -> OnRouteChange(Home)
  }
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  OnRouteChange(Route)
  UserUpdatedNewGuestName(String)
  UserAddedNewGuest(Guest)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route) -> #(
      Model(..model, current_route: route),
      effect.none(),
    )
    UserUpdatedNewGuestName(name) -> #(
      Model(..model, new_guest_name: name),
      effect.none(),
    )
    UserAddedNewGuest(guest) -> #(
      Model(
        ..model,
        guests: list.append(model.guests, [guest]),
        new_guest_name: "",
      ),
      effect.none(),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("margin", "15vh")]

  let page = case model.current_route {
    Home -> render_home(model)
    WelcomeGuest(name) -> render_welcome(model, name)
  }

  ui.stack([attribute.style(styles)], [render_nav(model), page])
}

fn render_home(model: Model) {
  let new_guest_input = fn(event) {
    use key_code <- result.try(dynamic.field("key", dynamic.string)(event))
    case key_code {
      "Enter" -> {
        let guest_slug =
          model.new_guest_name
          |> string.replace(" ", "-")
          |> string.lowercase
        Ok(
          UserAddedNewGuest(Guest(name: model.new_guest_name, slug: guest_slug)),
        )
      }
      _ -> {
        use value <- result.try(event.value(event))
        Ok(UserUpdatedNewGuestName(value))
      }
    }
  }

  render_body([
    render_title("Welcome to the Party 🏡"),
    html.p([], [element.text("Please sign the guest book:")]),
    ui.input([
      event.on("keyup", new_guest_input),
      attribute.value(model.new_guest_name),
    ]),
  ])
}

fn render_welcome(model: Model, slug) -> Element(a) {
  let guest =
    model.guests
    |> list.find(fn(guest: Guest) { guest.slug == slug })

  let title = case guest {
    Ok(guest) -> render_title("Hello, " <> guest.name <> "! 🎉")
    _ -> render_title("Sorry ... didn't quite catch that.")
  }

  render_body([title])
}

fn render_nav(model: Model) -> Element(a) {
  let item_styles = [#("margin", "1rem"), #("text-decoration", "underline")]

  let render_nav_item = fn(path, text) {
    html.a([attribute.href("/" <> path), attribute.style(item_styles)], [
      element.text(text),
    ])
  }

  let guest_nav_items =
    model.guests
    |> list.map(fn(guest: Guest) {
      render_nav_item("welcome/" <> guest.slug, guest.name)
    })

  html.nav([], [render_nav_item("", "Home"), ..guest_nav_items])
}

fn render_body(children) {
  ui.centre(
    [attribute.style([#("margin-top", "10vh")])],
    ui.stack([], children),
  )
}

fn render_title(text) {
  html.h1(
    [attribute.style([#("font-size", "2.25rem"), #("font-weight", "bold")])],
    [element.text(text)],
  )
}
