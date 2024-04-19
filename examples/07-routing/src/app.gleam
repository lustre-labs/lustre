import gleam/dynamic
import gleam/list
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/ui
import lustre/ui/layout/cluster
import lustre/ui/util/cn
import modem

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

fn init(_flags) -> #(Model, Effect(Msg)) {
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
    Home -> view_home(model)
    WelcomeGuest(name) -> view_welcome(model, name)
  }

  ui.stack([attribute.style(styles)], [view_nav(model), page])
}

fn view_home(model: Model) {
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

  view_body([
    view_title("Welcome to the Party 🏡"),
    html.p([], [element.text("Please sign the guest book:")]),
    ui.input([
      event.on("keyup", new_guest_input),
      attribute.value(model.new_guest_name),
    ]),
  ])
}

fn view_welcome(model: Model, slug) -> Element(a) {
  let guest =
    model.guests
    |> list.find(fn(guest: Guest) { guest.slug == slug })

  let title = case guest {
    Ok(guest) -> view_title("Hello, " <> guest.name <> "! 🎉")
    _ -> view_title("Sorry ... didn't quite catch that.")
  }

  view_body([title])
}

fn view_nav(model: Model) -> Element(a) {
  let item_styles = [#("text-decoration", "underline")]

  let view_nav_item = fn(path, text) {
    html.a([attribute.href("/" <> path), attribute.style(item_styles)], [
      element.text(text),
    ])
  }

  let guest_nav_items =
    model.guests
    |> list.map(fn(guest: Guest) {
      view_nav_item("welcome/" <> guest.slug, guest.name)
    })

  cluster.of(html.nav, [], [view_nav_item("", "Home"), ..guest_nav_items])
}

fn view_body(children) {
  ui.centre([cn.mt_xl()], ui.stack([], children))
}

fn view_title(text) {
  html.h1([cn.text_xl()], [element.text(text)])
}
