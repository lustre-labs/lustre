// IMPORTS ---------------------------------------------------------------------

import app/page/api/lustre as lustre_api
import app/page/api/lustre/attribute as attribute_api
import app/page/api/lustre/effect as effect_api
import app/page/api/lustre/element as element_api
import app/page/api/lustre/element/html as html_api
import app/page/api/lustre/element/svg as svg_api
import app/page/api/lustre/event as event_api
import app/page/docs/components as components_docs
import app/page/docs/managing_state as managing_state_docs
import app/page/docs/quickstart as quickstart_docs
import app/page/docs/server_side_rendering as server_side_rendering_docs
import app/page/docs/side_effects as side_effects_docs
import lustre
import lustre/element.{Element}

// MAIN ------------------------------------------------------------------------

pub fn main(route: Route) -> fn(Msg) -> Nil {
  let app = lustre.simple(init, update, view)
  let assert Ok(dispatch) = lustre.start(app, "body", route)

  dispatch
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(route: Route)
}

fn init(route: Route) -> Model {
  Model(route)
}

// UPDATE ----------------------------------------------------------------------

pub type Msg {
  OnRouteChange(Route)
}

pub type Route {
  Route(path: String, hash: String)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    OnRouteChange(route) -> Model(..model, route: route)
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  case model.route.path {
    "/" -> quickstart_docs.view()

    "/api" -> lustre_api.view()
    "/api/lustre" -> lustre_api.view()
    "/api/lustre/attribute" -> attribute_api.view()
    "/api/lustre/effect" -> effect_api.view()
    "/api/lustre/element" -> element_api.view()
    "/api/lustre/element/html" -> html_api.view()
    "/api/lustre/element/svg" -> svg_api.view()
    "/api/lustre/event" -> event_api.view()

    "/docs" -> quickstart_docs.view()
    "/docs/quickstart" -> quickstart_docs.view()
    "/docs/managing-state" -> managing_state_docs.view()
    "/docs/side-effects" -> side_effects_docs.view()
    "/docs/components" -> components_docs.view()
    "/docs/server-side-rendering" -> server_side_rendering_docs.view()

    _ -> quickstart_docs.view()
  }
}
