// IMPORTS ---------------------------------------------------------------------

import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/result
import gleam/string_builder
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html.{html}
import lustre/server
import lustre/ui/styles
import mist.{type Connection, type ResponseData}
import wisp.{type Request, type Response}

//

pub fn handle(req: HttpRequest(Connection)) -> HttpResponse(ResponseData) {
  wisp.mist_handler(handler, "")(req)
}

fn handler(req: Request) -> Response {
  use req <- wisp.handle_head(req)
  use <- wisp.serve_static(req, under: "/static", from: static_directory())

  html([attribute("lang", "en")], [
    html.head([], [
      html.meta([attribute("charset", "utf-8")]),
      html.meta([
        attribute("name", "viewport"),
        attribute("content", "width=device-width, initial-scale=1"),
      ]),
      styles.elements(),
      html.script(
        [attribute("type", "module")],
        "
        import '/static/lustre-server-component.mjs'

        document.addEventListener('DOMContentLoaded', () => {
          document
            .querySelector('lustre-server-component')
            .addEventListener('alert', event => {
              console.log(`The server count says: ${event.detail}`)
            })
        })
        ",
      ),
    ]),
    html.body([], [
      html.div(
        [
          attribute.style([
            #("width", "80ch"),
            #("margin", "0 auto"),
            #("padding", "2rem"),
          ]),
        ],
        [server.component([server.route("/ws")])],
      ),
    ]),
  ])
  |> element.to_string_builder
  |> string_builder.prepend("<!DOCTYPE html>\n")
  |> wisp.html_response(200)
}

fn static_directory() -> String {
  wisp.priv_directory("lustre")
  |> result.unwrap("")
}
