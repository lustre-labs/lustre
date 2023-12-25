// IMPORTS ---------------------------------------------------------------------

import demo/socket
import demo/web
import gleam/erlang/process
import mist

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) =
    mist.new(fn(req) {
      case req.path {
        "/ws" -> socket.handle(req)
        _ -> web.handle(req)
      }
    })
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}
