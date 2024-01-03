import gleam/int
import gleam/io
import gleam_community/ansi

pub fn main() {
  let host = "localhost"
  let port = 1234

  let on_start = fn(actual_port) {
    let address = "http://" <> host <> ":" <> int.to_string(actual_port)
    io.println("✨ Server has been started at " <> ansi.bold(address))
  }

  let on_port_taken = fn(taken_port) {
    io.println(
      "🚨 Port "
      <> ansi.bold(int.to_string(taken_port))
      <> " already in use, using next available port",
    )
  }

  serve(host, port, on_start, on_port_taken)
}

@external(erlang, "http_ffi", "serve")
@external(javascript, "../http.ffi.mjs", "serve")
fn serve(
  host: String,
  port: Int,
  on_start: fn(Int) -> Nil,
  on_port_taken: fn(Int) -> Nil,
) -> Nil
