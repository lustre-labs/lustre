import gleam/int
import gleam/io
import gleam_community/ansi

pub fn main() {
  let host = "localhost"
  let port = 1234

  let address = "http://" <> host <> ":" <> int.to_string(port)

  serve(host, port, fn() {
    io.println("✨ Server has been started at " <> ansi.bold(address))
  })
}

@external(erlang, "http_ffi", "serve")
@external(javascript, "../http.ffi.mjs", "serve")
fn serve(host: String, port: Int, on_start: fn() -> Nil) -> Nil
