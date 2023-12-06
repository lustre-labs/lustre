pub fn main() {
  serve(1234)
}

@external(erlang, "http_ffi", "serve")
@external(javascript, "../http.ffi.mjs", "serve")
fn serve(port: Int) -> Nil
