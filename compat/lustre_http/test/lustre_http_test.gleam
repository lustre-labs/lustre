import lustre_http as http
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub type ToMsgProvider {
  Wrapper(http.StringResult)
}

// We cannot run the resulting lustre.Cmd, but we can at least ensure it can be used/compiled this way
pub fn compilation_test() {
  http.get_as_text("http://localhost:8080/not_here", Wrapper)
}
