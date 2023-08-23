import gleeunit
import lustre_websocket as ws

pub fn main() {
  gleeunit.main()
}

pub type Wrapper {
  Wrapper(ws.WebSocketEvent)
}

pub fn rather_thin_compilation_test() {
  let _cmd = ws.init("/blah", Wrapper)
  // We cannot run the resulting lustre.Cmd, but we can at least ensure it can be used/compiled this way
}
