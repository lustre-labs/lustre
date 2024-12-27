import counter
import lustre
import lustre/element

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) = counter.register("my-counter")
  let app = lustre.element(element.element("my-counter", [], []))

  let assert Ok(_) = lustre.start(app, "#app", Nil)
}
