import lustre
import lustre/attribute
import lustre/element
import lustre/element/html

pub fn main() {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]
  let app =
    lustre.element(
      html.div([], [
        html.h1([], [element.text("Hello, world.")]),
        html.h2([], [element.text("Welcome to Lustre.")]),
      ]),
    )
    
    
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
