import lustre
import lustre/attribute
import lustre/element
import lustre/element/html
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-example-styles` flag:
//
//   $ gleam run -m lustre/dev start --use-example-styles
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui

pub fn main() {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  lustre.element(ui.centre(
    [attribute.style(styles)],
    html.div([], [
      html.h1([], [element.text("Hello, world.")]),
      html.h2([], [element.text("Welcome to Lustre.")]),
    ]),
  ))
}
