import lustre
import lustre/attribute.{attribute}
import lustre/element/html
import lustre/element/svg
import lustre/ui
import lustre/ui/icon

pub fn main() {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  lustre.element(ui.centre(
    [attribute.style(styles)],
    html.svg(
      [
        attribute("version", "1.1"),
        attribute("viewBox", "0 0 300 200"),
        attribute("width", "300"),
        attribute("height", "200"),
      ],
      [
        svg.rect([
          attribute("width", "100%"),
          attribute("height", "100%"),
          attribute("fill", "red"),
        ]),
        svg.circle([
          attribute("cx", "150"),
          attribute("cy", "100"),
          attribute("r", "80"),
          attribute("fill", "green"),
        ]),
      ],
    ),
  ))
}
