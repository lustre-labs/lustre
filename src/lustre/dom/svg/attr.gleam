import gleam/float
import gleam/list
import gleam/string
import lustre/dom/html/attr

// TYPES -----------------------------------------------------------------------

pub type Attr = attr.Attr

// COMMON CONSTRUCTORS ---------------------------------------------------------

pub fn x (x: Float) -> Attr {
    attr.from_float("x", x)
}

pub fn x1 (x1: Float) -> Attr {
    attr.from_float("x1", x1)
}

pub fn x2 (x2: Float) -> Attr {
    attr.from_float("x2", x2)
}

pub fn y (y: Float) -> Attr {
    attr.from_float("y", y)
}

pub fn y1 (y1: Float) -> Attr {
    attr.from_float("y1", y1)
}

pub fn y2 (y2: Float) -> Attr {
    attr.from_float("y2", y2)
}

pub fn width (width: Float) -> Attr {
    attr.from_float("width", width)
}

pub fn height (height: Float) -> Attr {
    attr.from_float("height", height)
}

pub fn viewbox (min_x: Float, min_y: Float, width: Float, height: Float) -> Attr {
    attr.from_string("viewbox", [ min_x, min_y, width, height ] |> list.map(float.to_string) |> string.join(" "))
}

// STYLING CONSTRUCTORS --------------------------------------------------------

pub fn rgb (r: Float, g: Float, b: Float) -> String {
    rgba(r, g, b, 1.0)
}

pub fn rgba (r: Float, g: Float, b: Float, a: Float) -> String {
    let r = clamp(r, between: 0.0, and: 255.0)  |> float.to_string
    let g = clamp(g, between: 0.0, and: 255.0)  |> float.to_string
    let b = clamp(b, between: 0.0, and: 255.0)  |> float.to_string
    let a = clamp(a, between: 0.0, and: 1.0)    |> float.to_string

    string.concat([ "rgba(", r, ",", g, ",", b, ",", a, ")" ])
}

pub fn fill (color: String) -> Attr {
    attr.from_string("fill", color)
}

pub fn stroke (color: String) -> Attr {
    attr.from_string("stroke", color)
}

pub fn stroke_width (width: Float) -> Attr {
    attr.from_float("stroke-width", width)
}

pub fn opacity (opacity: Float) -> Attr {
    attr.from_float("opacity", clamp(opacity, between: 0.0, and: 1.0))
}

// UTILS -----------------------------------------------------------------------

fn clamp (val: Float, between min: Float, and max: Float) -> Float {
    case val {
        _ if val <. min ->
            min
        
        _else if val >. max ->
            max
        
        _else ->
            val
    }
}