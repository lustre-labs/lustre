import lustre/dom/html.{Html}
import lustre/dom/html/attr.{Attr}

// CONSTANTS -------------------------------------------------------------------

const svg_namespace = "http://www.w3.org/2000/svg"

// COMMON CONSTRUCTORS ---------------------------------------------------------

pub fn svg (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("svg", svg_namespace, attrs, children)
}

pub fn rect (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("rect", svg_namespace, attrs, children)
}

pub fn circle (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("circle", svg_namespace, attrs, children)
}

pub fn ellipse (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("ellipse", svg_namespace, attrs, children)
}

pub fn line (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("line", svg_namespace, attrs, children)
}

pub fn text (attrs: List(Attr), children: List(Html)) -> Html {
    html.node_ns("text", svg_namespace, attrs, children)
}
