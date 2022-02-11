import lustre/dom/html/attr.{Attr}

// TYPES -----------------------------------------------------------------------

pub external type Html

// GENERIC CONSTRUCTORS --------------------------------------------------------

external fn create_node (String, String, List(Attr), List(Html)) -> Html = "./html.ffi.mjs" "createNode"

external fn create_text (String) -> Html = 
    "./html.ffi.mjs" "createText"

pub fn node (tag: String, attrs: List(Attr), children: List(Html)) -> Html {
    create_node(tag, "", attrs, children)
}

pub fn node_ns (tag: String, namespace: String, attrs: List(Attr), children: List(Html)) -> Html {
    create_node(tag, namespace, attrs, children)
}

pub fn text (content: String) -> Html {
    create_text(content)
}

// COMMON CONSTRUCTORS ---------------------------------------------------------

pub fn div (attrs: List(Attr), children: List(Html)) -> Html {
    node("div", attrs, children)
}

pub fn p (attrs: List(Attr), children: List(Html)) -> Html {
    node("p", attrs, children)
}

pub fn span (attrs: List(Attr), children: List(Html)) -> Html {
    node("span", attrs, children)
}

pub fn button (attrs: List(Attr), children: List(Html)) -> Html {
    node("button", attrs, children)
}

// INPUT CONSTRUCTORS ----------------------------------------------------------

pub fn input (attrs: List(Attr)) -> Html {
    node("input", attrs, [])
}

// GRAPHICS CONSTRUCTORS -------------------------------------------------------

pub fn img (attrs: List(Attr)) -> Html {
    node("img", attrs, [])
}