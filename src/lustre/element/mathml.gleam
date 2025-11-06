// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute}
import lustre/element.{type Element, namespaced}

// MATHML ELEMENTS: GROUPING ---------------------------------------------------

///
pub fn maction(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "maction", attrs, children)
}

///
pub fn math(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "math", attrs, children)
}

///
pub fn merror(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "merror", attrs, children)
}

///
pub fn mphantom(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mphantom", attrs, children)
}

///
pub fn mprescripts(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced(
    "http://www.w3.org/1998/Math/MathML",
    "mprescripts",
    attrs,
    children,
  )
}

///
pub fn mrow(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mrow", attrs, children)
}

///
pub fn mstyle(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mstyle", attrs, children)
}

///
pub fn semantics(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "semantics", attrs, children)
}

// MATHML ELEMENTS: SCRIPTED ---------------------------------------------------

///
pub fn mmultiscripts(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced(
    "http://www.w3.org/1998/Math/MathML",
    "mmultiscripts",
    attrs,
    children,
  )
}

///
pub fn mover(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mover", attrs, children)
}

///
pub fn msub(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "msub", attrs, children)
}

///
pub fn msubsup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "msubsup", attrs, children)
}

///
pub fn msup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "msup", attrs, children)
}

///
pub fn munder(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "munder", attrs, children)
}

///
pub fn munderover(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced(
    "http://www.w3.org/1998/Math/MathML",
    "munderover",
    attrs,
    children,
  )
}

// MATHML ELEMENTS: RADICAL ----------------------------------------------------

///
pub fn mroot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mroot", attrs, children)
}

///
pub fn msqrt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "msqrt", attrs, children)
}

// MATHML ELEMENTS: OTHER ------------------------------------------------------

///
pub fn annotation(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced(
    "http://www.w3.org/1998/Math/MathML",
    "annotation",
    attrs,
    children,
  )
}

///
pub fn annotation_xml(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced(
    "http://www.w3.org/1998/Math/MathML",
    "annotation-xml",
    attrs,
    children,
  )
}

///
pub fn mfrac(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mfrac", attrs, children)
}

///
pub fn mn(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mn", attrs, children)
}

///
pub fn mo(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mo", attrs, children)
}

///
pub fn mi(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mi", attrs, children)
}

///
pub fn mpadded(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mpadded", attrs, children)
}

///
pub fn ms(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "ms", attrs, children)
}

///
pub fn mspace(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mspace", attrs, children)
}

///
pub fn mtable(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mtable", attrs, children)
}

///
pub fn mtd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mtd", attrs, children)
}

///
pub fn mtext(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mtext", attrs, children)
}

///
pub fn mtr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  namespaced("http://www.w3.org/1998/Math/MathML", "mtr", attrs, children)
}
