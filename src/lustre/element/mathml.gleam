// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute}
import lustre/element.{type Element, namespaced}

// CONSTANTS -------------------------------------------------------------------

/// The MathML namespace URI: `"http://www.w3.org/1998/Math/MathML"`. You might use this
/// with [`element.namespaced`](../element.html#namespaced) to create elements
/// in the MathML namespace not provided here.
///
pub const namespace = "http://www.w3.org/1998/Math/MathML"

// The doc comments (and order) for functions in this module are taken from the
// MathML Core W3C technical report:
//
//   https://www.w3.org/TR/mathml-core/#mathml-elements-and-attributes
//

// MATHML ELEMENTS: GROUPING ---------------------------------------------------

///
pub fn merror(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "merror", attrs, children)
}

///
pub fn mphantom(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mphantom", attrs, children)
}

///
pub fn mprescripts(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mprescripts", attrs, children)
}

///
pub fn mrow(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mrow", attrs, children)
}

///
pub fn mstyle(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mstyle", attrs, children)
}

///
pub fn semantics(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "semantics", attrs, children)
}

// MATHML ELEMENTS: SCRIPTED ---------------------------------------------------

///
pub fn mmultiscripts(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mmultiscripts", attrs, children)
}

///
pub fn mover(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mover", attrs, children)
}

///
pub fn msub(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "msub", attrs, children)
}

///
pub fn msubsup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "msubsup", attrs, children)
}

///
pub fn msup(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "msup", attrs, children)
}

///
pub fn munder(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "munder", attrs, children)
}

///
pub fn munderover(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "munderover", attrs, children)
}

// MATHML ELEMENTS: RADICAL ----------------------------------------------------

///
pub fn mroot(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mroot", attrs, children)
}

///
pub fn msqrt(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "msqrt", attrs, children)
}

// MATHML ELEMENTS: OTHER ------------------------------------------------------

///
pub fn annotation(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "annotation", attrs, children)
}

///
pub fn annotation_xml(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "annotation-xml", attrs, children)
}

///
pub fn mfrac(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mfrac", attrs, children)
}

///
pub fn mn(attrs: List(Attribute(message)), text: String) -> Element(message) {
  namespaced(namespace, "mn", attrs, [element.text(text)])
}

///
pub fn mo(attrs: List(Attribute(message)), text: String) -> Element(message) {
  namespaced(namespace, "mo", attrs, [element.text(text)])
}

///
pub fn mi(attrs: List(Attribute(message)), text: String) -> Element(message) {
  namespaced(namespace, "mi", attrs, [element.text(text)])
}

///
pub fn mpadded(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mpadded", attrs, children)
}

///
pub fn ms(attrs: List(Attribute(message)), text: String) -> Element(message) {
  namespaced(namespace, "ms", attrs, [element.text(text)])
}

///
pub fn mspace(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "mspace", attrs, [])
}

///
pub fn mtable(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mtable", attrs, children)
}

///
pub fn mtd(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mtd", attrs, children)
}

///
pub fn mtext(attrs: List(Attribute(message)), text: String) -> Element(message) {
  namespaced(namespace, "mtext", attrs, [element.text(text)])
}

///
pub fn mtr(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mtr", attrs, children)
}
