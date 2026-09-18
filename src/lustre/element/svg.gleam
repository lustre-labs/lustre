// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute}
import lustre/element.{type Element, namespaced, text as inline_text}

// CONSTANTS -------------------------------------------------------------------

/// The SVG namespace URI: `"http://www.w3.org/2000/svg"`. You might use this
/// with [`element.namespaced`](../element.html#namespaced) to create elements
/// in the SVG namespace not provided here.
///
pub const namespace = "http://www.w3.org/2000/svg"

// The doc comments (and order) for functions in this module are taken from the
// MDN Element reference:
//
//   https://developer.mozilla.org/en-US/docs/Web/SVG/Element
//

// SVG ELEMENTS: ANIMATION ELEMENTS --------------------------------------------

///
pub fn animate(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "animate", attrs, [])
}

///
pub fn animate_motion(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "animateMotion", attrs, [])
}

///
pub fn animate_transform(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "animateTransform", attrs, [])
}

///
pub fn mpath(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "mpath", attrs, [])
}

///
pub fn set(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "set", attrs, [])
}

// SVG ELEMENTS: BASIC SHAPES --------------------------------------------------

///
pub fn circle(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "circle", attrs, [])
}

///
pub fn ellipse(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "ellipse", attrs, [])
}

///
pub fn line(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "line", attrs, [])
}

///
pub fn polygon(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "polygon", attrs, [])
}

///
pub fn polyline(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "polyline", attrs, [])
}

///
pub fn rect(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "rect", attrs, [])
}

// SVG ELEMENTS: CONTAINER ELEMENTS --------------------------------------------

///
pub fn a(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "a", attrs, children)
}

///
pub fn defs(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "defs", attrs, children)
}

///
pub fn g(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "g", attrs, children)
}

///
pub fn marker(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "marker", attrs, children)
}

///
pub fn mask(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "mask", attrs, children)
}

///
pub fn missing_glyph(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "missing-glyph", attrs, children)
}

///
pub fn pattern(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "pattern", attrs, children)
}

///
pub fn svg(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "svg", attrs, children)
}

///
pub fn switch(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "switch", attrs, children)
}

///
pub fn symbol(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "symbol", attrs, children)
}

///
pub fn view(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "view", attrs, children)
}

// SVG ELEMENTS: DESCRIPTIVE ELEMENTS ------------------------------------------

///
pub fn desc(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "desc", attrs, children)
}

///
pub fn metadata(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "metadata", attrs, children)
}

///
pub fn title(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "title", attrs, children)
}

// SVG ELEMENTS: FILTER EFFECTS ------------------------------------------------

///
pub fn filter(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "filter", attrs, children)
}

///
pub fn fe_blend(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feBlend", attrs, [])
}

///
pub fn fe_color_matrix(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feColorMatrix", attrs, [])
}

///
pub fn fe_component_transfer(
  attrs: List(Attribute(message)),
) -> Element(message) {
  namespaced(namespace, "feComponentTransfer", attrs, [])
}

///
pub fn fe_composite(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feComposite", attrs, [])
}

///
pub fn fe_convolve_matrix(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feConvolveMatrix", attrs, [])
}

///
pub fn fe_diffuse_lighting(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "feDiffuseLighting", attrs, children)
}

///
pub fn fe_displacement_map(
  attrs: List(Attribute(message)),
) -> Element(message) {
  namespaced(namespace, "feDisplacementMap", attrs, [])
}

///
pub fn fe_drop_shadow(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feDropShadow", attrs, [])
}

///
pub fn fe_flood(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feFlood", attrs, [])
}

///
pub fn fe_func_a(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feFuncA", attrs, [])
}

///
pub fn fe_func_b(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feFuncB", attrs, [])
}

///
pub fn fe_func_g(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feFuncG", attrs, [])
}

///
pub fn fe_func_r(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feFuncR", attrs, [])
}

///
pub fn fe_gaussian_blur(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feGaussianBlur", attrs, [])
}

///
pub fn fe_image(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feImage", attrs, [])
}

///
pub fn fe_merge(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "feMerge", attrs, children)
}

///
pub fn fe_merge_node(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feMergeNode", attrs, [])
}

///
pub fn fe_morphology(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feMorphology", attrs, [])
}

///
pub fn fe_offset(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feOffset", attrs, [])
}

///
pub fn fe_specular_lighting(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "feSpecularLighting", attrs, children)
}

///
pub fn fe_tile(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "feTile", attrs, children)
}

///
pub fn fe_turbulence(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feTurbulence", attrs, [])
}

// SVG ELEMENTS: GRADIENT ELEMENTS ---------------------------------------------

///
pub fn linear_gradient(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "linearGradient", attrs, children)
}

///
pub fn radial_gradient(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "radialGradient", attrs, children)
}

///
pub fn stop(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "stop", attrs, [])
}

// SVG ELEMENTS: GRAPHICAL ELEMENTS --------------------------------------------

///
pub fn image(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "image", attrs, [])
}

///
pub fn path(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "path", attrs, [])
}

///
pub fn text(
  attrs: List(Attribute(message)),
  content: String,
) -> Element(message) {
  namespaced(namespace, "text", attrs, [element.text(content)])
}

///
pub fn use_(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "use", attrs, [])
}

// SVG ELEMENTS: LIGHTING ELEMENTS ---------------------------------------------

///
pub fn fe_distant_light(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feDistantLight", attrs, [])
}

///
pub fn fe_point_light(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "fePointLight", attrs, [])
}

///
pub fn fe_spot_light(attrs: List(Attribute(message))) -> Element(message) {
  namespaced(namespace, "feSpotLight", attrs, [])
}

// SVG ELEMENTS: NEVER-RENDERED ELEMENTS ---------------------------------------

///
pub fn clip_path(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "clipPath", attrs, children)
}

///
pub fn script(attrs: List(Attribute(message)), js: String) -> Element(message) {
  namespaced(namespace, "script", attrs, [inline_text(js)])
}

///
pub fn style(attrs: List(Attribute(message)), css: String) -> Element(message) {
  namespaced(namespace, "style", attrs, [inline_text(css)])
}

// SVG ELEMENTS: RENDERABLE ELEMENTS -------------------------------------------

///
pub fn foreign_object(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "foreignObject", attrs, children)
}

///
pub fn text_path(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "textPath", attrs, children)
}

///
pub fn tspan(
  attrs: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  namespaced(namespace, "tspan", attrs, children)
}
