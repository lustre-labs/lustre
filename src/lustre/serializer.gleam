import lustre/vdom/vnode.{type Element, type RawContent}

// IMPORTS ---------------------------------------------------------------------

// TYPES -----------------------------------------------------------------------

/// A serializer converts elements and raw content to strings.
/// Used by headless platforms for server-side rendering.
///
pub type Serializer(msg) {
  Serializer(
    element: fn(Element(msg)) -> String,
    raw_content: fn(RawContent) -> String,
  )
}
