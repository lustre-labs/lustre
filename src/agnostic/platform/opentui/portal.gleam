//// A portal changes the physical placement of its children in the renderable
//// tree, while keeping them logically inside your Lustre app. This makes it
//// possible to implement things like modals and overlays that need to be
//// rendered at a different position in the terminal layout.
////

// IMPORTS ---------------------------------------------------------------------

import agnostic/attribute.{type Attribute}
import agnostic/element.{type Element}
import agnostic/event
import gleam/dynamic/decode.{type Decoder}

// CONSTANTS -------------------------------------------------------------------

/// The tag name of the portal element.
///
pub const name = "opentui-portal"

// TYPES -----------------------------------------------------------------------

/// It's possible for the portal to fail when teleporting its children for a
/// number of reasons. If that happens, the element will emit an `"error"` event
/// with details on what went wrong.
///
pub type Error {
  /// The portal's `"target"` attribute was missing or empty.
  ///
  MissingTarget

  /// No element could be found matching the portal's `"target"` attribute.
  ///
  TargetNotFound(id: String)

  /// The portal's `"target"` attribute points to another portal element.
  /// This could lead to elements being teleported back and forth between
  /// portals, causing unexpected behaviour.
  ///
  TargetIsPortal(id: String)
}

@internal
pub const missing_target_tag = "missing-target"

@internal
pub const target_not_found_tag = "target-not-found"

@internal
pub const target_is_portal_tag = "target-is-portal"

// ATTRIBUTES ------------------------------------------------------------------

/// Set the target element ID for the portal. This is used internally by `to`.
///
fn target(id: String) -> Attribute(msg) {
  attribute.attribute("target", id)
}

/// A portal could fail for a number of reasons. When it does, it will emit an
/// `"error"` event with some information on what went wrong. You might use this
/// event listener to log errors or recover your UI if something important could
/// not be teleported.
///
pub fn on_error(handler: fn(Error) -> msg) -> Attribute(msg) {
  let handle_error =
    decode.at(["detail"], error_decoder())
    |> decode.map(handler)

  event.on("error", handle_error)
}

/// Decode the `detail` of a portal's `"error"` event. You might use this
/// decoder if you're writing your own event handler instead of the
/// [provided one](#on_error).
///
pub fn error_decoder() -> Decoder(Error) {
  use tag <- decode.field("tag", decode.string)
  use id <- decode.field("id", decode.string)

  case tag {
    _ if tag == missing_target_tag -> decode.success(MissingTarget)

    _ if tag == target_not_found_tag -> decode.success(TargetNotFound(id:))

    _ if tag == target_is_portal_tag -> decode.success(TargetIsPortal(id:))

    _ -> decode.failure(MissingTarget, "portal.Error")
  }
}

// ELEMENTS --------------------------------------------------------------------

/// Render a portal, teleporting all children to another element in the
/// renderable tree. The target must be the `id` of an existing element.
///
pub fn to(
  target id: String,
  with attributes: List(Attribute(msg)),
  teleport children: List(Element(msg)),
) -> Element(msg) {
  element.element(name, [target(id), ..attributes], children)
}

/// Render a portal, teleporting all children to the root of the renderable
/// tree. This is useful for modals and overlays that should sit above all
/// other content.
///
pub fn to_root(
  with attributes: List(Attribute(msg)),
  teleport children: List(Element(msg)),
) -> Element(msg) {
  element.element(
    name,
    [attribute.attribute("use-root", "true"), ..attributes],
    children,
  )
}
