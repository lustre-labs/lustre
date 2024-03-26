// CONSTANTS -------------------------------------------------------------------
//
// These constants are used to identify different JSON payloads from the server
// component runtime. We do this because payloads are sent as arrays to cut down
// on the size of the payload. The first element of the array is always a tag
// that tells us how to interpret the rest of the array.

/// Represents the `Diff` variant of the `Patch` type.
///
pub const diff: Int = 0

/// Represents the `Emit` variant of the `Patch` type.
///
pub const emit: Int = 1

/// Represents the `Init` variant of the `Patch` type.
///
pub const init: Int = 2

/// Represents the `Event` variant of the `Action` type.
///
pub const event: Int = 4

/// Represents the `Attr` variant of the `Patch` type.
///
pub const attrs: Int = 5
