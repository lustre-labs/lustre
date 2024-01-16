// CONSTANTS -------------------------------------------------------------------
//
// These constants are used to identify different JSON payloads from the server
// component runtime. We do this because payloads are sent as arrays to cut down
// on the size of the payload. The first element of the array is always a tag
// that tells us how to interpret the rest of the array.

pub const diff: Int = 0

pub const emit: Int = 1

pub const morph: Int = 2
