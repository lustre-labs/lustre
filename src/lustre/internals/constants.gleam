//// Gleam's runtime representation of types in JavaScript is not quite clever
//// enough to notice that empty containers or nullary constructors can be
//// initialised once and reused every time they are needed.
////
//// This might sound like a micro-optimisation, but things like empty lists and
//// dicts are *incredibly* common just in Lustre's internals, so having constant
//// references to them makes a huge difference over the lifetime of a program.
////

// CONSTRUCTORS ----------------------------------------------------------------

pub const empty_list = []

pub const error_nil = Error(Nil)
