import gleam/list

// ERLANG VERSION --------------------------------------------------------------

@target(erlang)
import gleam/bit_array

@target(erlang)
pub fn escape(text: String) -> String {
  // This version is highly optimised for the Erlang target, it treats Strings
  // as BitArrays and slices them to share as much as possible. You can find
  // more details in `do_escape`.
  let bits = <<text:utf8>>
  let acc = do_escape(bits, 0, bits, [])

  list.reverse(acc)
  |> bit_array.concat
  |> coerce
}

@target(erlang)
@external(erlang, "lustre_escape_ffi", "coerce")
fn coerce(bit_array: BitArray) -> String

// A possible way to escape chars would be to split the string into graphemes,
// traverse those one by one and accumulate them back into a string escaping
// ">", "<", etc. as we see them.
//
// However, we can be a lot more performant by working directly on the
// `BitArray` representing a Gleam UTF-8 String.
// This means that, instead of popping a grapheme at a time, we can work
// directly on BitArray slices: this has the big advantage of making sure we
// share as much as possible with the original string without having to build
// a new one from scratch.
//
@target(erlang)
fn do_escape(
  bin: BitArray,
  skip: Int,
  original: BitArray,
  acc: List(BitArray),
) -> List(BitArray) {
  case bin {
    // If we find a char to escape we just advance the `skip` counter so that
    // it will be ignored in the following slice, then we append the escaped
    // version to the accumulator.
    <<"<":utf8, rest:bits>> -> {
      let acc = [<<"&lt;":utf8>>, ..acc]
      do_escape(rest, skip + 1, original, acc)
    }

    <<">":utf8, rest:bits>> -> {
      let acc = [<<"&gt;":utf8>>, ..acc]
      do_escape(rest, skip + 1, original, acc)
    }

    <<"&":utf8, rest:bits>> -> {
      let acc = [<<"&amp;":utf8>>, ..acc]
      do_escape(rest, skip + 1, original, acc)
    }

    <<"\"":utf8, rest:bits>> -> {
      let acc = [<<"&quot;":utf8>>, ..acc]
      do_escape(rest, skip + 1, original, acc)
    }

    <<"'":utf8, rest:bits>> -> {
      let acc = [<<"&#39;":utf8>>, ..acc]
      do_escape(rest, skip + 1, original, acc)
    }

    // For any other bit that doesn't need to be escaped we go into an inner
    // loop, consuming as much "non-escapable" chars as possible.
    <<_char, rest:bits>> -> do_escape_normal(rest, skip, original, acc, 1)

    <<>> -> acc

    _ -> panic as "non byte aligned string, all strings should be byte aligned"
  }
}

@target(erlang)
fn do_escape_normal(
  bin: BitArray,
  skip: Int,
  original: BitArray,
  acc: List(BitArray),
  len: Int,
) -> List(BitArray) {
  // Remember, if we're here it means we've found a char that doesn't need to be
  // escaped, so what we want to do is advance the `len` counter until we reach
  // a char that _does_ need to be escaped and take the slice going from
  // `skip` with size `len`.
  //
  // Imagine we're escaping this string: "abc<def&ghi" and we've reached 'd':
  // ```
  //    abc<def&ghi
  //       ^ `skip` points here
  // ```
  // We're going to be increasing `len` until we reach the '&':
  // ```
  //    abc<def&ghi
  //        ^^^ len will be 3 when we reach the '&' that needs escaping
  // ```
  // So we take the slice corresponding to "def".
  //
  case bin {
    // If we reach a char that has to be escaped we append the slice starting
    // from `skip` with size `len` and the escaped char.
    // This is what allows us to share as much of the original string as
    // possible: we only allocate a new BitArray for the escaped chars,
    // everything else is just a slice of the original String.
    <<"<":utf8, rest:bits>> -> {
      let assert Ok(slice) = bit_array.slice(original, skip, len)
      let acc = [<<"&lt;":utf8>>, slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<">":utf8, rest:bits>> -> {
      let assert Ok(slice) = bit_array.slice(original, skip, len)
      let acc = [<<"&gt;":utf8>>, slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"&":utf8, rest:bits>> -> {
      let assert Ok(slice) = bit_array.slice(original, skip, len)
      let acc = [<<"&amp;":utf8>>, slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"\"":utf8, rest:bits>> -> {
      let assert Ok(slice) = bit_array.slice(original, skip, len)
      let acc = [<<"&quot;":utf8>>, slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"'":utf8, rest:bits>> -> {
      let assert Ok(slice) = bit_array.slice(original, skip, len)
      let acc = [<<"&#39;":utf8>>, slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc)
    }

    // If a char doesn't need escaping we keep increasing the length of the
    // slice we're going to take.
    <<_char, rest:bits>> -> do_escape_normal(rest, skip, original, acc, len + 1)

    <<>> ->
      case skip {
        0 -> [original]
        _ -> {
          let assert Ok(slice) = bit_array.slice(original, skip, len)
          [slice, ..acc]
        }
      }

    _ -> panic as "non byte aligned string, all strings should be byte aligned"
  }
}

// JAVASCRIPT VERSION ----------------------------------------------------------

@target(javascript)
import gleam/string

@target(javascript)
pub fn escape(text: String) -> String {
  do_escape(text, 0, text, [], 0, False)
  |> list.reverse
  |> string.join(with: "")
}

// The logic behind this function is exactly the same as the erlang one: we
// iterate the string byte by byte and only ever take slices of it (constant
// time operation that ensures maximum sharing). However, this implementation is
// a little more convoluted since we cannot define it as two mutually recursive
// functions as we did with the Erlang one (or it won't be tail call optimised
// on the JS target).
//
@target(javascript)
fn do_escape(
  string: String,
  skip: Int,
  original: String,
  acc: List(String),
  len: Int,
  found_normal: Bool,
) -> List(String) {
  case found_normal, first(string) {
    False, "<" -> {
      let rest = drop_first(string)
      let acc = ["&lt;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, ">" -> {
      let rest = drop_first(string)
      let acc = ["&gt;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "&" -> {
      let rest = drop_first(string)
      let acc = ["&amp;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "\"" -> {
      let rest = drop_first(string)
      let acc = ["&quot;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "'" -> {
      let rest = drop_first(string)
      let acc = ["&#39;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "" -> acc

    // For any other bit that doesn't need to be escaped we go into an inner
    // loop, consuming as much "non-escapable" chars as possible.
    False, _ -> {
      let rest = drop_first(string)
      do_escape(rest, skip, original, acc, 1, True)
    }

    True, "<" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&lt;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, ">" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&gt;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "&" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&amp;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "\"" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&quot;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "'" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&#39;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "" ->
      case skip {
        0 -> [original]
        _ -> {
          let slice = slice(original, skip, len)
          [slice, ..acc]
        }
      }

    // If a char doesn't need escaping we keep increasing the length of the
    // slice we're going to take.
    True, _ -> {
      let rest = drop_first(string)
      do_escape(rest, skip, original, acc, len + 1, True)
    }
  }
}

@target(javascript)
@external(javascript, "../../lustre-escape.ffi.mjs", "first")
fn first(_string: String) -> String

@target(javascript)
@external(javascript, "../../lustre-escape.ffi.mjs", "drop_first")
fn drop_first(_string: String) -> String

@target(javascript)
@external(javascript, "../../lustre-escape.ffi.mjs", "slice")
fn slice(_string: String, _from: Int, _to: Int) -> String
