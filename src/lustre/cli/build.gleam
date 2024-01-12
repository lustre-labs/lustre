// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/result
import gleam/list
import gleam/set
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag.{type Flag}
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}
import tom
import justin

// TYPES -----------------------------------------------------------------------

type Error {
  MissingComponentPath
  MissingModule
  CompileError
}

// MAIN ------------------------------------------------------------------------

pub fn run() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(args, flags) = input

    case args {
      [] -> todo
      ["component", ..rest] ->
        build_component(rest, flags)
        |> result.map_error(explain)
        |> result.unwrap(Nil)
      _ -> todo
    }
  })
  |> glint.flag("minify", minify_flag())
}

// BUILD COMPONENT -------------------------------------------------------------

fn build_component(
  args: List(String),
  flags: Dict(String, Flag),
) -> Result(Nil, Error) {
  let stdout = exec("gleam build --target javascript")
  use <- bool.guard(string.contains(stdout, "error:"), Error(CompileError))

  todo
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    MissingComponentPath ->
      "🚨 Missing component argument. I need to know what module to build! "
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
  }
  |> io.println
}

// GLINT FLAGS -----------------------------------------------------------------

fn minify_flag() {
  flag.bool()
  |> flag.default(False)
  |> flag.description(string.join(
    [
      "A minified bundle renames variables to shorter names and obfuscates the code.",
      "Minified bundles are always emitted with the `.min.mjs` extension.",
    ],
    " ",
  ))
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_build_ffi", "exec")
fn exec(command: String) -> String
