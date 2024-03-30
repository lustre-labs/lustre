// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/io
import gleam/regex.{Options}
import gleam/result
import gleam/string
import shellout
import simplifile

// MAIN ------------------------------------------------------------------------

pub fn main() {
  io.debug({
    use exists <- try(verify_esbuild(), SimplifileError)
    use <- bool.guard(!exists, Error(MissingEsbuild))

    use _ <- try(build_for_javascript(), ShelloutError)
    use _ <- try(bundle_server_component(), ShelloutError)
    use _ <- try(bundle_minified_server_component(), ShelloutError)

    use script <- try(read_script(), SimplifileError)
    use module <- try(read_module(), SimplifileError)
    use _ <- try(inject_script(script, module), SimplifileError)

    use _ <- try(format_project(), ShelloutError)

    Ok(Nil)
  })
}

// CONSTANTS -------------------------------------------------------------------

const esbuild = "./build/.lustre/bin/esbuild"

// STEPS -----------------------------------------------------------------------

fn verify_esbuild() {
  simplifile.verify_is_file(esbuild)
}

fn build_for_javascript() {
  shellout.command(
    run: "gleam",
    with: ["build", "--target", "javascript"],
    in: ".",
    opt: [],
  )
}

fn bundle_server_component() {
  shellout.command(
    run: esbuild,
    with: [
      "./src/server-component.mjs", "--bundle", "--format=esm",
      "--outfile=./priv/static/lustre-server-component.mjs",
    ],
    in: ".",
    opt: [],
  )
}

fn bundle_minified_server_component() {
  shellout.command(
    run: esbuild,
    with: [
      "./src/server-component.mjs", "--bundle", "--minify", "--format=esm",
      "--outfile=./priv/static/lustre-server-component.min.mjs",
    ],
    in: ".",
    opt: [],
  )
}

fn read_script() {
  simplifile.read("./priv/static/lustre-server-component.min.mjs")
  |> result.map(string.replace(_, "\"", "\\\""))
  |> result.map(string.trim)
}

fn read_module() {
  simplifile.read("./src/lustre/server_component.gleam")
}

fn inject_script(script, module) {
  let inject_regex = "// <<INJECT RUNTIME>>\\n.+\\n.+\\n    \\),"
  let options = Options(case_insensitive: False, multi_line: True)
  let assert Ok(re) = regex.compile(inject_regex, options)
  let assert [before, after] = regex.split(re, module)

  simplifile.write(
    "./src/lustre/server_component.gleam",
    before
      <> "// <<INJECT RUNTIME>>\n    element.text(\""
      <> script
      <> "\"),"
      <> after,
  )
}

fn format_project() {
  shellout.command(run: "gleam", with: ["format"], in: ".", opt: [])
}

// ERROR HANDLING --------------------------------------------------------------

pub type Error {
  MissingEsbuild
  ShelloutError(#(Int, String))
  SimplifileError(simplifile.FileError)
}

fn try(
  result: Result(a, e),
  to_error: fn(e) -> Error,
  then: fn(a) -> Result(b, Error),
) -> Result(b, Error) {
  case result {
    Ok(value) -> then(value)
    Error(error) -> Error(to_error(error))
  }
}
