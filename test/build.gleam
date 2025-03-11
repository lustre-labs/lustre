// IMPORTS ---------------------------------------------------------------------

import esgleam
import esgleam/mod/install
import gleam/io
import gleam/regexp.{Options}
import gleam/result
import gleam/string
import shellout
import simplifile

// MAIN ------------------------------------------------------------------------

pub fn main() {
  io.debug({
    case simplifile.is_file("build/dev/bin/package/bin/esbuild") {
      Ok(True) -> Nil
      _ -> install.fetch()
    }

    use _ <- try(build_for_javascript(), ShelloutError)
    use _ <- try(bundle_server_component(), SimplifileError)
    use _ <- try(bundle_minified_server_component(), SimplifileError)

    use script <- try(read_script(), SimplifileError)
    use module <- try(read_module(), SimplifileError)
    use _ <- try(inject_script(script, module), SimplifileError)

    use _ <- try(format_project(), ShelloutError)

    Ok(Nil)
  })
}

// CONSTANTS -------------------------------------------------------------------

// For whatever reason, esgleam needs the input path to be relative to the location
// of the esbuild binary
const runtime = "../../../../src/lustre/runtime/client/server_component.ffi.mjs"

const outfile = "./priv/static/lustre-server-component"

const module = "./src/lustre/server_component.gleam"

// STEPS -----------------------------------------------------------------------

fn build_for_javascript() {
  shellout.command(
    run: "gleam",
    with: ["build", "--target", "javascript"],
    in: ".",
    opt: [],
  )
}

fn bundle_server_component() {
  esgleam.new("")
  |> esgleam.entry(runtime)
  |> esgleam.raw("--outfile=" <> outfile <> ".mjs")
  |> esgleam.bundle
}

fn bundle_minified_server_component() {
  esgleam.new("")
  |> esgleam.entry(runtime)
  |> esgleam.minify(True)
  |> esgleam.raw("--outfile=" <> outfile <> ".min.mjs")
  |> esgleam.bundle
}

fn read_script() {
  simplifile.read(outfile <> ".min.mjs")
  |> result.map(string.replace(_, "\n", "\\n"))
  |> result.map(string.replace(_, "\\", "\\\\"))
  |> result.map(string.replace(_, "\"", "\\\""))
  |> result.map(string.trim)
}

fn read_module() {
  simplifile.read(module)
}

fn inject_script(script, src) {
  let inject_regex = "// <<INJECT RUNTIME>>\\n    .+,"
  let options = Options(case_insensitive: False, multi_line: True)
  let assert Ok(re) = regexp.compile(inject_regex, options)
  let assert [before, after] = regexp.split(re, src)

  simplifile.write(
    to: module,
    contents: before
      <> "// <<INJECT RUNTIME>>\n    \""
      <> script
      <> "\","
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
