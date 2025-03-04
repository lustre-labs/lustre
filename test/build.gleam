// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/io
import gleam/regexp.{Options}
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

const esbuild = "./build/dev/bin/esbuild"

const runtime = "./src/lustre/runtime/client/server_component.ffi.mjs"

const outfile = "./priv/static/lustre-server-component"

const module = "./src/lustre/server_component.gleam"

// STEPS -----------------------------------------------------------------------

fn verify_esbuild() {
  simplifile.is_file(esbuild)
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
      runtime,
      "--bundle",
      "--format=esm",
      "--outfile=" <> outfile <> ".mjs",
    ],
    in: ".",
    opt: [],
  )
}

fn bundle_minified_server_component() {
  shellout.command(
    run: esbuild,
    with: [
      runtime,
      "--bundle",
      "--minify",
      "--format=esm",
      "--outfile=" <> outfile <> ".min.mjs",
    ],
    in: ".",
    opt: [],
  )
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
  let inject_regex = "// <<INJECT RUNTIME>>\\n.+\\n.+\\n    \\),"
  let options = Options(case_insensitive: False, multi_line: True)
  let assert Ok(re) = regexp.compile(inject_regex, options)
  let assert [before, after] = regexp.split(re, src)

  simplifile.write(
    to: module,
    contents: before
      <> "// <<INJECT RUNTIME>>\n    element.text(\""
      <> script
      <> "\"),"
      <> after,
  )
  |> io.debug
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
