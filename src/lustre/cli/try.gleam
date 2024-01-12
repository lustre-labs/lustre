// IMPORTS ---------------------------------------------------------------------

import gleam_community/ansi
import gleam/bool
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import simplifile
import tom

// TYPES -----------------------------------------------------------------------

type Options {
  /// It's important to remember that for Erlang, Gleam records have their field
  /// names erased and they degenerate to tuples. This means that the order of
  /// the fields is important!
  Options(name: String, host: String, port: Int, no_styles: Bool)
}

type Error {
  CompileError
}

// MAIN ------------------------------------------------------------------------

pub fn run() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(_, flags) = input
    let assert Ok(port) = flag.get_int(flags, "port")
    let assert Ok(host) = flag.get_string(flags, "host")
    let assert Ok(no_styles) = flag.get_bool(flags, "no-styles-styles")

    let result = {
      let stdout = exec("gleam build --target javascript")
      use <- bool.guard(string.contains(stdout, "error:"), Error(CompileError))
      // These are all safe to assert because the Gleam project wouldn't compile
      // if any of this stuff was invalid.
      let assert Ok(config) = simplifile.read("gleam.toml")
      let assert Ok(toml) = tom.parse(config)
      let assert Ok(name) = tom.get_string(toml, ["name"])
      let options = Options(name, host, port, no_styles)

      serve(options, on_start(host, _), on_port_taken)

      Ok(Nil)
    }

    case result {
      Ok(_) -> Nil
      Error(error) -> explain(error)
    }
  })
  |> glint.flag("host", host_flag())
  |> glint.flag("port", port_flag())
  |> glint.flag("no-styles", no_styles_flag())
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    CompileError -> Nil
  }
}

// GLINT FLAGS -----------------------------------------------------------------

fn host_flag() {
  flag.string()
  |> flag.default("localhost")
  |> flag.description("The host to run the server on.")
}

fn port_flag() {
  flag.int()
  |> flag.default(1234)
  |> flag.description("The port to run the server on.")
}

fn no_styles_flag() {
  flag.bool()
  |> flag.default(False)
  |> flag.description("When false, lustre/ui's styles will not be included.")
}

// UTILS -----------------------------------------------------------------------

fn on_start(host: String, port: Int) -> Nil {
  let address = "http://" <> host <> ":" <> int.to_string(port)
  io.println("✨ Server has been started at " <> ansi.bold(address))
}

fn on_port_taken(port) -> Nil {
  io.println(
    "🚨 Port "
    <> ansi.bold(int.to_string(port))
    <> " already in use, using next available port",
  )
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_try_ffi", "serve")
fn serve(
  options: Options,
  on_start: fn(Int) -> Nil,
  on_port_taken: fn(Int) -> Nil,
) -> Nil

@external(erlang, "lustre_try_ffi", "exec")
fn exec(command: String) -> String
