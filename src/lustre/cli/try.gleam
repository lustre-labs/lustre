// IMPORTS ---------------------------------------------------------------------

import gleam_community/ansi
import gleam/int
import gleam/io
import gleam/result
import glint.{type Command, CommandInput}
import glint/flag
import lustre/cli/project

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
    let CommandInput(_, flags, _) = input
    let assert Ok(port) = flag.get_int(flags, port_flag_name)
    let assert Ok(host) = flag.get_string(flags, host_flag_name)
    let assert Ok(no_styles) = flag.get_bool(flags, no_styles_flag_name)

    let result = {
      let compile = result.replace_error(project.build(), CompileError)
      use compiled <- result.try(compile)
      let configuration = project.read_configuration(compiled)
      let options = Options(configuration.name, host, port, no_styles)
      serve(options, on_start(host, _), on_port_taken)
      Ok(Nil)
    }

    case result {
      Ok(_) -> Nil
      Error(error) -> explain(error)
    }
  })
  |> glint.flag(host_flag_name, host_flag())
  |> glint.flag(port_flag_name, port_flag())
  |> glint.flag(no_styles_flag_name, no_styles_flag())
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    CompileError -> Nil
  }
}

// GLINT FLAGS -----------------------------------------------------------------

const host_flag_name = "host"

fn host_flag() {
  flag.string()
  |> flag.default("localhost")
  |> flag.description("The host to run the server on.")
}

const port_flag_name = "port"

fn port_flag() {
  flag.int()
  |> flag.default(1234)
  |> flag.description("The port to run the server on.")
}

const no_styles_flag_name = "no-styles"

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
