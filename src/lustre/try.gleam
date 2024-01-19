// IMPORTS ---------------------------------------------------------------------

import argv
import gleam_community/ansi
import gleam/int
import gleam/io
import glint.{CommandInput}
import glint/flag

// TYPES -----------------------------------------------------------------------

type Options {
  /// It's important to remember that for Erlang, Gleam records have their field
  /// names erased and they degenerate to tuples. This means that the order of
  /// the fields is important!
  Options(host: String, port: Int, include_styles: Bool)
}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let args = argv.load().arguments
  let program =
    glint.new()
    // There's an open issue on the glint repo to have the generated help text
    // include the `gleam run -m ` prefix. If/until that's addressed, we can kind
    // of hack it by telling glint the name of the program is the full command.
    //
    // See: https://github.com/TanklesXL/glint/issues/23
    //
    |> glint.with_name("gleam run -m lustre/try")
    |> glint.with_pretty_help(glint.default_pretty_help())
    |> glint.add(
      at: [],
      do: glint.command(fn(input) {
        let CommandInput(_, flags) = input
        let assert Ok(port) = flag.get_int(flags, "port")
        let assert Ok(host) = flag.get_string(flags, "host")
        let assert Ok(include_styles) = flag.get_bool(flags, "include-styles")
        let options = Options(host, port, include_styles)

        exec("gleam build --target js")
        serve(options, on_start(host, _), on_port_taken)
      })
      |> glint.flag("host", host_flag())
      |> glint.flag("port", port_flag())
      |> glint.flag("include-styles", include_styles_flag()),
    )

  glint.run(program, args)
}

// GLINT FLAGS -----------------------------------------------------------------

fn host_flag() {
  flag.string()
  |> flag.default("localhost")
  |> flag.description("The host to run the server on")
}

fn port_flag() {
  flag.int()
  |> flag.default(1234)
  |> flag.description("The port to run the server on")
}

fn include_styles_flag() {
  flag.bool()
  |> flag.default(False)
  |> flag.description("Include lustre_ui's default stylesheet in your app.")
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
