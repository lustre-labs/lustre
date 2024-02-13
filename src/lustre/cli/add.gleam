// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/option
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre/cli/esbuild

// MAIN ------------------------------------------------------------------------

pub fn esbuild() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(args: _, flags: flags, named_args: _) = input
    let os = option.from_result(flag.get_string(flags, os_flag_name))
    let cpu = option.from_result(flag.get_string(flags, cpu_flag_name))

    esbuild.download(os, cpu)
    |> result.map_error(explain)
    |> result.replace(Nil)
    |> result.unwrap_both
  })
  |> glint.flag(os_flag_name, os_flag())
  |> glint.flag(cpu_flag_name, cpu_flag())
}

// GLINT FLAGS -----------------------------------------------------------------

const os_flag_name = "os"

fn os_flag() {
  flag.string()
  |> flag.description("The host to run the server on")
}

const cpu_flag_name = "cpu"

fn cpu_flag() {
  flag.string()
  |> flag.description("The port to run the server on")
}

// UTILS -----------------------------------------------------------------------

fn explain(error: esbuild.Error) -> Nil {
  case error {
    esbuild.NetworkError(_) ->
      "🚨 A network error occured. Check your connection and try again "
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    esbuild.SimplifileError(_error, path) ->
      "🚨 An unknown error occured while writing the executable to `{path}` "
      |> string.replace(each: "{path}", with: path)
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    esbuild.UnknownPlatform(os, cpu) ->
      { "🚨 Could not locate download url for " <> os <> "/" <> cpu <> " " }
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    esbuild.UnzipError(_error) ->
      "🚨 An unknown error occured while extracting the archive "
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
  }
  |> io.println
}
