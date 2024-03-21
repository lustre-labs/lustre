// IMPORTS ---------------------------------------------------------------------

import glint.{type Command, CommandInput}
import glint/flag
import lustre/cli/esbuild
import lustre/cli/step
import lustre/cli/tailwind

// COMMANDS --------------------------------------------------------------------

pub fn esbuild() -> Command(Nil) {
  let description =
    "
Download a platform-appropriate version of the esbuild binary. Lustre uses this
to bundle applications and act as a development server.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(os) = flag.get_string(flags, "os")
    let assert Ok(cpu) = flag.get_string(flags, "cpu")
    let script = esbuild.download(os, cpu)
    case step.execute(script) {
      Ok(_) -> Nil
      Error(error) -> esbuild.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("os", {
    let description = ""
    let default = get_os()

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("cpu", {
    let description = ""
    let default = get_cpu()

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
}

pub fn tailwind() -> Command(Nil) {
  let description =
    "
Download a platform-appropriate version of the Tailwind binary.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(os) = flag.get_string(flags, "os")
    let assert Ok(cpu) = flag.get_string(flags, "cpu")

    let script = tailwind.setup(os, cpu)
    case step.execute(script) {
      Ok(_) -> Nil
      Error(error) -> tailwind.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("os", {
    let description = ""
    let default = get_os()

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("cpu", {
    let description = ""
    let default = get_cpu()

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "cli_ffi", "get_os")
fn get_os() -> String

@external(erlang, "cli_ffi", "get_cpu")
fn get_cpu() -> String
