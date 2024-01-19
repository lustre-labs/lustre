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
import lustre/cli/add
import lustre/cli/project

// TYPES -----------------------------------------------------------------------

type Error {
  MissingComponentPath
  MissingModule(module: String)
  /// In case a module is provided that doesn't expose a register function.
  MissingRegisterFunction(module: String)
  /// In case a module exposes a register function, but with an unexpected type.
  /// TODO: for now the type is just a string, later it's going to be a type
  ///       provided by the package interface decoder.
  RegisterFunctionWithWrongType(type_: String)
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

      ["app", ..rest] -> todo

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
  // Build the project to make sure it doesn't have any compile errors
  let compile = result.replace_error(project.build(), CompileError)
  use compiled <- result.try(compile)

  // Download esbuild (TODO: make sure this doesn't result in an error if
  // esbuild already exists)
  use _ <- result.try(result.map_error(add.download_esbuild(dict.new()), todo))

  let modules_to_bundle = case args {
    [] -> [project.read_configuration(compiled).name]
    [component, ..components] -> todo as "here are the modules expand wildcards"
  }

  // Ensure that all modules we're going to bundle actually expose the correct
  // `register` function needed to register a component.
  use _ <- result.try(list.try_each(modules_to_bundle, exposes_register))

  // Now we're good and can bundle things together
  // TODO: how should the script provided to esbuild look?
  todo
}

fn exposes_register(module: String) -> Result(Nil, Error) {
  // We will be able to do this once the compiler exposes the package interface
  // for now we just assume everything's ok
  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    CompileError -> todo
    MissingModule(..) -> todo
    MissingRegisterFunction(..) -> todo
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
