// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/io
import gleam/option.{None}
import gleam/result
import gleam/list
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag.{type Flag}
import lustre/cli/project
import lustre/cli/esbuild

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
  CannotDownloadEsbuild(error: esbuild.Error)
  CannotBundleComponents
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
  |> glint.flag(minify_flag_name, minify_flag())
}

// BUILD COMPONENT -------------------------------------------------------------

fn build_component(
  args: List(String),
  flags: Dict(String, Flag),
) -> Result(Nil, Error) {
  let assert Ok(minify) = flag.get_bool(flags, minify_flag_name)

  // Build the project to make sure it doesn't have any compile errors.
  let compile = result.replace_error(project.build(), CompileError)
  use compiled <- result.try(compile)

  // Download the esbuild executable.
  use esbuild <- result.try(
    esbuild.download(None, None)
    |> result.map_error(CannotDownloadEsbuild),
  )

  let modules_to_bundle = case args {
    [] -> {
      // If no module was provided as a command line argument we just bundle
      // the component in the module with the same name as the project.
      let configuration = project.read_configuration(compiled)
      [configuration.name]
    }
    [_, ..] ->
      // TODO: Here I should expand any * but I'll just skip that for now
      args
  }

  // Ensure that all modules we're going to bundle actually expose the correct
  // `register` function needed to register a component.
  use _ <- result.try(list.try_each(modules_to_bundle, check_can_be_bundled))

  // Now we're good and can bundle things together
  // TODO: how should the script provided to esbuild look?
  //       From what I can gather I need to have a single `app.jsx` (?) file
  //       where I import all those functions and then call esbuild and let it
  //       do its magic `esbuild app.jsx --bundle --minify --outfile=...`
  esbuild.bundle(esbuild, minify)
  |> result.replace_error(CannotBundleComponents)
}

fn check_can_be_bundled(_module_name: String) -> Result(Nil, Error) {
  // let package_interface = todo
  // let find_module =
  //   dict.get(package_interface.modules, module_name)
  //   |> result.replace_error(MissingModule(module_name))
  // use module <- result.try(find_module)
  // let find_register_function =
  //   dict.get(module.functions, "register")
  //   |> result.replace_error(MissingRegisterFunction(module_name))
  // use register_function <- result.try(find_register_function)
  // check_returns_component(register_function)

  // ^^^ We will be able to do all the above once the compiler exposes the
  //     package interface, for now we just assume everything's ok.
  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    CannotBundleComponents(..) -> todo
    CannotDownloadEsbuild(..) -> todo
    CompileError -> todo
    MissingModule(..) -> todo
    MissingRegisterFunction(..) -> todo
    RegisterFunctionWithWrongType(..) -> todo
    MissingComponentPath ->
      "🚨 Missing component argument. I need to know what module to build! "
  }
  |> string.pad_right(78, ".")
  |> string.append(" ❌")
  |> io.println
}

// GLINT FLAGS -----------------------------------------------------------------

const minify_flag_name = "minify"

fn minify_flag() -> flag.FlagBuilder(Bool) {
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
