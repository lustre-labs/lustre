// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/io
import gleam/option.{None}
import gleam/result
import gleam/list
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag.{type Flag}
import filepath
import simplifile
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
  CannotPerformCleanup(temp_file: String)
  CannotWriteTempFile(reason: simplifile.FileError, temp_file: String)
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

  let configuration = project.read_configuration(compiled)

  // Download the esbuild executable.
  use esbuild <- result.try(
    esbuild.download(None, None)
    |> result.map_error(CannotDownloadEsbuild),
  )

  let modules_to_bundle = case args {
    [] -> {
      // If no module was provided as a command line argument we just bundle
      // the component in the module with the same name as the project.
      [configuration.name]
    }
    [_, ..] ->
      // TODO: Here I should expand any * but I'll just skip that for now
      args
  }

  // Ensure that all modules we're going to bundle actually expose the correct
  // `register` function needed to register a component.
  use _ <- result.try(list.try_each(modules_to_bundle, check_can_be_bundled))

  // Figure out the outfile name based on the number of modules to bundle.
  // TODO: this could be configurable via flag.
  let output_file_name = case modules_to_bundle {
    // This is really unsatisfactory, but is it really worth it to add
    // `non_empty_list` as a dependency and the extra complexity?
    [] -> panic as "the modules are always going to be at least one"
    [module] -> {
      let component_name = string.replace(module, each: "/", with: "_")
      configuration.name <> "-" <> component_name
    }
    [_, ..] -> configuration.name <> "-components"
  }

  let priv = filepath.join(project.root_folder(), "priv")
  let output_file = join_all([priv, "components", output_file_name])
  let input_file = filepath.join(priv, "temp-bundle-input")
  let contents = todo as "I have to generate the actual script to bundle"

  use _ <- result.try(
    simplifile.write(contents, to: input_file)
    |> result.map_error(CannotWriteTempFile(_, input_file)),
  )

  let bundle_result =
    esbuild.bundle(esbuild, input_file, output_file, minify)
    |> result.replace_error(CannotBundleComponents)

  // Regardless of the result of the bundling process we delete the temporary
  // input file.
  case bundle_result {
    Ok(Nil) ->
      simplifile.delete(input_file)
      |> result.replace_error(CannotPerformCleanup(input_file))
    Error(error) -> {
      let _ = simplifile.delete(input_file)
      Error(error)
    }
  }
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

fn join_all(paths: List(String)) -> String {
  case paths {
    [] -> ""
    [path, ..rest] -> list.fold(over: rest, from: path, with: filepath.join)
  }
}

fn explain(error: Error) -> Nil {
  case error {
    _ -> todo as "explain all the errors"
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
