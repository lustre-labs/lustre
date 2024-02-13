// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/io
import gleam/option.{type Option, None}
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
  CannotBundleComponents(error: esbuild.Error)
  CannotPerformCleanup(temp_file: String)
  CannotWriteTempFile(reason: simplifile.FileError, temp_file: String)
}

// MAIN ------------------------------------------------------------------------

pub fn component() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(args: args, flags: flags, named_args: _) = input
    let assert Ok(minify) = flag.get_bool(flags, minify_flag_name)

    build_component(args, minify)
    |> result.map_error(explain)
    |> result.unwrap(Nil)
  })
  |> glint.flag(minify_flag_name, minify_flag())
  |> glint.count_args(glint.MinArgs(1))
}

pub fn app() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(args: _, flags: flags, named_args: named_args) = input
    let assert Ok(minify) = flag.get_bool(flags, minify_flag_name)
    let module = option.from_result(dict.get(named_args, module_named_arg))

    build_app(module, minify)
    |> result.map_error(explain)
    |> result.unwrap(Nil)
  })
  |> glint.flag(minify_flag_name, minify_flag())
  |> glint.named_args([module_named_arg])
}

// GLINT FLAGS -----------------------------------------------------------------

const module_named_arg = "module"

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

// BUILD COMPONENT -------------------------------------------------------------

fn build_component(modules: List(String), minify: Bool) -> Result(Nil, Error) {
  // Build the project to make sure it doesn't have any compile errors.
  let compile = result.replace_error(project.build(), CompileError)
  use compiled <- result.try(compile)
  let configuration = project.read_configuration(compiled)

  // Ensure that all modules we're going to bundle actually expose the correct
  // `register` function needed to register a component.
  use _ <- result.try(list.try_each(modules, check_can_be_bundled))

  // Figure out the outfile name based on the number of modules to bundle.
  let output_file_name = case modules {
    [module] -> {
      let component_name = string.replace(module, each: "/", with: "_")
      configuration.name <> "-" <> component_name
    }
    _ -> configuration.name <> "-components"
  }

  let priv = filepath.join(project.root_folder(), "priv")

  components_script(configuration.name, modules)
  |> bundle_script(minify, in: priv, named: output_file_name)
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

/// Generates the script that will be bundled, exposing the `register` function
/// of each one of the provided modules.
///
fn components_script(project_name: String, modules: List(String)) -> String {
  use script, module <- list.fold(over: modules, from: "")
  let module_path =
    "../build/dev/javascript/" <> project_name <> "/" <> module <> ".mjs"

  let alias = "register-" <> string.replace(each: "\\", with: "-", in: module)
  let export =
    "export { register as " <> alias <> " } from \"" <> module_path <> "\""
  let register = alias <> "();"
  script <> "\n" <> export <> "\n" <> register
}

// BUILD APP -------------------------------------------------------------------

fn build_app(module: Option(String), minify: Bool) -> Result(Nil, Error) {
  // Build the project to make sure it doesn't have any compile errors.
  let compile = result.replace_error(project.build(), CompileError)
  use compiled <- result.try(compile)
  let configuration = project.read_configuration(compiled)

  // If the module is missing we use the module with the same name as the
  // project as a fallback.
  let module = option.lazy_unwrap(module, fn() { configuration.name })
  use _ <- result.try(check_app_can_be_bundled(module))

  let priv = filepath.join(project.root_folder(), "priv")

  app_script(configuration.name, module)
  |> bundle_script(minify, in: priv, named: module <> "-app")
}

fn check_app_can_be_bundled(_module: String) -> Result(Nil, Error) {
  // Once we have the package interface we'll be able to check if the module
  // actually exposes a function with the appropriate type etc.
  // For now we just assume the user knows what they're doing.
  Ok(Nil)
}

fn app_script(project_name: String, module: String) -> String {
  let module_path =
    "../build/dev/javascript/" <> project_name <> "/" <> module <> ".mjs"
  let import_main = "import { main } from \"" <> module_path <> "\""
  let invoke_main = "main();"

  import_main <> "\n" <> invoke_main
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    _ -> {
      io.debug(error)
      todo as "explain the error"
    }
  }
  |> string.pad_right(78, ".")
  |> string.append(" ❌")
  |> io.println
}

fn bundle_script(
  script: String,
  minify: Bool,
  in folder: String,
  named output_file: String,
) -> Result(Nil, Error) {
  // First, let's make sure there's the esbuild executable that can be used.
  use esbuild <- result.try(
    esbuild.download(None, None)
    |> result.map_error(CannotDownloadEsbuild),
  )

  let output_file = filepath.join(folder, output_file)

  // Write the script to bundle to a temporary file that is going to be bundled
  // by escript.
  let temp_file = filepath.join(folder, "temp-bundle-input")
  use _ <- result.try(
    simplifile.write(script, to: temp_file)
    |> result.map_error(CannotWriteTempFile(_, temp_file)),
  )

  let bundle_result =
    esbuild.bundle(esbuild, temp_file, output_file, minify)
    |> result.map_error(CannotBundleComponents)

  // Regardless of the result of the bundling process we delete the temporary
  // input file.
  case bundle_result {
    Ok(Nil) ->
      simplifile.delete(temp_file)
      |> result.replace_error(CannotPerformCleanup(temp_file))
    Error(error) -> {
      let _ = simplifile.delete(temp_file)
      Error(error)
    }
  }
}
