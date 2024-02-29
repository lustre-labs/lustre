// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict
import gleam/io
import gleam/package_interface.{type Type, Fn, Named, Variable}
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre/attribute.{attribute}
import lustre/cli/esbuild
import lustre/cli/project.{type Module}
import lustre/cli/step
import lustre/cli/utils.{guard, keep, map, replace, try}
import lustre/element
import lustre/element/html.{html}
import simplifile

// COMMANDS --------------------------------------------------------------------

pub fn run() -> Command(Nil) {
  let description =
    "
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(host) = flag.get_string(flags, "host")
    let assert Ok(port) = flag.get_string(flags, "port")
    let assert Ok(include_styles) = flag.get_bool(flags, "include-styles")

    let script = {
      use <- step.new("Building your project")
      use interface <- step.try(project.interface(), replace(BuildError))
      use module <- step.try(
        dict.get(interface.modules, interface.name),
        replace(ModuleMissing(interface.name)),
      )
      use is_app <- step.try(check_is_lustre_app(interface.name, module), keep)
      use <- step.done("✅ Project compiled successfully")

      use <- step.new("Creating the application entry point")
      let root = project.root()
      let tempdir = filepath.join(root, "build/.lustre")
      let _ = simplifile.create_directory_all(tempdir)

      let entry =
        case is_app {
          True ->
            " import { start } from '../dev/javascript/lustre/lustre.mjs';
              import { main } from '../dev/javascript/${app_name}/${app_name}.mjs';

              start(main(), ${container_id});
            "
          False ->
            " import { main } from '../dev/javascript/${app_name}/${app_name}.mjs';

              main();
          "
        }
        |> string.replace("${app_name}", interface.name)
        |> string.replace("${container_id}", "app")

      let html = index_html(interface.name, "app", include_styles)

      let assert Ok(_) = simplifile.write(tempdir <> "/entry.mjs", entry)
      let assert Ok(_) = simplifile.write(tempdir <> "/index.html", html)

      use _ <- step.run(
        esbuild.bundle(
          filepath.join(tempdir, "entry.mjs"),
          filepath.join(tempdir, "index.mjs"),
          False,
        ),
        map(BundleError),
      )
      use _ <- step.run(esbuild.serve(host, port), map(BundleError))
      step.return(Nil)
    }

    case step.execute(script) {
      Ok(_) -> Nil
      Error(error) -> explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("host", {
    let description = ""
    let default = "0.0.0.0"

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("port", {
    let description = ""
    let default = "1234"

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("include-styles", {
    let description = ""
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
}

// ERROR HANDLING --------------------------------------------------------------

type Error {
  BuildError
  BundleError(esbuild.Error)
  MainMissing(module: String)
  MainIncorrectType(module: String, got: Type)
  MainBadAppType(module: String, got: Type)
  ModuleMissing(module: String)
}

fn explain(error: Error) -> Nil {
  case error {
    BuildError -> project.explain(project.BuildError)

    BundleError(error) -> esbuild.explain(error)

    MainMissing(module) -> io.println("
Module `" <> module <> "` doesn't have a public `main` function I can preview.")

    MainIncorrectType(module, type_) -> io.println("
I cannot preview the `main` function exposed by module `" <> module <> "`.
To start a preview server I need it to take no arguments and return a Lustre
`App`.
The one I found has type `" <> project.type_to_string(type_) <> "`.")

    // TODO: maybe this could have useful links to `App`/flags...
    MainBadAppType(module, type_) -> io.println("
I cannot preview the `main` function exposed by module `" <> module <> "`.
To start a preview server I need it to return a Lustre `App` that doesn't need
any flags.
The one I found has type `" <> project.type_to_string(type_) <> "`.

Its return type should look something like this:

  import lustre.{type App}
  pub fn main() -> App(flags, model, msg) {
    todo as \"your Lustre application to preview\"
  }")

    ModuleMissing(module) -> io.println("
I couldn't find a public module called `" <> module <> "` in your project.")
  }
}

// STEPS -----------------------------------------------------------------------

fn check_is_lustre_app(
  module_path: String,
  module: Module,
) -> Result(Bool, Error) {
  use main <- try(
    dict.get(module.functions, "main"),
    replace(MainMissing(module_path)),
  )
  use <- guard(
    main.parameters != [],
    MainIncorrectType(module_path, Fn(main.parameters, main.return)),
  )

  case main.return {
    Named(
      name: "App",
      package: "lustre",
      module: "lustre",
      parameters: [flags, ..],
    ) ->
      case is_compatible_flags_type(flags) {
        True -> Ok(True)
        False -> Error(MainBadAppType(module_path, main.return))
      }

    _ -> Ok(False)
  }
}

// UTILS -----------------------------------------------------------------------

fn index_html(
  app_name: String,
  container_id: String,
  include_styles: Bool,
) -> String {
  let styles = case include_styles {
    True ->
      html.link([
        attribute.rel("stylesheet"),
        attribute.href(
          "https://cdn.jsdelivr.net/gh/lustre-labs/ui/priv/styles.css",
        ),
      ])
    False -> element.none()
  }

  html([], [
    html.head([], [
      html.meta([attribute("charset", "utf-8")]),
      html.meta([
        attribute("name", "viewport"),
        attribute("content", "width=device-width, initial-scale=1"),
      ]),
      html.title([], app_name),
      html.script([attribute.type_("module"), attribute.src("./index.mjs")], ""),
      styles,
    ]),
    html.body([], [html.div([attribute.id(container_id)], [])]),
  ])
  |> element.to_string
  |> string.append("<!DOCTYPE html>", _)
}

fn is_nil_type(t: Type) -> Bool {
  case t {
    Named(name: "Nil", package: "", module: "gleam", parameters: []) -> True
    _ -> False
  }
}

fn is_type_variable(t: Type) -> Bool {
  case t {
    Variable(..) -> True
    _ -> False
  }
}

fn is_compatible_flags_type(t: Type) -> Bool {
  is_nil_type(t) || is_type_variable(t)
}
