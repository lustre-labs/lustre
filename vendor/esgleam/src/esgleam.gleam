import esgleam/internal
import esgleam/mod/install
import esgleam/mod/platform.{get_exe_name}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/string_tree.{append, append_tree, from_strings}
import simplifile

/// Kind of output
pub type Kind {
  /// Executes the `main()` function in your entry point
  Script
  /// Only exports the code
  Library
}

/// Output format of generated JavaScript
pub type Format {
  /// ECMAScript module
  Esm
  /// CommonJS
  Cjs
  /// immediately-invoked function expression
  Iife
}

/// [Platform](https://esbuild.github.io/api/#platform)
pub type Platform {
  /// Build platform independed code
  Neutral
  /// Build browser compatable code (default)
  Browser
  /// Build for Node
  Node
}

/// Config for esbuild
pub type Config {
  Config(
    /// If true, automatically install esbuild if has not been installed (needs Erlang installed)
    /// default `True`.
    autoinstall: Bool,
    /// Output directory
    outdir: String,
    /// List of entry points supplied
    entry_points: List(String),
    /// Output format for JavaScript. See [`Format`](#Format)
    /// default `Esm`
    format: Format,
    /// Kind for output
    /// default `Library`
    kind: Kind,
    /// default `False`
    minify: Bool,
    /// List of [target environments](https://esbuild.github.io/api/#target)
    /// default `[]`
    target: List(String),
    /// The root directory for the dev server (run server when defined)
    /// default `None`
    serve: Option(String),
    /// Generate sourcemap for JavaScript
    /// default `False`
    sourcemap: Bool,
    /// Platform for the output
    /// See [esbuild/platform](https://esbuild.github.io/api/#platform) for what each type does
    /// default `Neutral`
    platform: Option(Platform),
    /// Enable watchmode. Bundles files on change
    /// Note: `gleam build` need to be run manually
    /// default `False`
    watch: Bool,
    /// raw flags to pass to esbuild
    /// default `""`
    raw: String,
  )
}

/// Start to create a build script
/// ```gleam
/// > esbuild.new("./dist/static")
/// Config("./dist/static", ...)
/// ```
pub fn new(outdir path: String) -> Config {
  Config(
    autoinstall: False,
    outdir: path,
    entry_points: [],
    format: Esm,
    kind: Library,
    minify: False,
    target: [],
    serve: None,
    sourcemap: False,
    platform: None,
    watch: False,
    raw: "",
  )
}

/// Boolean for autoinstalling esbuild if it is not found.
/// If `True`, requires erlang to be install on the system.
pub fn autoinstall(config: Config, do_install: Bool) -> Config {
  Config(..config, autoinstall: do_install)
}

/// [Entry points](https://esbuild.github.io/api/#entry-points)
/// Can be use multiple times
pub fn entry(config: Config, path: String) -> Config {
  Config(..config, entry_points: [path, ..config.entry_points])
}

/// Output format. see [`Format`](#format)
pub fn format(config: Config, format: Format) {
  Config(..config, format: format)
}

/// Kind for output. see [`Kind`](#kind)
/// if set to `Script`, will ignore all entries except the first and will call its `main` function.
pub fn kind(config: Config, kind: Kind) {
  Config(..config, kind: kind)
}

/// [Target](https://esbuild.github.io/api/#target) for transpiled JavaScript.
/// Can be used mutiple times
/// ```gleam
/// > esbuild.new("./dist/static")
/// > |> esbuild.target("es2020")
/// > |> esbuild.target("firefox110")
/// > |> esbuild.target("edge90")
/// ```
pub fn target(config: Config, target: String) {
  Config(..config, target: [target, ..config.target])
}

/// Create minified JavaScript
pub fn minify(config: Config, do_minify: Bool) {
  Config(..config, minify: do_minify)
}

/// Start a development server on http://127.0.0.1:8000 with `path` being `/`
pub fn serve(config: Config, dir path: String) {
  Config(..config, serve: Some(path))
}

/// Generate code for a specified platform
pub fn platform(config: Config, platform platform: Platform) {
  Config(..config, platform: Some(platform))
}

// #TODO: Setup file watcher for all targets
/// Note: There is no file watcher for Gleam files so you have to manually run `gleam build` on change.
pub fn watch(config: Config, do_watch: Bool) {
  Config(..config, watch: do_watch)
}

/// Raw CLI argument to pass to esbuild
pub fn raw(config: Config, args: String) {
  Config(..config, raw: string.append(to: " ", suffix: args))
}

/// Executes `esbuild` with the provided config parsed as CLI arguments.
pub fn bundle(config: Config) {
  case simplifile.create_directory_all(config.outdir) {
    Ok(_) -> do_bundle(config)
    Error(_) -> Error("Failed to create output directory")
  }
}

fn do_bundle(config: Config) {
  let entries_list =
    list.map(config.entry_points, fn(entry) {
      "./build/dev/javascript/"
      <> internal.get_project_name()
      <> "/"
      <> string.replace(entry, ".gleam", with: ".mjs")
    })

  let assert Ok(first_entry_rel) =
    list.first(config.entry_points)
    |> result.map(string.replace(_, ".gleam", with: ".mjs"))

  let script_path = {
    "./build/dev/javascript/"
    <> internal.get_project_name()
    <> "/"
    <> "gleam.main.mjs"
  }

  let entries = case config.kind {
    Script -> {
      let content =
        "import { main } from \"" <> "./" <> first_entry_rel <> "\";main?.();"
      let assert Ok(_) = simplifile.write(content, to: script_path)
      script_path
    }

    Library -> string.join(entries_list, with: " ")
  }

  let exe_path = "./build/dev/bin/package/bin/" <> get_exe_name()

  let cmd =
    string_tree.from_string(exe_path <> " ")
    |> append(entries)
    |> append(" --bundle")
    |> append_tree(case config.kind {
      Script ->
        from_strings([
          " --outfile="
          <> config.outdir
          <> "/"
          <> internal.get_project_name()
          <> {
            case config.format {
              Esm -> ".mjs"
              _ -> ".js"
            }
          },
        ])
      Library -> from_strings([" --outdir=", config.outdir])
    })
    |> if_true(config.minify, " --minify")
    |> if_some(
      option.map(config.platform, fn(platform) {
        case platform {
          Neutral -> "neutral"
          Node -> "node"
          Browser -> "browser"
        }
      }),
      " --platform=",
    )
    |> if_true(config.watch, " --watch")
    |> append(" --format=")
    |> append(format_to_string(config.format))
    |> if_true(!list.is_empty(config.target), " --target=")
    |> append(string.join(config.target, with: ","))
    |> if_some(config.serve, flag: " --servedir=")
    |> append(config.raw)
    |> string_tree.to_string

  case config.autoinstall {
    True ->
      case simplifile.is_file(exe_path) {
        Error(_) | Ok(False) -> install.fetch()
        Ok(True) -> Nil
      }
    False -> Nil
  }

  let exe_check = case simplifile.is_file(exe_path) {
    Ok(True) -> Ok(Nil)
    Ok(False) ->
      case config.autoinstall {
        True -> Ok(install.fetch())
        False ->
          Error(
            "esbuild is not installed, run `gleam run esgleam/install` or set Config.autoinstall to True.",
          )
      }
    Error(_) -> Error("Gleam does not have permission to read the executable")
  }

  use _ <- result.map(exe_check)

  io.println("$ " <> cmd)

  case config.watch {
    True -> watch_gleam()
    False -> fn() { Nil }
  }

  internal.exec_shell(cmd)
  Ok(Nil)
}

fn format_to_string(format: Format) {
  case format {
    Esm -> "esm"
    Cjs -> "cjs"
    Iife -> "iife"
  }
}

fn if_true(
  builder: string_tree.StringTree,
  predicate: Bool,
  value: String,
) -> string_tree.StringTree {
  case predicate {
    True -> value
    False -> ""
  }
  |> append(builder, _)
}

fn if_some(
  builder: string_tree.StringTree,
  option opt: Option(String),
  flag flag: String,
) -> string_tree.StringTree {
  case opt {
    Some(value) -> flag <> value
    None -> ""
  }
  |> append(builder, _)
}

@target(javascript)
@external(javascript, "./ffi_esgleam.mjs", "watch_gleam")
pub fn watch_gleam() -> fn() -> Nil

@target(erlang)
pub fn watch_gleam() -> fn() -> Nil {
  fn() { Nil }
}
