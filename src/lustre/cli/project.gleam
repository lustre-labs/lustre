// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import lustre/cli/utils.{map, try}
import shellout
import simplifile
import tom.{type Toml}

// TYPES -----------------------------------------------------------------------

pub type Config {
  Config(name: String, version: String, toml: Dict(String, Toml))
}

pub type Interface {
  Interface(name: String, version: String, modules: Dict(String, Module))
}

pub type Module {
  Module(constants: Dict(String, Type), functions: Dict(String, Function))
}

pub type Function {
  Function(parameters: List(Type), return: Type)
}

pub type Type {
  Named(name: String, package: String, module: String, parameters: List(Type))
  Variable(id: Int)
  Fn(parameters: List(Type), return: Type)
  Tuple(elements: List(Type))
}

// COMMANDS --------------------------------------------------------------------

/// Compile the current project running the `gleam build` command.
///
pub fn build() -> Result(Nil, String) {
  use _ <- try(
    shellout.command(
      run: "gleam",
      in: ".",
      with: ["build", "--target=js"],
      opt: [],
    ),
    on_error: map(with: pair.second),
  )

  Ok(Nil)
}

pub fn interface() -> Result(Interface, String) {
  let dir = filepath.join(root(), "build/.lustre")
  let out = filepath.join(dir, "package-interface.json")

  use _ <- try(
    shellout.command(
      run: "gleam",
      in: ".",
      with: ["export", "package-interface", "--out", out],
      opt: [],
    ),
    on_error: map(with: pair.second),
  )

  let assert Ok(json) = simplifile.read(out)
  let assert Ok(interface) = json.decode(json, decode_interface)
  Ok(interface)
}

/// Read the project configuration in the `gleam.toml` file.
///
pub fn config() -> Result(Config, String) {
  use _ <- result.try(build())

  // Since we made sure that the project could compile we're sure that there is
  // bound to be a `gleam.toml` file somewhere in the current directory (or in
  // its parent directories). So we can safely call `root()` without
  // it looping indefinitely.
  let configuration_path = filepath.join(root(), "gleam.toml")

  // All these operations are safe to assert because the Gleam project wouldn't
  // compile if any of this stuff was invalid.
  let assert Ok(configuration) = simplifile.read(configuration_path)
  let assert Ok(toml) = tom.parse(configuration)
  let assert Ok(name) = tom.get_string(toml, ["name"])
  let assert Ok(version) = tom.get_string(toml, ["version"])

  Ok(Config(name: name, version: version, toml: toml))
}

// ERROR HANDLING --------------------------------------------------------------

///
///
pub type Error {
  BuildError
}

pub fn explain(error: Error) -> Nil {
  case error {
    BuildError ->
      "
It looks like your project has some compilation errors that need to be addressed
before I can do anything."
      |> io.println
  }
}

// UTILS -----------------------------------------------------------------------

/// Finds the path leading to the project's root folder. This recursively walks
/// up from the current directory until it finds a `gleam.toml`.
///
pub fn root() -> String {
  find_root(".")
}

fn find_root(path: String) -> String {
  let toml = filepath.join(path, "gleam.toml")

  case simplifile.verify_is_file(toml) {
    Ok(False) | Error(_) -> find_root(filepath.join("..", path))
    Ok(True) -> path
  }
}

pub fn type_to_string(type_: Type) -> String {
  case type_ {
    Tuple(elements) -> {
      let elements = list.map(elements, type_to_string)
      "#(" <> string.join(elements, with: ", ") <> ")"
    }

    Fn(params, return) -> {
      let params = list.map(params, type_to_string)
      let return = type_to_string(return)
      "fn(" <> string.join(params, with: ", ") <> ") -> " <> return
    }

    Named(name, _package, _module, []) -> name
    Named(name, _package, _module, params) -> {
      let params = list.map(params, type_to_string)
      name <> "(" <> string.join(params, with: ", ") <> ")"
    }

    Variable(id) -> "a_" <> int.to_string(id)
  }
}

// DECODERS --------------------------------------------------------------------

fn decode_interface(dyn: Dynamic) -> Result(Interface, List(DecodeError)) {
  dynamic.decode3(
    Interface,
    dynamic.field("name", dynamic.string),
    dynamic.field("version", dynamic.string),
    dynamic.field("modules", dynamic.dict(dynamic.string, decode_module)),
  )(dyn)
}

fn decode_module(dyn: Dynamic) -> Result(Module, List(DecodeError)) {
  dynamic.decode2(
    Module,
    dynamic.field(
      "constants",
      dynamic.dict(dynamic.string, dynamic.field("type", decode_type)),
    ),
    dynamic.field("functions", dynamic.dict(dynamic.string, decode_function)),
  )(dyn)
}

fn decode_function(dyn: Dynamic) -> Result(Function, List(DecodeError)) {
  dynamic.decode2(
    Function,
    dynamic.field("parameters", dynamic.list(decode_labelled_argument)),
    dynamic.field("return", decode_type),
  )(dyn)
}

fn decode_type(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  use kind <- result.try(dynamic.field("kind", dynamic.string)(dyn))

  case kind {
    "named" -> decode_named_type(dyn)
    "variable" -> decode_variable_type(dyn)
    "fn" -> decode_fn_type(dyn)
    "tuple" -> decode_tuple_type(dyn)

    _ ->
      Error([
        DecodeError(found: kind, expected: "'named' | 'variable' | 'fn'", path: [
          "kind",
        ]),
      ])
  }
}

fn decode_named_type(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  dynamic.decode4(
    Named,
    dynamic.field("name", dynamic.string),
    dynamic.field("package", dynamic.string),
    dynamic.field("module", dynamic.string),
    dynamic.field("parameters", dynamic.list(decode_type)),
  )(dyn)
}

fn decode_variable_type(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  dynamic.decode1(Variable, dynamic.field("id", dynamic.int))(dyn)
}

fn decode_fn_type(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  dynamic.decode2(
    Fn,
    dynamic.field("parameters", dynamic.list(decode_type)),
    dynamic.field("return", decode_type),
  )(dyn)
}

fn decode_tuple_type(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  dynamic.decode1(Tuple, dynamic.field("elements", dynamic.list(decode_type)))(
    dyn,
  )
}

fn decode_labelled_argument(dyn: Dynamic) -> Result(Type, List(DecodeError)) {
  // In this case we don't really care about the label, so we're just ignoring
  // it and returning the argument's type.
  dynamic.field("type", decode_type)(dyn)
}
