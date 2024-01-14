import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeErrors, type Dynamic, DecodeError}
import gleam/option.{type Option}
import gleam/result

pub type Package {
  Package(
    name: String,
    version: String,
    gleam_version: Option(String),
    modules: Dict(String, Module),
  )
}

pub type Module {
  Module(
    documentation: List(String),
    type_aliases: Dict(String, TypeAlias),
    types: Dict(String, TypeDefinition),
    values: Dict(String, Value),
  )
}

pub type TypeDefinition {
  TypeDefinition(
    documentation: Option(String),
    deprecation: Option(Deprecation),
    parameters: Int,
    constructors: List(String),
  )
}

pub type TypeAlias {
  TypeAlias(
    documentation: Option(String),
    deprecation: Option(Deprecation),
    parameters: Int,
    alias: Type,
  )
}

pub type Value {
  Value(
    documentation: Option(String),
    deprecation: Option(Deprecation),
    supported_targets: List(Target),
    type_: Type,
  )
}

pub type Target {
  Gleam
  Externals(erlang: Bool, javascript: Bool)
}

pub type Deprecation {
  Deprecation(message: String)
}

pub type Type {
  Tuple(elements: List(Type))
  Fn(parameters: List(Type), return: Type)
  Variable(id: Int)
  Named(package: String, module: String, name: String, parameters: List(Type))
}

pub fn decoder(dynamic: Dynamic) -> Result(Package, DecodeErrors) {
  dynamic.decode4(
    Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("version", dynamic.string),
    dynamic.field("gleam_version", dynamic.optional(dynamic.string)),
    dynamic.field("modules", dynamic.dict(dynamic.string, module_decoder)),
  )(dynamic)
}

fn module_decoder(dynamic: Dynamic) -> Result(Module, DecodeErrors) {
  dynamic.decode4(
    Module,
    dynamic.field("documentation", dynamic.list(dynamic.string)),
    dynamic.field("type_aliases", dynamic.dict(dynamic.string, alias_decoder)),
    dynamic.field("types", dynamic.dict(dynamic.string, type_definition_decoder),
    ),
    dynamic.field("values", dynamic.dict(dynamic.string, value_decoder)),
  )(dynamic)
}

fn type_definition_decoder(
  dynamic: Dynamic,
) -> Result(TypeDefinition, DecodeErrors) {
  dynamic.decode4(
    TypeDefinition,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("parameters", dynamic.int),
    dynamic.field("constructors", dynamic.list(dynamic.string)),
  )(dynamic)
}

fn alias_decoder(dynamic: Dynamic) -> Result(TypeAlias, DecodeErrors) {
  dynamic.decode4(
    TypeAlias,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("parameters", dynamic.int),
    dynamic.field("alias", type_decoder),
  )(dynamic)
}

fn deprecation_decoder(dynamic: Dynamic) -> Result(Deprecation, DecodeErrors) {
  dynamic.decode1(Deprecation, dynamic.field("message", dynamic.string))(dynamic,
  )
}

fn type_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  let function =
    dynamic.field("Fn", dynamic.dynamic)(dynamic)
    |> result.then(function_decoder)
  use <- result.lazy_or(function)

  let tuple =
    dynamic.field("Tuple", dynamic.dynamic)(dynamic)
    |> result.then(tuple_decoder)
  use <- result.lazy_or(tuple)

  let variable =
    dynamic.field("Variable", dynamic.dynamic)(dynamic)
    |> result.then(variable_decoder)
  use <- result.lazy_or(variable)

  let named =
    dynamic.field("Named", dynamic.dynamic)(dynamic)
    |> result.then(named_decoder)
  use <- result.lazy_or(named)

  Error([
    DecodeError(
      expected: "an object with a single field called \"Fn\", \"Tuple\", \"Variable\" or \"Named\"",
      found: "nothing",
      path: [],
    ),
  ])
}

fn function_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  dynamic.decode2(
    Fn,
    dynamic.field("parameters", dynamic.list(type_decoder)),
    dynamic.field("return", type_decoder),
  )(dynamic)
}

fn tuple_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  dynamic.decode1(Tuple, dynamic.field("elements", dynamic.list(type_decoder)))(
    dynamic,
  )
}

fn named_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  dynamic.decode4(
    Named,
    dynamic.field("package", dynamic.string),
    dynamic.field("module", dynamic.string),
    dynamic.field("name", dynamic.string),
    dynamic.field("parameters", dynamic.list(type_decoder)),
  )(dynamic)
}

fn variable_decoder(dynamic: Dynamic) -> Result(Type, DecodeErrors) {
  dynamic.decode1(Variable, dynamic.field("id", dynamic.int))(dynamic)
}

fn value_decoder(dynamic: Dynamic) -> Result(Value, DecodeErrors) {
  dynamic.decode4(
    Value,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("deprecation", dynamic.optional(deprecation_decoder)),
    dynamic.field("supported_targets", dynamic.list(target_decoder)),
    dynamic.field("type", type_decoder),
  )(dynamic)
}

fn target_decoder(dynamic: Dynamic) -> Result(Target, DecodeErrors) {
  todo as "the target encoding format is still in the making"
}
