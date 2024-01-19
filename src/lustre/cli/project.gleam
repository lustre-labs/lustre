import gleam/dict.{type Dict}
import gleam/string
import filepath
import simplifile
import tom.{type Toml}

/// The configuration of a Gleam project
pub type Configuration {
  Configuration(name: String, version: String, toml: Dict(String, Toml))
}

/// A proof that the project was compiled successfully.
/// 
pub opaque type Compiled {
  Compiled
}

/// Compile the current project running the `gleam build` command.
/// 
pub fn build() -> Result(Compiled, Nil) {
  let stdout = exec("gleam build --target javascript")
  let compilation_failed = string.contains(stdout, "error:")
  case compilation_failed {
    True -> Error(Nil)
    False -> Ok(Compiled)
  }
}

/// Read the project configuration in the `gleam.toml` file.
/// To call this function it's necessary to provide a proof that the project
/// was successfully built. To do so you can use the `build` function.
///
pub fn read_configuration(_compiled: Compiled) -> Configuration {
  // Since we have proof that the project could compile we're sure that there is
  // bound to be a `gleam.toml` file somewhere in the current directory (or in
  // its parent directories). So we can safely call `recursive_lookup` without
  // it looping indefinitely.
  let configuration_path = recursive_lookup("gleam.toml")
  // All these operations are safe to assert because the Gleam project wouldn't
  // compile if any of this stuff was invalid.
  let assert Ok(configuration) = simplifile.read(configuration_path)
  let assert Ok(toml) = tom.parse(configuration)
  let assert Ok(name) = tom.get_string(toml, ["name"])
  let assert Ok(version) = tom.get_string(toml, ["version"])
  Configuration(name: name, version: version, toml: toml)
}

/// Find the path of a file starting from a base path and recursively looking
/// into parent directories until the file is found.
/// 
fn recursive_lookup(file: String) -> String {
  case simplifile.is_file(file) {
    False -> recursive_lookup(filepath.join("..", file))
    True -> file
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_build_ffi", "exec")
fn exec(command: String) -> String
