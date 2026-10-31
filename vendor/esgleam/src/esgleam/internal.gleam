import gleam/list
import gleam/option.{type Option, Some}
import gleam/regexp as regex
import gleam/result
import gleam/string
import simplifile

@target(javascript)
pub fn exec_shell(command: String) -> Nil {
  do_exec_shell(string.split(command, on: " "))
}

@target(javascript)
@external(javascript, "../ffi_esgleam.mjs", "exec_shell")
pub fn do_exec_shell(command: List(String)) -> Nil

@target(erlang)
@external(erlang, "ffi_esgleam", "do_exec")
fn exec_erl(cmd: String, args: List(String)) -> Result(Nil, String)

@target(erlang)
pub fn exec_shell(command: String) -> Nil {
  let assert [cmd, ..args] = string.split(command, on: " ")
  let assert Ok(_) = exec_erl(cmd, args)
  Nil
}

pub fn get_project_name() -> String {
  let assert Ok(project_name) =
    simplifile.read("./gleam.toml")
    |> result.map(with: fn(file) {
      let assert Ok(re) = regex.from_string("name *= *\"(\\w[\\w_]*)\"")
      let assert Ok(match) =
        regex.scan(re, file)
        |> list.first

      let assert Ok(first_maybe) = list.first(match.submatches)
      let assert Some(name): Option(String) = first_maybe
      name
    })
  project_name
}
