// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/result
import gleam/set
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag.{type Flag}
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}

// TYPES -----------------------------------------------------------------------

pub type Error {
  EsbuildAlreadyExists
  NetworkError(Dynamic)
  SimplifileError(simplifile.FileError)
  UnknownPlatform(os: String, cpu: String)
  UnzipError(Dynamic)
}

// MAIN ------------------------------------------------------------------------

const esbuild_path = "priv/bin/esbuild"

pub fn run() -> Command(Nil) {
  glint.command(fn(input) {
    let CommandInput(args, flags) = input

    case args {
      ["esbuild", ..] ->
        download_esbuild(flags)
        |> result.map_error(explain)
        |> result.unwrap(Nil)

      _ ->
        io.println(
          "🚨 Unrecognised argument. Currently `add` can only install esbuild:

USAGE:
        gleam run -m lustre add esbuild [ --cpu=<STRING> --os=<STRING> ]

FLAGS:
        --cpu=<STRING>          The port to run the server on
        --os=<STRING>           The host to run the server on
          ",
        )
    }
  })
  |> glint.flag("os", os_flag())
  |> glint.flag("cpu", cpu_flag())
}

// DOWNLOAD ESBUILD ------------------------------------------------------------

pub fn download_esbuild(flags: Dict(String, Flag)) -> Result(Nil, Error) {
  let os = result.unwrap(flag.get_string(flags, "os"), get_os())
  let cpu = result.unwrap(flag.get_string(flags, "cpu"), get_cpu())
  let base = "https://registry.npmjs.org/@esbuild/"
  let path = case os, cpu {
    "android", "arm" -> Ok("android-arm/-/android-arm-0.19.10.tgz")
    "android", "arm64" -> Ok("android-arm64/-/android-arm64-0.19.10.tgz")
    "android", "x64" -> Ok("android-x64/-/android-x64-0.19.10.tgz")

    "darwin", "aarch64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "amd64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "arm64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "x86_64" -> Ok("darwin-x64/-/darwin-x64-0.19.10.tgz")

    "freebsd", "arm64" -> Ok("freebsd-arm64/-/freebsd-arm64-0.19.10.tgz")
    "freebsd", "x64" -> Ok("freebsd-x64/-/freebsd-x64-0.19.10.tgz")

    "linux", "arm" -> Ok("linux-arm/-/linux-arm-0.19.10.tgz")
    "linux", "arm64" -> Ok("linux-arm64/-/linux-arm64-0.19.10.tgz")
    "linux", "ia32" -> Ok("linux-ia32/-/linux-ia32-0.19.10.tgz")
    "linux", "x64" -> Ok("linux-x64/-/linux-x64-0.19.10.tgz")
    "linux", "x86_64" -> Ok("linux-x64/-/linux-x64-0.19.10.tgz")

    "win32", "arm64" -> Ok("win32-arm64/-/win32-arm64-0.19.10.tgz")
    "win32", "ia32" -> Ok("win32-ia32/-/win32-ia32-0.19.10.tgz")
    "win32", "x64" -> Ok("win32-x64/-/win32-x64-0.19.10.tgz")
    "win32", "x86_64" -> Ok("win32-x64/-/win32-x64-0.19.10.tgz")

    "netbsd", "x64" -> Ok("netbsd-x64/-/netbsd-x64-0.19.10.tgz")
    "openbsd", "x64" -> Ok("openbsd-x64/-/openbsd-x64-0.19.10.tgz")
    "sunos", "x64" -> Ok("sunos-x64/-/sunos-x64-0.19.10.tgz")

    _, _ -> Error(UnknownPlatform(os, cpu))
  }
  use url <- result.try(result.map(path, string.append(base, _)))
  use tarball <- result.try(get_esbuild(url))

  let _ = simplifile.create_directory_all("priv/bin")
  use esbuild <- result.try(unzip_esbuild(tarball))
  use _ <- result.try(
    esbuild_path
    |> simplifile.write_bits(esbuild)
    |> result.map_error(SimplifileError),
  )

  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )
  use _ <- result.try(
    esbuild_path
    |> simplifile.set_permissions(permissions)
    |> result.map_error(SimplifileError),
  )

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

fn explain(error: Error) -> Nil {
  case error {
    EsbuildAlreadyExists ->
      "✨ esbuild already exists in `{path}` "
      |> string.replace(each: "{path}", with: esbuild_path)
      |> string.pad_right(78, ".")
      |> string.append(" ✅")
    NetworkError(_) ->
      "🚨 A network error occured. Check your connection and try again "
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    SimplifileError(_error) ->
      "🚨 An unknown error occured while writing the executable to `{path}` "
      |> string.replace(each: "{path}", with: esbuild_path)
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    UnknownPlatform(os, cpu) ->
      { "🚨 Could not locate download url for " <> os <> "/" <> cpu <> " " }
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
    UnzipError(_error) ->
      "🚨 An unknown error occured while extracting the archive "
      |> string.pad_right(78, ".")
      |> string.append(" ❌")
  }
  |> io.println
}

// GLINT FLAGS -----------------------------------------------------------------

fn os_flag() {
  flag.string()
  |> flag.description("The host to run the server on")
}

fn cpu_flag() {
  flag.string()
  |> flag.description("The port to run the server on")
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_add_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_add_ffi", "get_cpu")
fn get_cpu() -> String

@external(erlang, "lustre_add_ffi", "get_esbuild")
fn get_esbuild(url: String) -> Result(BitArray, Error)

@external(erlang, "lustre_add_ffi", "unzip_esbuild")
fn unzip_esbuild(tarball: BitArray) -> Result(BitArray, Error)
