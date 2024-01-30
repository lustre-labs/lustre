import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import gleam/result
import gleam/set
import gleam/string
import filepath
import shellout
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}
import lustre/cli/project

// CONSTANTS -------------------------------------------------------------------

pub const executable_name = "esbuild"

// TYPES -----------------------------------------------------------------------

pub type Error {
  NetworkError(Dynamic)
  SimplifileError(reason: simplifile.FileError, path: String)
  UnknownPlatform(os: String, cpu: String)
  UnzipError(Dynamic)
}

pub opaque type Executable {
  Executable
}

// DOWNLOAD ESBUILD ------------------------------------------------------------

/// Download the esbuild executable for the given os and cpu. If those options
/// are not provided it tries detecting the system's os and cpu.
///
/// The executable will be at the project's root in the `priv/bin/esbuild`
/// folder.
///
/// Returns a proof that esbuild was successfully downloaded that can be used
/// to run all sort of things.
///
pub fn download(
  os: Option(String),
  cpu: Option(String),
) -> Result(Executable, Error) {
  let os = option.unwrap(os, get_os())
  let cpu = option.unwrap(cpu, get_cpu())
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

  let destination_folder =
    project.root_folder()
    |> filepath.join("priv")
    |> filepath.join("bin")
  let esbuild_path = filepath.join(destination_folder, "esbuild")

  let _ = simplifile.create_directory_all(destination_folder)
  use esbuild <- result.try(unzip_esbuild(tarball))
  use _ <- result.try(
    esbuild_path
    |> simplifile.write_bits(esbuild)
    |> result.map_error(SimplifileError(_, esbuild_path)),
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
    |> result.map_error(SimplifileError(_, esbuild_path)),
  )

  Ok(Executable)
}

// BUNDLE ----------------------------------------------------------------------

/// Bundles the given contents. The command will run in the project's root.
///
pub fn bundle(
  _esbuild: Executable,
  input_file: String,
  output_file: String,
  minify: Bool,
) -> Result(Nil, Nil) {
  let flags = [
    "--bundle",
    // The format is always "esm", we're not encouraging "cjs".
    "--format=\"esm\"",
    "--outfile=\"" <> output_file <> "\"",
  ]

  let options = case minify {
    True -> [input_file, "--minify", ..flags]
    False -> [input_file, ..flags]
  }

  shellout.command(
    run: filepath.join(filepath.join(".", "priv"), "esbuild"),
    in: project.root_folder(),
    with: options,
    opt: [],
  )
  |> result.replace(Nil)
  |> result.nil_error
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
