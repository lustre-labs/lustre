// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import gleam_community/ansi
import lustre/cli/project
import lustre/cli/utils.{keep, map, replace, try}
import shellout
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}
import spinner

// COMMANDS --------------------------------------------------------------------

pub fn download(os: String, cpu: String) -> Result(Nil, Error) {
  let outdir = filepath.join(project.root(), "build/.lustre/bin")
  let outfile = filepath.join(outdir, "esbuild")

  let spinner =
    spinner.new("Downloading esbuild")
    |> spinner.with_frames(spinner.snake_frames)
    |> spinner.with_colour(ansi.yellow)
    |> spinner.start

  use <- bool.lazy_guard(check_esbuild_exists(outfile), fn() {
    spinner.stop(spinner)
    io.println(ansi.green("✅ Esbuild already installed!"))
    Ok(Nil)
  })

  spinner.set_text(spinner, "Detecting platform")
  use url <- result.try(get_download_url(os, cpu))

  spinner.set_text(spinner, "Downloading from " <> url)
  use tarball <- try(get_esbuild(url), NetworkError)

  spinner.set_text(spinner, "Unzipping esbuild")
  use bin <- try(unzip_esbuild(tarball), UnzipError)
  use _ <- try(write_esbuild(bin, outdir, outfile), SimplifileError(_, outfile))
  use _ <- try(set_filepermissions(outfile), SimplifileError(_, outfile))

  spinner.stop(spinner)
  io.println(ansi.green("✅ Esbuild installed!"))
  Ok(Nil)
}

pub fn bundle(
  input_file: String,
  output_file: String,
  minify: Bool,
) -> Result(Nil, Error) {
  use _ <- try(download(get_os(), get_cpu()), keep)
  use _ <- try(project.build(), replace(BuildError))

  let spinner =
    spinner.new("Bundling with esbuild")
    |> spinner.with_frames(spinner.snake_frames)
    |> spinner.with_colour(ansi.yellow)
    |> spinner.start

  spinner.set_text(spinner, "Getting everything ready for tree shaking")
  let root = project.root()
  use _ <- try(configure_node_tree_shaking(root), map(SimplifileError(_, root)))

  let flags = [
    "--bundle",
    "--external:node:*",
    "--format=esm",
    "--outfile=" <> output_file,
  ]
  let options = case minify {
    True -> [input_file, "--minify", ..flags]
    False -> [input_file, ..flags]
  }

  spinner.set_text(spinner, "Bundling with esbuild")
  use _ <- try(
    shellout.command(
      run: "./build/.lustre/bin/esbuild",
      in: root,
      with: options,
      opt: [],
    ),
    on_error: map(function.compose(pair.second, BundleError)),
  )

  spinner.stop(spinner)
  io.println(ansi.green("✅ Bundle produced at `" <> output_file <> "`"))
  Ok(Nil)
}

pub fn serve(host: String, port: String) -> Result(Nil, Error) {
  use _ <- try(download(get_os(), get_cpu()), keep)
  let root = project.root()
  let flags = [
    "--serve=" <> host <> ":" <> port,
    "--servedir=" <> filepath.join(root, "build/.lustre"),
  ]

  io.println("\nStarting dev server at " <> host <> ":" <> port <> "...")
  use _ <- try(
    shellout.command(
      run: "./build/.lustre/bin/esbuild",
      in: root,
      with: flags,
      opt: [],
    ),
    on_error: map(function.compose(pair.second, BundleError)),
  )

  Ok(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_esbuild_exists(path) {
  case simplifile.verify_is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

fn get_download_url(os, cpu) {
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

  result.map(path, string.append(base, _))
}

fn write_esbuild(bin, outdir, outfile) {
  let _ = simplifile.create_directory_all(outdir)

  simplifile.write_bits(outfile, bin)
}

fn set_filepermissions(file) {
  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )

  simplifile.set_permissions(file, permissions)
}

fn configure_node_tree_shaking(root) {
  // This whole chunk of code is to force tree shaking on dependencies that esbuild
  // has a habit of including because it thinks their imports might have side
  // effects.
  //
  // This is a really grim hack but it's the only way I've found to get esbuild to
  // ignore unused deps like `shellout` that import node stuff but aren't used in
  // app code.
  let force_tree_shaking = "{ \"sideEffects\": false }"
  let assert Ok(_) =
    simplifile.write(
      filepath.join(root, "build/dev/javascript/package.json"),
      force_tree_shaking,
    )
  let pure_deps = ["lustre", "glint", "simplifile", "shellout"]

  list.try_each(pure_deps, fn(dep) {
    root
    |> filepath.join("build/dev/javascript/" <> dep)
    |> filepath.join("package.json")
    |> simplifile.write(force_tree_shaking)
  })
}

// ERROR HANDLING --------------------------------------------------------------

pub type Error {
  BuildError
  BundleError(message: String)
  NetworkError(Dynamic)
  SimplifileError(reason: simplifile.FileError, path: String)
  UnknownPlatform(os: String, cpu: String)
  UnzipError(Dynamic)
}

pub fn explain(error: Error) -> Nil {
  error
  |> string.inspect
  |> io.println
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "cli_ffi", "get_os")
fn get_os() -> String

@external(erlang, "cli_ffi", "get_cpu")
fn get_cpu() -> String

@external(erlang, "cli_ffi", "get_esbuild")
fn get_esbuild(url: String) -> Result(BitArray, Dynamic)

@external(erlang, "cli_ffi", "unzip_esbuild")
fn unzip_esbuild(tarball: BitArray) -> Result(BitArray, Dynamic)
