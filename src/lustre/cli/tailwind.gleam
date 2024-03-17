// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/result
import gleam/set
import gleam/string
import lustre/cli/project
import lustre/cli/step.{type Step}
import lustre/cli/utils.{keep}
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}

// COMMANDS --------------------------------------------------------------------

pub fn setup(os: String, cpu: String, version: String) -> Step(Nil, Error) {
  use _ <- step.run(download(os, cpu, version), on_error: keep)
  use _ <- step.run(write_tailwind_config(), on_error: keep)
  step.return(Nil)
}

fn download(os: String, cpu: String, version: String) -> Step(Nil, Error) {
  use <- step.new("Downloading Tailwind")

  let root = project.root()
  let outdir = filepath.join(root, "build/.lustre/bin")
  let outfile = filepath.join(outdir, "tailwind")

  //todo as "do something with the version and see if its different from the one we already have"

  use <- bool.guard(check_tailwind_exists(outfile), {
    use <- step.done("✅ Tailwind already installed!")
    step.return(Nil)
  })

  use <- step.new("Detecting platform")
  use url <- step.try(get_download_url(os, cpu, version), keep)

  use <- step.new("Downloading from " <> url)
  use bin <- step.try(get_tailwind(url), NetworkError)

  let write_tailwind =
    write_tailwind(bin, outdir, outfile)
    |> result.map_error(CannotWriteTailwind(_, outfile))
  use _ <- step.try(write_tailwind, keep)
  use _ <- step.try(set_filepermissions(outfile), fn(reason) {
    CannotSetPermissions(reason, outfile)
  })

  use <- step.done("✅ Tailwind installed!")

  step.return(Nil)
}

fn write_tailwind_config() -> Step(Nil, Error) {
  let config_filename = "tailwind.config.js"
  use <- step.new("Writing `" <> config_filename <> "`")
  let config_outfile = filepath.join(project.root(), config_filename)
  let write_config =
    simplifile.write(
      to: config_outfile,
      contents: "/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [\"./src/**/*.{gleam,mjs}\"],
  theme: {
    extend: {},
  },
  plugins: [],
}
",
    )

  use _ <- step.try(write_config, CannotWriteConfig(_, config_outfile))
  use <- step.done("✅ Written `" <> config_outfile <> "`")
  step.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_tailwind_exists(path) {
  case simplifile.verify_is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

fn get_download_url(os, cpu, version) {
  let base =
    "https://github.com/tailwindlabs/tailwindcss/releases/download/"
    <> version
    <> "/tailwindcss-"

  let path = case os, cpu {
    "linux", "armv7" -> Ok("linux-armv7")
    "linux", "arm64" -> Ok("linux-arm64")
    "linux", "x64" | "linux", "x86_64" -> Ok("linux-x64")

    "win32", "arm64" -> Ok("windows-arm64.exe")
    "win32", "x64" | "win32", "x86_64" -> Ok("windows-x64.exe")

    "darwin", "arm64" | "darwin", "aarch64" -> Ok("macos-arm64")
    "darwin", "x64" | "darwin", "x86_64" -> Ok("macos-x64")

    _, _ -> Error(UnknownPlatform(os, cpu))
  }

  result.map(path, string.append(base, _))
}

fn write_tailwind(bin, outdir, outfile) {
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

// ERROR HANDLING --------------------------------------------------------------

pub type Error {
  NetworkError(Dynamic)
  CannotWriteTailwind(reason: simplifile.FileError, path: String)
  CannotSetPermissions(reason: simplifile.FileError, path: String)
  CannotWriteConfig(reason: simplifile.FileError, path: String)
  UnknownPlatform(os: String, cpu: String)
}

pub fn explain(error: Error) -> Nil {
  case error {
    // TODO: Is there a better way to deal with this dynamic error?
    NetworkError(_dynamic) ->
      io.println(
        "
There was a network error!",
      )

    UnknownPlatform(os, cpu) -> io.println("
I couldn't figure out the correct Tailwind version for your
os (" <> os <> ") and cpu (" <> cpu <> ").")

    CannotSetPermissions(reason, _) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to set permissions for the Tailwind executable.
")

    CannotWriteConfig(reason, _) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to write the `tailwind.config.js` file to the project's root.
")

    CannotWriteTailwind(reason, path) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to write the Tailwind binary to
  `" <> path <> "`.
")
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "cli_ffi", "get_esbuild")
fn get_tailwind(url: String) -> Result(BitArray, Dynamic)
