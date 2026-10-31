import gleam/io

pub fn main() {
  io.println("Usage: gleam run -m esgleam/<command>")
  io.println(
    "Commands:
\tapp\t\tBundles `gleam.main.mjs` as a script using ESM to the `./dist` directory.
\tbundle\t\tBundles {%project_name}.gleam as a library using ESM to the `./dist` directory.
\thelp\t\tPrints this.
\tinstall\t\tInstalled `esbuild` (require to be run before building).",
  )
}
