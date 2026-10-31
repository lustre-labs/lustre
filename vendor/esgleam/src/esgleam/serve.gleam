import esgleam
import esgleam/internal

/// Serves the `/dist` directory as `/` on http://127.0.0.1:8000.
pub fn main() {
  esgleam.new(outdir: "./dist")
  |> esgleam.entry(internal.get_project_name() <> ".gleam")
  |> esgleam.serve(dir: "./dist")
  |> esgleam.bundle
}
