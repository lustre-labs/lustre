import esgleam/mod/install

/// Installs `esbuild` (required to be run before building)    
/// Run with `gleam run -m esgleam/install`
pub fn main() {
  install.internal_fetch()
}
