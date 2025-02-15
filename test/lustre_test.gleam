// IMPORTS ---------------------------------------------------------------------

import argv
import gleeunit
import lustre/diff_benchmark

// MAIN ------------------------------------------------------------------------

pub fn main() {
  case argv.load().arguments {
    ["benchmark", ..] -> {
      diff_benchmark.run()
    }
    _ -> gleeunit.main()
  }
}
