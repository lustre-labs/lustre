// IMPORTS ---------------------------------------------------------------------

import argv
import gleeunit
import lustre/diff_benchmark

// MAIN ------------------------------------------------------------------------

pub fn main() {
  case argv.load().arguments {
    ["benchmark", ..] -> {
      let _ = diff_benchmark.benchmark_10_rows()
      let _ = diff_benchmark.benchmark_100_rows()
      let _ = diff_benchmark.benchmark_1000_rows()
      let _ = diff_benchmark.benchmark_10_000_rows()

      Nil
    }
    _ -> gleeunit.main()
  }
}
