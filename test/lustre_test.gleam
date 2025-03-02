// IMPORTS ---------------------------------------------------------------------

import argv
import benchmarks/diff_benchmark
import gleeunit

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

/// Zed has a neat ui to run individual tests as a task that runs `gleam test`
/// with an argument `--test-name-filter=` followed by the name of the test
/// function.
///
/// Gleeunt doesn't have any test filtering built-in but we can fake it by
/// wrapping all of our tests in this function and passing in the name of the
/// test.
///
pub fn test_filter(name: String, f: fn() -> Nil) -> Nil {
  case argv.load().arguments {
    ["--test-name-filter=" <> test_name, ..] if test_name == name -> f()
    ["--test-name-filter=" <> _, ..] -> Nil

    // If the filter argument isn't provided we want to run every test!
    _ -> f()
  }
}
