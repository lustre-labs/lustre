import gleeunit/should
import lustre/animation.{add, new, tick, value}

pub fn evaluate_test() {
  new()
  |> add("a", 7.0, 42.0, 2.0)
  |> value("a", 0.0)
  |> should.equal(7.0)
}

pub fn evaluate_start_test() {
  new()
  |> add("a", 7.0, 42.0, 2.0)
  |> tick(1000.0)
  |> value("a", 0.0)
  |> should.equal(7.0)
}

pub fn evaluate_done_test() {
  new()
  |> add("a", 7.0, 42.0, 2.0)
  |> tick(1000.0)
  |> tick(3000.0)
  |> value("a", 0.0)
  |> should.equal(42.0)
}

pub fn evaluate_gone_test() {
  new()
  |> add("a", 7.0, 42.0, 2.0)
  // start
  |> tick(1000.0)
  // Done
  |> tick(3000.0001)
  // remove
  |> tick(3000.1)
  |> value("a", 0.0)
  |> should.equal(0.0)
}
