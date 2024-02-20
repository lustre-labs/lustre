import gleam/io
import spinner.{type Spinner}

pub opaque type Env {
  Env(spinner: Spinner)
}

pub opaque type Step(a, e) {
  Step(run: fn(Env) -> Result(a, e))
}

pub fn do(step: Step(a, e), then: fn(a) -> Step(b, e)) -> Step(b, e) {
  fn(env) {
    case step.run(env) {
      Ok(a) -> then(a).run(env)
      Error(e) -> Error(e)
    }
  }
  |> Step
}

// --- USING RESULTS INSIDE A STEP ---------------------------------------------

pub fn try(result: Result(a, e), then run: fn(a) -> Step(b, e)) -> Step(b, e) {
  fn(env) {
    case result {
      Ok(a) -> run(a).run(env)
      Error(e) -> Error(e)
    }
  }
  |> Step
}

pub fn run(step: Step(a, e), then run: fn(a) -> Step(b, e)) -> Step(b, e) {
  Step(fn(env) {
    case step.run(env) {
      Ok(a) -> run(a).run(env)
      Error(e) -> Error(e)
    }
  })
}

// --- CHANGING THE SPINNER ----------------------------------------------------

pub fn start(message: String, then run: fn() -> Step(a, e)) -> Step(a, e) {
  fn(env: Env) {
    // Stop the previous spinner and start a new one.
    spinner.stop(env.spinner)
    let new_env =
      spinner.new(message)
      |> spinner.with_frames(spinner.snake_frames)
      |> spinner.start
      |> Env

    run().run(new_env)
  }
  |> Step
}

pub fn complete(message message: String, return value: a) -> Step(a, e) {
  fn(env: Env) {
    spinner.stop(env.spinner)
    io.println(message)
    Ok(value)
  }
  |> Step
}

// --- ACCESSING THE ENVIRONMENT -----------------------------------------------

pub fn env() -> Step(Env, e) {
  Step(fn(env) { Ok(env) })
}

pub fn spinner() -> Step(Spinner, e) {
  Step(fn(env: Env) { Ok(env.spinner) })
}
