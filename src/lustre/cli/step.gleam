import gleam/io
import gleam_community/ansi
import spinner.{type Spinner}

type SpinnerStatus {
  Running(message: String)
  Stopped
}

type Env {
  Env(spinner: Spinner, spinner_status: SpinnerStatus)
}

pub opaque type Step(a, e) {
  Step(run: fn(Env) -> #(Env, Result(a, e)))
}

/// Replace the current spinner label with a new one.
///
pub fn new(message: String, then continue: fn() -> Step(a, e)) -> Step(a, e) {
  use Env(spinner, spinner_status) <- Step
  case spinner_status {
    Running(_) -> {
      spinner.set_text(spinner, message)
      continue().run(Env(spinner, Running(message)))
    }
    Stopped -> {
      let new_spinner =
        spinner.new(message)
        |> spinner.with_frames(spinner.snake_frames)
        |> spinner.start
      continue().run(Env(new_spinner, Running(message)))
    }
  }
}

/// Stops the current spinner and prints out the given message.
///
pub fn done(message: String, then continue: fn() -> Step(b, e)) -> Step(b, e) {
  use Env(spinner, spinner_status) <- Step
  case spinner_status {
    Running(_) -> spinner.stop(spinner)
    Stopped -> Nil
  }
  io.println(ansi.green(message))
  continue().run(Env(spinner, Stopped))
}

/// Runs another step as part of this one. The step will use the same spinner
/// as the previous one overriding its content.
///
pub fn run(
  step: Step(a, e),
  on_error map_error: fn(e) -> e1,
  then continue: fn(a) -> Step(b, e1),
) -> Step(b, e1) {
  use env <- Step
  case step.run(env) {
    #(new_env, Ok(res)) -> continue(res).run(new_env)
    #(new_env, Error(e)) -> #(new_env, Error(map_error(e)))
  }
}

/// If the result is `Ok` will continue by passing its wrapped value to the
/// `continue` function; otherwise will result in an error stopping the stepwise
/// execution.
///
pub fn try(
  result: Result(a, e),
  on_error map_error: fn(e) -> e1,
  then continue: fn(a) -> Step(b, e1),
) -> Step(b, e1) {
  Step(fn(env) { #(env, result) })
  |> run(map_error, continue)
}

/// Returns a value without changing the state of any spinner.
/// Any running spinner will still be running.
///
pub fn return(value: a) -> Step(a, e) {
  use env <- Step
  #(env, Ok(value))
}

pub fn execute(step: Step(a, e)) -> Result(a, e) {
  let initial_spinner =
    spinner.new("")
    |> spinner.with_frames(spinner.snake_frames)
    |> spinner.start

  let #(Env(spinner, status), res) = step.run(Env(initial_spinner, Running("")))
  case status {
    Running(message) -> {
      spinner.stop(spinner)
      io.println("❌ " <> ansi.red(message))
    }
    Stopped -> Nil
  }
  res
}
