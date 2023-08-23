import lustre/effect.{Effect}
import gleam/list.{filter, find, map}

/// A singleton holding all your animations, and a timestamp
/// 
/// Hayleigh note: since the stdlib got some improvmements a while ago, I think
/// you could use a `Map(String, Animation)` now instead of `List.find`ing all
/// over the place.
pub opaque type Animations {
  Animations(t: Float, List(Animation))
}

type Animation {
  Animation(name: String, range: #(Float, Float), state: AnimationState)
}

//   InParallel(Animations)
//   InSequence(Animations)
//   Repeat(Animation)
//   Cancelled(Animation)

// The State Done will guarantee the `stop` value is returned, before the Animation is removed from the list
type AnimationState {
  NotStarted(seconds: Float)
  Running(seconds: Float, since: Float)
  Done
}

pub fn new() {
  Animations(0.0, [])
}

/// Add an animation (linear interpolation) from `start` to `stop`, for `seconds` duration.
/// The `name` should be unique, so the animation can be retrieved and evaluated by `value()` later.
/// The animation is started when a subsequent command from `effect()` is returned by your
/// lustre `update()` function; followed by a call to `tick()`.
pub fn add(
  animations: Animations,
  name,
  start: Float,
  stop: Float,
  seconds: Float,
) -> Animations {
  use list <- change_list(animations)
  [Animation(name, #(start, stop), NotStarted(seconds)), ..list]
}

/// Remove an animation if it should stop before it is finished.
/// If an animation runs to its end normally, you do not have to `remove()` it manually,
/// will be automatically removed by `tick()`.
pub fn remove(animations: Animations, name) -> Animations {
  use list <- change_list(animations)
  filter(list, does_not_have_name(_, name))
  // Would not filter, but replace w/ Cancelled and filtered in the next call, see comments at bottom of this file
}

fn change_list(
  animations: Animations,
  f: fn(List(Animation)) -> List(Animation),
) -> Animations {
  let assert Animations(t, list) = animations
  Animations(t, f(list))
}

fn does_not_have_name(animation: Animation, name: String) {
  let assert Animation(n, _, _) = animation
  n != name
}

/// When called for the first time on an animation, its start time is
/// set to time-Offset. Its stop time then is the start time plus the
/// duration.
///
/// When called for the *first* time for animations that have finished,
/// they will be marked as such in the result. `value()` will return the `stop` value
/// that you passed with `add()`
///
/// When called the *second* time for animations that have finished, they will be absent
/// from the returned value.
pub fn tick(animations: Animations, time_offset) -> Animations {
  let assert Animations(_, list) = animations
  let new_list =
    list
    |> filter(not_done)
    |> map(tick_animation(_, time_offset))
  Animations(time_offset, new_list)
}

fn not_done(animation: Animation) -> Bool {
  case animation {
    Animation(_, _, Done) -> False
    _ -> True
  }
}

fn tick_animation(animation: Animation, time: Float) -> Animation {
  let assert Animation(name, range, state) = animation
  let new_state = case state {
    NotStarted(seconds) -> Running(seconds, since: time)
    Running(seconds, since) ->
      case time -. since >=. seconds *. 1000.0 {
        True -> Done
        False -> state
      }
    Done -> Done
  }
  Animation(name, range, new_state)
}

/// Returns `effect.none()` if none of the animations is running.
/// Otherwise returns an opaque `Effect` that will cause `msg(time)` to
/// be dispatched on a JavaScript `requestAnimationFrame`
/// 
/// Hayleigh note: Maybe this could have a better name like `next_tick` or
/// `request_tick`?
pub fn effect(animations: Animations, msg: fn(Float) -> m) -> Effect(m) {
  case animations {
    Animations(_, []) -> effect.none()
    _ -> request_animation_frame(msg)
  }
}

/// If the animation specified by `which` is not found, returns `default`.
/// Otherwise, the interpolated value for the `time` passed to `tick()` is returned.
pub fn value(animations: Animations, which: String, default: Float) -> Float {
  let assert Animations(t, list) = animations
  case find(list, has_name(_, which)) {
    Ok(animation) -> evaluate(animation, t)
    Error(Nil) -> default
  }
}

fn has_name(animation: Animation, name: String) {
  let assert Animation(n, _, _) = animation
  n == name
}

fn evaluate(animation: Animation, time: Float) -> Float {
  let assert Animation(_, #(start, stop), state) = animation
  case state {
    NotStarted(_) -> start
    Running(seconds, since) -> {
      let dt = time -. since
      let delta = dt /. { seconds *. 1000.0 }
      start +. { stop -. start } *. delta
    }
    Done -> stop
  }
}

pub fn request_animation_frame(msg: fn(Float) -> m) -> Effect(m) {
  effect.from(fn(dispatch) {
    js_request_animation_frame(fn(time_offset: Float) {
      dispatch(msg(time_offset))
    })
    Nil
  })
}

pub type RequestedFrame

// The returned 'long' can be passed to 'cancelAnimationFrame' - except we do not have any means to
// TODO Push the 'long' down into JS land, with the Animation, so we can
// make a mapping from Animation to RequestFrame and a `cancelFrame(Animation, msg) -> Effect(m)`
// that (again in JS land) *can* cancel the appropriate request frame.
external fn js_request_animation_frame(f) -> RequestedFrame =
  "../ffi.mjs" "request_animation_frame"

pub external fn cancel_animation_frame(frame: RequestedFrame) -> Nil =
  "../ffi.mjs" "cancel_animation_frame"
