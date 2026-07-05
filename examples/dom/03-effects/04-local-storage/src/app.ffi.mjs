import { Ok, Error } from "./gleam.mjs";

// It's often convenient to write FFI functions in a way that constructs or wraps
// return values in existing, known Gleam values. In Gleam we annotated this FFI
// function as returning a `Result` so we import the constructors and use them
// directly.
export function get_localstorage(key) {
  const json = window.localStorage.getItem(key);

  // Gleam's `Nil` value is represented as `undefined` in JavaScript.
  if (json === null) return new Error(undefined);

  try {
    return new Ok(JSON.parse(json));
  } catch {
    return new Error(undefined);
  }
}

// Not all FFI needs to return something, sometimes a side effect is just a side
// effect!
export function set_localstorage(key, json) {
  window.localStorage.setItem(key, json);
}
