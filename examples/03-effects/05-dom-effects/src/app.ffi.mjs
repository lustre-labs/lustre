import { Ok, Error } from "./gleam.mjs";

export function measure_height(element) {
  if (element) {
    return new Ok(element.getBoundingClientRect().height);
  } else {
    return new Error(undefined);
  }
}
