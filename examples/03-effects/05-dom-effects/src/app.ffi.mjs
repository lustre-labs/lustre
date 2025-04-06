import { Ok, Error } from "./gleam.mjs";

export function measure_height(selector) {
  const element = document.querySelector(selector);

  if (element) {
    return new Ok(element.getBoundingClientRect().height);
  } else {
    return new Error(undefined);
  }
}
