import { Ok, Error } from "./gleam.mjs";

export function measure_height(selector, root) {
  const element = root.querySelector(selector);

  if (element) {
    return new Ok(element.getBoundingClientRect().height);
  } else {
    return new Error(undefined);
  }
}
