import { Ok, Error } from "./gleam.mjs";

export function measure_height(root, selector) {
  // We use the `root` element as the parent here instead of `document` -
  // this way our function continues to work even inside component shadow roots!
  const element = root.querySelector(selector)

  if (element) {
    return new Ok(element.getBoundingClientRect().height);
  } else {
    return new Error(undefined);
  }
}
