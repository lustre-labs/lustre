import { Ok, Error } from './gleam.mjs';

export function get_text_content(selector) {
  const element = document.querySelector(selector)
  const textContent = element?.textContent

  if (textContent == null) {
    return new Error();
  }
  
  return new Ok(textContent);
}
