export function alert(message) {
  window.alert(message);
}

export function text_content(selector) {
  return document.querySelector(selector).textContent;
}
