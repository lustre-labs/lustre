export function setInterval(delay, cb) {
  return window.setInterval(cb, delay);
}

export function clearInterval(id) {
  window.clearInterval(id);
}
