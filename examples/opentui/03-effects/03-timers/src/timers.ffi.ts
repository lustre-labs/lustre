export function set_timeout(delay: number, cb: () => void): void {
  setTimeout(cb, delay);
}

export function get_current_time(): [number, number, number] {
  const now = new Date();
  return [now.getHours(), now.getMinutes(), now.getSeconds()];
}
