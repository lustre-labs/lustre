import { Ok, Error } from "../../gleam.mjs";

export function make() {
  return new Map();
}

export function get(map, key) {
  const value = map.get(key);

  if (value) {
    return new Ok(value);
  } else {
    return new Error(undefined);
  }
}

export function insert(map, key, value) {
  map.set(key, value);

  return map;
}

export function size(map) {
  return map.size;
}
