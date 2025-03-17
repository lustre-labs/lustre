import { Ok, Error } from "../../gleam.mjs";

export function empty() {
  return null;
}

export function from_list(list) {
  const map = new Map();

  for (list; list.tail; list = list.tail) {
    map.set(list.head[0], list.head[1]);
  }

  return map;
}

export function get(map, key) {
  const value = map?.get(key);

  if (value !== undefined) {
    return new Ok(value);
  } else {
    return new Error(undefined);
  }
}

export function insert(map, key, value) {
  map ??= new Map();
  map.set(key, value);

  return map;
}

export function size(map) {
  return map?.size ?? 0;
}
