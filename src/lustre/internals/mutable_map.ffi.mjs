import { Ok, Error } from "../../gleam.mjs";

export function empty() {
  return null;
}

export function get(map, key) {
  const value = map?.get(key);

  if (value != null) {
    return new Ok(value);
  } else {
    return new Error(undefined);
  }
}

export function has_key(map, key) {
  return map && map.has(key)
}

export function insert(map, key, value) {
  map ??= new Map();
  map.set(key, value);

  return map;
}

export function remove(map, key) {
  map?.delete(key);

  return map;
}

export function size(map) {
  return map?.size ?? 0;
}

