import { List, Empty, NonEmpty } from "./gleam.mjs";
import Dict from "../gleam_stdlib/dict.mjs";
import { new$ as set_new } from "../gleam_stdlib/gleam/set.mjs";
import { Gt, Lt, Eq } from "../gleam_stdlib/gleam/order.mjs";

// This has been merged! https://github.com/gleam-lang/gleam/pull/4264
List.prototype.hasLength = function (n) {
  let ptr = this;
  while (n-- > 0 && ptr) ptr = ptr.tail;
  return ptr instanceof Empty;
};
List.prototype.atLeastLength = function (n) {
  let ptr = this;
  while (n-- > 0 && ptr) ptr = ptr.tail;
  return !!ptr;
};

const EMPTY = new Empty();
// I implemented the function differently for some superstition that walking the
// array in order is better, but the main thing here is to re-use an empty list.
List.fromArray = function (array, tail) {
  tail = tail || EMPTY;
  if (array.length === 0) return tail;

  let start = new NonEmpty(array[0]);
  let ptr = start;

  for (let i = 1; i < array.length; ++i) {
    ptr = ptr.tail = new NonEmpty(array[i]);
  }
  ptr.tail = tail;

  return start;
};

// hoist order.Order values.
const GT = new Gt();
const LT = new Lt();
const EQ = new Eq();
export const compare_attributes = (a, b) => {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
};
