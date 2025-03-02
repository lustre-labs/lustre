import { Gt, Lt, Eq } from "../../../gleam_stdlib/gleam/order.mjs";

const GT = new Gt();

const LT = new Lt();

const EQ = new Eq();

export function compare(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}
