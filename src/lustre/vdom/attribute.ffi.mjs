import { Gt, Lt, Eq } from "../../../gleam_stdlib/gleam/order.mjs";

const GT = /* @__PURE__ */ new Gt();

const LT = /* @__PURE__ */ new Lt();

const EQ = /* @__PURE__ */ new Eq();

export function compare(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}
