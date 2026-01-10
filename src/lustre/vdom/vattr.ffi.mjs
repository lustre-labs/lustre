import {
  Order$Gt,
  Order$Lt,
  Order$Eq,
} from "../../../gleam_stdlib/gleam/order.mjs";

const GT = /* @__PURE__ */ Order$Gt();

const LT = /* @__PURE__ */ Order$Lt();

const EQ = /* @__PURE__ */ Order$Eq();

export function compare(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}
