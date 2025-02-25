import Dict from "../../../gleam_stdlib/dict.mjs";
import { new$ as set_new } from "../../../gleam_stdlib/gleam/set.mjs";
import { None } from "../../../gleam_stdlib/gleam/option.mjs";

const EMPTY_DICT = Dict.new();

export function empty_dict() {
  return EMPTY_DICT;
}

const EMPTY_SET = set_new();

export function empty_set() {
  return EMPTY_SET;
}

const OPTION_NONE = new None();

export function option_none() {
  return OPTION_NONE;
}
