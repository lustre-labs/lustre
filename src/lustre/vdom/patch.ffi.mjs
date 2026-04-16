import { prepend as $list_prepend } from "../../../gleam_stdlib/gleam/list.mjs";

export const addParent = (child, index) => {
  child.path = $list_prepend(child.path, child.index);
  child.index = index;

  return child;
};
