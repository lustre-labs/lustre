import {
  List$NonEmpty,
  List$NonEmpty$rest,
  List$NonEmpty$first,
} from "../../gleam.mjs";
import { empty_list } from "./constants.mjs";

export const toList = (arr) =>
  arr.reduceRight((xs, x) => List$NonEmpty(x, xs), empty_list);

/** Our reconciler is written in such a way that it can work without modification
 *  both in typical client-side Lustre apps like SPAs and client components, but
 *  also in the server component runtime.
 *
 *  This is notable because the typical client runtimes are working directly with
 *  Gleam values, but the server component runtime is working with deserialised
 *  JSON.
 *
 *  The most immediate discrepancy is that Gleam uses linked lists but of course
 *  these are serialised as arrays in JSON. This function lets us iterate over
 *  both kinds of collection without dropping into Gleam's slow list iterator.
 *
 */
export const iterate = (list, callback) => {
  if (Array.isArray(list)) {
    for (let i = 0; i < list.length; i++) {
      callback(list[i]);
    }
  } else if (list) {
    for (list; List$NonEmpty$rest(list); list = List$NonEmpty$rest(list)) {
      callback(List$NonEmpty$first(list));
    }
  }
};
