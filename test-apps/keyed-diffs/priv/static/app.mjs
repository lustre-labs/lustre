// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array2, tail) {
    let t = tail || new Empty();
    for (let i = array2.length - 1; i >= 0; --i) {
      t = new NonEmpty(array2[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  // @internal
  countLength() {
    let length2 = 0;
    for (let _ of this)
      length2++;
    return length2;
  }
};
function prepend(element3, tail) {
  return new NonEmpty(element3, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index4) {
    return this.buffer[index4];
  }
  // @internal
  floatFromSlice(start3, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start3, end, isBigEndian);
  }
  // @internal
  intFromSlice(start3, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start3, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start3, end) {
    const buffer = new Uint8Array(
      this.buffer.buffer,
      this.buffer.byteOffset + start3,
      end - start3
    );
    return new _BitArray(buffer);
  }
  // @internal
  sliceAfter(index4) {
    const buffer = new Uint8Array(
      this.buffer.buffer,
      this.buffer.byteOffset + index4,
      this.buffer.byteLength - index4
    );
    return new _BitArray(buffer);
  }
};
var UtfCodepoint = class {
  constructor(value3) {
    this.value = value3;
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  const byteSize = end - start3;
  if (byteSize <= 6) {
    let value3 = 0;
    if (isBigEndian) {
      for (let i = start3; i < end; i++) {
        value3 = value3 * 256 + byteArray[i];
      }
    } else {
      for (let i = end - 1; i >= start3; i--) {
        value3 = value3 * 256 + byteArray[i];
      }
    }
    if (isSigned) {
      const highBit = 2 ** (byteSize * 8 - 1);
      if (value3 >= highBit) {
        value3 -= highBit * 2;
      }
    }
    return value3;
  } else {
    let value3 = 0n;
    if (isBigEndian) {
      for (let i = start3; i < end; i++) {
        value3 = (value3 << 8n) + BigInt(byteArray[i]);
      }
    } else {
      for (let i = end - 1; i >= start3; i--) {
        value3 = (value3 << 8n) + BigInt(byteArray[i]);
      }
    }
    if (isSigned) {
      const highBit = 1n << BigInt(byteSize * 8 - 1);
      if (value3 >= highBit) {
        value3 -= highBit * 2n;
      }
    }
    return Number(value3);
  }
}
function byteArrayToFloat(byteArray, start3, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start3;
  if (byteSize === 8) {
    return view2.getFloat64(start3, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start3, !isBigEndian);
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value3) {
    super();
    this[0] = value3;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values2 = [x, y];
  while (values2.length) {
    let a = values2.pop();
    let b = values2.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get2] = getters(a);
    for (let k of keys2(a)) {
      values2.push(get2(a, k), get2(b, k));
    }
  }
  return true;
}
function getters(object2) {
  if (object2 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object2 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Eq = class extends CustomType {
};
var Gt = class extends CustomType {
};

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function insert(dict2, key, value3) {
  return map_insert(key, value3, dict2);
}

// build/dev/javascript/gleam_stdlib/gleam/pair.mjs
function second(pair) {
  let a = pair[1];
  return a;
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
var Ascending = class extends CustomType {
};
var Descending = class extends CustomType {
};
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function reverse(list2) {
  return reverse_and_prepend(list2, toList([]));
}
function map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list2.head;
      let rest$1 = list2.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map(list2, fun) {
  return map_loop(list2, fun, toList([]));
}
function append_loop(loop$first, loop$second) {
  while (true) {
    let first2 = loop$first;
    let second2 = loop$second;
    if (first2.hasLength(0)) {
      return second2;
    } else {
      let first$1 = first2.head;
      let rest$1 = first2.tail;
      loop$first = rest$1;
      loop$second = prepend(first$1, second2);
    }
  }
}
function append(first2, second2) {
  return append_loop(reverse(first2), second2);
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list2 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list2.hasLength(0)) {
      return initial;
    } else {
      let first$1 = list2.head;
      let rest$1 = list2.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, first$1);
      loop$fun = fun;
    }
  }
}
function index_fold_loop(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index4 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index4);
      loop$with = with$;
      loop$index = index4 + 1;
    }
  }
}
function index_fold(list2, initial, fun) {
  return index_fold_loop(list2, initial, fun, 0);
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let compare4 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list2.hasLength(0)) {
      if (direction instanceof Ascending) {
        return prepend(reverse(growing$1), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list2.head;
      let rest$1 = list2.tail;
      let $ = compare4(prev, new$1);
      if ($ instanceof Gt && direction instanceof Descending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Lt && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Eq && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Gt && direction instanceof Ascending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(reverse(growing$1), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Lt && direction instanceof Descending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(reverse(growing$1), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(reverse(growing$1), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list3 = list2;
      return reverse_and_prepend(list3, acc);
    } else if (list2.hasLength(0)) {
      let list3 = list1;
      return reverse_and_prepend(list3, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      }
    }
  }
}
function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (sequences2.hasLength(0)) {
      return reverse(acc);
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return reverse(prepend(reverse(sequence), acc));
    } else {
      let ascending1 = sequences2.head;
      let ascending2 = sequences2.tail.head;
      let rest$1 = sequences2.tail.tail;
      let descending = merge_ascendings(
        ascending1,
        ascending2,
        compare4,
        toList([])
      );
      loop$sequences = rest$1;
      loop$compare = compare4;
      loop$acc = prepend(descending, acc);
    }
  }
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list3 = list2;
      return reverse_and_prepend(list3, acc);
    } else if (list2.hasLength(0)) {
      let list3 = list1;
      return reverse_and_prepend(list3, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      }
    }
  }
}
function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (sequences2.hasLength(0)) {
      return reverse(acc);
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return reverse(prepend(reverse(sequence), acc));
    } else {
      let descending1 = sequences2.head;
      let descending2 = sequences2.tail.head;
      let rest$1 = sequences2.tail.tail;
      let ascending = merge_descendings(
        descending1,
        descending2,
        compare4,
        toList([])
      );
      loop$sequences = rest$1;
      loop$compare = compare4;
      loop$acc = prepend(ascending, acc);
    }
  }
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences2 = loop$sequences;
    let direction = loop$direction;
    let compare4 = loop$compare;
    if (sequences2.hasLength(0)) {
      return toList([]);
    } else if (sequences2.hasLength(1) && direction instanceof Ascending) {
      let sequence = sequences2.head;
      return sequence;
    } else if (sequences2.hasLength(1) && direction instanceof Descending) {
      let sequence = sequences2.head;
      return reverse(sequence);
    } else if (direction instanceof Ascending) {
      let sequences$1 = merge_ascending_pairs(sequences2, compare4, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Descending();
      loop$compare = compare4;
    } else {
      let sequences$1 = merge_descending_pairs(sequences2, compare4, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Ascending();
      loop$compare = compare4;
    }
  }
}
function sort(list2, compare4) {
  if (list2.hasLength(0)) {
    return toList([]);
  } else if (list2.hasLength(1)) {
    let x = list2.head;
    return toList([x]);
  } else {
    let x = list2.head;
    let y = list2.tail.head;
    let rest$1 = list2.tail.tail;
    let direction = (() => {
      let $ = compare4(x, y);
      if ($ instanceof Lt) {
        return new Ascending();
      } else if ($ instanceof Eq) {
        return new Ascending();
      } else {
        return new Descending();
      }
    })();
    let sequences$1 = sequences(
      rest$1,
      compare4,
      toList([x]),
      direction,
      y,
      toList([])
    );
    return merge_all(sequences$1, new Ascending(), compare4);
  }
}
function shuffle_pair_unwrap_loop(loop$list, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return acc;
    } else {
      let elem_pair = list2.head;
      let enumerable = list2.tail;
      loop$list = enumerable;
      loop$acc = prepend(elem_pair[1], acc);
    }
  }
}
function do_shuffle_by_pair_indexes(list_of_pairs) {
  return sort(
    list_of_pairs,
    (a_pair, b_pair) => {
      return compare(a_pair[0], b_pair[0]);
    }
  );
}
function shuffle(list2) {
  let _pipe = list2;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, a) => {
      return prepend([random_uniform(), a], acc);
    }
  );
  let _pipe$2 = do_shuffle_by_pair_indexes(_pipe$1);
  return shuffle_pair_unwrap_loop(_pipe$2, toList([]));
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function split2(x, substring) {
  if (substring === "") {
    return graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = identity(_pipe);
    let _pipe$2 = split(_pipe$1, substring);
    return map(_pipe$2, identity);
  }
}
function inspect2(term) {
  let _pipe = inspect(term);
  return identity(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function is_ok(result) {
  if (!result.isOk()) {
    return false;
  } else {
    return true;
  }
}
function map2(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at2, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at2] = val;
  return out;
}
function spliceIn(arr, at2, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at2) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at2) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at2) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root, shift, hash, key, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: ARRAY_NODE,
        size: root.size,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root;
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function assocIndex(root, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  if ((root.bitmap & bit) !== 0) {
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root, shift, hash, key, val, addedLeaf) {
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key);
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size2 = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size2, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root.hash, shift),
      array: [root]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root, key) {
  const size2 = root.array.length;
  for (let i = 0; i < size2; i++) {
    if (isEqual(key, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root, key);
  }
}
function findArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return void 0;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return void 0;
  }
  return root.array[idx];
}
function without(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root, key);
  }
}
function withoutArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return root;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
  }
  if (n === void 0) {
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function withoutIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  return root;
}
function withoutCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return root;
  }
  if (root.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx)
  };
}
function forEach(root, fn) {
  if (root === void 0) {
    return;
  }
  const items = root.array;
  const size2 = items.length;
  for (let i = 0; i < size2; i++) {
    const item = items[i];
    if (item === void 0) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root, size2) {
    this.root = root;
    this.size = size2;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key, val) {
    const addedLeaf = { val: false };
    const root = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    try {
      this.forEach((v, k) => {
        if (!isEqual(o.get(k, !v), v)) {
          throw unequalDictSymbol;
        }
      });
      return true;
    } catch (e) {
      if (e === unequalDictSymbol) {
        return false;
      }
      throw e;
    }
  }
};
var unequalDictSymbol = Symbol();

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function to_string(term) {
  return term.toString();
}
function float_to_string(float3) {
  const string3 = float3.toString().replace("+", "");
  if (string3.indexOf(".") >= 0) {
    return string3;
  } else {
    const index4 = string3.indexOf("e");
    if (index4 >= 0) {
      return string3.slice(0, index4) + ".0" + string3.slice(index4);
    } else {
      return string3 + ".0";
    }
  }
}
function graphemes(string3) {
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string3.match(/./gsu));
  }
}
var segmenter = void 0;
function graphemes_iterator(string3) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string3)[Symbol.iterator]();
  }
}
function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}
function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}
function ends_with(haystack, needle) {
  return haystack.endsWith(needle);
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join("");
var trim_start_regex = new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = new RegExp(`[${unicode_whitespaces}]*$`);
function print_debug(string3) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string3 + "\n");
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string3 + "\n"));
  } else {
    console.log(string3);
  }
}
function random_uniform() {
  const random_uniform_result = Math.random();
  if (random_uniform_result === 1) {
    return random_uniform();
  }
  return random_uniform_result;
}
function new_map() {
  return Dict.new();
}
function map_get(map4, key) {
  const value3 = map4.get(key, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
}
function map_insert(key, value3, map4) {
  return map4.set(key, value3);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === void 0) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function inspect(v) {
  const t = typeof v;
  if (v === true)
    return "True";
  if (v === false)
    return "False";
  if (v === null)
    return "//js(null)";
  if (v === void 0)
    return "Nil";
  if (t === "string")
    return inspectString(v);
  if (t === "bigint" || Number.isInteger(v))
    return v.toString();
  if (t === "number")
    return float_to_string(v);
  if (Array.isArray(v))
    return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof List)
    return inspectList(v);
  if (v instanceof UtfCodepoint)
    return inspectUtfCodepoint(v);
  if (v instanceof BitArray)
    return inspectBitArray(v);
  if (v instanceof CustomType)
    return inspectCustomType(v);
  if (v instanceof Dict)
    return inspectDict(v);
  if (v instanceof Set)
    return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp)
    return `//js(${v})`;
  if (v instanceof Date)
    return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return inspectObject(v);
}
function inspectString(str) {
  let new_str = '"';
  for (let i = 0; i < str.length; i++) {
    let char = str[i];
    switch (char) {
      case "\n":
        new_str += "\\n";
        break;
      case "\r":
        new_str += "\\r";
        break;
      case "	":
        new_str += "\\t";
        break;
      case "\f":
        new_str += "\\f";
        break;
      case "\\":
        new_str += "\\\\";
        break;
      case '"':
        new_str += '\\"';
        break;
      default:
        if (char < " " || char > "~" && char < "\xA0") {
          new_str += "\\u{" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") + "}";
        } else {
          new_str += char;
        }
    }
  }
  new_str += '"';
  return new_str;
}
function inspectDict(map4) {
  let body = "dict.from_list([";
  let first2 = true;
  map4.forEach((value3, key) => {
    if (!first2)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value3) + ")";
    first2 = false;
  });
  return body + "])";
}
function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label) => {
    const value3 = inspect(record[label]);
    return isNaN(parseInt(label)) ? `${label}: ${value3}` : value3;
  }).join(", ");
  return props ? `${record.constructor.name}(${props})` : record.constructor.name;
}
function inspectList(list2) {
  return `[${list2.toArray().map(inspect).join(", ")}]`;
}
function inspectBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(", ")}>>`;
}
function inspectUtfCodepoint(codepoint2) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint2.value)})`;
}

// build/dev/javascript/gleam_stdlib/gleam/float.mjs
function compare(a, b) {
  let $ = a === b;
  if ($) {
    return new Eq();
  } else {
    let $1 = a < b;
    if ($1) {
      return new Lt();
    } else {
      return new Gt();
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/io.mjs
function debug(term) {
  let _pipe = term;
  let _pipe$1 = inspect2(_pipe);
  print_debug(_pipe$1);
  return term;
}

// build/dev/javascript/gleam_stdlib/gleam_stdlib_decode_ffi.mjs
function index2(data, key) {
  const int4 = Number.isInteger(key);
  if (data instanceof Dict || data instanceof WeakMap || data instanceof Map) {
    const token2 = {};
    const entry = data.get(key, token2);
    if (entry === token2)
      return new Ok(new None());
    return new Ok(new Some(entry));
  }
  if ((key === 0 || key === 1 || key === 2) && data instanceof List) {
    let i = 0;
    for (const value3 of data) {
      if (i === key)
        return new Ok(new Some(value3));
      i++;
    }
    return new Error("Indexable");
  }
  if (int4 && Array.isArray(data) || data && typeof data === "object" || data && Object.getPrototypeOf(data) === Object.prototype) {
    if (key in data)
      return new Ok(new Some(data[key]));
    return new Ok(new None());
  }
  return new Error(int4 ? "Indexable" : "Dict");
}
function int(data) {
  if (Number.isInteger(data))
    return new Ok(data);
  return new Error(0);
}
function string(data) {
  if (typeof data === "string")
    return new Ok(data);
  return new Error(0);
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic/decode.mjs
var DecodeError2 = class extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
};
var Decoder = class extends CustomType {
  constructor(function$) {
    super();
    this.function = function$;
  }
};
function run(data, decoder) {
  let $ = decoder.function(data);
  let maybe_invalid_data = $[0];
  let errors = $[1];
  if (errors.hasLength(0)) {
    return new Ok(maybe_invalid_data);
  } else {
    return new Error(errors);
  }
}
function success(data) {
  return new Decoder((_) => {
    return [data, toList([])];
  });
}
function map3(decoder, transformer) {
  return new Decoder(
    (d) => {
      let $ = decoder.function(d);
      let data = $[0];
      let errors = $[1];
      return [transformer(data), errors];
    }
  );
}
function run_decoders(loop$data, loop$failure, loop$decoders) {
  while (true) {
    let data = loop$data;
    let failure = loop$failure;
    let decoders = loop$decoders;
    if (decoders.hasLength(0)) {
      return failure;
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder.function(data);
      let layer = $;
      let errors = $[1];
      if (errors.hasLength(0)) {
        return layer;
      } else {
        loop$data = data;
        loop$failure = failure;
        loop$decoders = decoders$1;
      }
    }
  }
}
function one_of(first2, alternatives) {
  return new Decoder(
    (dynamic_data) => {
      let $ = first2.function(dynamic_data);
      let layer = $;
      let errors = $[1];
      if (errors.hasLength(0)) {
        return layer;
      } else {
        return run_decoders(dynamic_data, layer, alternatives);
      }
    }
  );
}
function run_dynamic_function(data, name, f) {
  let $ = f(data);
  if ($.isOk()) {
    let data$1 = $[0];
    return [data$1, toList([])];
  } else {
    let zero = $[0];
    return [
      zero,
      toList([new DecodeError2(name, classify_dynamic(data), toList([]))])
    ];
  }
}
function decode_int2(data) {
  return run_dynamic_function(data, "Int", int);
}
var int2 = /* @__PURE__ */ new Decoder(decode_int2);
function decode_string2(data) {
  return run_dynamic_function(data, "String", string);
}
var string2 = /* @__PURE__ */ new Decoder(decode_string2);
function push_path(layer, path) {
  let decoder = one_of(
    string2,
    toList([
      (() => {
        let _pipe = int2;
        return map3(_pipe, to_string);
      })()
    ])
  );
  let path$1 = map(
    path,
    (key) => {
      let key$1 = identity(key);
      let $ = run(key$1, decoder);
      if ($.isOk()) {
        let key$2 = $[0];
        return key$2;
      } else {
        return "<" + classify_dynamic(key$1) + ">";
      }
    }
  );
  let errors = map(
    layer[1],
    (error) => {
      let _record = error;
      return new DecodeError2(
        _record.expected,
        _record.found,
        append(path$1, error.path)
      );
    }
  );
  return [layer[0], errors];
}
function index3(loop$path, loop$position, loop$inner, loop$data, loop$handle_miss) {
  while (true) {
    let path = loop$path;
    let position = loop$position;
    let inner = loop$inner;
    let data = loop$data;
    let handle_miss = loop$handle_miss;
    if (path.hasLength(0)) {
      let _pipe = inner(data);
      return push_path(_pipe, reverse(position));
    } else {
      let key = path.head;
      let path$1 = path.tail;
      let $ = index2(data, key);
      if ($.isOk() && $[0] instanceof Some) {
        let data$1 = $[0][0];
        loop$path = path$1;
        loop$position = prepend(key, position);
        loop$inner = inner;
        loop$data = data$1;
        loop$handle_miss = handle_miss;
      } else if ($.isOk() && $[0] instanceof None) {
        return handle_miss(data, prepend(key, position));
      } else {
        let kind = $[0];
        let $1 = inner(data);
        let default$ = $1[0];
        let _pipe = [
          default$,
          toList([new DecodeError2(kind, classify_dynamic(data), toList([]))])
        ];
        return push_path(_pipe, reverse(position));
      }
    }
  }
}
function at(path, inner) {
  return new Decoder(
    (data) => {
      return index3(
        path,
        toList([]),
        inner.function,
        data,
        (data2, position) => {
          let $ = inner.function(data2);
          let default$ = $[0];
          let _pipe = [
            default$,
            toList([new DecodeError2("Field", "Nothing", toList([]))])
          ];
          return push_path(_pipe, reverse(position));
        }
      );
    }
  );
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
};
function new$() {
  return new Set2(new_map());
}
function contains(set, member) {
  let _pipe = set.dict;
  let _pipe$1 = map_get(_pipe, member);
  return is_ok(_pipe$1);
}
var token = void 0;
function insert2(set, member) {
  return new Set2(insert(set.dict, member, token));
}

// build/dev/javascript/lustre/lustre/internals/constants.ffi.mjs
var EMPTY_DICT = Dict.new();
function empty_dict() {
  return EMPTY_DICT;
}
var EMPTY_SET = new$();
function empty_set() {
  return EMPTY_SET;
}

// build/dev/javascript/lustre/lustre/internals/constants.mjs
var empty_list = /* @__PURE__ */ toList([]);
var option_none = /* @__PURE__ */ new None();

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
var none = /* @__PURE__ */ new Effect(/* @__PURE__ */ toList([]));

// build/dev/javascript/gleam_stdlib/gleam/function.mjs
function identity3(x) {
  return x;
}

// build/dev/javascript/lustre/lustre/vdom/attribute.ffi.mjs
var GT = new Gt();
var LT = new Lt();
var EQ = new Eq();
function compare3(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}

// build/dev/javascript/lustre/lustre/vdom/attribute.mjs
var Attribute = class extends CustomType {
  constructor(name, value3) {
    super();
    this.name = name;
    this.value = value3;
  }
};
var Property = class extends CustomType {
  constructor(name, value3) {
    super();
    this.name = name;
    this.value = value3;
  }
};
var Event = class extends CustomType {
  constructor(name, handler, include, prevent_default, stop_propagation, immediate) {
    super();
    this.name = name;
    this.handler = handler;
    this.include = include;
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.immediate = immediate;
  }
};
function merge(loop$attributes, loop$merged) {
  while (true) {
    let attributes = loop$attributes;
    let merged = loop$merged;
    if (attributes.hasLength(0)) {
      return merged;
    } else if (attributes.atLeastLength(2) && attributes.head instanceof Attribute && attributes.head.name === "class" && attributes.tail.head instanceof Attribute && attributes.tail.head.name === "class") {
      let class1 = attributes.head.value;
      let class2 = attributes.tail.head.value;
      let rest = attributes.tail.tail;
      let value3 = class1 + " " + class2;
      let attribute2 = new Attribute("class", value3);
      loop$attributes = prepend(attribute2, rest);
      loop$merged = merged;
    } else if (attributes.atLeastLength(2) && attributes.head instanceof Attribute && attributes.head.name === "style" && attributes.tail.head instanceof Attribute && attributes.tail.head.name === "style") {
      let style1 = attributes.head.value;
      let style2 = attributes.tail.head.value;
      let rest = attributes.tail.tail;
      let value3 = style1 + ";" + style2;
      let attribute2 = new Attribute("style", value3);
      loop$attributes = prepend(attribute2, rest);
      loop$merged = merged;
    } else {
      let attribute2 = attributes.head;
      let rest = attributes.tail;
      loop$attributes = rest;
      loop$merged = prepend(attribute2, merged);
    }
  }
}
function prepare(attributes) {
  let _pipe = attributes;
  let _pipe$1 = sort(_pipe, (a, b) => {
    return compare3(b, a);
  });
  return merge(_pipe$1, empty_list);
}

// build/dev/javascript/lustre/lustre/vdom/node.mjs
var Fragment = class extends CustomType {
  constructor(key, mapper, children, keyed_children, children_count) {
    super();
    this.key = key;
    this.mapper = mapper;
    this.children = children;
    this.keyed_children = keyed_children;
    this.children_count = children_count;
  }
};
var Element = class extends CustomType {
  constructor(key, mapper, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
    super();
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Text = class extends CustomType {
  constructor(key, mapper, content) {
    super();
    this.key = key;
    this.mapper = mapper;
    this.content = content;
  }
};

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attributes, children) {
  if (tag === "area") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "base") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "br") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "col") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "embed") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "hr") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "img") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "input") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "link") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "meta") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "param") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "source") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "track") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else if (tag === "wbr") {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      empty_list,
      empty_dict(),
      false,
      true
    );
  } else {
    return new Element(
      "",
      option_none,
      "",
      tag,
      prepare(attributes),
      children,
      empty_dict(),
      false,
      false
    );
  }
}
function text(content) {
  return new Text("", option_none, content);
}
function count_fragment_children(loop$children, loop$count) {
  while (true) {
    let children = loop$children;
    let count = loop$count;
    if (children.hasLength(0)) {
      return count;
    } else if (children.atLeastLength(1) && children.head instanceof Fragment) {
      let children_count = children.head.children_count;
      let rest = children.tail;
      loop$children = rest;
      loop$count = count + children_count;
    } else {
      let rest = children.tail;
      loop$children = rest;
      loop$count = count + 1;
    }
  }
}
function fragment(children) {
  if (children.hasLength(0)) {
    return new Fragment(
      "",
      option_none,
      toList([new Text("", option_none, "")]),
      empty_dict(),
      1
    );
  } else {
    return new Fragment(
      "",
      option_none,
      children,
      empty_dict(),
      count_fragment_children(children, 0)
    );
  }
}

// build/dev/javascript/lustre/lustre/internals/mutable_map.ffi.mjs
function make() {
  return /* @__PURE__ */ new Map();
}
function get(map4, key) {
  const value3 = map4.get(key);
  if (value3) {
    return new Ok(value3);
  } else {
    return new Error(void 0);
  }
}
function insert3(map4, key, value3) {
  map4.set(key, value3);
  return map4;
}
function size(map4) {
  return map4.size;
}

// build/dev/javascript/lustre/lustre/internals/mutable_map.mjs
function is_empty(map4) {
  return size(map4) === 0;
}

// build/dev/javascript/lustre/lustre/vdom/events.mjs
var Events = class extends CustomType {
  constructor(mapper, handlers, children) {
    super();
    this.mapper = mapper;
    this.handlers = handlers;
    this.children = children;
  }
};
function new$3(mapper) {
  return new Events(mapper, make(), make());
}
function attributes_to_handlers(loop$attributes, loop$mapper, loop$handlers) {
  while (true) {
    let attributes = loop$attributes;
    let mapper = loop$mapper;
    let handlers = loop$handlers;
    if (attributes.hasLength(0)) {
      return new Events(mapper, handlers, make());
    } else if (attributes.atLeastLength(1) && attributes.head instanceof Event) {
      let event2 = attributes.head;
      let rest = attributes.tail;
      let _pipe = handlers;
      let _pipe$1 = insert3(_pipe, event2.name, event2.handler);
      return ((_capture) => {
        return attributes_to_handlers(rest, mapper, _capture);
      })(_pipe$1);
    } else {
      let rest = attributes.tail;
      loop$attributes = rest;
      loop$mapper = mapper;
      loop$handlers = handlers;
    }
  }
}
function add_event_listener(events, name, handler) {
  let _record = events;
  return new Events(
    _record.mapper,
    insert3(events.handlers, name, handler),
    _record.children
  );
}
function add_child_events(parent, index4, child) {
  let _record = parent;
  return new Events(
    _record.mapper,
    _record.handlers,
    insert3(parent.children, to_string(index4), child)
  );
}
function add_keyed_child_events(parent, key, child) {
  let _record = parent;
  return new Events(
    _record.mapper,
    _record.handlers,
    insert3(parent.children, key, child)
  );
}
function do_handle(loop$events, loop$path, loop$name, loop$event) {
  while (true) {
    let events = loop$events;
    let path = loop$path;
    let name = loop$name;
    let event2 = loop$event;
    if (path.hasLength(0)) {
      let $ = get(events.handlers, name);
      if ($.isOk()) {
        let handler = $[0];
        let _pipe = event2;
        let _pipe$1 = run(_pipe, handler);
        return map2(_pipe$1, identity3(events.mapper));
      } else {
        return new Error(toList([]));
      }
    } else {
      let key = path.head;
      let path$1 = path.tail;
      let $ = get(events.children, key);
      if ($.isOk()) {
        let child = $[0];
        loop$events = child;
        loop$path = path$1;
        loop$name = name;
        loop$event = event2;
      } else {
        return new Error(toList([]));
      }
    }
  }
}
function handle(events, path, name, event2) {
  let _pipe = path;
  let _pipe$1 = split2(_pipe, ".");
  return ((_capture) => {
    return do_handle(events, _capture, name, event2);
  })(
    _pipe$1
  );
}
function is_empty2(events) {
  return is_empty(events.handlers) && is_empty(
    events.children
  );
}
function add_children(loop$parent, loop$mapper, loop$index, loop$children) {
  while (true) {
    let parent = loop$parent;
    let mapper = loop$mapper;
    let index4 = loop$index;
    let children = loop$children;
    if (children.hasLength(0)) {
      return parent;
    } else if (children.atLeastLength(1) && children.head instanceof Fragment) {
      let fragment2 = children.head;
      let rest = children.tail;
      let _pipe = parent;
      let _pipe$1 = add_child(_pipe, mapper, index4, fragment2);
      loop$parent = _pipe$1;
      loop$mapper = mapper;
      loop$index = index4 + fragment2.children_count;
      loop$children = rest;
    } else {
      let child = children.head;
      let rest = children.tail;
      let _pipe = parent;
      let _pipe$1 = add_child(_pipe, mapper, index4, child);
      loop$parent = _pipe$1;
      loop$mapper = mapper;
      loop$index = index4 + 1;
      loop$children = rest;
    }
  }
}
function add_child(parent, mapper, index4, child) {
  if (child instanceof Element) {
    let key = child.key;
    let attributes = child.attributes;
    let children = child.children;
    let composed_mapper = (() => {
      let $ = child.mapper;
      if ($ instanceof None) {
        return mapper;
      } else {
        let child_mapper = $[0];
        return (msg) => {
          let _pipe = msg;
          let _pipe$1 = child_mapper(_pipe);
          return mapper(_pipe$1);
        };
      }
    })();
    let child_events = (() => {
      let _pipe = attributes;
      let _pipe$1 = attributes_to_handlers(
        _pipe,
        composed_mapper,
        parent.handlers
      );
      return add_children(_pipe$1, composed_mapper, index4, children);
    })();
    let _record = parent;
    return new Events(
      _record.mapper,
      _record.handlers,
      insert3(
        parent.children,
        (() => {
          if (key === "") {
            return to_string(index4);
          } else {
            let key$1 = key;
            return key$1;
          }
        })(),
        child_events
      )
    );
  } else if (child instanceof Fragment) {
    let children = child.children;
    let composed_mapper = (() => {
      let $ = child.mapper;
      if ($ instanceof None) {
        return mapper;
      } else {
        let child_mapper = $[0];
        return (msg) => {
          let _pipe = msg;
          let _pipe$1 = child_mapper(_pipe);
          return mapper(_pipe$1);
        };
      }
    })();
    return index_fold(
      children,
      parent,
      (parent2, child2, i) => {
        return add_child(parent2, composed_mapper, index4 + i, child2);
      }
    );
  } else {
    return parent;
  }
}

// build/dev/javascript/lustre/lustre/vdom/diff.mjs
var Diff = class extends CustomType {
  constructor(patch, events) {
    super();
    this.patch = patch;
    this.events = events;
  }
};
var Patch = class extends CustomType {
  constructor(index4, removed, changes, children) {
    super();
    this.index = index4;
    this.removed = removed;
    this.changes = changes;
    this.children = children;
  }
};
var Replace = class extends CustomType {
  constructor(element3) {
    super();
    this.element = element3;
  }
};
var ReplaceText = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Update = class extends CustomType {
  constructor(added, removed) {
    super();
    this.added = added;
    this.removed = removed;
  }
};
var Insert = class extends CustomType {
  constructor(child, before) {
    super();
    this.child = child;
    this.before = before;
  }
};
var Move = class extends CustomType {
  constructor(key, before, count) {
    super();
    this.key = key;
    this.before = before;
    this.count = count;
  }
};
var RemoveKey = class extends CustomType {
  constructor(key, count) {
    super();
    this.key = key;
    this.count = count;
  }
};
var InsertMany = class extends CustomType {
  constructor(children, before) {
    super();
    this.children = children;
    this.before = before;
  }
};
var Remove = class extends CustomType {
  constructor(from, count) {
    super();
    this.from = from;
    this.count = count;
  }
};
var AttributeChange = class extends CustomType {
  constructor(added, removed, events) {
    super();
    this.added = added;
    this.removed = removed;
    this.events = events;
  }
};
function offset_root_patch(root, offset) {
  let changes = map(
    root.changes,
    (change) => {
      if (change instanceof Insert) {
        let before = change.before;
        let _record2 = change;
        return new Insert(_record2.child, before + offset);
      } else if (change instanceof InsertMany) {
        let before = change.before;
        let _record2 = change;
        return new InsertMany(_record2.children, before + offset);
      } else if (change instanceof Move) {
        let before = change.before;
        let _record2 = change;
        return new Move(_record2.key, before + offset, _record2.count);
      } else if (change instanceof Remove) {
        let from = change.from;
        let _record2 = change;
        return new Remove(from + offset, _record2.count);
      } else if (change instanceof Replace) {
        return change;
      } else if (change instanceof ReplaceText) {
        return change;
      } else if (change instanceof Update) {
        return change;
      } else {
        return change;
      }
    }
  );
  let children = map(
    root.children,
    (child) => {
      let _record2 = child;
      return new Patch(
        child.index + offset,
        _record2.removed,
        _record2.changes,
        _record2.children
      );
    }
  );
  let _record = root;
  return new Patch(_record.index, _record.removed, changes, children);
}
function diff_attributes(loop$old, loop$new, loop$added, loop$removed, loop$events) {
  while (true) {
    let old = loop$old;
    let new$4 = loop$new;
    let added = loop$added;
    let removed = loop$removed;
    let events = loop$events;
    if (old.hasLength(0) && new$4.hasLength(0)) {
      return new AttributeChange(added, removed, events);
    } else if (old.atLeastLength(1) && new$4.hasLength(0)) {
      let prev = old.head;
      let old$1 = old.tail;
      loop$old = old$1;
      loop$new = new$4;
      loop$added = added;
      loop$removed = prepend(prev, removed);
      loop$events = events;
    } else if (old.hasLength(0) && new$4.atLeastLength(1) && new$4.head instanceof Event) {
      let next = new$4.head;
      let new$1 = new$4.tail;
      loop$old = old;
      loop$new = new$1;
      loop$added = prepend(next, added);
      loop$removed = removed;
      loop$events = add_event_listener(events, next.name, next.handler);
    } else if (old.hasLength(0) && new$4.atLeastLength(1)) {
      let next = new$4.head;
      let new$1 = new$4.tail;
      loop$old = old;
      loop$new = new$1;
      loop$added = prepend(next, added);
      loop$removed = removed;
      loop$events = events;
    } else {
      let prev = old.head;
      let remaining_old = old.tail;
      let next = new$4.head;
      let remaining_new = new$4.tail;
      let $ = compare3(prev, next);
      if (prev instanceof Attribute && $ instanceof Eq && next instanceof Attribute) {
        let $1 = next.name;
        if ($1 === "value") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "checked") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "selected") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if (prev.value === next.value) {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added;
          loop$removed = removed;
          loop$events = events;
        } else {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        }
      } else if (prev instanceof Property && $ instanceof Eq && next instanceof Property) {
        let $1 = next.name;
        if ($1 === "value") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "checked") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "selected") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "scrollLeft") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if ($1 === "scrollRight") {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        } else if (isEqual(prev.value, next.value)) {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added;
          loop$removed = removed;
          loop$events = events;
        } else {
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = removed;
          loop$events = events;
        }
      } else if (prev instanceof Event && $ instanceof Eq && next instanceof Event) {
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = (() => {
          let $1 = prev.prevent_default !== next.prevent_default || prev.stop_propagation !== next.stop_propagation || prev.immediate !== next.immediate;
          if ($1) {
            return prepend(next, added);
          } else {
            return added;
          }
        })();
        loop$removed = removed;
        loop$events = add_event_listener(
          events,
          next.name,
          next.handler
        );
      } else if ($ instanceof Eq && next instanceof Event) {
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = prepend(next, added);
        loop$removed = removed;
        loop$events = add_event_listener(
          events,
          next.name,
          next.handler
        );
      } else if ($ instanceof Eq) {
        loop$old = remaining_old;
        loop$new = remaining_new;
        loop$added = prepend(next, added);
        loop$removed = prepend(prev, removed);
        loop$events = events;
      } else if ($ instanceof Gt) {
        loop$old = old;
        loop$new = remaining_new;
        loop$added = prepend(next, added);
        loop$removed = removed;
        loop$events = events;
      } else {
        loop$old = remaining_old;
        loop$new = new$4;
        loop$added = added;
        loop$removed = prepend(prev, removed);
        loop$events = events;
      }
    }
  }
}
function advance(node) {
  if (node instanceof Fragment) {
    let children_count = node.children_count;
    return children_count;
  } else {
    return 1;
  }
}
function do_diff(loop$old, loop$old_keyed, loop$new, loop$new_keyed, loop$moved, loop$moved_offset, loop$removed, loop$node_index, loop$patch_index, loop$changes, loop$children, loop$events, loop$mapper) {
  while (true) {
    let old = loop$old;
    let old_keyed = loop$old_keyed;
    let new$4 = loop$new;
    let new_keyed = loop$new_keyed;
    let moved = loop$moved;
    let moved_offset = loop$moved_offset;
    let removed = loop$removed;
    let node_index = loop$node_index;
    let patch_index = loop$patch_index;
    let changes = loop$changes;
    let children = loop$children;
    let events = loop$events;
    let mapper = loop$mapper;
    if (old.hasLength(0) && new$4.hasLength(0)) {
      return new Diff(
        new Patch(patch_index, removed, changes, children),
        events
      );
    } else if (old.atLeastLength(1) && new$4.hasLength(0)) {
      let prev = old.head;
      let old$1 = old.tail;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$4;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = (() => {
        let $ = prev.key === "" || !contains(moved, prev.key);
        if ($) {
          return removed + advance(prev);
        } else {
          return removed;
        }
      })();
      loop$node_index = node_index;
      loop$patch_index = patch_index;
      loop$changes = changes;
      loop$children = children;
      loop$events = events;
      loop$mapper = mapper;
    } else if (old.hasLength(0) && new$4.atLeastLength(1)) {
      let events$1 = add_children(
        events,
        mapper,
        node_index - moved_offset,
        new$4
      );
      let insert5 = new InsertMany(new$4, node_index - moved_offset);
      let changes$1 = prepend(insert5, changes);
      return new Diff(
        new Patch(patch_index, removed, changes$1, children),
        events$1
      );
    } else if (old.atLeastLength(1) && new$4.atLeastLength(1) && old.head.key !== new$4.head.key) {
      let prev = old.head;
      let old_remaining = old.tail;
      let next = new$4.head;
      let new_remaining = new$4.tail;
      let next_did_exist = map_get(old_keyed, next.key);
      let prev_does_exist = map_get(new_keyed, prev.key);
      let prev_has_moved = contains(moved, prev.key);
      if (prev_does_exist.isOk() && next_did_exist.isOk() && prev_has_moved) {
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new$4;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset - advance(prev);
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$changes = changes;
        loop$children = children;
        loop$events = events;
        loop$mapper = mapper;
      } else if (prev_does_exist.isOk() && next_did_exist.isOk()) {
        let match = next_did_exist[0];
        let count = advance(next);
        let before = node_index - moved_offset;
        let move2 = new Move(next.key, before, count);
        let changes$1 = prepend(move2, changes);
        let moved$1 = insert2(moved, next.key);
        let moved_offset$1 = moved_offset + count;
        loop$old = prepend(match, old);
        loop$old_keyed = old_keyed;
        loop$new = new$4;
        loop$new_keyed = new_keyed;
        loop$moved = moved$1;
        loop$moved_offset = moved_offset$1;
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$changes = changes$1;
        loop$children = children;
        loop$events = events;
        loop$mapper = mapper;
      } else if (!prev_does_exist.isOk() && next_did_exist.isOk()) {
        let count = advance(prev);
        let moved_offset$1 = moved_offset - count;
        let remove2 = new RemoveKey(prev.key, count);
        let changes$1 = prepend(remove2, changes);
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new$4;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset$1;
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$changes = changes$1;
        loop$children = children;
        loop$events = events;
        loop$mapper = mapper;
      } else if (prev_does_exist.isOk() && !next_did_exist.isOk()) {
        let before = node_index - moved_offset;
        let count = advance(next);
        let events$1 = add_child(events, mapper, node_index, next);
        let insert5 = new Insert(next, before);
        let changes$1 = prepend(insert5, changes);
        loop$old = old;
        loop$old_keyed = old_keyed;
        loop$new = new_remaining;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset + count;
        loop$removed = removed;
        loop$node_index = node_index + count;
        loop$patch_index = patch_index;
        loop$changes = changes$1;
        loop$children = children;
        loop$events = events$1;
        loop$mapper = mapper;
      } else {
        let prev_count = advance(prev);
        let changes$1 = (() => {
          let $ = prev_count > 1;
          if (!$) {
            return changes;
          } else {
            let from = node_index - moved_offset + 1;
            let remove2 = new Remove(from, prev_count - 1);
            return prepend(remove2, changes);
          }
        })();
        let child = new Patch(
          node_index,
          0,
          toList([new Replace(next)]),
          empty_list
        );
        let events$1 = add_child(events, mapper, node_index, next);
        loop$old = old_remaining;
        loop$old_keyed = old_keyed;
        loop$new = new_remaining;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset - prev_count + 1;
        loop$removed = removed;
        loop$node_index = node_index + 1;
        loop$patch_index = patch_index;
        loop$changes = changes$1;
        loop$children = prepend(child, children);
        loop$events = events$1;
        loop$mapper = mapper;
      }
    } else if (old.atLeastLength(1) && old.head instanceof Fragment && new$4.atLeastLength(1) && new$4.head instanceof Fragment) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$4.head;
      let new$1 = new$4.tail;
      let prev_count = prev.children_count;
      let next_count = next.children_count;
      let changes$1 = (() => {
        let $ = prev_count - next_count;
        if ($ > 0) {
          let remove_count = $;
          let remove_from = node_index + next_count - moved_offset;
          let remove2 = new Remove(remove_from, remove_count);
          return prepend(remove2, changes);
        } else {
          return changes;
        }
      })();
      let composed_mapper = (() => {
        let $ = next.mapper;
        if ($ instanceof Some) {
          let child_mapper = $[0];
          return (msg) => {
            let _pipe = msg;
            let _pipe$1 = child_mapper(_pipe);
            return mapper(_pipe$1);
          };
        } else {
          return mapper;
        }
      })();
      let child = do_diff(
        prev.children,
        prev.keyed_children,
        next.children,
        next.keyed_children,
        empty_set(),
        moved_offset,
        0,
        node_index,
        -1,
        changes$1,
        children,
        events,
        composed_mapper
      );
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset + next_count - prev_count;
      loop$removed = removed;
      loop$node_index = node_index + next_count;
      loop$patch_index = patch_index;
      loop$changes = child.patch.changes;
      loop$children = child.patch.children;
      loop$events = child.events;
      loop$mapper = mapper;
    } else if (old.atLeastLength(1) && old.head instanceof Element && new$4.atLeastLength(1) && new$4.head instanceof Element && (old.head.namespace === new$4.head.namespace && old.head.tag === new$4.head.tag)) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$4.head;
      let new$1 = new$4.tail;
      let composed_mapper = (() => {
        let $ = next.mapper;
        if ($ instanceof Some) {
          let child_mapper = $[0];
          return (msg) => {
            let _pipe = msg;
            let _pipe$1 = child_mapper(_pipe);
            return mapper(_pipe$1);
          };
        } else {
          return mapper;
        }
      })();
      let attribute_change = diff_attributes(
        prev.attributes,
        next.attributes,
        empty_list,
        empty_list,
        new$3(composed_mapper)
      );
      let initial_child_changes = (() => {
        let $ = attribute_change.added;
        let $1 = attribute_change.removed;
        if ($.hasLength(0) && $1.hasLength(0)) {
          return empty_list;
        } else {
          let added = $;
          let removed$1 = $1;
          return toList([new Update(added, removed$1)]);
        }
      })();
      let child = do_diff(
        prev.children,
        prev.keyed_children,
        next.children,
        next.keyed_children,
        empty_set(),
        0,
        0,
        0,
        node_index,
        initial_child_changes,
        empty_list,
        attribute_change.events,
        composed_mapper
      );
      let children$1 = (() => {
        let $ = child.patch;
        if ($ instanceof Patch && $.removed === 0 && $.changes.hasLength(0) && $.children.hasLength(0)) {
          return children;
        } else {
          return prepend(child.patch, children);
        }
      })();
      let events$1 = (() => {
        let $ = is_empty2(child.events);
        if ($) {
          return events;
        } else if (!$ && next.key !== "") {
          return add_keyed_child_events(events, next.key, child.events);
        } else {
          return add_child_events(events, node_index, child.events);
        }
      })();
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$changes = changes;
      loop$children = children$1;
      loop$events = events$1;
      loop$mapper = mapper;
    } else if (old.atLeastLength(1) && old.head instanceof Text && new$4.atLeastLength(1) && new$4.head instanceof Text && old.head.content === new$4.head.content) {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$4.head;
      let new$1 = new$4.tail;
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$changes = changes;
      loop$children = children;
      loop$events = events;
      loop$mapper = mapper;
    } else if (old.atLeastLength(1) && old.head instanceof Text && new$4.atLeastLength(1) && new$4.head instanceof Text) {
      let old$1 = old.tail;
      let next = new$4.head;
      let new$1 = new$4.tail;
      let child = new Patch(
        node_index,
        0,
        toList([new ReplaceText(next.content)]),
        empty_list
      );
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$changes = changes;
      loop$children = prepend(child, children);
      loop$events = events;
      loop$mapper = mapper;
    } else {
      let prev = old.head;
      let old$1 = old.tail;
      let next = new$4.head;
      let new$1 = new$4.tail;
      let prev_count = advance(prev);
      let changes$1 = (() => {
        let $ = prev_count > 1;
        if (!$) {
          return changes;
        } else {
          let from = node_index - moved_offset + 1;
          let remove2 = new Remove(from, prev_count - 1);
          return prepend(remove2, changes);
        }
      })();
      let child = new Patch(
        node_index,
        0,
        toList([new Replace(next)]),
        empty_list
      );
      let events$1 = add_child(events, mapper, node_index, next);
      loop$old = old$1;
      loop$old_keyed = old_keyed;
      loop$new = new$1;
      loop$new_keyed = new_keyed;
      loop$moved = moved;
      loop$moved_offset = moved_offset - prev_count + 1;
      loop$removed = removed;
      loop$node_index = node_index + 1;
      loop$patch_index = patch_index;
      loop$changes = changes$1;
      loop$children = prepend(child, children);
      loop$events = events$1;
      loop$mapper = mapper;
    }
  }
}
function diff(old, new$4, initial_element_offset) {
  let diff$1 = do_diff(
    toList([old]),
    empty_dict(),
    toList([new$4]),
    empty_dict(),
    empty_set(),
    0,
    0,
    0,
    0,
    empty_list,
    empty_list,
    new$3(identity3),
    identity3
  );
  let $ = initial_element_offset > 0;
  if (!$) {
    return diff$1;
  } else {
    let _pipe = diff$1.patch;
    let _pipe$1 = offset_root_patch(_pipe, initial_element_offset);
    return ((_capture) => {
      return new Diff(_capture, diff$1.events);
    })(
      _pipe$1
    );
  }
}

// build/dev/javascript/lustre/reconciler.ffi.mjs
var meta = Symbol("metadata");
var LustreReconciler = class {
  #root = null;
  #dispatch = () => {
  };
  #stack = [];
  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
  }
  mount(vnode) {
    this.#root.appendChild(createElement(vnode, this.#dispatch, this.#root));
  }
  push(patch) {
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }
  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();
      for (let list2 = patch.changes; list2.tail; list2 = list2.tail) {
        const change = list2.head;
        switch (change.constructor) {
          case InsertMany:
            insertMany(
              node,
              change.children,
              change.before,
              this.#dispatch,
              this.#root
            );
            break;
          case Insert:
            insert4(
              node,
              change.child,
              change.before,
              this.#dispatch,
              this.#root
            );
            break;
          case Move:
            move(node, change.key, change.before, change.count);
            break;
          case RemoveKey:
            removeKey(node, change.key, change.count);
            break;
          case Remove:
            remove(node, change.from, change.count);
            break;
          case Replace:
            replace2(node, change.element, this.#dispatch, this.#root);
            break;
          case ReplaceText:
            replaceText(node, change.content);
            break;
          case Update:
            update(
              node,
              change.added,
              change.removed,
              this.#dispatch,
              this.#root
            );
            break;
        }
      }
      while (patch.remove_count-- > 0) {
        const child = node.lastChild;
        const key = child[meta].key;
        if (key) {
          node[meta].keyedChildren.delete(key);
        }
        node.removeChild(child);
      }
      for (let list2 = patch.children; list2.tail; list2 = list2.tail) {
        const child = list2.head;
        this.#stack.push({
          node: node.childNodes[child.index],
          patch: child
        });
      }
    }
  }
};
function insertMany(node, children, before, dispatch, root) {
  const fragment2 = document.createDocumentFragment();
  for (let list2 = children; list2.tail; list2 = list2.tail) {
    const child = list2.head;
    const el = createElement(child, dispatch, root);
    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
    }
    fragment2.appendChild(el);
  }
  node.insertBefore(fragment2, node.childNodes[before]);
}
function insert4(node, child, before, dispatch, root) {
  const el = createElement(child, dispatch, root);
  if (child.key) {
    node[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }
  node.insertBefore(el, node.childNodes[before]);
}
function move(node, key, before, count) {
  let el = node[meta].keyedChildren.get(key).deref();
  if (count > 1) {
    const fragment2 = document.createDocumentFragment();
    for (let i = 0; i < count && el !== null; ++i) {
      let next = el.nextSibling;
      fragment2.append(el);
      el = next;
    }
    el = fragment2;
  }
  node.insertBefore(el, node.childNodes[before]);
}
function removeKey(node, key, count) {
  let el = node[meta].keyedChildren.get(key).deref();
  node[meta].keyedChildren.delete(key);
  while (count-- > 0 && el !== null) {
    let next = el.nextSibling;
    node.removeChild(el);
    el = next;
  }
}
function remove(node, from, count) {
  let el = node.childNodes[from];
  while (count-- > 0 && el !== null) {
    const next = el.nextSibling;
    node.removeChild(el);
    el = next;
  }
}
function replace2(node, child, dispatch, root) {
  const el = createElement(child, dispatch, root);
  const parent = node.parentNode;
  if (child.key) {
    parent[meta].keyedChildren.set(child.key, new WeakRef(unwrapFragment(el)));
  }
  parent.replaceChild(el, node);
}
function replaceText(node, content) {
  node.data = content;
}
function update(node, added, removed, dispatch, root) {
  for (let list2 = removed; list2.tail; list2 = list2.tail) {
    const name = list2.head.name;
    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
    }
  }
  for (let list2 = added; list2.tail; list2 = list2.tail) {
    createAttribute(node, list2.head, dispatch, root);
  }
}
function unwrapFragment(node) {
  while (node.nodeType === DocumentFragment.DOCUMENT_FRAGMENT_NODE) {
    node = node.firstChild;
  }
  return node;
}
function createElement(vnode, dispatch, root) {
  switch (vnode.constructor) {
    case Element: {
      const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
      node[meta] = {
        key: vnode.key,
        keyedChildren: /* @__PURE__ */ new Map(),
        handlers: /* @__PURE__ */ new Map()
      };
      for (let list2 = vnode.attributes; list2.tail; list2 = list2.tail) {
        const attribute2 = list2.head;
        createAttribute(node, attribute2, dispatch, root);
      }
      insertMany(node, vnode.children, 0, dispatch, root);
      return node;
    }
    case Text: {
      const node = document.createTextNode(vnode.content);
      node[meta] = { constructor: Text, key: vnode.key };
      return node;
    }
    case Fragment: {
      const node = document.createDocumentFragment();
      for (let list2 = vnode.children; list2.tail; list2 = list2.tail) {
        const child = list2.head;
        node.appendChild(createElement(child, dispatch, root));
      }
      return node;
    }
  }
}
function createAttribute(node, attribute2, dispatch, root) {
  switch (attribute2.constructor) {
    case Attribute:
      if (attribute2.value !== node.getAttribute(attribute2.name)) {
        node.setAttribute(attribute2.name, attribute2.value);
        if (SYNCED_ATTRIBUTES.includes(attribute2.name)) {
          node[attribute2.name] = attribute2.value;
        }
      }
      break;
    case Property:
      node[attribute2.name] = attribute2.value;
      break;
    case Event:
      if (!node[meta].handlers.has(attribute2.name)) {
        node.addEventListener(attribute2.name, handleEvent, {
          passive: !attribute2.prevent_default
        });
      }
      const prevent = attribute2.prevent_default;
      const stop = attribute2.stop_propagation;
      const immediate = attribute2.immediate || IMMEDIATE_EVENTS.includes(attribute2.name);
      node[meta].handlers.set(attribute2.name, (event2) => {
        if (prevent)
          event2.preventDefault();
        if (stop)
          event2.stopPropagation();
        let node2 = event2.target;
        let path = node2[meta].key || Array.from(node2.parentNode.childNodes).indexOf(node2);
        node2 = node2.parentNode;
        while (node2 !== root) {
          console.log(node2, root);
          const key = node2[meta].key;
          const index4 = Array.from(node2.parentNode.childNodes).indexOf(node2);
          path = key ? `${key}.${path}` : `${index4}.${path}`;
          node2 = node2.parentNode;
        }
        dispatch(event2, path, event2.type, immediate);
      });
      break;
  }
}
function handleEvent(event2) {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  handler(event2);
}
var SYNCED_ATTRIBUTES = ["checked", "disabled", "selected", "value"];
var IMMEDIATE_EVENTS = [
  // Input synchronization
  "input",
  "change",
  // Focus management
  "focusin",
  "focusout",
  "focus",
  "blur",
  // Text selection
  "select"
];

// build/dev/javascript/lustre/runtime.ffi.mjs
var is_browser = () => globalThis.window && window.document;
var LustreSPA = class _LustreSPA {
  static start({ init: init2, update: update3, view: view2 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreSPA(root, init2(flags), update3, view2);
    return new Ok((action) => app.send(action));
  }
  #runtime;
  constructor(root, [init2, effects], update3, view2) {
    this.#runtime = new LustreClientRuntime(
      root,
      [init2, effects],
      view2,
      update3
    );
  }
  send(action) {
  }
};
var start = LustreSPA.start;
var LustreClientRuntime = class {
  #root;
  #model;
  #view;
  #update;
  #vdom;
  #events;
  #reconciler;
  #viewTimer = null;
  initialNodeOffset = 0;
  constructor(root, [model, effects], view2, update3) {
    this.#root = root;
    this.#model = model;
    this.#view = view2;
    this.#update = update3;
    this.#vdom = this.#view(this.#model);
    this.#events = add_child(new$3(), (msg) => msg, 0, this.#vdom);
    this.#reconciler = new LustreReconciler(
      this.#root,
      (event2, id, immediate) => {
        this.#handleEvent(event2, id, immediate);
      }
    );
    this.#reconciler.mount(this.#vdom);
    this.#tick(effects.all, false);
  }
  dispatch(msg, immediate = false) {
    const [model, effects] = this.#update(this.#model, msg);
    this.#model = model;
    this.#tick(effects.all, immediate);
  }
  #handleEvent(event2, path, name, immediate) {
    console.log({ path, name });
    const msg = handle(this.#events, path, name, event2);
    console.log({ msg });
    if (msg.isOk()) {
      this.dispatch(msg[0], immediate);
    }
  }
  #tick(effects, immediate = false) {
    const queue = [];
    const effect_params = {
      root: this.#root,
      emit: (event2, data) => this.#emit(event2, data),
      dispatch: (msg) => queue.push(msg),
      select: () => {
      }
    };
    while (true) {
      for (let effect = effects; effect.tail; effect = effect.tail) {
        effect.head(effect_params);
      }
      if (!queue.length) {
        break;
      }
      const msg = queue.shift();
      [this.#model, effects] = this.#update(this.#model, msg);
    }
    this.#render();
  }
  #render() {
    this.#viewTimer = null;
    const next = this.#view(this.#model);
    const { patch, events } = diff(
      this.#vdom,
      next,
      this.#events,
      this.initialNodeOffset
    );
    this.#events = events;
    this.#vdom = next;
    this.#reconciler.push(patch, this.#events);
  }
  #emit(event2, data) {
    const targetElement = this.#root.host ?? this.#root;
    targetElement.dispatchEvent(
      new CustomEvent(event2, {
        detail: data,
        bubbles: true,
        composed: true
      })
    );
  }
};

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init2, update3, view2, on_attribute_change) {
    super();
    this.init = init2;
    this.update = update3;
    this.view = view2;
    this.on_attribute_change = on_attribute_change;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init2, update3, view2) {
  return new App(init2, update3, view2, new None());
}
function simple(init2, update3, view2) {
  let init$1 = (flags) => {
    return [init2(flags), none];
  };
  let update$1 = (model, msg) => {
    return [update3(model, msg), none];
  };
  return application(init$1, update$1, view2);
}
function start2(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value3) {
  return new Attribute(name, value3);
}
function on(name, handler) {
  return new Event(name, handler, empty_list, false, false, false);
}
function style(properties) {
  return attribute(
    "style",
    fold(
      properties,
      "",
      (styles, _use1) => {
        let name$1 = _use1[0];
        let value$1 = _use1[1];
        return styles + name$1 + ":" + value$1 + ";";
      }
    )
  );
}
function value(val) {
  return attribute("value", val);
}
function placeholder(text3) {
  return attribute("placeholder", text3);
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function div(attrs, children) {
  return element("div", attrs, children);
}
function br(attrs) {
  return element("br", attrs, empty_list);
}
function button(attrs, children) {
  return element("button", attrs, children);
}
function input(attrs) {
  return element("input", attrs, empty_list);
}
function textarea(attrs, content) {
  return element("textarea", attrs, toList([text(content)]));
}

// build/dev/javascript/lustre/lustre/element/keyed.mjs
function key_element(key, element3) {
  if (element3 instanceof Fragment) {
    let _record = element3;
    return new Fragment(
      key,
      _record.mapper,
      _record.children,
      _record.keyed_children,
      _record.children_count
    );
  } else if (element3 instanceof Element) {
    let _record = element3;
    return new Element(
      key,
      _record.mapper,
      _record.namespace,
      _record.tag,
      _record.attributes,
      _record.children,
      _record.keyed_children,
      _record.self_closing,
      _record.void
    );
  } else {
    let _record = element3;
    return new Text(key, _record.mapper, _record.content);
  }
}
function extract_keyed_children(children) {
  let init2 = [empty_dict(), empty_list, 0];
  let $ = fold(
    children,
    init2,
    (_use0, _use1) => {
      let keyed_children2 = _use0[0];
      let children$12 = _use0[1];
      let children_count2 = _use0[2];
      let key = _use1[0];
      let element$1 = _use1[1];
      let keyed_element = key_element(key, element$1);
      let keyed_children$1 = (() => {
        if (key === "") {
          return keyed_children2;
        } else {
          return insert(keyed_children2, key, keyed_element);
        }
      })();
      return [
        keyed_children$1,
        prepend(keyed_element, children$12),
        children_count2 + 1
      ];
    }
  );
  let keyed_children = $[0];
  let children$1 = $[1];
  let children_count = $[2];
  return [keyed_children, reverse(children$1), children_count];
}
function element2(tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return new Element(
    "",
    option_none,
    "",
    tag,
    prepare(attributes),
    children$1,
    keyed_children,
    false,
    false
  );
}
function div2(attributes, children) {
  return element2("div", attributes, children);
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function on_click(msg) {
  return on2("click", success(msg));
}
function value2() {
  return at(toList(["target", "value"]), string2);
}
function on_input(msg) {
  return on2(
    "input",
    (() => {
      let _pipe = value2();
      return map3(_pipe, msg);
    })()
  );
}

// build/dev/javascript/app/app.mjs
var Model = class extends CustomType {
  constructor(revision, keyed, prev, curr, next) {
    super();
    this.revision = revision;
    this.keyed = keyed;
    this.prev = prev;
    this.curr = curr;
    this.next = next;
  }
};
var UserClickedUpdate = class extends CustomType {
};
var UserClickedShuffle = class extends CustomType {
};
var UserClickedRegenerate = class extends CustomType {
};
var UserChangedNext = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Fragment2 = class extends CustomType {
  constructor(children) {
    super();
    this.children = children;
  }
};
var Node = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
function init(_) {
  return new Model(
    1,
    true,
    toList([]),
    toList([new Node("a"), new Node("b"), new Node("c")]),
    "a b c"
  );
}
function parse_loop(loop$input, loop$acc) {
  while (true) {
    let input2 = loop$input;
    let acc = loop$acc;
    if (input2.hasLength(0)) {
      return [reverse(acc), toList([])];
    } else if (input2.atLeastLength(1) && input2.head === "") {
      let rest = input2.tail;
      loop$input = rest;
      loop$acc = acc;
    } else {
      let first2 = input2.head;
      let rest = input2.tail;
      let $ = starts_with(first2, "[");
      let $1 = ends_with(first2, "]");
      if ($ && $1) {
        loop$input = rest;
        loop$acc = prepend(new Fragment2(toList([new Node(first2)])), acc);
      } else if ($ && !$1) {
        let $2 = parse_loop(rest, toList([new Node(first2)]));
        let children = $2[0];
        let rest$1 = $2[1];
        loop$input = rest$1;
        loop$acc = prepend(new Fragment2(children), acc);
      } else if (!$ && $1) {
        return [reverse(prepend(new Node(first2), acc)), rest];
      } else {
        loop$input = rest;
        loop$acc = prepend(new Node(first2), acc);
      }
    }
  }
}
function parse(str) {
  let $ = parse_loop(split2(str, " "), toList([]));
  if (!$[1].hasLength(0)) {
    throw makeError(
      "let_assert",
      "app",
      71,
      "parse",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  let nodes = $[0];
  return nodes;
}
function update2(loop$model, loop$msg) {
  while (true) {
    let model = loop$model;
    let msg = loop$msg;
    let $ = debug(msg);
    if ($ instanceof UserChangedNext) {
      let next = $[0];
      let _record = model;
      return new Model(
        _record.revision,
        _record.keyed,
        _record.prev,
        _record.curr,
        next
      );
    } else if ($ instanceof UserClickedRegenerate) {
      let _record = model;
      return new Model(
        model.revision + 1,
        _record.keyed,
        _record.prev,
        _record.curr,
        _record.next
      );
    } else if ($ instanceof UserClickedShuffle) {
      let shuffled = shuffle(model.curr);
      let $1 = isEqual(shuffled, model.curr);
      if ($1) {
        loop$model = model;
        loop$msg = msg;
      } else {
        let _record = model;
        return new Model(
          _record.revision,
          _record.keyed,
          model.curr,
          shuffled,
          _record.next
        );
      }
    } else {
      let _record = model;
      return new Model(
        _record.revision,
        _record.keyed,
        model.curr,
        parse(model.next),
        _record.next
      );
    }
  }
}
function fragment_key(loop$children) {
  while (true) {
    let children = loop$children;
    if (children.hasLength(0)) {
      return "[]";
    } else if (children.atLeastLength(1) && children.head instanceof Node) {
      let key = children.head.key;
      return key;
    } else {
      let children$1 = children.head.children;
      loop$children = children$1;
    }
  }
}
function view_node(node) {
  if (node instanceof Node) {
    let key = node.key;
    return [
      key,
      div(toList([attribute("data-key", key)]), toList([text2(key)]))
    ];
  } else {
    let children = node.children;
    return [
      fragment_key(children),
      (() => {
        let _pipe = children;
        let _pipe$1 = map(
          _pipe,
          (child) => {
            return second(view_node(child));
          }
        );
        return fragment(_pipe$1);
      })()
    ];
  }
}
function view_nodes_classic(children) {
  return div(
    toList([style(toList([["display", "flex"], ["gap", "1em"]]))]),
    map(children, (child) => {
      return second(view_node(child));
    })
  );
}
function view_nodes_keyed(children) {
  return div2(
    toList([style(toList([["display", "flex"], ["gap", "1em"]]))]),
    map(children, view_node)
  );
}
function view(model) {
  return div2(
    toList([
      attribute("data-revision", to_string(model.revision)),
      style(toList([["margin", "2em"]]))
    ]),
    toList([
      [
        to_string(model.revision),
        div(
          toList([
            style(
              toList([
                ["display", "flex"],
                ["flex-direction", "column"],
                ["gap", "1em"],
                ["max-inline-size", "60ch"]
              ])
            )
          ]),
          toList([
            div(
              toList([]),
              toList([
                text2("Previous: "),
                text2(inspect2(model.prev)),
                br(toList([])),
                text2("Current: "),
                text2(inspect2(model.curr))
              ])
            ),
            div(
              toList([
                style(
                  toList([
                    ["display", "flex"],
                    ["align-items", "center"],
                    ["gap", "0.5em"]
                  ])
                )
              ]),
              toList([
                input(
                  toList([
                    value(model.next),
                    on_input(
                      (var0) => {
                        return new UserChangedNext(var0);
                      }
                    )
                  ])
                ),
                button(
                  toList([on_click(new UserClickedUpdate())]),
                  toList([text2("Update")])
                ),
                button(
                  toList([on_click(new UserClickedShuffle())]),
                  toList([text2("Shuffle")])
                ),
                button(
                  toList([on_click(new UserClickedRegenerate())]),
                  toList([text2("Regenerate")])
                )
              ])
            ),
            div(
              toList([
                style(
                  toList([
                    ["display", "grid"],
                    ["grid-template-columns", "auto 1fr"],
                    ["column-gap", "1em"]
                  ])
                )
              ]),
              toList([
                text2("classic"),
                view_nodes_classic(model.curr),
                text2("keyed"),
                view_nodes_keyed(model.curr)
              ])
            ),
            textarea(
              toList([placeholder("Scratch area :)")]),
              ""
            )
          ])
        )
      ]
    ])
  );
}
function main() {
  let app = simple(init, update2, view);
  let $ = start2(app, "#app", void 0);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "app",
      15,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
