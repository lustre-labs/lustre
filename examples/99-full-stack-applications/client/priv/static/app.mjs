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
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
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
  countLength() {
    let length4 = 0;
    for (let _ of this)
      length4++;
    return length4;
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
  byteAt(index3) {
    return this.buffer[index3];
  }
  // @internal
  floatAt(index3) {
    return byteArrayToFloat(this.buffer.slice(index3, index3 + 8));
  }
  // @internal
  intFromSlice(start4, end) {
    return byteArrayToInt(this.buffer.slice(start4, end));
  }
  // @internal
  binaryFromSlice(start4, end) {
    return new _BitArray(this.buffer.slice(start4, end));
  }
  // @internal
  sliceAfter(index3) {
    return new _BitArray(this.buffer.slice(index3));
  }
};
function byteArrayToInt(byteArray) {
  byteArray = byteArray.reverse();
  let value3 = 0;
  for (let i = byteArray.length - 1; i >= 0; i--) {
    value3 = value3 * 256 + byteArray[i];
  }
  return value3;
}
function byteArrayToFloat(byteArray) {
  return new Float64Array(byteArray.reverse().buffer)[0];
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
  let values = [x, y];
  while (values.length) {
    let a = values.pop();
    let b = values.pop();
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
      values.push(get2(a, k), get2(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
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
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function to_result(option, e) {
  if (option instanceof Some) {
    let a = option[0];
    return new Ok(a);
  } else {
    return new Error(e);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function count_length(loop$list, loop$count) {
  while (true) {
    let list3 = loop$list;
    let count = loop$count;
    if (list3.atLeastLength(1)) {
      let list$1 = list3.tail;
      loop$list = list$1;
      loop$count = count + 1;
    } else {
      return count;
    }
  }
}
function length(list3) {
  return count_length(list3, 0);
}
function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function reverse(xs) {
  return do_reverse(xs, toList([]));
}
function first(list3) {
  if (list3.hasLength(0)) {
    return new Error(void 0);
  } else {
    let x = list3.head;
    return new Ok(x);
  }
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list3.head;
      let xs = list3.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = prepend(fun(x), acc);
    }
  }
}
function map(list3, fun) {
  return do_map(list3, fun, toList([]));
}
function do_index_map(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let index3 = loop$index;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list3.head;
      let xs = list3.tail;
      let acc$1 = prepend(fun(x, index3), acc);
      loop$list = xs;
      loop$fun = fun;
      loop$index = index3 + 1;
      loop$acc = acc$1;
    }
  }
}
function index_map(list3, fun) {
  return do_index_map(list3, fun, 0, toList([]));
}
function do_try_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return new Ok(reverse(acc));
    } else {
      let x = list3.head;
      let xs = list3.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let y = $[0];
        loop$list = xs;
        loop$fun = fun;
        loop$acc = prepend(y, acc);
      } else {
        let error = $[0];
        return new Error(error);
      }
    }
  }
}
function try_map(list3, fun) {
  return do_try_map(list3, fun, toList([]));
}
function drop(loop$list, loop$n) {
  while (true) {
    let list3 = loop$list;
    let n = loop$n;
    let $ = n <= 0;
    if ($) {
      return list3;
    } else {
      if (list3.hasLength(0)) {
        return toList([]);
      } else {
        let xs = list3.tail;
        loop$list = xs;
        loop$n = n - 1;
      }
    }
  }
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list3 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list3.hasLength(0)) {
      return initial;
    } else {
      let x = list3.head;
      let rest$1 = list3.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map2(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function then$(result, fun) {
  return try$(result, fun);
}
function nil_error(result) {
  return map_error(result, (_) => {
    return void 0;
  });
}
function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    return new Error(error);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function from_strings(strings) {
  return concat(strings);
}
function to_string(builder) {
  return identity(builder);
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
};
function from(a) {
  return identity(a);
}
function dynamic(value3) {
  return new Ok(value3);
}
function string(data) {
  return decode_string(data);
}
function classify(data) {
  return classify_dynamic(data);
}
function int(data) {
  return decode_int(data);
}
function shallow_list(value3) {
  return decode_list(value3);
}
function at_least_decode_tuple_error(size2, data) {
  let s = (() => {
    if (size2 === 1) {
      return "";
    } else {
      return "s";
    }
  })();
  let error = (() => {
    let _pipe = toList([
      "Tuple of at least ",
      to_string2(size2),
      " element",
      s
    ]);
    let _pipe$1 = from_strings(_pipe);
    let _pipe$2 = to_string(_pipe$1);
    return new DecodeError(_pipe$2, classify(data), toList([]));
  })();
  return new Error(toList([error]));
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify(data), toList([]))])
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function push_path(error, name) {
  let name$1 = from(name);
  let decoder = any(
    toList([string, (x) => {
      return map2(int(x), to_string2);
    }])
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify(name$1), ">"]);
      let _pipe$1 = from_strings(_pipe);
      return to_string(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function list(decoder_type) {
  return (dynamic2) => {
    return try$(
      shallow_list(dynamic2),
      (list3) => {
        let _pipe = list3;
        let _pipe$1 = try_map(_pipe, decoder_type);
        return map_errors(
          _pipe$1,
          (_capture) => {
            return push_path(_capture, "*");
          }
        );
      }
    );
  };
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map(_capture, f);
    }
  );
}
function field(name, inner_type) {
  return (value3) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value3, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name);
          }
        );
      }
    );
  };
}
function element(index3, inner_type) {
  return (data) => {
    return try$(
      decode_tuple(data),
      (tuple) => {
        let size2 = length2(tuple);
        return try$(
          (() => {
            let $ = index3 >= 0;
            if ($) {
              let $1 = index3 < size2;
              if ($1) {
                return tuple_get(tuple, index3);
              } else {
                return at_least_decode_tuple_error(index3 + 1, data);
              }
            } else {
              let $1 = absolute_value(index3) <= size2;
              if ($1) {
                return tuple_get(tuple, size2 + index3);
              } else {
                return at_least_decode_tuple_error(
                  absolute_value(index3),
                  data
                );
              }
            }
          })(),
          (data2) => {
            let _pipe = inner_type(data2);
            return map_errors(
              _pipe,
              (_capture) => {
                return push_path(_capture, index3);
              }
            );
          }
        );
      }
    );
  };
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
function assoc(root2, shift, hash, key, val, addedLeaf) {
  switch (root2.type) {
    case ARRAY_NODE:
      return assocArray(root2, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root2, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root2, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root2, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size + 1,
      array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: ARRAY_NODE,
        size: root2.size,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root2;
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function assocIndex(root2, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root2.bitmap, bit);
  if ((root2.bitmap & bit) !== 0) {
    const node = root2.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root2.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root2.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root2.array[j++];
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
      const newArray = spliceIn(root2.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root2, shift, hash, key, val, addedLeaf) {
  if (hash === root2.hash) {
    const idx = collisionIndexOf(root2, key);
    if (idx !== -1) {
      const entry = root2.array[idx];
      if (entry.v === val) {
        return root2;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size2 = root2.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root2.array, size2, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root2.hash, shift),
      array: [root2]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root2, key) {
  const size2 = root2.array.length;
  for (let i = 0; i < size2; i++) {
    if (isEqual(key, root2.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return findArray(root2, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root2, key);
  }
}
function findArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
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
function findIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return void 0;
  }
  return root2.array[idx];
}
function without(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return withoutArray(root2, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root2, key);
  }
}
function withoutArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    return root2;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root2;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
  }
  if (n === void 0) {
    if (root2.size <= MIN_ARRAY_NODE) {
      const arr = root2.array;
      const out = new Array(root2.size - 1);
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
      size: root2.size - 1,
      array: cloneAndSet(root2.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function withoutIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return root2;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  return root2;
}
function withoutCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return root2;
  }
  if (root2.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root2.hash,
    array: spliceOut(root2.array, idx)
  };
}
function forEach(root2, fn) {
  if (root2 === void 0) {
    return;
  }
  const items = root2.array;
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
  constructor(root2, size2) {
    this.root = root2;
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
    const root2 = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root2, 0, getHash(key), key, val, addedLeaf);
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
    let equal = true;
    this.forEach((v, k) => {
      equal = equal && isEqual(o.get(k, !v), v);
    });
    return equal;
  }
};

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function parse_int(value3) {
  if (/^[-+]?(\d+)$/.test(value3)) {
    return new Ok(parseInt(value3));
  } else {
    return new Error(Nil);
  }
}
function to_string3(term) {
  return term.toString();
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
function length2(data) {
  return data.length;
}
function map_get(map5, key) {
  const value3 = map5.get(key, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
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
function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}
function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))])
  );
}
function decode_string(data) {
  return typeof data === "string" ? new Ok(data) : decoder_error("String", data);
}
function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}
function decode_tuple(data) {
  return Array.isArray(data) ? new Ok(data) : decoder_error("Tuple", data);
}
function tuple_get(data, index3) {
  return index3 >= 0 && data.length > index3 ? new Ok(data[index3]) : new Error(Nil);
}
function decode_list(data) {
  if (Array.isArray(data)) {
    return new Ok(List.fromArray(data));
  }
  return data instanceof List ? new Ok(data) : decoder_error("List", data);
}
function decode_field(value3, name) {
  const not_a_map_error = () => decoder_error("Dict", value3);
  if (value3 instanceof Dict || value3 instanceof WeakMap || value3 instanceof Map) {
    const entry = map_get(value3, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value3 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value3) == Object.prototype) {
    return try_get_field(value3, name, () => new Ok(new None()));
  } else {
    return try_get_field(value3, name, not_a_map_error);
  }
}
function try_get_field(value3, field2, or_else) {
  try {
    return field2 in value3 ? new Ok(new Some(value3[field2])) : or_else();
  } catch {
    return or_else();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function absolute_value(x) {
  let $ = x >= 0;
  if ($) {
    return x;
  } else {
    return x * -1;
  }
}
function parse(string3) {
  return parse_int(string3);
}
function to_string2(x) {
  return to_string3(x);
}

// build/dev/javascript/birl/birl/duration.mjs
var MicroSecond = class extends CustomType {
};
var MilliSecond = class extends CustomType {
};
var Second = class extends CustomType {
};
var Minute = class extends CustomType {
};
var Hour = class extends CustomType {
};
var Day = class extends CustomType {
};
var Week = class extends CustomType {
};
var Month = class extends CustomType {
};
var Year = class extends CustomType {
};
var milli_second = 1e3;
var second = 1e6;
var minute = 6e7;
var hour = 36e8;
var day = 864e8;
var week = 6048e8;
var month = 2592e9;
var year = 31536e9;
var unit_values = toList([
  [new Year(), year],
  [new Month(), month],
  [new Week(), week],
  [new Day(), day],
  [new Hour(), hour],
  [new Minute(), minute],
  [new Second(), second],
  [new MilliSecond(), milli_second],
  [new MicroSecond(), 1]
]);
var year_units = toList(["y", "year", "years"]);
var month_units = toList(["mon", "month", "months"]);
var week_units = toList(["w", "week", "weeks"]);
var day_units = toList(["d", "day", "days"]);
var hour_units = toList(["h", "hour", "hours"]);
var minute_units = toList(["m", "min", "minute", "minutes"]);
var second_units = toList(["s", "sec", "secs", "second", "seconds"]);
var milli_second_units = toList([
  "ms",
  "msec",
  "msecs",
  "millisecond",
  "milliseconds",
  "milli-second",
  "milli-seconds",
  "milli_second",
  "milli_seconds"
]);
var units = toList([
  [new Year(), year_units],
  [new Month(), month_units],
  [new Week(), week_units],
  [new Day(), day_units],
  [new Hour(), hour_units],
  [new Minute(), minute_units],
  [new Second(), second_units],
  [new MilliSecond(), milli_second_units]
]);

// build/dev/javascript/birl/birl/zones.mjs
var list2 = toList([
  ["Africa/Abidjan", 0],
  ["Africa/Algiers", 3600],
  ["Africa/Bissau", 0],
  ["Africa/Cairo", 7200],
  ["Africa/Casablanca", 3600],
  ["Africa/Ceuta", 3600],
  ["Africa/El_Aaiun", 3600],
  ["Africa/Johannesburg", 7200],
  ["Africa/Juba", 7200],
  ["Africa/Khartoum", 7200],
  ["Africa/Lagos", 3600],
  ["Africa/Maputo", 7200],
  ["Africa/Monrovia", 0],
  ["Africa/Nairobi", 10800],
  ["Africa/Ndjamena", 3600],
  ["Africa/Sao_Tome", 0],
  ["Africa/Tripoli", 7200],
  ["Africa/Tunis", 3600],
  ["Africa/Windhoek", 7200],
  ["America/Adak", -36e3],
  ["America/Anchorage", -32400],
  ["America/Araguaina", -10800],
  ["America/Argentina/Buenos_Aires", -10800],
  ["America/Argentina/Catamarca", -10800],
  ["America/Argentina/Cordoba", -10800],
  ["America/Argentina/Jujuy", -10800],
  ["America/Argentina/La_Rioja", -10800],
  ["America/Argentina/Mendoza", -10800],
  ["America/Argentina/Rio_Gallegos", -10800],
  ["America/Argentina/Salta", -10800],
  ["America/Argentina/San_Juan", -10800],
  ["America/Argentina/San_Luis", -10800],
  ["America/Argentina/Tucuman", -10800],
  ["America/Argentina/Ushuaia", -10800],
  ["America/Asuncion", -14400],
  ["America/Bahia", -10800],
  ["America/Bahia_Banderas", -21600],
  ["America/Barbados", -14400],
  ["America/Belem", -10800],
  ["America/Belize", -21600],
  ["America/Boa_Vista", -14400],
  ["America/Bogota", -18e3],
  ["America/Boise", -25200],
  ["America/Cambridge_Bay", -25200],
  ["America/Campo_Grande", -14400],
  ["America/Cancun", -18e3],
  ["America/Caracas", -14400],
  ["America/Cayenne", -10800],
  ["America/Chicago", -21600],
  ["America/Chihuahua", -21600],
  ["America/Ciudad_Juarez", -25200],
  ["America/Costa_Rica", -21600],
  ["America/Cuiaba", -14400],
  ["America/Danmarkshavn", 0],
  ["America/Dawson", -25200],
  ["America/Dawson_Creek", -25200],
  ["America/Denver", -25200],
  ["America/Detroit", -18e3],
  ["America/Edmonton", -25200],
  ["America/Eirunepe", -18e3],
  ["America/El_Salvador", -21600],
  ["America/Fort_Nelson", -25200],
  ["America/Fortaleza", -10800],
  ["America/Glace_Bay", -14400],
  ["America/Goose_Bay", -14400],
  ["America/Grand_Turk", -18e3],
  ["America/Guatemala", -21600],
  ["America/Guayaquil", -18e3],
  ["America/Guyana", -14400],
  ["America/Halifax", -14400],
  ["America/Havana", -18e3],
  ["America/Hermosillo", -25200],
  ["America/Indiana/Indianapolis", -18e3],
  ["America/Indiana/Knox", -21600],
  ["America/Indiana/Marengo", -18e3],
  ["America/Indiana/Petersburg", -18e3],
  ["America/Indiana/Tell_City", -21600],
  ["America/Indiana/Vevay", -18e3],
  ["America/Indiana/Vincennes", -18e3],
  ["America/Indiana/Winamac", -18e3],
  ["America/Inuvik", -25200],
  ["America/Iqaluit", -18e3],
  ["America/Jamaica", -18e3],
  ["America/Juneau", -32400],
  ["America/Kentucky/Louisville", -18e3],
  ["America/Kentucky/Monticello", -18e3],
  ["America/La_Paz", -14400],
  ["America/Lima", -18e3],
  ["America/Los_Angeles", -28800],
  ["America/Maceio", -10800],
  ["America/Managua", -21600],
  ["America/Manaus", -14400],
  ["America/Martinique", -14400],
  ["America/Matamoros", -21600],
  ["America/Mazatlan", -25200],
  ["America/Menominee", -21600],
  ["America/Merida", -21600],
  ["America/Metlakatla", -32400],
  ["America/Mexico_City", -21600],
  ["America/Miquelon", -10800],
  ["America/Moncton", -14400],
  ["America/Monterrey", -21600],
  ["America/Montevideo", -10800],
  ["America/New_York", -18e3],
  ["America/Nome", -32400],
  ["America/Noronha", -7200],
  ["America/North_Dakota/Beulah", -21600],
  ["America/North_Dakota/Center", -21600],
  ["America/North_Dakota/New_Salem", -21600],
  ["America/Nuuk", -7200],
  ["America/Ojinaga", -21600],
  ["America/Panama", -18e3],
  ["America/Paramaribo", -10800],
  ["America/Phoenix", -25200],
  ["America/Port-au-Prince", -18e3],
  ["America/Porto_Velho", -14400],
  ["America/Puerto_Rico", -14400],
  ["America/Punta_Arenas", -10800],
  ["America/Rankin_Inlet", -21600],
  ["America/Recife", -10800],
  ["America/Regina", -21600],
  ["America/Resolute", -21600],
  ["America/Rio_Branco", -18e3],
  ["America/Santarem", -10800],
  ["America/Santiago", -14400],
  ["America/Santo_Domingo", -14400],
  ["America/Sao_Paulo", -10800],
  ["America/Scoresbysund", -7200],
  ["America/Sitka", -32400],
  ["America/St_Johns", -12600],
  ["America/Swift_Current", -21600],
  ["America/Tegucigalpa", -21600],
  ["America/Thule", -14400],
  ["America/Tijuana", -28800],
  ["America/Toronto", -18e3],
  ["America/Vancouver", -28800],
  ["America/Whitehorse", -25200],
  ["America/Winnipeg", -21600],
  ["America/Yakutat", -32400],
  ["Antarctica/Casey", 28800],
  ["Antarctica/Davis", 25200],
  ["Antarctica/Macquarie", 36e3],
  ["Antarctica/Mawson", 18e3],
  ["Antarctica/Palmer", -10800],
  ["Antarctica/Rothera", -10800],
  ["Antarctica/Troll", 0],
  ["Antarctica/Vostok", 18e3],
  ["Asia/Almaty", 18e3],
  ["Asia/Amman", 10800],
  ["Asia/Anadyr", 43200],
  ["Asia/Aqtau", 18e3],
  ["Asia/Aqtobe", 18e3],
  ["Asia/Ashgabat", 18e3],
  ["Asia/Atyrau", 18e3],
  ["Asia/Baghdad", 10800],
  ["Asia/Baku", 14400],
  ["Asia/Bangkok", 25200],
  ["Asia/Barnaul", 25200],
  ["Asia/Beirut", 7200],
  ["Asia/Bishkek", 21600],
  ["Asia/Chita", 32400],
  ["Asia/Choibalsan", 28800],
  ["Asia/Colombo", 19800],
  ["Asia/Damascus", 10800],
  ["Asia/Dhaka", 21600],
  ["Asia/Dili", 32400],
  ["Asia/Dubai", 14400],
  ["Asia/Dushanbe", 18e3],
  ["Asia/Famagusta", 7200],
  ["Asia/Gaza", 7200],
  ["Asia/Hebron", 7200],
  ["Asia/Ho_Chi_Minh", 25200],
  ["Asia/Hong_Kong", 28800],
  ["Asia/Hovd", 25200],
  ["Asia/Irkutsk", 28800],
  ["Asia/Jakarta", 25200],
  ["Asia/Jayapura", 32400],
  ["Asia/Jerusalem", 7200],
  ["Asia/Kabul", 16200],
  ["Asia/Kamchatka", 43200],
  ["Asia/Karachi", 18e3],
  ["Asia/Kathmandu", 20700],
  ["Asia/Khandyga", 32400],
  ["Asia/Kolkata", 19800],
  ["Asia/Krasnoyarsk", 25200],
  ["Asia/Kuching", 28800],
  ["Asia/Macau", 28800],
  ["Asia/Magadan", 39600],
  ["Asia/Makassar", 28800],
  ["Asia/Manila", 28800],
  ["Asia/Nicosia", 7200],
  ["Asia/Novokuznetsk", 25200],
  ["Asia/Novosibirsk", 25200],
  ["Asia/Omsk", 21600],
  ["Asia/Oral", 18e3],
  ["Asia/Pontianak", 25200],
  ["Asia/Pyongyang", 32400],
  ["Asia/Qatar", 10800],
  ["Asia/Qostanay", 18e3],
  ["Asia/Qyzylorda", 18e3],
  ["Asia/Riyadh", 10800],
  ["Asia/Sakhalin", 39600],
  ["Asia/Samarkand", 18e3],
  ["Asia/Seoul", 32400],
  ["Asia/Shanghai", 28800],
  ["Asia/Singapore", 28800],
  ["Asia/Srednekolymsk", 39600],
  ["Asia/Taipei", 28800],
  ["Asia/Tashkent", 18e3],
  ["Asia/Tbilisi", 14400],
  ["Asia/Tehran", 12600],
  ["Asia/Thimphu", 21600],
  ["Asia/Tokyo", 32400],
  ["Asia/Tomsk", 25200],
  ["Asia/Ulaanbaatar", 28800],
  ["Asia/Urumqi", 21600],
  ["Asia/Ust-Nera", 36e3],
  ["Asia/Vladivostok", 36e3],
  ["Asia/Yakutsk", 32400],
  ["Asia/Yangon", 23400],
  ["Asia/Yekaterinburg", 18e3],
  ["Asia/Yerevan", 14400],
  ["Atlantic/Azores", -3600],
  ["Atlantic/Bermuda", -14400],
  ["Atlantic/Canary", 0],
  ["Atlantic/Cape_Verde", -3600],
  ["Atlantic/Faroe", 0],
  ["Atlantic/Madeira", 0],
  ["Atlantic/South_Georgia", -7200],
  ["Atlantic/Stanley", -10800],
  ["Australia/Adelaide", 34200],
  ["Australia/Brisbane", 36e3],
  ["Australia/Broken_Hill", 34200],
  ["Australia/Darwin", 34200],
  ["Australia/Eucla", 31500],
  ["Australia/Hobart", 36e3],
  ["Australia/Lindeman", 36e3],
  ["Australia/Lord_Howe", 37800],
  ["Australia/Melbourne", 36e3],
  ["Australia/Perth", 28800],
  ["Australia/Sydney", 36e3],
  ["CET", 3600],
  ["CST6CDT", -21600],
  ["EET", 7200],
  ["EST", -18e3],
  ["EST5EDT", -18e3],
  ["Etc/GMT", 0],
  ["Etc/GMT+1", -3600],
  ["Etc/GMT+10", -36e3],
  ["Etc/GMT+11", -39600],
  ["Etc/GMT+12", -43200],
  ["Etc/GMT+2", -7200],
  ["Etc/GMT+3", -10800],
  ["Etc/GMT+4", -14400],
  ["Etc/GMT+5", -18e3],
  ["Etc/GMT+6", -21600],
  ["Etc/GMT+7", -25200],
  ["Etc/GMT+8", -28800],
  ["Etc/GMT+9", -32400],
  ["Etc/GMT-1", 3600],
  ["Etc/GMT-10", 36e3],
  ["Etc/GMT-11", 39600],
  ["Etc/GMT-12", 43200],
  ["Etc/GMT-13", 46800],
  ["Etc/GMT-14", 50400],
  ["Etc/GMT-2", 7200],
  ["Etc/GMT-3", 10800],
  ["Etc/GMT-4", 14400],
  ["Etc/GMT-5", 18e3],
  ["Etc/GMT-6", 21600],
  ["Etc/GMT-7", 25200],
  ["Etc/GMT-8", 28800],
  ["Etc/GMT-9", 32400],
  ["Etc/UTC", 0],
  ["Europe/Andorra", 3600],
  ["Europe/Astrakhan", 14400],
  ["Europe/Athens", 7200],
  ["Europe/Belgrade", 3600],
  ["Europe/Berlin", 3600],
  ["Europe/Brussels", 3600],
  ["Europe/Bucharest", 7200],
  ["Europe/Budapest", 3600],
  ["Europe/Chisinau", 7200],
  ["Europe/Dublin", 3600],
  ["Europe/Gibraltar", 3600],
  ["Europe/Helsinki", 7200],
  ["Europe/Istanbul", 10800],
  ["Europe/Kaliningrad", 7200],
  ["Europe/Kirov", 10800],
  ["Europe/Kyiv", 7200],
  ["Europe/Lisbon", 0],
  ["Europe/London", 0],
  ["Europe/Madrid", 3600],
  ["Europe/Malta", 3600],
  ["Europe/Minsk", 10800],
  ["Europe/Moscow", 10800],
  ["Europe/Paris", 3600],
  ["Europe/Prague", 3600],
  ["Europe/Riga", 7200],
  ["Europe/Rome", 3600],
  ["Europe/Samara", 14400],
  ["Europe/Saratov", 14400],
  ["Europe/Simferopol", 10800],
  ["Europe/Sofia", 7200],
  ["Europe/Tallinn", 7200],
  ["Europe/Tirane", 3600],
  ["Europe/Ulyanovsk", 14400],
  ["Europe/Vienna", 3600],
  ["Europe/Vilnius", 7200],
  ["Europe/Volgograd", 10800],
  ["Europe/Warsaw", 3600],
  ["Europe/Zurich", 3600],
  ["HST", -36e3],
  ["Indian/Chagos", 21600],
  ["Indian/Maldives", 18e3],
  ["Indian/Mauritius", 14400],
  ["MET", 3600],
  ["MST", -25200],
  ["MST7MDT", -25200],
  ["PST8PDT", -28800],
  ["Pacific/Apia", 46800],
  ["Pacific/Auckland", 43200],
  ["Pacific/Bougainville", 39600],
  ["Pacific/Chatham", 45900],
  ["Pacific/Easter", -21600],
  ["Pacific/Efate", 39600],
  ["Pacific/Fakaofo", 46800],
  ["Pacific/Fiji", 43200],
  ["Pacific/Galapagos", -21600],
  ["Pacific/Gambier", -32400],
  ["Pacific/Guadalcanal", 39600],
  ["Pacific/Guam", 36e3],
  ["Pacific/Honolulu", -36e3],
  ["Pacific/Kanton", 46800],
  ["Pacific/Kiritimati", 50400],
  ["Pacific/Kosrae", 39600],
  ["Pacific/Kwajalein", 43200],
  ["Pacific/Marquesas", -34200],
  ["Pacific/Nauru", 43200],
  ["Pacific/Niue", -39600],
  ["Pacific/Norfolk", 39600],
  ["Pacific/Noumea", 39600],
  ["Pacific/Pago_Pago", -39600],
  ["Pacific/Palau", 32400],
  ["Pacific/Pitcairn", -28800],
  ["Pacific/Port_Moresby", 36e3],
  ["Pacific/Rarotonga", -36e3],
  ["Pacific/Tahiti", -36e3],
  ["Pacific/Tarawa", 43200],
  ["Pacific/Tongatapu", 46800],
  ["WET", 0]
]);

// build/dev/javascript/birl/birl.mjs
var Time = class extends CustomType {
  constructor(wall_time, offset, timezone, monotonic_time) {
    super();
    this.wall_time = wall_time;
    this.offset = offset;
    this.timezone = timezone;
    this.monotonic_time = monotonic_time;
  }
};
var Mon = class extends CustomType {
};
var Tue = class extends CustomType {
};
var Wed = class extends CustomType {
};
var Thu = class extends CustomType {
};
var Fri = class extends CustomType {
};
var Sat = class extends CustomType {
};
var Sun = class extends CustomType {
};
var Jan = class extends CustomType {
};
var Feb = class extends CustomType {
};
var Mar = class extends CustomType {
};
var Apr = class extends CustomType {
};
var May = class extends CustomType {
};
var Jun = class extends CustomType {
};
var Jul = class extends CustomType {
};
var Aug = class extends CustomType {
};
var Sep = class extends CustomType {
};
var Oct = class extends CustomType {
};
var Nov = class extends CustomType {
};
var Dec = class extends CustomType {
};
var unix_epoch = new Time(0, 0, new None(), new None());
var string_to_units = toList([
  ["year", new Year()],
  ["month", new Month()],
  ["week", new Week()],
  ["day", new Day()],
  ["hour", new Hour()],
  ["minute", new Minute()],
  ["second", new Second()]
]);
var units_to_string = toList([
  [new Year(), "year"],
  [new Month(), "month"],
  [new Week(), "week"],
  [new Day(), "day"],
  [new Hour(), "hour"],
  [new Minute(), "minute"],
  [new Second(), "second"]
]);
var weekday_strings = toList([
  [new Mon(), ["Monday", "Mon"]],
  [new Tue(), ["Tuesday", "Tue"]],
  [new Wed(), ["Wednesday", "Wed"]],
  [new Thu(), ["Thursday", "Thu"]],
  [new Fri(), ["Friday", "Fri"]],
  [new Sat(), ["Saturday", "Sat"]],
  [new Sun(), ["Sunday", "Sun"]]
]);
var month_strings = toList([
  [new Jan(), ["January", "Jan"]],
  [new Feb(), ["February", "Feb"]],
  [new Mar(), ["March", "Mar"]],
  [new Apr(), ["April", "Apr"]],
  [new May(), ["May", "May"]],
  [new Jun(), ["June", "Jun"]],
  [new Jul(), ["July", "Jul"]],
  [new Aug(), ["August", "Aug"]],
  [new Sep(), ["September", "Sep"]],
  [new Oct(), ["October", "Oct"]],
  [new Nov(), ["November", "Nov"]],
  [new Dec(), ["December", "Dec"]]
]);

// build/dev/javascript/decipher/decipher.mjs
function index_list(idx, decoder) {
  return (dynamic2) => {
    return try$(
      list(dynamic)(dynamic2),
      (list3) => {
        let $ = idx >= 0;
        if ($) {
          let _pipe = list3;
          let _pipe$1 = drop(_pipe, idx);
          let _pipe$2 = first(_pipe$1);
          let _pipe$3 = replace_error(
            _pipe$2,
            toList([
              new DecodeError(
                "A list with at least" + to_string2(idx + 1) + "elements",
                "A list with" + to_string2(length(list3)) + "elements",
                toList([to_string2(idx)])
              )
            ])
          );
          return then$(_pipe$3, decoder);
        } else {
          return new Error(
            toList([
              new DecodeError(
                "An 'index' decoder with a non-negative index",
                to_string2(idx),
                toList([])
              )
            ])
          );
        }
      }
    );
  };
}
function index2(idx, decoder) {
  return any(
    toList([
      element(idx, decoder),
      field(to_string2(idx), decoder),
      index_list(idx, decoder)
    ])
  );
}
function do_at(path, decoder, dynamic2) {
  if (path.hasLength(0)) {
    return decoder(dynamic2);
  } else {
    let head = path.head;
    let rest2 = path.tail;
    let $ = parse(head);
    if ($.isOk()) {
      let idx = $[0];
      let _pipe = dynamic2;
      let _pipe$1 = index2(idx, dynamic)(_pipe);
      return then$(
        _pipe$1,
        (_capture) => {
          return do_at(rest2, decoder, _capture);
        }
      );
    } else {
      let _pipe = dynamic2;
      let _pipe$1 = field(head, dynamic)(_pipe);
      return then$(
        _pipe$1,
        (_capture) => {
          return do_at(rest2, decoder, _capture);
        }
      );
    }
  }
}
function at(path, decoder) {
  return (dynamic2) => {
    return do_at(path, decoder, dynamic2);
  };
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all2) {
    super();
    this.all = all2;
  }
};
function none() {
  return new Effect(toList([]));
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element = class extends CustomType {
  constructor(key, namespace, tag, attrs, children, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Fragment = class extends CustomType {
  constructor(elements, key) {
    super();
    this.elements = elements;
    this.key = key;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value3) {
  return new Attribute(name, from(value3), false);
}
function on(name, handler) {
  return new Event("on" + name, handler);
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
function type_(name) {
  return attribute("type", name);
}
function value(val) {
  return attribute("value", val);
}
function min(val) {
  return attribute("min", val);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element2(tag, attrs, children) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children, false, false);
  }
}
function do_keyed(el2, key) {
  if (el2 instanceof Element) {
    let namespace = el2.namespace;
    let tag = el2.tag;
    let attrs = el2.attrs;
    let children = el2.children;
    let self_closing = el2.self_closing;
    let void$ = el2.void;
    return new Element(
      key,
      namespace,
      tag,
      attrs,
      children,
      self_closing,
      void$
    );
  } else if (el2 instanceof Map2) {
    let subtree = el2.subtree;
    return new Map2(() => {
      return do_keyed(subtree(), key);
    });
  } else if (el2 instanceof Fragment) {
    let elements = el2.elements;
    let _pipe = elements;
    let _pipe$1 = index_map(
      _pipe,
      (element3, idx) => {
        if (element3 instanceof Element) {
          let el_key = element3.key;
          let new_key = (() => {
            if (el_key === "") {
              return key + "-" + to_string2(idx);
            } else {
              return key + "-" + el_key;
            }
          })();
          return do_keyed(element3, new_key);
        } else {
          return do_keyed(element3, key);
        }
      }
    );
    return new Fragment(_pipe$1, key);
  } else {
    return el2;
  }
}
function keyed(el2, children) {
  return el2(
    map(
      children,
      (_use0) => {
        let key = _use0[0];
        let child = _use0[1];
        return do_keyed(child, key);
      }
    )
  );
}
function text(content) {
  return new Text(content);
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Shutdown = class extends CustomType {
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch, isComponent = false) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    if (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack,
        isComponent
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    } else if (next2.elements !== void 0) {
      iterateElement(next2, (fragmentElement) => {
        stack.unshift({ prev: prev2, next: fragmentElement, parent });
        prev2 = prev2?.nextSibling;
      });
    } else if (next2.subtree !== void 0) {
      stack.push({ prev: prev2, next: next2, parent });
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style2 = null;
  let innerHTML = null;
  for (const attr of next.attrs) {
    const name = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el2[name] !== value3)
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value3);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value3);
    } else if (name === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name === "style") {
      style2 = style2 === null ? value3 : style2 + value3;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (typeof value3 === "string")
        el2.setAttribute(name, value3);
      if (name === "value" || name === "selected")
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style2 !== null) {
    el2.setAttribute("style", style2);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    iterateElement(child, (currElement) => {
      if (currElement.key !== void 0 && seenKeys !== null) {
        prevChild = diffKeyedChild(
          prevChild,
          currElement,
          el2,
          stack,
          incomingKeyedChildren,
          keyedChildren,
          seenKeys
        );
      } else {
        stack.unshift({ prev: prevChild, next: currElement, parent: el2 });
        prevChild = prevChild?.nextSibling;
      }
    });
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target = event2.currentTarget;
  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target);
  if (!handlersForEventTarget.has(event2.type)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.target;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property) => {
        const path = property.split(".");
        for (let i = 0, o = data2, e = event2; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of el2.children) {
      iterateElement(child, (currElement) => {
        const key = currElement?.key || currElement?.getAttribute?.("data-lustre-key");
        if (key)
          keyedChildren.set(key, currElement);
      });
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el2, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    iterateElement(child, (currChild) => {
      stack.unshift({ prev: prevChild, next: currChild, parent: el2 });
      prevChild = prevChild?.nextSibling;
    });
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el2.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el2 });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el2.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el2 });
  return prevChild;
}
function iterateElement(element3, processElement) {
  if (element3.elements !== void 0) {
    for (const currElement of element3.elements) {
      processElement(currElement);
    }
  } else {
    processElement(element3);
  }
}

// build/dev/javascript/lustre/client-runtime.ffi.mjs
var LustreClientApplication2 = class _LustreClientApplication {
  #root = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;
  #isComponent = false;
  #model = null;
  #update = null;
  #view = null;
  static start(flags, selector, init3, update3, view2) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root2 = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root2)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(init3(flags), update3, view2, root2);
    return new Ok((msg) => app.send(msg));
  }
  constructor([model, effects], update3, view2, root2 = document.body, isComponent = false) {
    this.#model = model;
    this.#update = update3;
    this.#view = view2;
    this.#root = root2;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;
    this.#isComponent = isComponent;
    window.requestAnimationFrame(() => this.#tick());
  }
  send(action) {
    switch (true) {
      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();
        return;
      }
      case action instanceof Shutdown: {
        this.#shutdown();
        return;
      }
      case action instanceof Debug: {
        this.#debug(action[0]);
        return;
      }
      default:
        return;
    }
  }
  emit(event2, data) {
    this.#root.dispatchEvent(
      new CustomEvent(event2, {
        bubbles: true,
        detail: data,
        composed: true
      })
    );
  }
  #tick() {
    this.#flush_queue();
    const vdom = this.#view(this.#model);
    const dispatch = (handler) => (e) => {
      const result = handler(e);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0]));
      }
    };
    this.#didUpdate = false;
    this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
  }
  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());
      this.#didUpdate ||= !isEqual(this.#model, next);
      this.#model = next;
      this.#effects = this.#effects.concat(effects.all.toArray());
    }
    while (this.#effects.length) {
      this.#effects.shift()(
        (msg) => this.send(new Dispatch(msg)),
        (event2, data) => this.emit(event2, data)
      );
    }
    if (this.#queue.length) {
      if (iterations < 5) {
        this.#flush_queue(++iterations);
      } else {
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
  #debug(action) {
    switch (true) {
      case action instanceof ForceModel: {
        const vdom = this.#view(action[0]);
        const dispatch = (handler) => (e) => {
          const result = handler(e);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0]));
          }
        };
        this.#queue = [];
        this.#effects = [];
        this.#didUpdate = false;
        this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
      }
    }
  }
  #shutdown() {
    this.#root.remove();
    this.#root = null;
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {
    };
    this.#view = () => {
    };
  }
};
var start = (app, selector, flags) => LustreClientApplication2.start(
  flags,
  selector,
  app.init,
  app.update,
  app.view
);
var is_browser = () => window && window.document;

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init3, update3, view2, on_attribute_change) {
    super();
    this.init = init3;
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
function application(init3, update3, view2) {
  return new App(init3, update3, view2, new None());
}
function start3(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function div(attrs, children) {
  return element2("div", attrs, children);
}
function span(attrs, children) {
  return element2("span", attrs, children);
}
function button(attrs, children) {
  return element2("button", attrs, children);
}
function input(attrs) {
  return element2("input", attrs, toList([]));
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function value2(event2) {
  let _pipe = event2;
  return field("target", field("value", string))(
    _pipe
  );
}

// build/dev/javascript/app/app.mjs
var ServerSavedList = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserAddedProduct = class extends CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
};
var UserSavedList = class extends CustomType {
};
var UserUpdatedQuantity = class extends CustomType {
  constructor(name, amount) {
    super();
    this.name = name;
    this.amount = amount;
  }
};
function init2(_) {
  let model = toList([]);
  let effect = none();
  return [model, effect];
}
function update2(model, msg) {
  if (msg instanceof ServerSavedList) {
    return [model, none()];
  } else if (msg instanceof UserAddedProduct) {
    let name = msg.name;
    return [prepend([name, 1], model), none()];
  } else if (msg instanceof UserSavedList) {
    return [model, none()];
  } else {
    let name = msg.name;
    let quantity = msg.amount;
    let model$1 = map(
      model,
      (item) => {
        let $ = item[0] === name;
        if ($) {
          return [name, quantity];
        } else {
          return item;
        }
      }
    );
    return [model$1, none()];
  }
}
function view_new_item() {
  let handle_click = (event2) => {
    let path = toList(["target", "previousElementSibling", "value"]);
    let _pipe = event2;
    let _pipe$1 = at(path, string)(_pipe);
    return map2(
      _pipe$1,
      (var0) => {
        return new UserAddedProduct(var0);
      }
    );
  };
  return div(
    toList([]),
    toList([
      input(toList([])),
      button(
        toList([on2("click", handle_click)]),
        toList([text2("Add")])
      )
    ])
  );
}
function view_grocery_item(name, quantity) {
  let handle_input = (e) => {
    let _pipe = value2(e);
    let _pipe$1 = nil_error(_pipe);
    let _pipe$2 = then$(_pipe$1, parse);
    let _pipe$3 = map2(
      _pipe$2,
      (_capture) => {
        return new UserUpdatedQuantity(name, _capture);
      }
    );
    return replace_error(_pipe$3, toList([]));
  };
  return div(
    toList([style(toList([["display", "flex"], ["gap", "1em"]]))]),
    toList([
      span(
        toList([style(toList([["flex", "1"]]))]),
        toList([text2(name)])
      ),
      input(
        toList([
          style(toList([["width", "4em"]])),
          type_("number"),
          value(to_string2(quantity)),
          min("0"),
          on2("input", handle_input)
        ])
      )
    ])
  );
}
function view_grocery_list(model) {
  let styles = toList([
    ["display", "flex"],
    ["flex-direction", "column-reverse"]
  ]);
  return keyed(
    (_capture) => {
      return div(toList([style(styles)]), _capture);
    },
    map(
      model,
      (_use0) => {
        let name = _use0[0];
        let quantity = _use0[1];
        let item = view_grocery_item(name, quantity);
        return [name, item];
      }
    )
  );
}
function view(model) {
  let styles = toList([
    ["max-width", "30ch"],
    ["margin", "0 auto"],
    ["display", "flex"],
    ["flex-direction", "column"],
    ["gap", "1em"]
  ]);
  return div(
    toList([style(styles)]),
    toList([
      view_grocery_list(model),
      view_new_item(),
      div(
        toList([]),
        toList([button(toList([]), toList([text2("Sync")]))])
      )
    ])
  );
}
function main() {
  let app = application(init2, update2, view);
  let $ = start3(app, "#app", void 0);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "app",
      19,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
