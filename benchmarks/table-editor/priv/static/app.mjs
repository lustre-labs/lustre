// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label2) => label2 in fields ? fields[label2] : this[label2]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array4, tail) {
    let t = tail || new Empty();
    for (let i = array4.length - 1; i >= 0; --i) {
      t = new NonEmpty(array4[i], t);
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
    let length5 = 0;
    for (let _ of this)
      length5++;
    return length5;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements2, tail) {
  return List.fromArray(elements2, tail);
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
  floatFromSlice(start4, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start4, end, isBigEndian);
  }
  // @internal
  intFromSlice(start4, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start4, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start4, end) {
    const buffer = new Uint8Array(
      this.buffer.buffer,
      this.buffer.byteOffset + start4,
      end - start4
    );
    return new _BitArray(buffer);
  }
  // @internal
  sliceAfter(index3) {
    const buffer = new Uint8Array(
      this.buffer.buffer,
      this.buffer.byteOffset + index3,
      this.buffer.byteLength - index3
    );
    return new _BitArray(buffer);
  }
};
var UtfCodepoint = class {
  constructor(value2) {
    this.value = value2;
  }
};
function byteArrayToInt(byteArray, start4, end, isBigEndian, isSigned) {
  const byteSize = end - start4;
  if (byteSize <= 6) {
    let value2 = 0;
    if (isBigEndian) {
      for (let i = start4; i < end; i++) {
        value2 = value2 * 256 + byteArray[i];
      }
    } else {
      for (let i = end - 1; i >= start4; i--) {
        value2 = value2 * 256 + byteArray[i];
      }
    }
    if (isSigned) {
      const highBit = 2 ** (byteSize * 8 - 1);
      if (value2 >= highBit) {
        value2 -= highBit * 2;
      }
    }
    return value2;
  } else {
    let value2 = 0n;
    if (isBigEndian) {
      for (let i = start4; i < end; i++) {
        value2 = (value2 << 8n) + BigInt(byteArray[i]);
      }
    } else {
      for (let i = end - 1; i >= start4; i--) {
        value2 = (value2 << 8n) + BigInt(byteArray[i]);
      }
    }
    if (isSigned) {
      const highBit = 1n << BigInt(byteSize * 8 - 1);
      if (value2 >= highBit) {
        value2 -= highBit * 2n;
      }
    }
    return Number(value2);
  }
}
function byteArrayToFloat(byteArray, start4, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start4;
  if (byteSize === 8) {
    return view2.getFloat64(start4, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start4, !isBigEndian);
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
  constructor(value2) {
    super();
    this[0] = value2;
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
function remainderInt(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a % b;
  }
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
var None = class extends CustomType {
};

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function do_has_key(key, dict2) {
  return !isEqual(map_get(dict2, key), new Error(void 0));
}
function has_key(dict2, key) {
  return do_has_key(key, dict2);
}
function insert(dict2, key, value2) {
  return map_insert(key, value2, dict2);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let first2 = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(first2, accumulator);
    }
  }
}
function do_keys_loop(loop$list, loop$acc) {
  while (true) {
    let list2 = loop$list;
    let acc = loop$acc;
    if (list2.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let key = list2.head[0];
      let rest = list2.tail;
      loop$list = rest;
      loop$acc = prepend(key, acc);
    }
  }
}
function keys(dict2) {
  return do_keys_loop(map_to_list(dict2), toList([]));
}
function delete$(dict2, key) {
  return map_remove(key, dict2);
}
function fold_loop(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list2 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list2.hasLength(0)) {
      return initial;
    } else {
      let k = list2.head[0];
      let v = list2.head[1];
      let rest = list2.tail;
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    }
  }
}
function fold(dict2, initial, fun) {
  return fold_loop(map_to_list(dict2), initial, fun);
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function length_loop(loop$list, loop$count) {
  while (true) {
    let list2 = loop$list;
    let count = loop$count;
    if (list2.atLeastLength(1)) {
      let list$1 = list2.tail;
      loop$list = list$1;
      loop$count = count + 1;
    } else {
      return count;
    }
  }
}
function length(list2) {
  return length_loop(list2, 0);
}
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
function is_empty(list2) {
  return isEqual(list2, toList([]));
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
function fold2(loop$list, loop$initial, loop$fun) {
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
    let index3 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index3);
      loop$with = with$;
      loop$index = index3 + 1;
    }
  }
}
function index_fold(list2, initial, fun) {
  return index_fold_loop(list2, initial, fun, 0);
}
function range_loop(loop$start, loop$stop, loop$acc) {
  while (true) {
    let start4 = loop$start;
    let stop = loop$stop;
    let acc = loop$acc;
    let $ = compare2(start4, stop);
    if ($ instanceof Eq) {
      return prepend(stop, acc);
    } else if ($ instanceof Gt) {
      loop$start = start4;
      loop$stop = stop + 1;
      loop$acc = prepend(stop, acc);
    } else {
      loop$start = start4;
      loop$stop = stop - 1;
      loop$acc = prepend(stop, acc);
    }
  }
}
function range(start4, stop) {
  return range_loop(start4, stop, toList([]));
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function drop_start(loop$string, loop$num_graphemes) {
  while (true) {
    let string4 = loop$string;
    let num_graphemes = loop$num_graphemes;
    let $ = num_graphemes > 0;
    if (!$) {
      return string4;
    } else {
      let $1 = pop_grapheme(string4);
      if ($1.isOk()) {
        let string$1 = $1[0][1];
        loop$string = string$1;
        loop$num_graphemes = num_graphemes - 1;
      } else {
        return string4;
      }
    }
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
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
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
function float_to_string(float4) {
  const string4 = float4.toString().replace("+", "");
  if (string4.indexOf(".") >= 0) {
    return string4;
  } else {
    const index3 = string4.indexOf("e");
    if (index3 >= 0) {
      return string4.slice(0, index3) + ".0" + string4.slice(index3);
    } else {
      return string4 + ".0";
    }
  }
}
var segmenter = void 0;
function graphemes_iterator(string4) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string4)[Symbol.iterator]();
  }
}
function pop_grapheme(string4) {
  let first2;
  const iterator = graphemes_iterator(string4);
  if (iterator) {
    first2 = iterator.next().value?.segment;
  } else {
    first2 = string4.match(/./su)?.[0];
  }
  if (first2) {
    return new Ok([first2, string4.slice(first2.length)]);
  } else {
    return new Error(Nil);
  }
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
function print_debug(string4) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string4 + "\n");
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string4 + "\n"));
  } else {
    console.log(string4);
  }
}
function new_map() {
  return Dict.new();
}
function map_to_list(map5) {
  return List.fromArray(map5.entries());
}
function map_remove(key, map5) {
  return map5.delete(key);
}
function map_get(map5, key) {
  const value2 = map5.get(key, NOT_FOUND);
  if (value2 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value2);
}
function map_insert(key, value2, map5) {
  return map5.set(key, value2);
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
function inspectDict(map5) {
  let body = "dict.from_list([";
  let first2 = true;
  map5.forEach((value2, key) => {
    if (!first2)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value2) + ")";
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
  const props = Object.keys(record).map((label2) => {
    const value2 = inspect(record[label2]);
    return isNaN(parseInt(label2)) ? `${label2}: ${value2}` : value2;
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

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function compare2(a, b) {
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

// build/dev/javascript/iv/iv_ffi.mjs
var empty = () => [];
var singleton = (x) => [x];
var pair = (x, y) => [x, y];
var append4 = (xs, x) => [...xs, x];
var prepend2 = (xs, x) => [x, ...xs];
var concat2 = (xs, ys) => [...xs, ...ys];
var get1 = (idx, xs) => xs[idx - 1];
var set1 = (idx, xs, x) => xs.with(idx - 1, x);
var length3 = (xs) => xs.length;
var split1 = (idx, xs) => [xs.slice(0, idx - 1), xs.slice(idx - 1)];
var map3 = (xs, f) => xs.map(f);
var bsl = (a, b) => a << b;
var bsr = (a, b) => a >> b;

// build/dev/javascript/iv/iv/internal/vector.mjs
function fold_loop2(loop$xs, loop$state, loop$idx, loop$len, loop$fun) {
  while (true) {
    let xs = loop$xs;
    let state = loop$state;
    let idx = loop$idx;
    let len = loop$len;
    let fun = loop$fun;
    let $ = idx <= len;
    if ($) {
      loop$xs = xs;
      loop$state = fun(state, get1(idx, xs));
      loop$idx = idx + 1;
      loop$len = len;
      loop$fun = fun;
    } else {
      return state;
    }
  }
}
function fold3(xs, state, fun) {
  let len = length3(xs);
  return fold_loop2(xs, state, 1, len, fun);
}
function fold_skip_last(xs, state, fun) {
  let len = length3(xs);
  return fold_loop2(xs, state, 1, len - 1, fun);
}
function fold_skip_first(xs, state, fun) {
  let len = length3(xs);
  return fold_loop2(xs, state, 2, len, fun);
}
function fold_right_loop(loop$xs, loop$state, loop$idx, loop$fun) {
  while (true) {
    let xs = loop$xs;
    let state = loop$state;
    let idx = loop$idx;
    let fun = loop$fun;
    let $ = idx > 0;
    if ($) {
      loop$xs = xs;
      loop$state = fun(state, get1(idx, xs));
      loop$idx = idx - 1;
      loop$fun = fun;
    } else {
      return state;
    }
  }
}
function fold_right(xs, state, fun) {
  let len = length3(xs);
  return fold_right_loop(xs, state, len, fun);
}

// build/dev/javascript/iv/iv.mjs
var Empty2 = class extends CustomType {
};
var Array2 = class extends CustomType {
  constructor(shift, root) {
    super();
    this.shift = shift;
    this.root = root;
  }
};
var Balanced = class extends CustomType {
  constructor(size2, children2) {
    super();
    this.size = size2;
    this.children = children2;
  }
};
var Unbalanced = class extends CustomType {
  constructor(sizes, children2) {
    super();
    this.sizes = sizes;
    this.children = children2;
  }
};
var Leaf = class extends CustomType {
  constructor(children2) {
    super();
    this.children = children2;
  }
};
var Builder = class extends CustomType {
  constructor(nodes, items) {
    super();
    this.nodes = nodes;
    this.items = items;
  }
};
var Concatenated = class extends CustomType {
  constructor(merged) {
    super();
    this.merged = merged;
  }
};
var NoFreeSlot = class extends CustomType {
  constructor(left, right) {
    super();
    this.left = left;
    this.right = right;
  }
};
var RebalanceState = class extends CustomType {
  constructor(balance, roots, subtrees, overflow) {
    super();
    this.balance = balance;
    this.roots = roots;
    this.subtrees = subtrees;
    this.overflow = overflow;
  }
};
var FromLeft = class extends CustomType {
  constructor(left, merged) {
    super();
    this.left = left;
    this.merged = merged;
  }
};
var FromRight = class extends CustomType {
  constructor(merged, right) {
    super();
    this.merged = merged;
    this.right = right;
  }
};
var RebalanceMerge = class extends CustomType {
  constructor(left, merged, right) {
    super();
    this.left = left;
    this.merged = merged;
    this.right = right;
  }
};
var Split = class extends CustomType {
  constructor(prefix, suffix) {
    super();
    this.prefix = prefix;
    this.suffix = suffix;
  }
};
var EmptyPrefix = class extends CustomType {
};
function node_size(node) {
  if (node instanceof Balanced) {
    let size2 = node.size;
    return size2;
  } else if (node instanceof Leaf) {
    let children2 = node.children;
    return length3(children2);
  } else {
    let sizes = node.sizes;
    return get1(length3(sizes), sizes);
  }
}
function compute_sizes(nodes) {
  let first_size = node_size(get1(1, nodes));
  return fold_skip_first(
    nodes,
    singleton(first_size),
    (sizes, node) => {
      let size2 = get1(length3(sizes), sizes) + node_size(node);
      return append4(sizes, size2);
    }
  );
}
function unbalanced(_, nodes) {
  let sizes = compute_sizes(nodes);
  return new Unbalanced(sizes, nodes);
}
function wrap(item) {
  return new Array2(0, new Leaf(singleton(item)));
}
function builder_new() {
  return new Builder(toList([]), empty());
}
function length4(array4) {
  if (array4 instanceof Empty2) {
    return 0;
  } else {
    let root = array4.root;
    return node_size(root);
  }
}
function find_size(loop$sizes, loop$size_idx_plus_one, loop$index) {
  while (true) {
    let sizes = loop$sizes;
    let size_idx_plus_one = loop$size_idx_plus_one;
    let index3 = loop$index;
    let $ = get1(size_idx_plus_one, sizes) > index3;
    if ($) {
      return size_idx_plus_one - 1;
    } else {
      loop$sizes = sizes;
      loop$size_idx_plus_one = size_idx_plus_one + 1;
      loop$index = index3;
    }
  }
}
function extract_children(node) {
  if (node instanceof Balanced) {
    let children2 = node.children;
    return children2;
  } else if (node instanceof Unbalanced) {
    let children2 = node.children;
    return children2;
  } else {
    throw makeError(
      "panic",
      "iv",
      1955,
      "extract_children",
      "`panic` expression evaluated.",
      {}
    );
  }
}
function extract_items(node) {
  if (!(node instanceof Leaf)) {
    throw makeError(
      "let_assert",
      "iv",
      1960,
      "extract_items",
      "Pattern match failed, no pattern matched the value.",
      { value: node }
    );
  }
  let children2 = node.children;
  return children2;
}
function sum_children_counts(count, node) {
  if (node instanceof Balanced) {
    let children2 = node.children;
    return count + length3(children2);
  } else if (node instanceof Leaf) {
    let children2 = node.children;
    return count + length3(children2);
  } else {
    let children2 = node.children;
    return count + length3(children2);
  }
}
function rebalance_skip_subtree(state, overflow) {
  let _record = state;
  return new RebalanceState(
    state.balance - 1,
    _record.roots,
    _record.subtrees,
    overflow
  );
}
function map_node(node, fun) {
  if (node instanceof Balanced) {
    let size2 = node.size;
    let children2 = node.children;
    return new Balanced(
      size2,
      map3(children2, (_capture) => {
        return map_node(_capture, fun);
      })
    );
  } else if (node instanceof Unbalanced) {
    let sizes = node.sizes;
    let children2 = node.children;
    return new Unbalanced(
      sizes,
      map3(children2, (_capture) => {
        return map_node(_capture, fun);
      })
    );
  } else {
    let children2 = node.children;
    return new Leaf(map3(children2, fun));
  }
}
function map4(array4, fun) {
  if (array4 instanceof Empty2) {
    return new Empty2();
  } else {
    let shift = array4.shift;
    let root = array4.root;
    return new Array2(shift, map_node(root, fun));
  }
}
function fold_right_node(node, state, fun) {
  if (node instanceof Balanced) {
    let children2 = node.children;
    return fold_right(
      children2,
      state,
      (state2, node2) => {
        return fold_right_node(node2, state2, fun);
      }
    );
  } else if (node instanceof Unbalanced) {
    let children2 = node.children;
    return fold_right(
      children2,
      state,
      (state2, node2) => {
        return fold_right_node(node2, state2, fun);
      }
    );
  } else {
    let children2 = node.children;
    return fold_right(children2, state, fun);
  }
}
function fold_right2(array4, state, fun) {
  if (array4 instanceof Empty2) {
    return state;
  } else {
    let root = array4.root;
    return fold_right_node(root, state, fun);
  }
}
function balanced(shift, nodes) {
  let len = length3(nodes);
  let last_child = get1(len, nodes);
  let max_size = bsl(1, shift);
  let size2 = max_size * (len - 1) + node_size(last_child);
  return new Balanced(size2, nodes);
}
function branch(shift, nodes) {
  let len = length3(nodes);
  let max_size = bsl(1, shift);
  let sizes = compute_sizes(nodes);
  let prefix_size = (() => {
    if (len === 1) {
      return 0;
    } else {
      return get1(len - 1, sizes);
    }
  })();
  let is_balanced = prefix_size === max_size * (len - 1);
  if (is_balanced) {
    let size2 = get1(len, sizes);
    return new Balanced(size2, nodes);
  } else {
    return new Unbalanced(sizes, nodes);
  }
}
var branch_bits = 5;
function array(shift, nodes) {
  let $ = length3(nodes);
  if ($ === 0) {
    return new Empty2();
  } else if ($ === 1) {
    return new Array2(shift, get1(1, nodes));
  } else {
    let shift$1 = shift + branch_bits;
    return new Array2(shift$1, branch(shift$1, nodes));
  }
}
function get_from_node(loop$node, loop$shift, loop$index) {
  while (true) {
    let node = loop$node;
    let shift = loop$shift;
    let index3 = loop$index;
    if (node instanceof Balanced) {
      let children2 = node.children;
      let node_index = bsr(index3, shift);
      let index$1 = index3 - bsl(node_index, shift);
      let child = get1(node_index + 1, children2);
      loop$node = child;
      loop$shift = shift - branch_bits;
      loop$index = index$1;
    } else if (node instanceof Unbalanced) {
      let sizes = node.sizes;
      let children2 = node.children;
      let start_search_index = bsr(index3, shift);
      let node_index = find_size(sizes, start_search_index + 1, index3);
      let index$1 = (() => {
        if (node_index === 0) {
          return index3;
        } else {
          return index3 - get1(node_index, sizes);
        }
      })();
      let child = get1(node_index + 1, children2);
      loop$node = child;
      loop$shift = shift - branch_bits;
      loop$index = index$1;
    } else {
      let children2 = node.children;
      return get1(index3 + 1, children2);
    }
  }
}
function get(array4, index3) {
  if (array4 instanceof Array2) {
    let shift = array4.shift;
    let root = array4.root;
    let $ = 0 <= index3 && index3 < node_size(root);
    if ($) {
      return new Ok(get_from_node(root, shift, index3));
    } else {
      return new Error(void 0);
    }
  } else {
    return new Error(void 0);
  }
}
function update_node(shift, node, index3, fun) {
  if (node instanceof Balanced) {
    let size2 = node.size;
    let children2 = node.children;
    let node_index = bsr(index3, shift);
    let index$1 = index3 - bsl(node_index, shift);
    let new_children = (() => {
      let _pipe = get1(node_index + 1, children2);
      let _pipe$1 = ((_capture) => {
        return update_node(shift - branch_bits, _capture, index$1, fun);
      })(_pipe);
      return ((_capture) => {
        return set1(node_index + 1, children2, _capture);
      })(_pipe$1);
    })();
    return new Balanced(size2, new_children);
  } else if (node instanceof Unbalanced) {
    let sizes = node.sizes;
    let children2 = node.children;
    let start_search_index = bsr(index3, shift);
    let node_index = find_size(sizes, start_search_index + 1, index3);
    let index$1 = (() => {
      if (node_index === 0) {
        return index3;
      } else {
        return index3 - get1(node_index, sizes);
      }
    })();
    let new_children = (() => {
      let _pipe = get1(node_index + 1, children2);
      let _pipe$1 = ((_capture) => {
        return update_node(shift - branch_bits, _capture, index$1, fun);
      })(_pipe);
      return ((_capture) => {
        return set1(node_index + 1, children2, _capture);
      })(_pipe$1);
    })();
    return new Unbalanced(sizes, new_children);
  } else {
    let children2 = node.children;
    let new_children = set1(
      index3 + 1,
      children2,
      fun(get1(index3 + 1, children2))
    );
    return new Leaf(new_children);
  }
}
function try_update(array4, index3, fun) {
  if (array4 instanceof Array2) {
    let shift = array4.shift;
    let root = array4.root;
    let $ = 0 <= index3 && index3 < node_size(root);
    if ($) {
      return new Array2(shift, update_node(shift, root, index3, fun));
    } else {
      return array4;
    }
  } else {
    return array4;
  }
}
function try_set(array4, index3, item) {
  return try_update(array4, index3, (_) => {
    return item;
  });
}
function split_node(shift, node, index3) {
  if (node instanceof Balanced) {
    let children2 = node.children;
    let node_index = bsr(index3, shift);
    let index$1 = index3 - bsl(node_index, shift);
    let child = get1(node_index + 1, children2);
    let child_shift = shift - branch_bits;
    let $ = split_node(child_shift, child, index$1);
    if ($ instanceof EmptyPrefix && node_index === 0) {
      return new EmptyPrefix();
    } else if ($ instanceof EmptyPrefix) {
      let $1 = split1(node_index + 1, children2);
      let before_children = $1[0];
      let after_children = $1[1];
      let prefix = balanced(shift, before_children);
      let suffix = balanced(shift, after_children);
      return new Split(prefix, suffix);
    } else {
      let prefix = $.prefix;
      let suffix = $.suffix;
      let $1 = split1(node_index + 1, children2);
      let before_children = $1[0];
      let after_children = $1[1];
      let prefix$1 = balanced(shift, append4(before_children, prefix));
      let suffix$1 = unbalanced(shift, set1(1, after_children, suffix));
      return new Split(prefix$1, suffix$1);
    }
  } else if (node instanceof Unbalanced) {
    let sizes = node.sizes;
    let children2 = node.children;
    let start_search_index = bsr(index3, shift);
    let node_index = find_size(sizes, start_search_index + 1, index3);
    let index$1 = (() => {
      if (node_index === 0) {
        return index3;
      } else {
        return index3 - get1(node_index, sizes);
      }
    })();
    let child = get1(node_index + 1, children2);
    let child_shift = shift - branch_bits;
    let $ = split_node(child_shift, child, index$1);
    if ($ instanceof EmptyPrefix && node_index === 0) {
      return new EmptyPrefix();
    } else if ($ instanceof EmptyPrefix) {
      let $1 = split1(node_index + 1, children2);
      let before_children = $1[0];
      let after_children = $1[1];
      let $2 = split1(node_index + 1, sizes);
      let before_sizes = $2[0];
      let after_sizes = $2[1];
      let before_size = get1(node_index, before_sizes);
      let after_sizes$1 = map3(
        after_sizes,
        (s) => {
          return s - before_size;
        }
      );
      let prefix = new Unbalanced(before_sizes, before_children);
      let suffix = new Unbalanced(after_sizes$1, after_children);
      return new Split(prefix, suffix);
    } else {
      let prefix = $.prefix;
      let suffix = $.suffix;
      let $1 = split1(node_index + 1, children2);
      let before_children = $1[0];
      let after_children = $1[1];
      let $2 = split1(node_index + 1, sizes);
      let before_sizes = $2[0];
      let after_sizes = $2[1];
      let before_children$1 = append4(before_children, prefix);
      let before_size = (() => {
        if (node_index === 0) {
          return 0;
        } else {
          return get1(node_index, before_sizes);
        }
      })();
      let before_sizes$1 = append4(
        before_sizes,
        before_size + node_size(prefix)
      );
      let after_children$1 = set1(1, after_children, suffix);
      let after_delta = node_size(suffix) - get1(1, after_sizes);
      let after_sizes$1 = map3(
        after_sizes,
        (s) => {
          return s + after_delta;
        }
      );
      let prefix$1 = new Unbalanced(before_sizes$1, before_children$1);
      let suffix$1 = new Unbalanced(after_sizes$1, after_children$1);
      return new Split(prefix$1, suffix$1);
    }
  } else {
    let children2 = node.children;
    if (index3 === 0) {
      return new EmptyPrefix();
    } else {
      let $ = split1(index3 + 1, children2);
      let before = $[0];
      let after = $[1];
      return new Split(new Leaf(before), new Leaf(after));
    }
  }
}
function split2(array4, index3) {
  if (array4 instanceof Empty2) {
    return [new Empty2(), new Empty2()];
  } else if (index3 <= 0) {
    return [new Empty2(), array4];
  } else {
    let shift = array4.shift;
    let root = array4.root;
    let $ = node_size(root);
    if (index3 >= $) {
      let length$1 = $;
      return [array4, new Empty2()];
    } else {
      let $1 = split_node(shift, root, index3);
      if ($1 instanceof Split) {
        let prefix = $1.prefix;
        let suffix = $1.suffix;
        return [new Array2(shift, prefix), new Array2(shift, suffix)];
      } else {
        return [new Empty2(), array4];
      }
    }
  }
}
function drop_first(array4, n) {
  let $ = split2(array4, n);
  let result = $[1];
  return result;
}
function take_first(array4, n) {
  let $ = split2(array4, n);
  let result = $[0];
  return result;
}
var branch_factor = 32;
function push_node(nodes, node, shift) {
  if (nodes.hasLength(0)) {
    return toList([singleton(node)]);
  } else {
    let nodes$1 = nodes.head;
    let rest$1 = nodes.tail;
    let $ = length3(nodes$1) < branch_factor;
    if ($) {
      return prepend(append4(nodes$1, node), rest$1);
    } else {
      let shift$1 = shift + branch_bits;
      let new_node = balanced(shift$1, nodes$1);
      return prepend(
        singleton(node),
        push_node(rest$1, new_node, shift$1)
      );
    }
  }
}
function builder_append(builder, item) {
  let nodes = builder.nodes;
  let items = builder.items;
  let $ = length3(items) === branch_factor;
  if ($) {
    let leaf = new Leaf(items);
    return new Builder(push_node(nodes, leaf, 0), singleton(item));
  } else {
    return new Builder(nodes, append4(items, item));
  }
}
function compress_nodes(loop$nodes, loop$shift) {
  while (true) {
    let nodes = loop$nodes;
    let shift = loop$shift;
    if (nodes.hasLength(0)) {
      return new Empty2();
    } else if (nodes.hasLength(1)) {
      let root = nodes.head;
      return array(shift, root);
    } else {
      let nodes$1 = nodes.head;
      let rest$1 = nodes.tail;
      let shift$1 = shift + branch_bits;
      loop$nodes = push_node(rest$1, balanced(shift$1, nodes$1), shift$1);
      loop$shift = shift$1;
    }
  }
}
function builder_to_array(builder) {
  let nodes = builder.nodes;
  let items = builder.items;
  let items_len = length3(items);
  let $ = items_len === 0 && is_empty(nodes);
  if ($) {
    return new Empty2();
  } else {
    let nodes$1 = (() => {
      let $1 = items_len > 0;
      if ($1) {
        return push_node(nodes, new Leaf(items), 0);
      } else {
        return nodes;
      }
    })();
    return compress_nodes(nodes$1, 0);
  }
}
function initialise_loop(loop$idx, loop$length, loop$builder, loop$fun) {
  while (true) {
    let idx = loop$idx;
    let length5 = loop$length;
    let builder = loop$builder;
    let fun = loop$fun;
    let $ = idx < length5;
    if ($) {
      loop$idx = idx + 1;
      loop$length = length5;
      loop$builder = builder_append(builder, fun(idx));
      loop$fun = fun;
    } else {
      return builder_to_array(builder);
    }
  }
}
function initialise(length5, fun) {
  return initialise_loop(0, length5, builder_new(), fun);
}
function range2(start4, stop) {
  let $ = start4 <= stop;
  if ($) {
    return initialise(stop - start4 + 1, (x) => {
      return x + start4;
    });
  } else {
    return initialise(start4 - stop + 1, (x) => {
      return start4 - x;
    });
  }
}
function compute_balance(params) {
  let s = (() => {
    if (params instanceof FromLeft) {
      let left = params.left;
      let merged = params.merged;
      return fold_skip_last(left, 0, sum_children_counts) + fold3(
        merged,
        0,
        sum_children_counts
      );
    } else if (params instanceof FromRight) {
      let merged = params.merged;
      let right = params.right;
      return fold3(merged, 0, sum_children_counts) + fold_skip_first(
        right,
        0,
        sum_children_counts
      );
    } else {
      let left = params.left;
      let merged = params.merged;
      let right = params.right;
      return fold_skip_last(left, 0, sum_children_counts) + fold3(
        merged,
        0,
        sum_children_counts
      ) + fold_skip_first(right, 0, sum_children_counts);
    }
  })();
  let n = (() => {
    if (params instanceof FromLeft) {
      let left = params.left;
      let merged = params.merged;
      return length3(left) + length3(merged) - 1;
    } else if (params instanceof FromRight) {
      let merged = params.merged;
      let right = params.right;
      return length3(merged) + length3(right) - 1;
    } else {
      let left = params.left;
      let merged = params.merged;
      let right = params.right;
      return length3(left) + length3(merged) + length3(
        right
      ) - 2;
    }
  })();
  let n_opt = bsr(s + branch_factor - 1, branch_bits);
  return n - n_opt - 2;
}
function rebalance_push_subtree(state, subtree, overflow, shift) {
  let $ = length3(state.subtrees) === branch_factor;
  if ($) {
    let root = branch(shift + branch_bits, state.subtrees);
    let _record = state;
    return new RebalanceState(
      _record.balance,
      append4(state.roots, root),
      singleton(subtree),
      overflow
    );
  } else {
    let _record = state;
    return new RebalanceState(
      _record.balance,
      _record.roots,
      append4(state.subtrees, subtree),
      overflow
    );
  }
}
function rebalance_push(state, subtree, extract, construct, shift) {
  let children2 = extract(subtree);
  let subtree_len = length3(children2);
  let $ = length3(state.overflow);
  if ($ === 0) {
    let $1 = state.balance <= 0 || subtree_len >= branch_factor - 1;
    if ($1) {
      return rebalance_push_subtree(state, subtree, state.overflow, shift);
    } else {
      return rebalance_skip_subtree(state, children2);
    }
  } else {
    let overflow_len = $;
    let $1 = overflow_len + subtree_len <= branch_factor;
    if ($1) {
      let $2 = overflow_len + subtree_len < branch_factor - 1;
      if ($2) {
        let overflow = concat2(state.overflow, children2);
        return rebalance_skip_subtree(state, overflow);
      } else {
        let children$1 = concat2(state.overflow, children2);
        let subtree$1 = construct(children$1);
        return rebalance_push_subtree(state, subtree$1, empty(), shift);
      }
    } else {
      let subtree_len$1 = (() => {
        let $22 = subtree_len + overflow_len;
        if ($22 < 32) {
          let subtree_len$12 = $22;
          return subtree_len$12;
        } else {
          return branch_factor;
        }
      })();
      let $2 = split1(subtree_len$1 - overflow_len + 1, children2);
      let to_move = $2[0];
      let overflow = $2[1];
      let subtree$1 = construct(concat2(state.overflow, to_move));
      return rebalance_push_subtree(state, subtree$1, overflow, shift);
    }
  }
}
function rebalance_finalise(state, construct, shift) {
  let state$1 = (() => {
    let $2 = length3(state.overflow) > 0;
    if ($2) {
      let node = construct(state.overflow);
      return rebalance_push_subtree(state, node, empty(), shift);
    } else {
      return state;
    }
  })();
  let $ = length3(state$1.subtrees) > 0;
  if ($) {
    let root = branch(shift + branch_bits, state$1.subtrees);
    return append4(state$1.roots, root);
  } else {
    return state$1.roots;
  }
}
function do_rebalance(shift, params, extract, construct) {
  let balance = compute_balance(params);
  let state = new RebalanceState(
    balance,
    empty(),
    empty(),
    empty()
  );
  let push = (state2, node) => {
    return rebalance_push(state2, node, extract, construct, shift);
  };
  let state$1 = (() => {
    if (params instanceof FromLeft) {
      let left = params.left;
      let merged = params.merged;
      let _pipe = state;
      let _pipe$1 = fold_skip_last(left, _pipe, push);
      return fold3(merged, _pipe$1, push);
    } else if (params instanceof FromRight) {
      let merged = params.merged;
      let right = params.right;
      let _pipe = state;
      let _pipe$1 = fold3(merged, _pipe, push);
      return fold_skip_first(right, _pipe$1, push);
    } else {
      let left = params.left;
      let merged = params.merged;
      let right = params.right;
      let _pipe = state;
      let _pipe$1 = fold_skip_last(left, _pipe, push);
      let _pipe$2 = fold3(merged, _pipe$1, push);
      return fold_skip_first(right, _pipe$2, push);
    }
  })();
  return rebalance_finalise(state$1, construct, shift);
}
function rebalance(shift, params) {
  let shift$1 = shift - branch_bits;
  let $ = shift$1 > 0;
  if ($) {
    let construct = (children2) => {
      return branch(shift$1, children2);
    };
    return do_rebalance(shift$1, params, extract_children, construct);
  } else {
    return do_rebalance(
      shift$1,
      params,
      extract_items,
      (var0) => {
        return new Leaf(var0);
      }
    );
  }
}
function concat_children(left_shift, left_children, right_shift, right_children) {
  let left_down = left_shift - branch_bits;
  let left_init = get1(length3(left_children), left_children);
  let right_down = right_shift - branch_bits;
  let right_head = get1(1, right_children);
  let merged = concat_nodes(left_init, left_down, right_head, right_down);
  let params = new RebalanceMerge(left_children, merged, right_children);
  return rebalance(left_shift, params);
}
function concat_nodes(left, left_shift, right, right_shift) {
  if (left instanceof Balanced && left_shift > right_shift) {
    let cl = left.children;
    return concat_nodes_left(left_shift, cl, right_shift, right);
  } else if (left instanceof Unbalanced && left_shift > right_shift) {
    let cl = left.children;
    return concat_nodes_left(left_shift, cl, right_shift, right);
  } else if (right instanceof Balanced && right_shift > left_shift) {
    let cr = right.children;
    return concat_nodes_right(left_shift, left, right_shift, cr);
  } else if (right instanceof Unbalanced && right_shift > left_shift) {
    let cr = right.children;
    return concat_nodes_right(left_shift, left, right_shift, cr);
  } else if (left instanceof Balanced && right instanceof Leaf) {
    let cl = left.children;
    return concat_nodes_left(left_shift, cl, right_shift, right);
  } else if (left instanceof Unbalanced && right instanceof Leaf) {
    let cl = left.children;
    return concat_nodes_left(left_shift, cl, right_shift, right);
  } else if (left instanceof Leaf && right instanceof Balanced) {
    let cr = right.children;
    return concat_nodes_right(left_shift, left, right_shift, cr);
  } else if (left instanceof Leaf && right instanceof Unbalanced) {
    let cr = right.children;
    return concat_nodes_right(left_shift, left, right_shift, cr);
  } else if (left instanceof Balanced && right instanceof Balanced) {
    let cl = left.children;
    let cr = right.children;
    return concat_children(left_shift, cl, right_shift, cr);
  } else if (left instanceof Balanced && right instanceof Unbalanced) {
    let cl = left.children;
    let cr = right.children;
    return concat_children(left_shift, cl, right_shift, cr);
  } else if (left instanceof Unbalanced && right instanceof Balanced) {
    let cl = left.children;
    let cr = right.children;
    return concat_children(left_shift, cl, right_shift, cr);
  } else if (left instanceof Unbalanced && right instanceof Unbalanced) {
    let cl = left.children;
    let cr = right.children;
    return concat_children(left_shift, cl, right_shift, cr);
  } else {
    return pair(left, right);
  }
}
function concat3(left, right) {
  if (left instanceof Empty2) {
    return right;
  } else if (right instanceof Empty2) {
    return left;
  } else {
    let shift1 = left.shift;
    let root1 = left.root;
    let shift2 = right.shift;
    let root2 = right.root;
    let max_shift = (() => {
      let $ = shift1 >= shift2;
      if ($) {
        return shift1;
      } else {
        return shift2;
      }
    })();
    let root = concat_nodes(root1, shift1, root2, shift2);
    return array(max_shift, root);
  }
}
function delete$2(array4, index3) {
  let $ = 0 <= index3 && index3 < length4(array4);
  if ($) {
    return new Ok(
      concat3(take_first(array4, index3), drop_first(array4, index3 + 1))
    );
  } else {
    return new Error(void 0);
  }
}
function concat_nodes_left(left_shift, left_children, right_shift, right_child) {
  let down_shift = left_shift - branch_bits;
  let left_init = get1(length3(left_children), left_children);
  let merged = concat_nodes(left_init, down_shift, right_child, right_shift);
  return rebalance(left_shift, new FromLeft(left_children, merged));
}
function concat_nodes_right(left_shift, left_child, right_shift, right_children) {
  let down_shift = right_shift - branch_bits;
  let right_head = get1(1, right_children);
  let merged = concat_nodes(left_child, left_shift, right_head, down_shift);
  return rebalance(right_shift, new FromRight(merged, right_children));
}
function direct_append_balanced(left_shift, left, left_children, right_shift, right) {
  let left_len = length3(left_children);
  let left_last = get1(left_len, left_children);
  let $ = direct_concat_node(
    left_shift - branch_bits,
    left_last,
    right_shift,
    right
  );
  if ($ instanceof Concatenated) {
    let updated = $.merged;
    let children2 = set1(left_len, left_children, updated);
    return new Concatenated(balanced(left_shift, children2));
  } else if ($ instanceof NoFreeSlot && left_len < 32) {
    let node = $.right;
    let children2 = append4(left_children, node);
    let $1 = node_size(left_last) === bsl(1, left_shift);
    if ($1) {
      return new Concatenated(balanced(left_shift, children2));
    } else {
      return new Concatenated(unbalanced(left_shift, children2));
    }
  } else {
    let node = $.right;
    return new NoFreeSlot(left, balanced(left_shift, singleton(node)));
  }
}
function direct_concat_node(left_shift, left, right_shift, right) {
  if (left instanceof Balanced && right instanceof Leaf) {
    let cl = left.children;
    return direct_append_balanced(left_shift, left, cl, right_shift, right);
  } else if (left instanceof Unbalanced && right instanceof Leaf) {
    let sizes = left.sizes;
    let cl = left.children;
    return direct_append_unbalanced(
      left_shift,
      left,
      cl,
      sizes,
      right_shift,
      right
    );
  } else if (left instanceof Leaf && right instanceof Balanced) {
    let cr = right.children;
    return direct_prepend_balanced(left_shift, left, right_shift, right, cr);
  } else if (left instanceof Leaf && right instanceof Unbalanced) {
    let sr = right.sizes;
    let cr = right.children;
    return direct_prepend_unbalanced(
      left_shift,
      left,
      right_shift,
      right,
      cr,
      sr
    );
  } else if (left instanceof Leaf && right instanceof Leaf) {
    let cl = left.children;
    let cr = right.children;
    let $ = length3(cl) + length3(cr) <= branch_factor;
    if ($) {
      return new Concatenated(new Leaf(concat2(cl, cr)));
    } else {
      return new NoFreeSlot(left, right);
    }
  } else if (left instanceof Balanced && left_shift > right_shift) {
    let cl = left.children;
    return direct_append_balanced(left_shift, left, cl, right_shift, right);
  } else if (left instanceof Unbalanced && left_shift > right_shift) {
    let sizes = left.sizes;
    let cl = left.children;
    return direct_append_unbalanced(
      left_shift,
      left,
      cl,
      sizes,
      right_shift,
      right
    );
  } else if (right instanceof Balanced && right_shift > left_shift) {
    let cr = right.children;
    return direct_prepend_balanced(left_shift, left, right_shift, right, cr);
  } else if (right instanceof Unbalanced && right_shift > left_shift) {
    let sr = right.sizes;
    let cr = right.children;
    return direct_prepend_unbalanced(
      left_shift,
      left,
      right_shift,
      right,
      cr,
      sr
    );
  } else if (left instanceof Balanced && right instanceof Balanced) {
    let cl = left.children;
    let cr = right.children;
    let $ = length3(cl) + length3(cr) <= branch_factor;
    if ($) {
      let merged = concat2(cl, cr);
      let left_last = get1(length3(cl), cl);
      let $1 = node_size(left_last) === bsl(1, left_shift);
      if ($1) {
        return new Concatenated(balanced(left_shift, merged));
      } else {
        return new Concatenated(unbalanced(left_shift, merged));
      }
    } else {
      return new NoFreeSlot(left, right);
    }
  } else if (left instanceof Balanced && right instanceof Unbalanced) {
    let cl = left.children;
    let cr = right.children;
    let $ = length3(cl) + length3(cr) <= branch_factor;
    if ($) {
      let merged = concat2(cl, cr);
      return new Concatenated(unbalanced(left_shift, merged));
    } else {
      return new NoFreeSlot(left, right);
    }
  } else if (left instanceof Unbalanced && right instanceof Balanced) {
    let cl = left.children;
    let cr = right.children;
    let $ = length3(cl) + length3(cr) <= branch_factor;
    if ($) {
      let merged = concat2(cl, cr);
      return new Concatenated(unbalanced(left_shift, merged));
    } else {
      return new NoFreeSlot(left, right);
    }
  } else {
    let cl = left.children;
    let cr = right.children;
    let $ = length3(cl) + length3(cr) <= branch_factor;
    if ($) {
      let merged = concat2(cl, cr);
      return new Concatenated(unbalanced(left_shift, merged));
    } else {
      return new NoFreeSlot(left, right);
    }
  }
}
function direct_concat(left, right) {
  if (left instanceof Empty2) {
    return right;
  } else if (right instanceof Empty2) {
    return left;
  } else {
    let left_shift = left.shift;
    let left$1 = left.root;
    let right_shift = right.shift;
    let right$1 = right.root;
    let shift = (() => {
      let $2 = left_shift > right_shift;
      if ($2) {
        return left_shift;
      } else {
        return right_shift;
      }
    })();
    let $ = direct_concat_node(left_shift, left$1, right_shift, right$1);
    if ($ instanceof Concatenated) {
      let root = $.merged;
      return new Array2(shift, root);
    } else {
      let left$2 = $.left;
      let right$2 = $.right;
      return array(shift, pair(left$2, right$2));
    }
  }
}
function append5(array4, item) {
  return direct_concat(array4, wrap(item));
}
function insert2(array4, index3, item) {
  let $ = length4(array4);
  if (0 <= index3 && index3 <= $) {
    let len = $;
    let $1 = split2(array4, index3);
    let before = $1[0];
    let after = $1[1];
    return new Ok(concat3(direct_concat(before, wrap(item)), after));
  } else {
    return new Error(void 0);
  }
}
function direct_append_unbalanced(left_shift, left, left_children, sizes, right_shift, right) {
  let left_len = length3(left_children);
  let left_last = get1(left_len, left_children);
  let $ = direct_concat_node(
    left_shift - branch_bits,
    left_last,
    right_shift,
    right
  );
  if ($ instanceof Concatenated) {
    let updated = $.merged;
    let children2 = set1(left_len, left_children, updated);
    let last_size = get1(left_len, sizes) + node_size(updated);
    let sizes$1 = set1(left_len, sizes, last_size);
    return new Concatenated(new Unbalanced(sizes$1, children2));
  } else if ($ instanceof NoFreeSlot && left_len < 32) {
    let node = $.right;
    let children2 = append4(left_children, node);
    let sizes$1 = append4(
      sizes,
      get1(left_len, sizes) + node_size(node)
    );
    return new Concatenated(new Unbalanced(sizes$1, children2));
  } else {
    let node = $.right;
    return new NoFreeSlot(left, balanced(left_shift, singleton(node)));
  }
}
function direct_prepend_balanced(left_shift, left, right_shift, right, right_children) {
  let right_len = length3(right_children);
  let right_first = get1(1, right_children);
  let $ = direct_concat_node(
    left_shift,
    left,
    right_shift - branch_bits,
    right_first
  );
  if ($ instanceof Concatenated) {
    let updated = $.merged;
    let children2 = set1(1, right_children, updated);
    return new Concatenated(unbalanced(right_shift, children2));
  } else if ($ instanceof NoFreeSlot && right_len < 32) {
    let node = $.left;
    let children2 = prepend2(right_children, node);
    return new Concatenated(unbalanced(right_shift, children2));
  } else {
    let node = $.left;
    return new NoFreeSlot(balanced(right_shift, singleton(node)), right);
  }
}
function direct_prepend_unbalanced(left_shift, left, right_shift, right, right_children, sizes) {
  let right_len = length3(right_children);
  let right_first = get1(1, right_children);
  let $ = direct_concat_node(
    left_shift,
    left,
    right_shift - branch_bits,
    right_first
  );
  if ($ instanceof Concatenated) {
    let updated = $.merged;
    let children2 = set1(1, right_children, updated);
    let size_delta = node_size(updated) - node_size(right_first);
    let sizes$1 = map3(sizes, (s) => {
      return s + size_delta;
    });
    return new Concatenated(new Unbalanced(sizes$1, children2));
  } else if ($ instanceof NoFreeSlot && right_len < 32) {
    let node = $.left;
    let children2 = prepend2(right_children, node);
    let size2 = node_size(node);
    let sizes$1 = (() => {
      let _pipe = sizes;
      let _pipe$1 = map3(_pipe, (s) => {
        return s + size2;
      });
      return prepend2(_pipe$1, size2);
    })();
    return new Concatenated(new Unbalanced(sizes$1, children2));
  } else {
    let node = $.left;
    return new NoFreeSlot(balanced(right_shift, singleton(node)), right);
  }
}

// build/dev/javascript/gleam_json/gleam_json_ffi.mjs
function identity2(x) {
  return x;
}

// build/dev/javascript/gleam_json/gleam/json.mjs
function bool(input2) {
  return identity2(input2);
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
function contains(set2, member) {
  let _pipe = set2.dict;
  let _pipe$1 = map_get(_pipe, member);
  return is_ok(_pipe$1);
}
var token = void 0;
function insert3(set2, member) {
  return new Set2(insert(set2.dict, member, token));
}

// build/dev/javascript/lustre/lustre/internals/keyed_lookup.mjs
var KeyedLookup = class extends CustomType {
  constructor(values2, visited2) {
    super();
    this.values = values2;
    this.visited = visited2;
  }
};
function set(lookup, key, value2) {
  let values2 = lookup.values;
  let visited$1 = lookup.visited;
  return new KeyedLookup(insert(values2, key, value2), visited$1);
}
function new$2() {
  return new KeyedLookup(new_map(), new$());
}
function has(lookup, key) {
  return !contains(lookup.visited, key) && has_key(
    lookup.values,
    key
  );
}
function visited(lookup, key) {
  return contains(lookup.visited, key);
}
function delete$3(lookup, key) {
  let values2 = lookup.values;
  let visited$1 = lookup.visited;
  return new KeyedLookup(values2, insert3(visited$1, key));
}
function pop(lookup, key) {
  let values2 = lookup.values;
  let visited$1 = lookup.visited;
  let $ = contains(visited$1, key);
  if (!$) {
    let $1 = map_get(values2, key);
    if ($1.isOk()) {
      let value2 = $1[0];
      return new Ok(
        [value2, new KeyedLookup(values2, insert3(visited$1, key))]
      );
    } else {
      return new Error(void 0);
    }
  } else {
    return new Error(void 0);
  }
}
function remaining_keys(lookup) {
  let values2 = lookup.values;
  let visited$1 = lookup.visited;
  return fold(
    values2,
    toList([]),
    (result, key, _) => {
      let $ = contains(visited$1, key);
      if ($) {
        return result;
      } else {
        return prepend(key, result);
      }
    }
  );
}

// build/dev/javascript/lustre/lustre/runtime/vdom.mjs
var Fragment = class extends CustomType {
  constructor(key, children2) {
    super();
    this.key = key;
    this.children = children2;
  }
};
var Node2 = class extends CustomType {
  constructor(key, namespace, tag, attributes, mapper, children2, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.mapper = mapper;
    this.children = children2;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Text = class extends CustomType {
  constructor(key, content) {
    super();
    this.key = key;
    this.content = content;
  }
};
var Attribute = class extends CustomType {
  constructor(name, value2) {
    super();
    this.name = name;
    this.value = value2;
  }
};
var Property = class extends CustomType {
  constructor(name, value2) {
    super();
    this.name = name;
    this.value = value2;
  }
};
var Event = class extends CustomType {
  constructor(name, handler, prevent_default2, stop_propagation2, immediate) {
    super();
    this.name = name;
    this.handler = handler;
    this.prevent_default = prevent_default2;
    this.stop_propagation = stop_propagation2;
    this.immediate = immediate;
  }
};
var Diff = class extends CustomType {
  constructor(patch, handlers2) {
    super();
    this.patch = patch;
    this.handlers = handlers2;
  }
};
var Patch = class extends CustomType {
  constructor(index3, changes, children2) {
    super();
    this.index = index3;
    this.changes = changes;
    this.children = children2;
  }
};
var Append = class extends CustomType {
  constructor(children2) {
    super();
    this.children = children2;
  }
};
var Insert = class extends CustomType {
  constructor(child, at) {
    super();
    this.child = child;
    this.at = at;
  }
};
var Move = class extends CustomType {
  constructor(key, to) {
    super();
    this.key = key;
    this.to = to;
  }
};
var RemoveAll = class extends CustomType {
  constructor(from) {
    super();
    this.from = from;
  }
};
var RemoveKey = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var Remove = class extends CustomType {
  constructor(from, count) {
    super();
    this.from = from;
    this.count = count;
  }
};
var Replace = class extends CustomType {
  constructor(element2) {
    super();
    this.element = element2;
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
var Cursor = class extends CustomType {
  constructor(meta2, node, idx, old, new$4) {
    super();
    this.meta = meta2;
    this.node = node;
    this.idx = idx;
    this.old = old;
    this.new = new$4;
  }
};
var Metadata = class extends CustomType {
  constructor(fragment, keyed, keyed_children, keyed_moves) {
    super();
    this.fragment = fragment;
    this.keyed = keyed;
    this.keyed_children = keyed_children;
    this.keyed_moves = keyed_moves;
  }
};
var AttributeChange = class extends CustomType {
  constructor(added, removed) {
    super();
    this.added = added;
    this.removed = removed;
  }
};
function do_diff_attributes(loop$prev, loop$next, loop$added) {
  while (true) {
    let prev = loop$prev;
    let next = loop$next;
    let added = loop$added;
    if (next.hasLength(0)) {
      return new AttributeChange(added, keys(prev));
    } else {
      let attr = next.head;
      let rest = next.tail;
      let $ = map_get(prev, attr.name);
      if (!$.isOk()) {
        loop$prev = prev;
        loop$next = rest;
        loop$added = prepend(attr, added);
      } else if (attr instanceof Attribute && $.isOk() && $[0] instanceof Attribute) {
        let old = $[0];
        let $1 = attr.name;
        if ($1 === "value") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "checked") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "selected") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if (attr.value === old.value) {
          loop$prev = delete$(prev, attr.name);
          loop$next = rest;
          loop$added = added;
        } else {
          loop$prev = delete$(prev, attr.name);
          loop$next = rest;
          loop$added = prepend(attr, added);
        }
      } else if (attr instanceof Property && $.isOk() && $[0] instanceof Property) {
        let old = $[0];
        let $1 = attr.name;
        if ($1 === "value") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "checked") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "selected") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "scrollLeft") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if ($1 === "scrollTop") {
          loop$prev = prev;
          loop$next = rest;
          loop$added = prepend(attr, added);
        } else if (isEqual(attr.value, old.value)) {
          loop$prev = delete$(prev, attr.name);
          loop$next = rest;
          loop$added = added;
        } else {
          loop$prev = delete$(prev, attr.name);
          loop$next = rest;
          loop$added = prepend(attr, added);
        }
      } else if (attr instanceof Event && $.isOk() && $[0] instanceof Event && isEqual(attr, $[0])) {
        let old = $[0];
        loop$prev = delete$(prev, attr.name);
        loop$next = rest;
        loop$added = added;
      } else if (attr instanceof Event && $.isOk() && $[0] instanceof Event) {
        loop$prev = delete$(prev, attr.name);
        loop$next = rest;
        loop$added = prepend(attr, added);
      } else {
        loop$prev = prev;
        loop$next = rest;
        loop$added = prepend(attr, added);
      }
    }
  }
}
function diff_attributes(prev, next) {
  if (prev.hasLength(0) && next.hasLength(0)) {
    return new AttributeChange(toList([]), toList([]));
  } else if (prev.hasLength(0)) {
    return new AttributeChange(next, toList([]));
  } else if (next.hasLength(0)) {
    return new AttributeChange(
      toList([]),
      map(prev, (attribute2) => {
        return attribute2.name;
      })
    );
  } else {
    let prev$1 = fold2(
      prev,
      new_map(),
      (acc, attr) => {
        return insert(acc, attr.name, attr);
      }
    );
    return do_diff_attributes(prev$1, next, toList([]));
  }
}
function do_add_keyed_children(loop$keyed_children, loop$index, loop$children) {
  while (true) {
    let keyed_children = loop$keyed_children;
    let index3 = loop$index;
    let children2 = loop$children;
    if (children2.hasLength(0)) {
      return keyed_children;
    } else {
      let child = children2.head;
      let rest = children2.tail;
      let _pipe = keyed_children;
      let _pipe$1 = set(_pipe, child.key, child);
      loop$keyed_children = _pipe$1;
      loop$index = index3;
      loop$children = rest;
    }
  }
}
function add_keyed_children(keyed_children, children2) {
  if (children2.hasLength(0)) {
    return keyed_children;
  } else if (children2.atLeastLength(1) && children2.head.key === "") {
    let child = children2.head;
    return keyed_children;
  } else {
    return do_add_keyed_children(keyed_children, 0, children2);
  }
}
function do_diff(loop$handlers, loop$stack) {
  while (true) {
    let handlers2 = loop$handlers;
    let stack = loop$stack;
    if (stack.hasLength(0)) {
      return new Diff(new Patch(0, toList([]), toList([])), handlers2);
    } else if (stack.hasLength(1) && stack.head instanceof Cursor && stack.head.old.hasLength(0) && stack.head.new.hasLength(0)) {
      let node = stack.head.node;
      return new Diff(node, handlers2);
    } else if (stack.atLeastLength(2) && stack.head instanceof Cursor && stack.head.old.hasLength(0) && stack.head.new.hasLength(0)) {
      let meta2 = stack.head.meta;
      let node = stack.head.node;
      let idx = stack.head.idx;
      let next = stack.tail.head;
      let stack$1 = stack.tail.tail;
      let $ = meta2.fragment;
      let $1 = node.changes;
      let $2 = node.children;
      if (!$ && $1.hasLength(0) && $2.hasLength(0)) {
        loop$handlers = handlers2;
        loop$stack = prepend(next, stack$1);
      } else if (!$) {
        let children2 = prepend(node, next.node.children);
        let parent = (() => {
          let _record = next.node;
          return new Patch(_record.index, _record.changes, children2);
        })();
        let next$1 = (() => {
          let _record = next;
          return new Cursor(
            _record.meta,
            parent,
            _record.idx,
            _record.old,
            _record.new
          );
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(next$1, stack$1);
      } else if ($ && $1.hasLength(0) && $2.hasLength(0)) {
        let meta$1 = (() => {
          let _record = next.meta;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            _record.keyed_children,
            meta2.keyed_moves
          );
        })();
        let node$1 = (() => {
          let _record = node;
          return new Patch(next.node.index, _record.changes, _record.children);
        })();
        let next$1 = (() => {
          let _record = next;
          return new Cursor(meta$1, node$1, idx, _record.old, _record.new);
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(next$1, stack$1);
      } else {
        let meta$1 = (() => {
          let _record = next.meta;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            _record.keyed_children,
            meta2.keyed_moves
          );
        })();
        let node$1 = (() => {
          let _record = node;
          return new Patch(next.node.index, _record.changes, _record.children);
        })();
        let next$1 = (() => {
          let _record = next;
          return new Cursor(meta$1, node$1, idx, _record.old, _record.new);
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(next$1, stack$1);
      }
    } else {
      let cursor = stack.head;
      let meta2 = stack.head.meta;
      let node = stack.head.node;
      let idx = stack.head.idx;
      let old = stack.head.old;
      let new$4 = stack.head.new;
      let stack$1 = stack.tail;
      let has_key2 = (() => {
        if (new$4.atLeastLength(1) && meta2.keyed) {
          let head = new$4.head;
          return has(meta2.keyed_children, head.key) || visited(
            meta2.keyed_children,
            head.key
          );
        } else {
          return false;
        }
      })();
      if (old.hasLength(0)) {
        let node$1 = (() => {
          let _record = node;
          return new Patch(
            _record.index,
            prepend(new Append(new$4), node.changes),
            _record.children
          );
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(
            _record.meta,
            node$1,
            _record.idx,
            _record.old,
            toList([])
          );
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$4.hasLength(0) && meta2.keyed) {
        let changes = (() => {
          let _pipe = remaining_keys(meta2.keyed_children);
          return fold2(
            _pipe,
            node.changes,
            (changes2, key) => {
              return prepend(new RemoveKey(key), changes2);
            }
          );
        })();
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(
            _record.meta,
            node$1,
            _record.idx,
            toList([]),
            _record.new
          );
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$4.hasLength(0) && meta2.fragment) {
        let changes = prepend(
          new Remove(idx, length(old)),
          node.changes
        );
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(
            _record.meta,
            node$1,
            _record.idx,
            toList([]),
            _record.new
          );
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$4.hasLength(0)) {
        let changes = prepend(new RemoveAll(idx), node.changes);
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(
            _record.meta,
            node$1,
            _record.idx,
            toList([]),
            _record.new
          );
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (old.atLeastLength(1) && new$4.atLeastLength(1) && (has_key2 && old.head.key !== new$4.head.key)) {
        let prev = old.head;
        let old$1 = old.tail;
        let new$1 = new$4;
        let next = new$4.head;
        let $ = pop(meta2.keyed_children, next.key);
        if (!$.isOk()) {
          throw makeError(
            "let_assert",
            "lustre/runtime/vdom",
            211,
            "do_diff",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          );
        }
        let match = $[0][0];
        let keyed_children = $[0][1];
        let old$2 = prepend(match, prepend(prev, old$1));
        let changes = prepend(
          new Move(next.key, idx - meta2.keyed_moves),
          node.changes
        );
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let meta$1 = (() => {
          let _record = meta2;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            keyed_children,
            meta2.keyed_moves + 1
          );
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(meta$1, node$1, _record.idx, old$2, new$1);
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$4.atLeastLength(1) && (meta2.keyed && !has_key2)) {
        let next = new$4.head;
        let new$1 = new$4.tail;
        let changes = prepend(
          new Insert(next, idx - meta2.keyed_moves),
          node.changes
        );
        let meta$1 = (() => {
          let _record = meta2;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            _record.keyed_children,
            meta2.keyed_moves + 1
          );
        })();
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(meta$1, node$1, idx, _record.old, new$1);
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      } else if (old.atLeastLength(1) && old.head instanceof Fragment && new$4.atLeastLength(1) && new$4.head instanceof Fragment) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$4.head;
        let new$1 = new$4.tail;
        let child_meta = (() => {
          let $ = prev.children;
          if ($.atLeastLength(1) && $.head.key !== "") {
            let head = $.head;
            return new Metadata(
              true,
              true,
              add_keyed_children(new$2(), prev.children),
              meta2.keyed_moves
            );
          } else {
            return new Metadata(
              true,
              false,
              new$2(),
              meta2.keyed_moves
            );
          }
        })();
        let child_cursor = new Cursor(
          child_meta,
          (() => {
            let _record = node;
            return new Patch(idx, _record.changes, _record.children);
          })(),
          idx,
          prev.children,
          next.children
        );
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(_record.meta, node, _record.idx, old$1, new$1);
        })();
        loop$handlers = handlers2;
        loop$stack = prepend(child_cursor, prepend(cursor$1, stack$1));
      } else if (old.atLeastLength(1) && old.head instanceof Node2 && new$4.atLeastLength(1) && new$4.head instanceof Node2 && (old.head.namespace === new$4.head.namespace && old.head.tag === new$4.head.tag)) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$4.head;
        let new$1 = new$4.tail;
        let child_meta = (() => {
          let $ = prev.children;
          if ($.atLeastLength(1) && $.head.key !== "") {
            let head = $.head;
            return new Metadata(
              false,
              true,
              add_keyed_children(new$2(), prev.children),
              meta2.keyed_moves
            );
          } else {
            return new Metadata(
              false,
              false,
              new$2(),
              meta2.keyed_moves
            );
          }
        })();
        let child_cursor = new Cursor(
          child_meta,
          (() => {
            let $ = diff_attributes(prev.attributes, next.attributes);
            if ($ instanceof AttributeChange && $.added.hasLength(0) && $.removed.hasLength(0)) {
              return new Patch(idx, toList([]), toList([]));
            } else {
              let added = $.added;
              let removed = $.removed;
              return new Patch(
                idx,
                toList([new Update(added, removed)]),
                toList([])
              );
            }
          })(),
          0,
          prev.children,
          next.children
        );
        let keyed_children = delete$3(
          meta2.keyed_children,
          next.key
        );
        let meta$1 = (() => {
          let _record = meta2;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            keyed_children,
            _record.keyed_moves
          );
        })();
        let cursor$1 = new Cursor(meta$1, node, idx + 1, old$1, new$1);
        loop$handlers = handlers2;
        loop$stack = prepend(child_cursor, prepend(cursor$1, stack$1));
      } else if (old.atLeastLength(1) && old.head instanceof Text && new$4.atLeastLength(1) && new$4.head instanceof Text) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$4.head;
        let new$1 = new$4.tail;
        let $ = prev.content === next.content;
        if ($) {
          let keyed_children = delete$3(
            meta2.keyed_children,
            next.key
          );
          let meta$1 = (() => {
            let _record = meta2;
            return new Metadata(
              _record.fragment,
              _record.keyed,
              keyed_children,
              _record.keyed_moves
            );
          })();
          let cursor$1 = (() => {
            let _record = cursor;
            return new Cursor(meta$1, _record.node, idx + 1, old$1, new$1);
          })();
          loop$handlers = handlers2;
          loop$stack = prepend(cursor$1, stack$1);
        } else {
          let changes = toList([new ReplaceText(next.content)]);
          let child = new Patch(idx, changes, toList([]));
          let node$1 = (() => {
            let _record = node;
            return new Patch(
              _record.index,
              _record.changes,
              prepend(child, node.children)
            );
          })();
          let keyed_children = delete$3(
            meta2.keyed_children,
            next.key
          );
          let meta$1 = (() => {
            let _record = meta2;
            return new Metadata(
              _record.fragment,
              _record.keyed,
              keyed_children,
              _record.keyed_moves
            );
          })();
          let cursor$1 = new Cursor(meta$1, node$1, idx + 1, old$1, new$1);
          loop$handlers = handlers2;
          loop$stack = prepend(cursor$1, stack$1);
        }
      } else {
        let old$1 = old.tail;
        let next = new$4.head;
        let new$1 = new$4.tail;
        let child = new Patch(idx, toList([new Replace(next)]), toList([]));
        let node$1 = (() => {
          let _record = node;
          return new Patch(
            _record.index,
            _record.changes,
            prepend(child, node.children)
          );
        })();
        let keyed_children = delete$3(
          meta2.keyed_children,
          next.key
        );
        let meta$1 = (() => {
          let _record = meta2;
          return new Metadata(
            _record.fragment,
            _record.keyed,
            keyed_children,
            _record.keyed_moves
          );
        })();
        let cursor$1 = new Cursor(meta$1, node$1, idx + 1, old$1, new$1);
        loop$handlers = handlers2;
        loop$stack = prepend(cursor$1, stack$1);
      }
    }
  }
}
function diff(prev, next, handlers2) {
  return do_diff(
    handlers2,
    toList([
      new Cursor(
        new Metadata(false, false, new$2(), 0),
        new Patch(0, toList([]), toList([])),
        0,
        toList([prev]),
        toList([next])
      )
    ])
  );
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value2) {
  return new Attribute(name, value2);
}
function property(name, value2) {
  return new Property(name, value2);
}
function on(name, handler) {
  return new Event(name, handler, false, false, false);
}
function class$(name) {
  return attribute("class", name);
}
function type_(name) {
  return attribute("type", name);
}
function value(val) {
  return attribute("value", val);
}
function placeholder(text3) {
  return attribute("placeholder", text3);
}
function rows(val) {
  return attribute("rows", to_string(val));
}
function open(is_open) {
  return property("open", bool(is_open));
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attributes, children2) {
  if (tag === "area") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "base") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "br") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "col") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "embed") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "hr") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "img") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "input") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "link") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "meta") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "param") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "source") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "track") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else if (tag === "wbr") {
    return new Node2(
      "",
      "",
      tag,
      attributes,
      new None(),
      toList([]),
      false,
      true
    );
  } else {
    return new Node2("", "", tag, attributes, new None(), children2, false, false);
  }
}
function text(content) {
  return new Text("", content);
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text2 = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Attribute2 = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
function attribute_to_event_handler(attribute2) {
  if (attribute2 instanceof Attribute2) {
    return new Error(void 0);
  } else {
    let name = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_start(name, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element2, index3) => {
      let key$1 = key + "-" + to_string(index3);
      return do_handlers(element2, handlers3, key$1);
    }
  );
}
function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element2 = loop$element;
    let handlers2 = loop$handlers;
    let key = loop$key;
    if (element2 instanceof Text2) {
      return handlers2;
    } else if (element2 instanceof Map2) {
      let subtree = element2.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key;
    } else {
      let attrs = element2.attrs;
      let children2 = element2.children;
      let handlers$1 = fold2(
        attrs,
        handlers2,
        (handlers3, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key + "-" + name, handler);
          } else {
            return handlers3;
          }
        }
      );
      return do_element_list_handlers(children2, handlers$1, key);
    }
  }
}
function handlers(element2) {
  return do_handlers(element2, new_map(), "0");
}

// build/dev/javascript/lustre/lustre/internals/patch.mjs
var Diff2 = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Init = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function is_empty_element_diff(diff3) {
  return isEqual(diff3.created, new_map()) && isEqual(
    diff3.removed,
    new$()
  ) && isEqual(diff3.updated, new_map());
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Attrs = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Batch = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
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
var Emit2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Event3 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Shutdown = class extends CustomType {
};
var Subscribe = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unsubscribe = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
if (globalThis.customElements && !globalThis.customElements.get("lustre-fragment")) {
  globalThis.customElements.define(
    "lustre-fragment",
    class LustreFragment extends HTMLElement {
      constructor() {
        super();
      }
    }
  );
}
function morph(prev, next, dispatch) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    while (next2.subtree !== void 0)
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
        stack
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style = null;
  let innerHTML = null;
  if (canMorph && next.tag === "textarea") {
    const innertText = next.children[Symbol.iterator]().next().value?.content;
    if (innertText !== void 0)
      el.value = innertText;
  }
  const delegated = [];
  for (const attr of next.attrs) {
    const name = attr[0];
    const value2 = attr[1];
    if (attr.as_property) {
      if (el[name] !== value2)
        el[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value2, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el.setAttribute(name, value2);
      if (canMorph) {
        prevHandlers.delete(eventName);
        prevAttributes.delete(name);
      }
    } else if (name.startsWith("delegate:data-") || name.startsWith("delegate:aria-")) {
      el.setAttribute(name, value2);
      delegated.push([name.slice(10), value2]);
    } else if (name === "class") {
      className = className === null ? value2 : className + " " + value2;
    } else if (name === "style") {
      style = style === null ? value2 : style + value2;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value2;
    } else {
      if (el.getAttribute(name) !== value2)
        el.setAttribute(name, value2);
      if (name === "value" || name === "selected")
        el[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style !== null) {
    el.setAttribute("style", style);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.tag === "slot") {
    window.queueMicrotask(() => {
      for (const child of el.assignedElements()) {
        for (const [name, value2] of delegated) {
          if (!child.hasAttribute(name)) {
            child.setAttribute(name, value2);
          }
        }
      }
    });
  }
  if (next.key !== void 0 && next.key !== "") {
    el.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el.innerHTML = innerHTML;
    return el;
  }
  let prevChild = el.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = children(next).next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
    for (const child of children(next)) {
      prevChild = diffKeyedChild(
        prevChild,
        child,
        el,
        stack,
        incomingKeyedChildren,
        keyedChildren,
        seenKeys
      );
    }
  } else {
    for (const child of children(next)) {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = next2;
  }
  return el;
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
  const el = event2.currentTarget;
  const tag = el.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property2) => {
        const path = property2.split(".");
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
function getKeyedChildren(el) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el) {
    for (const child of children(el)) {
      const key = child?.key || child?.getAttribute?.("data-lustre-key");
      if (key)
        keyedChildren.set(key, child);
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder2 = document.createTextNode("");
    el.insertBefore(placeholder2, prevChild);
    stack.unshift({ prev: placeholder2, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el });
  return prevChild;
}
function* children(element2) {
  for (const child of element2.children) {
    yield* forceChild(child);
  }
}
function* forceChild(element2) {
  if (element2.subtree !== void 0) {
    yield* forceChild(element2.subtree());
  } else {
    yield element2;
  }
}

// build/dev/javascript/lustre/lustre.ffi.mjs
var LustreClientApplication = class _LustreClientApplication {
  /**
   * @template Flags
   *
   * @param {object} app
   * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
   * @param {(msg: Msg, model: Model) => [Model, Lustre.Effect<Msg>]} app.update
   * @param {(model: Model) => Lustre.Element<Msg>} app.view
   * @param {string | HTMLElement} selector
   * @param {Flags} flags
   *
   * @returns {Gleam.Ok<(action: Lustre.Action<Lustre.Client, Msg>>) => void>}
   */
  static start({ init: init3, update: update3, view: view2 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(root, init3(flags), update3, view2);
    return new Ok((action) => app.send(action));
  }
  /**
   * @param {Element} root
   * @param {[Model, Lustre.Effect<Msg>]} init
   * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} update
   * @param {(model: Model) => Lustre.Element<Msg>} view
   *
   * @returns {LustreClientApplication}
   */
  constructor(root, [init3, effects], update3, view2) {
    this.root = root;
    this.#model = init3;
    this.#update = update3;
    this.#view = view2;
    this.#tickScheduled = window.requestAnimationFrame(
      () => this.#tick(effects.all.toArray(), true)
    );
  }
  /** @type {Element} */
  root;
  /**
   * @param {Lustre.Action<Lustre.Client, Msg>} action
   *
   * @returns {void}
   */
  send(action) {
    if (action instanceof Debug) {
      if (action[0] instanceof ForceModel) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#queue = [];
        this.#model = action[0][0];
        const vdom = this.#view(this.#model);
        const dispatch = (handler, immediate = false) => (event2) => {
          const result = handler(event2);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
        const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
        morph(prev, vdom, dispatch);
      }
    } else if (action instanceof Dispatch) {
      const msg = action[0];
      const immediate = action[1] ?? false;
      this.#queue.push(msg);
      if (immediate) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#tick();
      } else if (!this.#tickScheduled) {
        this.#tickScheduled = window.requestAnimationFrame(() => this.#tick());
      }
    } else if (action instanceof Emit2) {
      const event2 = action[0];
      const data = action[1];
      this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
    } else if (action instanceof Shutdown) {
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#queue = null;
      while (this.root.firstChild) {
        this.root.firstChild.remove();
      }
    }
  }
  /** @type {Model} */
  #model;
  /** @type {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} */
  #update;
  /** @type {(model: Model) => Lustre.Element<Msg>} */
  #view;
  /** @type {Array<Msg>} */
  #queue = [];
  /** @type {number | undefined} */
  #tickScheduled;
  /**
   * @param {Lustre.Effect<Msg>[]} effects
   */
  #tick(effects = []) {
    this.#tickScheduled = void 0;
    this.#flush(effects);
    const vdom = this.#view(this.#model);
    const dispatch = (handler, immediate = false) => (event2) => {
      const result = handler(event2);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0], immediate));
      }
    };
    const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
    morph(prev, vdom, dispatch);
  }
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select2 = () => {
      };
      const root = this.root;
      effect({ dispatch, emit: emit2, select: select2, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start = LustreClientApplication.start;
var LustreServerApplication = class _LustreServerApplication {
  static start({ init: init3, update: update3, view: view2, on_attribute_change }, flags) {
    const app = new _LustreServerApplication(
      init3(flags),
      update3,
      view2,
      on_attribute_change
    );
    return new Ok((action) => app.send(action));
  }
  constructor([model, effects], update3, view2, on_attribute_change) {
    this.#model = model;
    this.#update = update3;
    this.#view = view2;
    this.#html = view2(model);
    this.#onAttributeChange = on_attribute_change;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects.all.toArray());
  }
  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder = this.#onAttributeChange.get(attr[0]);
        if (!decoder)
          continue;
        const msg = decoder(attr[1]);
        if (msg instanceof Error)
          continue;
        this.#queue.push(msg);
      }
      this.#tick();
    } else if (action instanceof Batch) {
      this.#queue = this.#queue.concat(action[0].toArray());
      this.#tick(action[1].all.toArray());
    } else if (action instanceof Debug) {
    } else if (action instanceof Dispatch) {
      this.#queue.push(action[0]);
      this.#tick();
    } else if (action instanceof Emit2) {
      const event2 = new Emit(action[0], action[1]);
      for (const [_, renderer] of this.#renderers) {
        renderer(event2);
      }
    } else if (action instanceof Event3) {
      const handler = this.#handlers.get(action[0]);
      if (!handler)
        return;
      const msg = handler(action[1]);
      if (msg instanceof Error)
        return;
      this.#queue.push(msg[0]);
      this.#tick();
    } else if (action instanceof Subscribe) {
      const attrs = keys(this.#onAttributeChange);
      const patch = new Init(attrs, this.#html);
      this.#renderers = this.#renderers.set(action[0], action[1]);
      action[1](patch);
    } else if (action instanceof Unsubscribe) {
      this.#renderers = this.#renderers.delete(action[0]);
    }
  }
  #model;
  #update;
  #queue;
  #view;
  #html;
  #renderers;
  #handlers;
  #onAttributeChange;
  #tick(effects = []) {
    this.#flush(effects);
    const vdom = this.#view(this.#model);
    const diff3 = elements(this.#html, vdom);
    if (!is_empty_element_diff(diff3)) {
      const patch = new Diff2(diff3);
      for (const [_, renderer] of this.#renderers) {
        renderer(patch);
      }
    }
    this.#html = vdom;
    this.#handlers = diff3.handlers;
  }
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select2 = () => {
      };
      const root = null;
      effect({ dispatch, emit: emit2, select: select2, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start_server_application = LustreServerApplication.start;
var is_browser = () => globalThis.window && window.document;

// build/dev/javascript/lustre/reconciler.ffi.mjs
var meta = Symbol("metadata");
var LustreReconciler = class {
  #root = null;
  #dispatch = () => {
  };
  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
  }
  mount(vnode) {
    this.#root.appendChild(createElement(vnode, this.#dispatch));
  }
  push(patch) {
    reconcile(this.#root, patch, this.#dispatch);
  }
};
function reconcile(root, patch, dispatch) {
  const stack = [{ node: root, patch }];
  while (stack.length) {
    const { node, patch: patch2 } = stack.pop();
    for (const child of patch2.children) {
      stack.push({ node: node.childNodes[child.index], patch: child });
    }
    for (const change of patch2.changes) {
      switch (change.constructor) {
        case Append:
          append6(node, change.children, dispatch);
          break;
        case Insert:
          insert4(node, change.child, change.at, dispatch);
          break;
        case Move:
          move(node, change.key, change.to);
          break;
        case RemoveAll:
          removeAll(node, change.from);
          break;
        case RemoveKey:
          removeKey(node, change.key);
          break;
        case Remove:
          remove(node, change.from, change.count);
          break;
        case Replace:
          replace2(node, change.element, dispatch);
          break;
        case ReplaceText:
          replaceText(node, change.content);
          break;
        case Update:
          update(node, change.added, change.removed, dispatch);
          break;
      }
    }
  }
}
function append6(node, children2, dispatch) {
  const fragment = document.createDocumentFragment();
  for (const child of children2) {
    const el = createElement(child, dispatch);
    fragment.appendChild(el);
    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(el));
    }
  }
  node.appendChild(fragment);
}
function insert4(node, child, at, dispatch) {
  const el = createElement(child, dispatch);
  node.insertBefore(el, node.children[at]);
  if (child.key) {
    node[meta].keyedChildren.set(child.key, new WeakRef(el));
  }
}
function move(node, key, to) {
  node.insertBefore(
    node[meta].keyedChildren.get(key).deref(),
    node.children[to]
  );
}
function removeAll(node, from) {
  node[meta].keyedChildren = /* @__PURE__ */ new Map();
  node.innerHTML = "";
}
function removeKey(node, key) {
  const el = node[meta].keyedChildren.get(key).deref();
  node.removeChild(el);
  node[meta].keyedChildren.delete(key);
}
function remove(node, from, count) {
  while (count && node.children[from]) {
    const el = node.children[from];
    node.removeChild(el);
    if (el.key) {
      node[meta].keyedChildren.delete(el.key);
    }
  }
}
function replace2(node, child, dispatch) {
  const el = createElement(child, dispatch);
  node.parentNode.replaceChild(el, node);
  if (child.key) {
    node.parentNode[meta].keyedChildren.set(child.key, new WeakRef(el));
  }
}
function replaceText(node, content) {
  node.data = content;
}
function update(node, added, removed, dispatch) {
  for (const attribute2 of removed) {
    if (node[meta].handlers.has(attribute2)) {
      node.removeEventListener(attribute2, handleEvent);
      node[meta].handlers.delete(attribute2);
    } else {
      node.removeAttribute(attribute2);
    }
  }
  for (const attribute2 of added) {
    createAttribute(node, attribute2, dispatch);
  }
}
function createElement(vnode, dispatch) {
  switch (vnode.constructor) {
    case Node2: {
      const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
      node[meta] = {
        constructor: Node2,
        key: vnode.key,
        keyedChildren: /* @__PURE__ */ new Map(),
        handlers: /* @__PURE__ */ new Map()
      };
      for (const attribute2 of vnode.attributes) {
        createAttribute(node, attribute2, dispatch);
      }
      append6(node, vnode.children, dispatch);
      return node;
    }
    case Text: {
      const node = document.createTextNode(vnode.content);
      node[meta] = { constructor: Text, key: vnode.key };
      return node;
    }
    case Fragment: {
      const node = document.createDocumentFragment();
      for (const child of vnode.children) {
        node.appendChild(createElement(child, dispatch));
      }
      return node;
    }
  }
}
function createAttribute(node, attribute2, dispatch) {
  switch (attribute2.constructor) {
    case Attribute:
      node.setAttribute(attribute2.name, attribute2.value);
      if (SYNCED_ATTRIBUTES.has(attribute2.name)) {
        node[attribute2.name] = attribute2.value;
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
      node[meta].handlers.set(attribute2.name, (event2) => {
        if (attribute2.prevent_default)
          event2.preventDefault();
        if (attribute2.stop_propagation)
          event2.stopPropagation();
        const msg = attribute2.handler(event2);
        dispatch(
          node[meta].mapper ? node[meta].mapper(msg) : msg,
          attribute2.immediate || IMMEDIATE_EVENTS.has(event2.type)
        );
      });
      break;
  }
}
function handleEvent(event2) {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  handler(event2);
}
var SYNCED_ATTRIBUTES = /* @__PURE__ */ new Set(["checked", "disabled", "selected", "value"]);
var IMMEDIATE_EVENTS = /* @__PURE__ */ new Set([
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
]);

// build/dev/javascript/lustre/runtime.ffi.mjs
var is_browser2 = () => globalThis.window && window.document;
var LustreSPA = class _LustreSPA {
  static start({ init: init3, update: update3, view: view2 }, selector, flags) {
    if (!is_browser2())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreSPA(root, init3(flags), update3, view2);
    return new Ok((action) => app.send(action));
  }
  #model;
  #update;
  #view;
  #prev;
  #reconciler;
  #reconciler_handlers = new Dict();
  constructor(root, [init3, effects], update3, view2) {
    this.root = root;
    this.#model = init3;
    this.#update = update3;
    this.#view = view2;
    this.#reconciler = new LustreReconciler(root, (msg, immediate) => {
      if (msg.constructor === Ok) {
        this.#dispatch(msg[0], immediate);
      }
    });
    this.#prev = view2(init3);
    this.#reconciler.mount(this.#prev);
    if (effects.all instanceof NonEmpty) {
      this.#tick(effects.all);
    }
  }
  send(action) {
    switch (action.constructor) {
      case Dispatch: {
        this.#dispatch(action[0], action[1]);
        break;
      }
    }
  }
  #dispatch(msg, immediate = false) {
    const [next, effects] = this.#update(this.#model, msg);
    this.#model = next;
    this.#tick(effects.all, immediate);
  }
  #tick(effects, immediate = false) {
    const dispatch = (msg, immediate2) => {
      this.#dispatch(msg, immediate2);
    };
    const emit2 = (event2, data) => this.root.dispatchEvent(
      new CustomEvent(event2, {
        detail: data,
        bubbles: true,
        composed: true
      })
    );
    const select2 = () => {
    };
    const root = this.root;
    for (const effect of effects) {
      effect({ dispatch, emit: emit2, select: select2, root });
    }
    const next = this.#view(this.#model);
    const { patch, handlers: handlers2 } = diff(
      this.#prev,
      next,
      this.#reconciler_handlers
    );
    this.#reconciler_handlers = handlers2;
    this.#reconciler.push(patch);
    this.#prev = next;
  }
};
var start2 = LustreSPA.start;

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
function simple(init3, update3, view2) {
  let init$1 = (flags) => {
    return [init3(flags), none()];
  };
  let update$1 = (model, msg) => {
    return [update3(model, msg), none()];
  };
  return application(init$1, update$1, view2);
}
function start3(app, selector, flags) {
  return guard(
    !is_browser2(),
    new Error(new NotABrowser()),
    () => {
      return start2(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function h4(attrs, children2) {
  return element("h4", attrs, children2);
}
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function button(attrs, children2) {
  return element("button", attrs, children2);
}
function input(attrs) {
  return element("input", attrs, toList([]));
}
function label(attrs, children2) {
  return element("label", attrs, children2);
}
function option(attrs, label2) {
  return element("option", attrs, toList([text(label2)]));
}
function select(attrs, children2) {
  return element("select", attrs, children2);
}
function textarea(attrs, content) {
  return element("textarea", attrs, toList([text(content)]));
}
function details(attrs, children2) {
  return element("details", attrs, children2);
}
function summary(attrs, children2) {
  return element("summary", attrs, children2);
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
}

// build/dev/javascript/app/app.ffi.mjs
var random_id = () => window.crypto.randomUUID();

// build/dev/javascript/app/app.mjs
var Model2 = class extends CustomType {
  constructor(name, description, columns, open2) {
    super();
    this.name = name;
    this.description = description;
    this.columns = columns;
    this.open = open2;
  }
};
var Column = class extends CustomType {
  constructor(id, kind, name, value2, open2) {
    super();
    this.id = id;
    this.kind = kind;
    this.name = name;
    this.value = value2;
    this.open = open2;
  }
};
var ShortText = class extends CustomType {
};
var LongText = class extends CustomType {
};
var Integer = class extends CustomType {
};
var Real = class extends CustomType {
};
var Date2 = class extends CustomType {
};
var DateTime = class extends CustomType {
};
var Choice = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Unset = class extends CustomType {
};
var Option = class extends CustomType {
  constructor(id, name, value2, open2) {
    super();
    this.id = id;
    this.name = name;
    this.value = value2;
    this.open = open2;
  }
};
var UserAddedColumn = class extends CustomType {
};
var UserDeletedColumn = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserInsertedColumnBefore = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserInsertedColumnAfter = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserMovedColumnUp = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserMovedColumnDown = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function init_option(index3) {
  let id = random_id();
  let name = "Option #" + to_string(index3);
  let value2 = to_string(index3);
  let open2 = false;
  return new Option(id, name, value2, open2);
}
function init_column(index3) {
  let id = random_id();
  let kind = (() => {
    let $ = remainderInt(index3, 10);
    if ($ === 0) {
      return new ShortText();
    } else if ($ === 1) {
      return new LongText();
    } else if ($ === 2) {
      return new Integer();
    } else if ($ === 3) {
      return new Real();
    } else if ($ === 4) {
      return new Date2();
    } else if ($ === 5) {
      return new DateTime();
    } else if ($ === 6) {
      return new Choice(
        (() => {
          let _pipe = range(0, 60);
          return map(_pipe, init_option);
        })()
      );
    } else {
      return new Unset();
    }
  })();
  let name = "Column " + id;
  let value2 = "";
  let open2 = false;
  return new Column(id, kind, name, value2, open2);
}
function init2(_) {
  let name = "My Table";
  let description = "Description for my table";
  let columns = (() => {
    let _pipe = range2(0, 600);
    return map4(_pipe, init_column);
  })();
  return new Model2(name, description, columns, false);
}
function update2(model, msg) {
  let $ = debug(msg);
  if ($ instanceof UserAddedColumn) {
    let columns = append5(
      model.columns,
      init_column(length4(model.columns))
    );
    let _record = model;
    return new Model2(_record.name, _record.description, columns, _record.open);
  } else if ($ instanceof UserDeletedColumn) {
    let index3 = $[0];
    let $1 = delete$2(model.columns, index3);
    if (!$1.isOk()) {
      throw makeError(
        "let_assert",
        "app",
        102,
        "update",
        "Pattern match failed, no pattern matched the value.",
        { value: $1 }
      );
    }
    let columns = $1[0];
    let _record = model;
    return new Model2(_record.name, _record.description, columns, _record.open);
  } else if ($ instanceof UserInsertedColumnBefore) {
    let index3 = $[0];
    let $1 = insert2(model.columns, index3, init_column(index3));
    if (!$1.isOk()) {
      throw makeError(
        "let_assert",
        "app",
        108,
        "update",
        "Pattern match failed, no pattern matched the value.",
        { value: $1 }
      );
    }
    let columns = $1[0];
    let _record = model;
    return new Model2(_record.name, _record.description, columns, _record.open);
  } else if ($ instanceof UserInsertedColumnAfter) {
    let index3 = $[0];
    let $1 = (() => {
      let $2 = index3 === length4(model.columns) - 1;
      if ($2) {
        return new Ok(append5(model.columns, init_column(index3 + 1)));
      } else {
        return insert2(model.columns, index3 + 1, init_column(index3 + 1));
      }
    })();
    if (!$1.isOk()) {
      throw makeError(
        "let_assert",
        "app",
        115,
        "update",
        "Pattern match failed, no pattern matched the value.",
        { value: $1 }
      );
    }
    let columns = $1[0];
    let _record = model;
    return new Model2(_record.name, _record.description, columns, _record.open);
  } else if ($ instanceof UserMovedColumnUp) {
    let index3 = $[0];
    return guard(
      index3 === 0,
      model,
      () => {
        let $1 = get(model.columns, index3);
        if (!$1.isOk()) {
          throw makeError(
            "let_assert",
            "app",
            125,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $1 }
          );
        }
        let a = $1[0];
        let $2 = get(model.columns, index3 - 1);
        if (!$2.isOk()) {
          throw makeError(
            "let_assert",
            "app",
            126,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $2 }
          );
        }
        let b = $2[0];
        let columns = (() => {
          let _pipe = model.columns;
          let _pipe$1 = try_set(_pipe, index3 - 1, a);
          return try_set(_pipe$1, index3, b);
        })();
        let _record = model;
        return new Model2(
          _record.name,
          _record.description,
          columns,
          _record.open
        );
      }
    );
  } else {
    let index3 = $[0];
    return guard(
      index3 === length4(model.columns) - 1,
      model,
      () => {
        let $1 = get(model.columns, index3);
        if (!$1.isOk()) {
          throw makeError(
            "let_assert",
            "app",
            138,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $1 }
          );
        }
        let a = $1[0];
        let $2 = get(model.columns, index3 + 1);
        if (!$2.isOk()) {
          throw makeError(
            "let_assert",
            "app",
            139,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $2 }
          );
        }
        let b = $2[0];
        let columns = (() => {
          let _pipe = model.columns;
          let _pipe$1 = try_set(_pipe, index3, b);
          return try_set(_pipe$1, index3 + 1, a);
        })();
        let _record = model;
        return new Model2(
          _record.name,
          _record.description,
          columns,
          _record.open
        );
      }
    );
  }
}
function view_column(column, index3) {
  return details(
    toList([class$("accordion-item"), open(column.open)]),
    toList([
      summary(
        toList([
          (() => {
            let $ = column.open;
            if ($) {
              return class$("accordion-button px-2 py-2");
            } else {
              return class$(
                "accordion-button px-2 py-2 collapsed bg-light"
              );
            }
          })()
        ]),
        toList([
          text2(column.id),
          text2(" "),
          text2(to_string(index3))
        ])
      ),
      div(
        toList([class$("accordion-body border-bottom px-2 py-2")]),
        toList([
          div(
            toList([class$("row")]),
            toList([
              div(
                toList([class$("col-md")]),
                toList([
                  div(
                    toList([class$("input-group")]),
                    toList([
                      label(
                        toList([class$("input-group-text")]),
                        toList([text2("Name")])
                      ),
                      input(
                        toList([
                          type_("text"),
                          value(column.name),
                          class$("form-control"),
                          placeholder("Column name..."),
                          attribute("aria-label", "Column name")
                        ])
                      )
                    ])
                  )
                ])
              ),
              div(
                toList([class$("col-md")]),
                toList([
                  div(
                    toList([class$("input-group")]),
                    toList([
                      label(
                        toList([class$("input-group-text")]),
                        toList([text2("Type")])
                      ),
                      select(
                        toList([class$("form-control")]),
                        toList([
                          option(
                            toList([value("short_text")]),
                            "Text"
                          ),
                          option(
                            toList([value("long_text")]),
                            "Long text"
                          ),
                          option(
                            toList([value("integer")]),
                            "Integer"
                          ),
                          option(
                            toList([value("real")]),
                            "Real"
                          ),
                          option(
                            toList([value("date")]),
                            "Date"
                          ),
                          option(
                            toList([value("date_time")]),
                            "Date and time"
                          ),
                          option(
                            toList([value("choice")]),
                            "Choice"
                          )
                        ])
                      )
                    ])
                  )
                ])
              )
            ])
          ),
          textarea(
            toList([
              class$("form-control mt-2"),
              rows(4),
              placeholder("Description")
            ]),
            ""
          ),
          button(
            toList([
              class$("btn btn-sm btn-outline-danger mt-2"),
              on_click(new UserDeletedColumn(index3))
            ]),
            toList([text2("Delete column")])
          ),
          div(
            toList([class$("btn-group ms-2")]),
            toList([
              button(
                toList([
                  class$("btn btn-sm btn-outline-primary mt-2"),
                  on_click(new UserInsertedColumnBefore(index3))
                ]),
                toList([text2("Create above")])
              ),
              button(
                toList([
                  class$("btn btn-sm btn-outline-primary mt-2"),
                  on_click(new UserInsertedColumnAfter(index3))
                ]),
                toList([text2("Create below")])
              )
            ])
          ),
          div(
            toList([class$("btn-group ms-2")]),
            toList([
              button(
                toList([
                  class$("btn btn-sm btn-outline-primary mt-2"),
                  on_click(new UserMovedColumnUp(index3))
                ]),
                toList([text2("Move up")])
              ),
              button(
                toList([
                  class$("btn btn-sm btn-outline-primary mt-2"),
                  on_click(new UserMovedColumnDown(index3))
                ]),
                toList([text2("Move down")])
              )
            ])
          )
        ])
      )
    ])
  );
}
function view_table(columns) {
  let size2 = length4(columns) - 1;
  let $ = fold_right2(
    columns,
    [toList([]), size2],
    (_use0, column) => {
      let elements3 = _use0[0];
      let index3 = _use0[1];
      return [prepend(view_column(column, index3), elements3), index3 - 1];
    }
  );
  let elements2 = $[0];
  return div(
    toList([]),
    toList([
      h4(toList([]), toList([text2("Columns")])),
      (() => {
        if (elements2.hasLength(0)) {
          return button(
            toList([
              class$("btn btn-primary mt-3"),
              on_click(new UserAddedColumn())
            ]),
            toList([text2("Add column")])
          );
        } else {
          return div(toList([class$("accordion")]), elements2);
        }
      })()
    ])
  );
}
function view(model) {
  return div(
    toList([class$("container mt-3")]),
    toList([
      div(
        toList([class$("accordion")]),
        toList([
          div(
            toList([class$("accordion-item"), open(true)]),
            toList([
              summary(
                toList([class$("accordion-button px-2 py-2")]),
                toList([
                  div(
                    toList([class$("accordion-header")]),
                    toList([text2(model.name)])
                  )
                ])
              ),
              div(
                toList([
                  class$("accordion-body border-bottom px-2 py-2")
                ]),
                toList([
                  div(
                    toList([]),
                    toList([
                      div(
                        toList([class$("input-group mb-2")]),
                        toList([
                          label(
                            toList([class$("input-group-text")]),
                            toList([text2("Name")])
                          ),
                          input(
                            toList([
                              type_("text"),
                              value(model.name),
                              class$("form-control"),
                              placeholder("Table name..."),
                              attribute("aria-label", "Table name"),
                              attribute("aria-describedby", "basic-addon1")
                            ])
                          )
                        ])
                      ),
                      textarea(
                        toList([
                          class$("form-control mb-2"),
                          rows(4)
                        ]),
                        model.description
                      )
                    ])
                  ),
                  view_table(model.columns)
                ])
              )
            ])
          )
        ])
      )
    ])
  );
}
function main() {
  let app = simple(init2, update2, view);
  let $ = start3(app, "#app", void 0);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "app",
      14,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
