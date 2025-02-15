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
  // @internal
  countLength() {
    let length4 = 0;
    for (let _ of this)
      length4++;
    return length4;
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
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value) {
    super();
    this[0] = value;
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
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var None = class extends CustomType {
};

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function is_empty(dict) {
  return isEqual(dict, new$());
}
function get(from, get2) {
  return map_get(from, get2);
}
function do_has_key(key, dict) {
  return !isEqual(get(dict, key), new Error(void 0));
}
function has_key(dict, key) {
  return do_has_key(key, dict);
}
function insert(dict, key, value) {
  return map_insert(key, value, dict);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_keys_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let first2 = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$acc = prepend(first2[0], acc);
    }
  }
}
function do_keys(dict) {
  let list_of_pairs = map_to_list(dict);
  return do_keys_loop(list_of_pairs, toList([]));
}
function keys(dict) {
  return do_keys(dict);
}
function delete$(dict, key) {
  return map_remove(key, dict);
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function length_loop(loop$list, loop$count) {
  while (true) {
    let list = loop$list;
    let count = loop$count;
    if (list.atLeastLength(1)) {
      let list$1 = list.tail;
      loop$list = list$1;
      loop$count = count + 1;
    } else {
      return count;
    }
  }
}
function length(list) {
  return length_loop(list, 0);
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function index_fold_loop(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index2 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index2);
      loop$with = with$;
      loop$index = index2 + 1;
    }
  }
}
function index_fold(list, initial, fun) {
  return index_fold_loop(list, initial, fun, 0);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function length3(string3) {
  return string_length(string3);
}
function slice(string3, idx, len) {
  let $ = len < 0;
  if ($) {
    return "";
  } else {
    let $1 = idx < 0;
    if ($1) {
      let translated_idx = length3(string3) + idx;
      let $2 = translated_idx < 0;
      if ($2) {
        return "";
      } else {
        return string_slice(string3, translated_idx, len);
      }
    } else {
      return string_slice(string3, idx, len);
    }
  }
}
function drop_start(string3, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string3;
  } else {
    return slice(string3, num_graphemes, length3(string3) - num_graphemes);
  }
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
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key, v: val })
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
  const size = root.array.length;
  for (let i = 0; i < size; i++) {
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
  const size = items.length;
  for (let i = 0; i < size; i++) {
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
  constructor(root, size) {
    this.root = root;
    this.size = size;
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
function to_string3(term) {
  return term.toString();
}
function string_length(string3) {
  if (string3 === "") {
    return 0;
  }
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    let i = 0;
    for (const _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string3.match(/./gsu).length;
  }
}
var segmenter = void 0;
function graphemes_iterator(string3) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string3)[Symbol.iterator]();
  }
}
function string_slice(string3, idx, len) {
  if (len <= 0 || idx >= string3.length) {
    return "";
  }
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    while (idx-- > 0) {
      iterator.next();
    }
    let result = "";
    while (len-- > 0) {
      const v = iterator.next().value;
      if (v === void 0) {
        break;
      }
      result += v.segment;
    }
    return result;
  } else {
    return string3.match(/./gsu).slice(idx, idx + len).join("");
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
var left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
var right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");
function new_map() {
  return Dict.new();
}
function map_to_list(map4) {
  return List.fromArray(map4.entries());
}
function map_remove(key, map4) {
  return map4.delete(key);
}
function map_get(map4, key) {
  const value = map4.get(key, NOT_FOUND);
  if (value === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value);
}
function map_insert(key, value, map4) {
  return map4.set(key, value);
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function to_string2(x) {
  return to_string3(x);
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function none() {
  return new Effect(toList([]));
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict) {
    super();
    this.dict = dict;
  }
};
function new$2() {
  return new Set2(new$());
}
function is_empty2(set2) {
  return isEqual(set2, new$2());
}
function contains(set2, member) {
  let _pipe = set2.dict;
  let _pipe$1 = get(_pipe, member);
  return is_ok(_pipe$1);
}
var token = void 0;
function insert2(set2, member) {
  return new Set2(insert(set2.dict, member, token));
}

// build/dev/javascript/lustre/lustre/runtime/lookup.mjs
function has(lookup, key) {
  return has_key(lookup[0], key);
}
function set(lookup, key, value) {
  return [insert(lookup[0], key, value), lookup[1]];
}
function new$3() {
  return [new$(), new$2()];
}
function pop(lookup, key) {
  let $ = get(lookup[0], key);
  if ($.isOk()) {
    let value = $[0];
    return new Ok(
      [value, [delete$(lookup[0], key), insert2(lookup[1], key)]]
    );
  } else {
    return new Error(void 0);
  }
}
function visited(lookup, key) {
  return contains(lookup[1], key);
}
function is_empty3(lookup) {
  return is_empty(lookup[0]) && is_empty2(lookup[1]);
}

// build/dev/javascript/lustre/lustre/runtime/vdom.mjs
var Fragment = class extends CustomType {
  constructor(key, children2) {
    super();
    this.key = key;
    this.children = children2;
  }
};
var Map2 = class extends CustomType {
  constructor(key, element2) {
    super();
    this.key = key;
    this.element = element2;
  }
};
var Node2 = class extends CustomType {
  constructor(key, namespace, tag, attributes, children2, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
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
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
};
var Property = class extends CustomType {
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
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
var Patch = class extends CustomType {
  constructor(index2, changes, children2) {
    super();
    this.index = index2;
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
var Update = class extends CustomType {
  constructor(added, removed) {
    super();
    this.added = added;
    this.removed = removed;
  }
};
var Cursor = class extends CustomType {
  constructor(meta2, node, idx, old, new$5) {
    super();
    this.meta = meta2;
    this.node = node;
    this.idx = idx;
    this.old = old;
    this.new = new$5;
  }
};
var Metadata = class extends CustomType {
  constructor(fragment, keyed_prefix, keyed_children, keyed_moves) {
    super();
    this.fragment = fragment;
    this.keyed_prefix = keyed_prefix;
    this.keyed_children = keyed_children;
    this.keyed_moves = keyed_moves;
  }
};
function do_diff_attributes(loop$prev, loop$next, loop$added) {
  while (true) {
    let prev = loop$prev;
    let next = loop$next;
    let added = loop$added;
    if (next.hasLength(0)) {
      return [added, keys(prev)];
    } else if (next.atLeastLength(1) && next.head instanceof Attribute) {
      let a = next.head;
      let rest = next.tail;
      let $ = get(prev, a.name);
      if ($.isOk() && $[0] instanceof Attribute && a.value === $[0].value) {
        let b = $[0];
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = added;
      } else if ($.isOk()) {
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = prepend(a, added);
      } else {
        loop$prev = prev;
        loop$next = rest;
        loop$added = prepend(a, added);
      }
    } else if (next.atLeastLength(1) && next.head instanceof Property) {
      let a = next.head;
      let rest = next.tail;
      let $ = get(prev, a.name);
      if ($.isOk() && $[0] instanceof Property && isEqual(a.value, $[0].value)) {
        let b = $[0];
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = added;
      } else if ($.isOk()) {
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = prepend(a, added);
      } else {
        loop$prev = prev;
        loop$next = rest;
        loop$added = prepend(a, added);
      }
    } else {
      let a = next.head;
      let rest = next.tail;
      let $ = get(prev, a.name);
      if ($.isOk() && $[0] instanceof Event && isEqual(a, $[0])) {
        let b = $[0];
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = added;
      } else if ($.isOk()) {
        loop$prev = delete$(prev, a.name);
        loop$next = rest;
        loop$added = prepend(a, added);
      } else {
        loop$prev = prev;
        loop$next = rest;
        loop$added = prepend(a, added);
      }
    }
  }
}
function diff_attributes(prev, next) {
  let prev$1 = fold(
    prev,
    new$(),
    (acc, attr) => {
      return insert(acc, attr.name, attr);
    }
  );
  return do_diff_attributes(prev$1, next, toList([]));
}
function do_add_keyed_children(loop$keyed_children, loop$prefix, loop$index, loop$children) {
  while (true) {
    let keyed_children = loop$keyed_children;
    let prefix = loop$prefix;
    let index2 = loop$index;
    let children2 = loop$children;
    if (children2.hasLength(0)) {
      return keyed_children;
    } else if (children2.atLeastLength(1) && children2.head.key === "") {
      let child = children2.head;
      let rest = children2.tail;
      let _pipe = keyed_children;
      let _pipe$1 = set(_pipe, prefix + to_string2(index2), child);
      loop$keyed_children = _pipe$1;
      loop$prefix = prefix;
      loop$index = index2 + 1;
      loop$children = rest;
    } else {
      let child = children2.head;
      let rest = children2.tail;
      let _pipe = keyed_children;
      let _pipe$1 = set(_pipe, prefix + child.key, child);
      loop$keyed_children = _pipe$1;
      loop$prefix = prefix;
      loop$index = index2;
      loop$children = rest;
    }
  }
}
function add_keyed_children(keyed_children, prefix, children2) {
  if (children2.hasLength(0)) {
    return keyed_children;
  } else if (children2.atLeastLength(1) && children2.head.key === "") {
    let child = children2.head;
    return keyed_children;
  } else {
    return do_add_keyed_children(keyed_children, prefix, 0, children2);
  }
}
function drop_visited_orphans(loop$prev, loop$keyed_children) {
  while (true) {
    let prev = loop$prev;
    let keyed_children = loop$keyed_children;
    if (prev.hasLength(0)) {
      return toList([]);
    } else {
      let head = prev.head;
      let rest = prev.tail;
      let $ = visited(keyed_children, head.key);
      if ($) {
        loop$prev = rest;
        loop$keyed_children = keyed_children;
      } else {
        return prev;
      }
    }
  }
}
function do_diff(loop$stack) {
  while (true) {
    let stack = loop$stack;
    if (stack.hasLength(0)) {
      return new Patch(0, toList([]), toList([]));
    } else if (stack.hasLength(1) && stack.head instanceof Cursor && stack.head.old.hasLength(0) && stack.head.new.hasLength(0)) {
      let node = stack.head.node;
      return node;
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
        loop$stack = prepend(next$1, stack$1);
      } else if ($ && $1.hasLength(0) && $2.hasLength(0)) {
        let meta$1 = (() => {
          let _record = next.meta;
          return new Metadata(
            _record.fragment,
            _record.keyed_prefix,
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
        loop$stack = prepend(next$1, stack$1);
      } else {
        let meta$1 = (() => {
          let _record = next.meta;
          return new Metadata(
            _record.fragment,
            _record.keyed_prefix,
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
        loop$stack = prepend(next$1, stack$1);
      }
    } else {
      let cursor = stack.head;
      let meta2 = stack.head.meta;
      let node = stack.head.node;
      let idx = stack.head.idx;
      let old = stack.head.old;
      let new$5 = stack.head.new;
      let stack$1 = stack.tail;
      let is_keyed_diff = !is_empty3(meta2.keyed_children);
      let has_key2 = (() => {
        if (new$5.atLeastLength(1) && is_keyed_diff) {
          let head = new$5.head;
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
            prepend(new Append(new$5), node.changes),
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
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$5.hasLength(0)) {
        let $ = drop_visited_orphans(old, meta2.keyed_children);
        if ($.hasLength(0)) {
          let cursor$1 = (() => {
            let _record = cursor;
            return new Cursor(
              _record.meta,
              _record.node,
              _record.idx,
              toList([]),
              toList([])
            );
          })();
          loop$stack = prepend(cursor$1, stack$1);
        } else if (meta2.fragment) {
          let old$1 = $;
          let changes = prepend(
            new Remove(idx, length(old$1)),
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
          loop$stack = prepend(cursor$1, stack$1);
        } else {
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
          loop$stack = prepend(cursor$1, stack$1);
        }
      } else if (old.atLeastLength(1) && old.head instanceof Map2 && new$5.atLeastLength(1) && new$5.head instanceof Map2) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$5.head;
        let new$1 = new$5.tail;
        let old$2 = prepend(prev.element(), old$1);
        let new$22 = prepend(next.element(), new$1);
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(
            _record.meta,
            _record.node,
            _record.idx,
            old$2,
            new$22
          );
        })();
        loop$stack = prepend(cursor$1, stack$1);
      } else if (old.atLeastLength(1) && old.head instanceof Map2) {
        let prev = old.head;
        let old$1 = old.tail;
        let old$2 = prepend(prev.element(), old$1);
        let stack$2 = prepend(
          (() => {
            let _record = cursor;
            return new Cursor(
              _record.meta,
              _record.node,
              _record.idx,
              old$2,
              new$5
            );
          })(),
          stack$1
        );
        loop$stack = stack$2;
      } else if (new$5.atLeastLength(1) && new$5.head instanceof Map2) {
        let next = new$5.head;
        let new$1 = new$5.tail;
        let new$22 = prepend(next.element(), new$1);
        let stack$2 = prepend(
          (() => {
            let _record = cursor;
            return new Cursor(
              _record.meta,
              _record.node,
              _record.idx,
              _record.old,
              new$22
            );
          })(),
          stack$1
        );
        loop$stack = stack$2;
      } else if (old.atLeastLength(1) && new$5.atLeastLength(1) && (has_key2 && old.head.key !== new$5.head.key)) {
        let prev = old.head;
        let old$1 = old.tail;
        let new$1 = new$5;
        let next = new$5.head;
        let $ = pop(meta2.keyed_children, next.key);
        if (!$.isOk()) {
          throw makeError(
            "let_assert",
            "lustre/runtime/vdom",
            202,
            "do_diff",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          );
        }
        let match = $[0][0];
        let keyed_children = $[0][1];
        let meta$1 = (() => {
          let _record = meta2;
          return new Metadata(
            _record.fragment,
            _record.keyed_prefix,
            keyed_children,
            meta2.keyed_moves + 1
          );
        })();
        let old$2 = prepend(match, prepend(prev, old$1));
        let changes = prepend(new Move(next.key, idx), node.changes);
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(meta$1, node$1, _record.idx, old$2, new$1);
        })();
        loop$stack = prepend(cursor$1, stack$1);
      } else if (new$5.atLeastLength(1) && (is_keyed_diff && !has_key2)) {
        let next = new$5.head;
        let new$1 = new$5.tail;
        let changes = prepend(
          new Insert(next, idx - meta2.keyed_moves),
          node.changes
        );
        let node$1 = (() => {
          let _record = node;
          return new Patch(_record.index, changes, _record.children);
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(_record.meta, node$1, idx + 1, _record.old, new$1);
        })();
        loop$stack = prepend(cursor$1, stack$1);
      } else if (old.atLeastLength(1) && old.head instanceof Fragment && new$5.atLeastLength(1) && new$5.head instanceof Fragment) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$5.head;
        let new$1 = new$5.tail;
        let child_cursor = new Cursor(
          new Metadata(
            true,
            (() => {
              let $ = prev.key;
              if ($ === "") {
                return ":fragment:" + to_string2(idx) + ":";
              } else {
                return prev.key;
              }
            })(),
            add_keyed_children(
              new$3(),
              (() => {
                let $ = prev.key;
                if ($ === "") {
                  return ":fragment:" + to_string2(idx) + ":";
                } else {
                  return prev.key;
                }
              })(),
              prev.children
            ),
            meta2.keyed_moves
          ),
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
        loop$stack = prepend(child_cursor, prepend(cursor$1, stack$1));
      } else if (old.atLeastLength(1) && old.head instanceof Node2 && new$5.atLeastLength(1) && new$5.head instanceof Node2 && (old.head.namespace === new$5.head.namespace && old.head.tag === new$5.head.tag)) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$5.head;
        let new$1 = new$5.tail;
        let child_cursor = new Cursor(
          new Metadata(
            false,
            "",
            add_keyed_children(new$3(), "", prev.children),
            0
          ),
          (() => {
            let $ = diff_attributes(prev.attributes, next.attributes);
            if ($[0].hasLength(0) && $[1].hasLength(0)) {
              return new Patch(idx, toList([]), toList([]));
            } else {
              let added = $[0];
              let removed = $[1];
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
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(_record.meta, node, idx + 1, old$1, new$1);
        })();
        loop$stack = prepend(child_cursor, prepend(cursor$1, stack$1));
      } else if (old.atLeastLength(1) && old.head instanceof Text && new$5.atLeastLength(1) && new$5.head instanceof Text && old.head.content === new$5.head.content) {
        let prev = old.head;
        let old$1 = old.tail;
        let next = new$5.head;
        let new$1 = new$5.tail;
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(_record.meta, _record.node, idx + 1, old$1, new$1);
        })();
        loop$stack = prepend(cursor$1, stack$1);
      } else {
        let old$1 = old.tail;
        let next = new$5.head;
        let new$1 = new$5.tail;
        let child = new Patch(idx, toList([new Replace(next)]), toList([]));
        let node$1 = (() => {
          let _record = node;
          return new Patch(
            _record.index,
            _record.changes,
            prepend(child, node.children)
          );
        })();
        let cursor$1 = (() => {
          let _record = cursor;
          return new Cursor(_record.meta, node$1, idx + 1, old$1, new$1);
        })();
        loop$stack = prepend(cursor$1, stack$1);
      }
    }
  }
}
function diff(prev, next) {
  return do_diff(
    toList([
      new Cursor(
        new Metadata(false, "", new$3(), 0),
        new Patch(0, toList([]), toList([])),
        0,
        toList([prev]),
        toList([next])
      )
    ])
  );
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function on(name, handler) {
  return new Event(name, handler, false, false, false);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attributes, children2) {
  if (tag === "area") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "base") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "br") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "col") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "embed") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "hr") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "img") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "input") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "link") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "meta") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "param") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "source") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "track") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Node2("", "", tag, attributes, toList([]), false, true);
  } else {
    return new Node2("", "", tag, attributes, children2, false, false);
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
var Map3 = class extends CustomType {
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
    (handlers3, element2, index2) => {
      let key$1 = key + "-" + to_string2(index2);
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
    } else if (element2 instanceof Map3) {
      let subtree = element2.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key;
    } else {
      let attrs = element2.attrs;
      let children2 = element2.children;
      let handlers$1 = fold(
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
  return do_handlers(element2, new$(), "0");
}

// build/dev/javascript/lustre/lustre/internals/patch.mjs
var Diff = class extends CustomType {
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
  return isEqual(diff3.created, new$()) && isEqual(
    diff3.removed,
    new$2()
  ) && isEqual(diff3.updated, new$());
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
function morph(prev, next, dispatch2) {
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
        dispatch: dispatch2,
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
function createElementNode({ prev, next, dispatch: dispatch2, stack }) {
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
    const value = attr[1];
    if (attr.as_property) {
      if (el[name] !== value)
        el[name] = value;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch2(value, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch2(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el.setAttribute(name, value);
      if (canMorph) {
        prevHandlers.delete(eventName);
        prevAttributes.delete(name);
      }
    } else if (name.startsWith("delegate:data-") || name.startsWith("delegate:aria-")) {
      el.setAttribute(name, value);
      delegated.push([name.slice(10), value]);
    } else if (name === "class") {
      className = className === null ? value : className + " " + value;
    } else if (name === "style") {
      style = style === null ? value : style + value;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value;
    } else {
      if (el.getAttribute(name) !== value)
        el.setAttribute(name, value);
      if (name === "value" || name === "selected")
        el[name] = value;
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
        for (const [name, value] of delegated) {
          if (!child.hasAttribute(name)) {
            child.setAttribute(name, value);
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
    const placeholder = document.createTextNode("");
    el.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el });
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
        const dispatch2 = (handler, immediate = false) => (event2) => {
          const result = handler(event2);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
        const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
        morph(prev, vdom, dispatch2);
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
    const dispatch2 = (handler, immediate = false) => (event2) => {
      const result = handler(event2);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0], immediate));
      }
    };
    const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
    morph(prev, vdom, dispatch2);
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
      const dispatch2 = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = this.root;
      effect({ dispatch: dispatch2, emit: emit2, select, root });
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
      const patch = new Diff(diff3);
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
      const dispatch2 = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = null;
      effect({ dispatch: dispatch2, emit: emit2, select, root });
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
  #nextTick = null;
  #queue = [];
  #dispatch = () => {
  };
  constructor(root, dispatch2, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch2;
  }
  mount(vnode) {
    this.#root.appendChild(createElement(vnode, this.#dispatch));
  }
  push(diff3, { flush = false } = {}) {
    this.#queue.push(diff3);
    if (flush) {
      this.flush();
    } else if (!this.#nextTick) {
      this.#nextTick = window.requestAnimationFrame(() => {
        this.flush();
      });
    }
  }
  flush() {
    if (this.#nextTick) {
      this.#nextTick = window.cancelAnimationFrame(this.#nextTick);
    }
    for (const patch of this.#queue) {
      reconcile(this.#root, patch, this.#dispatch);
    }
  }
};
function reconcile(root, patch, dispatch2) {
  const stack = [{ node: root, patch }];
  while (stack.length) {
    const { node, patch: patch2 } = stack.pop();
    for (const child of patch2.children) {
      stack.push({ node: node.childNodes[child.index], patch: child });
    }
    for (const change of patch2.changes) {
      switch (change.constructor) {
        case Append:
          append4(node, change.children, dispatch2);
          break;
        case Insert:
          insert3(node, change.element, change.at, dispatch2);
          break;
        case Move:
          move(node, change.key, change.to);
          break;
        case RemoveAll:
          removeAll(node, change.from);
          break;
        case Remove:
          remove(node, change.from, change.count);
          break;
        case Replace:
          replace2(node, change.element, dispatch2);
          break;
        case Update:
          update(node, change.added, change.removed, dispatch2);
          break;
      }
    }
  }
}
function append4(node, children2, dispatch2) {
  for (const child of children2) {
    const el = createElement(child, dispatch2);
    node.appendChild(el);
    if (child.key) {
      node[meta].keyedChildren.set(child.key, new WeakRef(el));
    }
  }
}
function insert3(node, child, at, dispatch2) {
  const el = createElement(child, dispatch2);
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
  while (node.children[from]) {
    const el = node.children[from];
    node.removeChild(el);
    if (el.key) {
      node[meta].keyedChildren.delete(el.key);
    }
  }
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
function replace2(node, child, dispatch2) {
  const el = createElement(child, dispatch2);
  node.parentNode.replaceChild(el, node);
  if (child.key) {
    node.parentNode[meta].keyedChildren.set(child.key, new WeakRef(el));
  }
}
function update(node, added, removed, dispatch2) {
  for (const attribute2 of removed) {
    if (attribute2.startsWith("on")) {
      node.removeEventListener(attribute2, handleEvent);
      node[meta].handlers.delete(attribute2);
    } else {
      node.removeAttribute(attribute2);
    }
  }
  for (const attribute2 of added) {
    switch (attribute2.constructor) {
      case Attribute:
        node.setAttribute(attribute2.name, attribute2.value);
        break;
      case Property:
        node[attribute2.name] = attribute2.value;
        break;
      case Event:
        if (!node[meta].handlers.has(attribute2.name)) {
          node.addEventListener(attribute2.name, handleEvent);
        }
        node[meta].handlers.set(attribute2.name, (event2) => {
          if (attribute2.prevent_default)
            event2.preventDefault();
          if (attribute2.stop_propagation)
            event2.stopPropagation();
          dispatch2(attribute2.handler(event2), attribute2.immediate);
        });
        break;
    }
  }
}
function createElement(vnode, dispatch2) {
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
        switch (attribute2.constructor) {
          case Attribute:
            node.setAttribute(attribute2.name, attribute2.value);
            break;
          case Property:
            node[attribute2.name] = attribute2.value;
            break;
          case Event:
            node.addEventListener(attribute2.name, handleEvent);
            node[meta].handlers.set(attribute2.name, (event2) => {
              if (attribute2.prevent_default)
                event2.preventDefault();
              if (attribute2.stop_propagation)
                event2.stopPropagation();
              dispatch2(attribute2.handler(event2), attribute2.immediate);
            });
            break;
        }
      }
      for (const child of vnode.children) {
        const el = createElement(child);
        const ref = new WeakRef(el);
        node.appendChild(createElement(child));
        if (child.key) {
          node[meta].keyedChildren.set(child.key, ref);
        }
      }
      return node;
    }
    case Text: {
      const node = document.createTextNode(vnode.content);
      node[meta] = { constructor: Text, key: vnode.key };
      return node;
    }
  }
}
function handleEvent(event2) {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  console.log({ target, handler });
  handler(event2);
}

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
      window.requestAnimationFrame(() => {
        this.#tick(effects.all.toArray());
      });
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
    this.#flush(effects, immediate);
    const next = this.#view(this.#model);
    const patches = diff(this.#prev, next);
    this.#reconciler.push(patches, { flush: immediate });
    this.#prev = next;
  }
  #flush(effects = []) {
    const dispatch2 = (msg, immediate) => this.#dispatch(msg, immediate);
    const emit2 = (event2, data) => this.root.dispatchEvent(
      new CustomEvent(event2, {
        detail: data,
        bubbles: true,
        composed: true
      })
    );
    const select = () => {
    };
    const root = this.root;
    for (const effect of effects) {
      effect({ dispatch: dispatch2, emit: emit2, select, root });
    }
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
function dispatch(msg) {
  return new Dispatch(msg);
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
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function p(attrs, children2) {
  return element("p", attrs, children2);
}
function button(attrs, children2) {
  return element("button", attrs, children2);
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

// build/dev/javascript/app/app.mjs
var Incr = class extends CustomType {
};
var Decr = class extends CustomType {
};
function init2(initial_count) {
  let $ = initial_count < 0;
  if ($) {
    return 0;
  } else {
    return initial_count;
  }
}
function update2(model, msg) {
  if (msg instanceof Incr) {
    return model + 1;
  } else {
    return model - 1;
  }
}
function view(model) {
  let count = to_string2(model);
  return div(
    toList([]),
    toList([
      button(
        toList([on_click(new Incr())]),
        toList([text("+")])
      ),
      p(toList([]), toList([text(count)])),
      button(
        toList([on_click(new Decr())]),
        toList([text("-")])
      )
    ])
  );
}
function main() {
  let app = simple(init2, update2, view);
  let $ = start3(app, "#app", 0);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "app",
      12,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  let to_runtime = $[0];
  let _pipe = new Incr();
  let _pipe$1 = dispatch(_pipe);
  to_runtime(_pipe$1);
  let _pipe$2 = new Incr();
  let _pipe$3 = dispatch(_pipe$2);
  to_runtime(_pipe$3);
  let _pipe$4 = new Incr();
  let _pipe$5 = dispatch(_pipe$4);
  to_runtime(_pipe$5);
  return void 0;
}

// build/.lustre/entry.mjs
main();
