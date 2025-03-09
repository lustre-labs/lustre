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
    let length2 = 0;
    for (let _ of this)
      length2++;
    return length2;
  }
};
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

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Eq = class extends CustomType {
};
var Gt = class extends CustomType {
};

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
function new_map() {
  return Dict.new();
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

// build/dev/javascript/lustre/lustre/internals/constants.ffi.mjs
var EMPTY_DICT = Dict.new();
var EMPTY_SET = new$();

// build/dev/javascript/lustre/lustre/vdom/attribute.ffi.mjs
var GT = new Gt();
var LT = new Lt();
var EQ = new Eq();

// build/dev/javascript/lustre/lustre/runtime/transport.mjs
var mount_variant = 0;
var mount_vdom = 1;
var reconcile_variant = 1;
var reconcile_patch = 1;
var emit_variant = 2;
var emit_name = 1;
var emit_data = 2;
var attributes_changed_variant = 0;
var event_fired_variant = 1;
var fragment_variant = 0;
var fragment_children = 2;
var element_variant = 1;
var element_key = 1;
var element_namespace = 2;
var element_tag = 3;
var element_attributes = 4;
var element_children = 5;
var unsafe_inner_html_variant = 2;
var unsafe_inner_html_key = 1;
var unsafe_inner_html_namespace = 2;
var unsafe_inner_html_tag = 3;
var unsafe_inner_html_attributes = 4;
var unsafe_inner_html_inner_html = 5;
var text_variant = 3;
var text_key = 1;
var text_content = 2;
var attribute_variant = 0;
var attribute_name = 1;
var attribute_value = 2;
var property_variant = 1;
var property_name = 1;
var property_value = 2;
var event_variant = 2;
var event_name = 1;
var event_prevent_default = 3;
var event_stop_propagation = 4;
var event_immediate = 5;
var patch_index = 0;
var patch_removed = 1;
var patch_changes = 2;
var patch_children = 3;
var replace_variant = 0;
var replace_element = 1;
var replace_text_variant = 1;
var replace_text_content = 1;
var replace_inner_html_variant = 2;
var replace_inner_html_inner_html = 1;
var update_variant = 3;
var update_added = 1;
var update_removed = 2;
var move_variant = 4;
var move_key = 1;
var move_before = 2;
var move_count = 3;
var remove_key_variant = 5;
var remove_key_key = 1;
var remove_key_count = 2;
var insert_variant = 6;
var insert_children = 1;
var insert_before = 2;
var remove_variant = 7;
var remove_from = 1;
var remove_count = 2;

// build/dev/javascript/lustre/lustre/runtime/client/server_component_reconciler.ffi.mjs
var meta = Symbol("metadata");
var Reconciler = class {
  #root = null;
  #dispatch = () => {
  };
  #stack = [];
  constructor(root, dispatch) {
    this.#root = root;
    this.#dispatch = dispatch;
  }
  mount(vdom) {
    this.#root.appendChild(createElement(vdom, this.#dispatch, this.#root));
  }
  push(patch) {
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }
  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();
      for (let i = 0; i < patch[patch_changes].length; i++) {
        const change = patch[patch_changes][i];
        switch (change[0]) {
          case insert_variant:
            insert4(
              node,
              change[insert_children],
              change[insert_before],
              this.#dispatch,
              this.#root
            );
            break;
          case move_variant:
            move(
              node,
              change[move_key],
              change[move_before],
              change[move_count]
            );
            break;
          case remove_key_variant:
            removeKey(node, change[remove_key_key], change[remove_key_count]);
            break;
          case remove_variant:
            remove(node, change[remove_from], change[remove_count]);
            break;
          case replace_variant:
            replace2(node, change[replace_element], this.#dispatch, this.#root);
            break;
          case replace_text_variant:
            replaceText(node, change[replace_text_content]);
            break;
          case replace_inner_html_variant:
            replaceInnerHtml(node, change[replace_inner_html_inner_html]);
            break;
          case update_variant:
            update(
              node,
              change[update_added],
              change[update_removed],
              this.#dispatch,
              this.#root
            );
            break;
        }
      }
      for (let i = 0; i < patch[patch_removed]; ++i) {
        const child = node.lastChild;
        const key = child[meta].key;
        if (key) {
          node[meta].keyedChildren.delete(key);
        }
        node.removeChild(child);
      }
      for (let i = 0; i < patch[patch_children].length; i++) {
        const child = patch[patch_children][i];
        this.#stack.push({
          node: node.childNodes[child[patch_index]],
          patch: child
        });
      }
    }
  }
};
function insert4(node, children, before, dispatch, root) {
  const fragment2 = document.createDocumentFragment();
  for (let i = 0; i < children.length; i++) {
    const child = children[i];
    const el = createElement(child, dispatch, root);
    if (child[element_key]) {
      const ref = new WeakRef(unwrapFragment(el));
      node[meta].keyedChildren.set(child[element_key], ref);
    }
    fragment2.appendChild(el);
  }
  node.insertBefore(fragment2, node.childNodes[before] ?? null);
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
  node.insertBefore(el, node.childNodes[before] ?? null);
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
  if (child[element_key]) {
    const ref = new WeakRef(unwrapFragment(el));
    parent[meta].keyedChildren.set(child[element_key], ref);
  }
  parent.replaceChild(el, node);
}
function replaceText(node, content) {
  node.data = content;
}
function replaceInnerHtml(node, inner_html) {
  node.innerHTML = inner_html;
}
function update(node, added, removed, dispatch, root) {
  for (let i = 0; i < removed.length; i++) {
    const name = removed[i][attribute_name];
    if (node[meta].handlers.has(name)) {
      node.removeEventListener(name, handleEvent);
      node[meta].handlers.delete(name);
    } else {
      node.removeAttribute(name);
      ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
    }
  }
  for (let i = 0; i < added.length; i++) {
    createAttribute(node, added[i], dispatch, root);
  }
}
function unwrapFragment(node) {
  while (node.nodeType === DocumentFragment.DOCUMENT_FRAGMENT_NODE) {
    node = node.firstChild;
  }
  return node;
}
function createElement(vnode, dispatch, root) {
  switch (vnode[0]) {
    case element_variant: {
      const node = vnode[element_namespace] ? document.createElementNS(vnode[element_namespace], vnode[element_tag]) : document.createElement(vnode[element_tag]);
      node[meta] = {
        key: vnode[element_key],
        keyedChildren: /* @__PURE__ */ new Map(),
        handlers: /* @__PURE__ */ new Map()
      };
      for (let i = 0; i < vnode[element_attributes].length; i++) {
        createAttribute(node, vnode[element_attributes][i], dispatch, root);
      }
      insert4(node, vnode[element_children], 0, dispatch, root);
      return node;
    }
    case text_variant: {
      const node = document.createTextNode(vnode[text_content]);
      node[meta] = { key: vnode[text_key] };
      return node;
    }
    case fragment_variant: {
      const node = document.createDocumentFragment();
      for (let i = 0; i < vnode[fragment_children].length; i++) {
        node.appendChild(
          createElement(vnode[fragment_children][i], dispatch, root)
        );
      }
      return node;
    }
    case unsafe_inner_html_variant: {
      const node = vnode[unsafe_inner_html_namespace] ? document.createElementNS(
        vnode[unsafe_inner_html_namespace],
        vnode[unsafe_inner_html_tag]
      ) : document.createElement(vnode[unsafe_inner_html_tag]);
      node[meta] = {
        key: vnode[unsafe_inner_html_key],
        handlers: /* @__PURE__ */ new Map()
      };
      for (let i = 0; i < vnode[unsafe_inner_html_attributes].length; i++) {
        createAttribute(
          node,
          vnode[unsafe_inner_html_attributes][i],
          dispatch,
          root
        );
      }
      replaceInnerHtml(node, vnode[unsafe_inner_html_inner_html]);
      return node;
    }
  }
}
function createAttribute(node, attribute2, dispatch, root) {
  switch (attribute2[0]) {
    case attribute_variant:
      {
        const name = attribute2[attribute_name];
        const value = attribute2[attribute_value];
        if (value !== node.getAttribute(name)) {
          node.setAttribute(name, value);
        }
        ATTRIBUTE_HOOKS[name]?.added?.(node, value);
      }
      break;
    case property_variant:
      node[attribute2[property_name]] = attribute2[property_value];
      break;
    case event_variant:
      {
        if (!node[meta].handlers.has(attribute2[event_name])) {
          node.addEventListener(attribute2[event_name], handleEvent, {
            passive: !attribute2[event_prevent_default]
          });
        }
        const prevent = attribute2[event_prevent_default];
        const stop = attribute2[event_stop_propagation];
        const immediate = attribute2[event_immediate] || IMMEDIATE_EVENTS.includes(attribute2[event_name]);
        node[meta].handlers.set(attribute2[event_name], (event) => {
          if (prevent)
            event.preventDefault();
          if (stop)
            event.stopPropagation();
          let node2 = event.target;
          let path = node2[meta].key || [].indexOf.call(node2.parentNode.childNodes, node2).toString();
          node2 = node2.parentNode;
          while (node2 !== root) {
            const key = node2[meta].key;
            if (key) {
              path = `${key}.${path}`;
            } else {
              const index3 = [].indexOf.call(node2.parentNode.childNodes, node2);
              path = `${index3}.${path}`;
            }
            node2 = node2.parentNode;
          }
          dispatch(event, path, event.type, immediate);
        });
      }
      break;
  }
}
function handleEvent(event) {
  const target = event.currentTarget;
  const handler = target[meta].handlers.get(event.type);
  handler(event);
}
var ATTRIBUTE_HOOKS = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),
  autofocus: {
    added(node) {
      node.focus?.();
    }
  },
  autoplay: {
    added(node) {
      node.play?.();
    }
  }
};
function syncedBooleanAttribute(name) {
  return {
    added(node, value) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
}
function syncedAttribute(name) {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
}
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

// build/dev/javascript/lustre/lustre/runtime/client/reconciler.ffi.mjs
var meta2 = Symbol("metadata");
var ATTRIBUTE_HOOKS2 = {
  checked: syncedBooleanAttribute2("checked"),
  selected: syncedBooleanAttribute2("selected"),
  value: syncedAttribute2("value"),
  autofocus: {
    added(node) {
      node.focus?.();
    }
  },
  autoplay: {
    added(node) {
      node.play?.();
    }
  }
};
function syncedBooleanAttribute2(name) {
  return {
    added(node, value) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
}
function syncedAttribute2(name) {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
}

// build/dev/javascript/lustre/lustre/runtime/client/core.ffi.mjs
var copiedStyleSheets = /* @__PURE__ */ new WeakMap();
async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of document.querySelectorAll("link[rel=stylesheet], style")) {
    if (node.sheet)
      continue;
    pendingParentStylesheets.push(
      new Promise((resolve, reject) => {
        node.addEventListener("load", resolve);
        node.addEventListener("error", reject);
      })
    );
  }
  await Promise.allSettled(pendingParentStylesheets);
  shadowRoot.adoptedStyleSheets = shadowRoot.host.getRootNode().adoptedStyleSheets;
  const pending = [];
  for (const sheet of document.styleSheets) {
    try {
      shadowRoot.adoptedStyleSheets.push(sheet);
    } catch {
      try {
        let copiedSheet = copiedStyleSheets.get(sheet);
        if (!copiedSheet) {
          copiedSheet = new CSSStyleSheet();
          for (const rule of sheet.cssRules) {
            copiedSheet.insertRule(rule.cssText, copiedSheet.cssRules.length);
          }
          copiedStyleSheets.set(sheet, copiedSheet);
        }
        shadowRoot.adoptedStyleSheets.push(copiedSheet);
      } catch {
        const node = sheet.ownerNode.cloneNode();
        shadowRoot.prepend(node);
        pending.push(node);
      }
    }
  }
  return pending;
}

// src/lustre/runtime/client/server_component.ffi.mjs
var WebsocketTransport = class {
  #url;
  #socket;
  constructor(url, onMessage, {}) {
    this.#url = url;
    this.#socket = new WebSocket(this.#url);
    this.#socket.onmessage = ({ data }) => {
      try {
        onMessage(JSON.parse(data));
      } catch {
      }
    };
  }
  send(data) {
    this.#socket.send(JSON.stringify(data));
  }
  close() {
    this.#socket.close();
  }
};
var SseTransport = class {
  #url;
  #eventSource;
  constructor(url, onMessage, {}) {
    this.#url = url;
    this.#eventSource = new EventSource(url);
    this.#eventSource.onmessage = ({ data }) => {
      try {
        onMessage(JSON.parse(data));
      } catch {
      }
    };
  }
  send(data) {
  }
  close() {
    this.#eventSource.close();
  }
};
var PollingTransport = class {
  #url;
  #onMessage;
  #interval;
  #timer;
  constructor(url, onMessage, opts = {}) {
    this.#url = url;
    this.#onMessage = onMessage;
    this.#interval = opts.interval ?? 5e3;
    this.#fetch().finally(() => {
      this.#timer = window.setInterval(() => this.#fetch(), this.#interval);
    });
  }
  async send(data) {
  }
  close() {
    clearInterval(this.#timer);
  }
  #fetch() {
    return fetch(this.#url).then((response) => response.json()).then(this.#onMessage).catch(console.error);
  }
};
var ServerComponent = class extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method"];
  }
  #method = "ws";
  #route = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #observer;
  #remoteObservedAttributes = [];
  constructor() {
    super();
    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }
    this.internals = this.attachInternals();
    this.#reconciler = new Reconciler(this.shadowRoot, (event, path, name) => {
      this.#transport?.send([event_fired_variant, path, name, event]);
    });
    this.#observer = new MutationObserver((mutations) => {
      const changed = [];
      for (const mutation of mutations) {
        if (mutation.type !== "attributes")
          continue;
        const name = mutation.attributeName;
        if (!this.#remoteObservedAttributes.includes(name))
          continue;
        changed.push([name, this.getAttribute(name)]);
      }
      if (changed.length) {
        this.#transport?.send([attributes_changed_variant, changed]);
      }
    });
  }
  connectedCallback() {
    this.#adoptStyleSheets();
    this.#observer.observe(this, {
      attributes: true
    });
    this.#method = this.getAttribute("method") || "ws";
    if (this.hasAttribute("route")) {
      this.#route = new URL(this.getAttribute("route"), window.location.href);
      this.#connect();
    }
  }
  adoptedCallback() {
    this.#adoptStyleSheets();
  }
  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case prev !== next: {
        this.#route = new URL(next, window.location.href);
        this.#connect();
        return;
      }
      case "method": {
        const normalised = next.toLowerCase();
        if (normalised == this.#method)
          return;
        if (["ws", "sse", "polling", "http"].includes(normalised)) {
          this.#method = normalised;
          if (this.#method == "ws") {
            if (this.#route.protocol == "https:")
              this.#route.protocol = "wss:";
            if (this.#route.protocol == "http:")
              this.#route.protocol = "ws:";
          }
          this.#connect();
        }
        return;
      }
    }
  }
  eventReceivedCallback(event, path, name) {
    this.#transport?.send("hi!");
  }
  messageReceivedCallback(data) {
    switch (data[0]) {
      case mount_variant: {
        while (this.shadowRoot.children[this.#adoptedStyleNodes.length]) {
          this.shadowRoot.children[this.#adoptedStyleNodes.length].remove();
        }
        this.#reconciler.mount(data[mount_vdom]);
        break;
      }
      case reconcile_variant: {
        this.#reconciler.push(data[reconcile_patch]);
        break;
      }
      case emit_variant: {
        this.dispatchEvent(
          new CustomEvent(data[emit_name], { detail: data[emit_data] })
        );
        break;
      }
    }
  }
  //
  #connect() {
    if (!this.#route || !this.#method)
      return;
    if (this.#transport)
      this.#transport.close();
    const onMessage = (data) => {
      this.messageReceivedCallback(data);
    };
    switch (this.#method) {
      case "ws":
        this.#transport = new WebsocketTransport(this.#route, onMessage, {});
        break;
      case "sse":
        this.#transport = new SseTransport(this.#route, onMessage, {});
        break;
      case "polling":
        this.#transport = new PollingTransport(this.#route, onMessage, {});
        break;
    }
  }
  //
  async #adoptStyleSheets() {
    while (this.#adoptedStyleNodes.length) {
      this.#adoptedStyleNodes.pop().remove();
      this.shadowRoot.firstChild.remove();
    }
    this.#adoptedStyleNodes = await adoptStylesheets(this.shadowRoot);
  }
};
window.customElements.define("lustre-server-component", ServerComponent);
export {
  ServerComponent
};
