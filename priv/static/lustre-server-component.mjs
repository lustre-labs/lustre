// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};
var BitArray = class {
  /**
   * The size in bits of this bit array's data.
   *
   * @type {number}
   */
  bitSize;
  /**
   * The size in bytes of this bit array's data. If this bit array doesn't store
   * a whole number of bytes then this value is rounded up.
   *
   * @type {number}
   */
  byteSize;
  /**
   * The number of unused high bits in the first byte of this bit array's
   * buffer prior to the start of its data. The value of any unused high bits is
   * undefined.
   *
   * The bit offset will be in the range 0-7.
   *
   * @type {number}
   */
  bitOffset;
  /**
   * The raw bytes that hold this bit array's data.
   *
   * If `bitOffset` is not zero then there are unused high bits in the first
   * byte of this buffer.
   *
   * If `bitOffset + bitSize` is not a multiple of 8 then there are unused low
   * bits in the last byte of this buffer.
   *
   * @type {Uint8Array}
   */
  rawBuffer;
  /**
   * Constructs a new bit array from a `Uint8Array`, an optional size in
   * bits, and an optional bit offset.
   *
   * If no bit size is specified it is taken as `buffer.length * 8`, i.e. all
   * bytes in the buffer make up the new bit array's data.
   *
   * If no bit offset is specified it defaults to zero, i.e. there are no unused
   * high bits in the first byte of the buffer.
   *
   * @param {Uint8Array} buffer
   * @param {number} [bitSize]
   * @param {number} [bitOffset]
   */
  constructor(buffer, bitSize, bitOffset) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error(
        "BitArray can only be constructed from a Uint8Array"
      );
    }
    this.bitSize = bitSize ?? buffer.length * 8;
    this.byteSize = Math.trunc((this.bitSize + 7) / 8);
    this.bitOffset = bitOffset ?? 0;
    if (this.bitSize < 0) {
      throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);
    }
    if (this.bitOffset < 0 || this.bitOffset > 7) {
      throw globalThis.Error(
        `BitArray bit offset is invalid: ${this.bitOffset}`
      );
    }
    if (buffer.length !== Math.trunc((this.bitOffset + this.bitSize + 7) / 8)) {
      throw globalThis.Error("BitArray buffer length is invalid");
    }
    this.rawBuffer = buffer;
  }
  /**
   * Returns a specific byte in this bit array. If the byte index is out of
   * range then `undefined` is returned.
   *
   * When returning the final byte of a bit array with a bit size that's not a
   * multiple of 8, the content of the unused low bits are undefined.
   *
   * @param {number} index
   * @returns {number | undefined}
   */
  byteAt(index3) {
    if (index3 < 0 || index3 >= this.byteSize) {
      return void 0;
    }
    return bitArrayByteAt(this.rawBuffer, this.bitOffset, index3);
  }
  /** @internal */
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }
    const wholeByteCount = Math.trunc(this.bitSize / 8);
    if (this.bitOffset === 0 && other.bitOffset === 0) {
      for (let i = 0; i < wholeByteCount; i++) {
        if (this.rawBuffer[i] !== other.rawBuffer[i]) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (this.rawBuffer[wholeByteCount] >> unusedLowBitCount !== other.rawBuffer[wholeByteCount] >> unusedLowBitCount) {
          return false;
        }
      }
    } else {
      for (let i = 0; i < wholeByteCount; i++) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, i);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, i);
        if (a !== b) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const a = bitArrayByteAt(
          this.rawBuffer,
          this.bitOffset,
          wholeByteCount
        );
        const b = bitArrayByteAt(
          other.rawBuffer,
          other.bitOffset,
          wholeByteCount
        );
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (a >> unusedLowBitCount !== b >> unusedLowBitCount) {
          return false;
        }
      }
    }
    return true;
  }
  /**
   * Returns this bit array's internal buffer.
   *
   * @deprecated Use `BitArray.byteAt()` or `BitArray.rawBuffer` instead.
   *
   * @returns {Uint8Array}
   */
  get buffer() {
    bitArrayPrintDeprecationWarning(
      "buffer",
      "Use BitArray.byteAt() or BitArray.rawBuffer instead"
    );
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error(
        "BitArray.buffer does not support unaligned bit arrays"
      );
    }
    return this.rawBuffer;
  }
  /**
   * Returns the length in bytes of this bit array's internal buffer.
   *
   * @deprecated Use `BitArray.bitSize` or `BitArray.byteSize` instead.
   *
   * @returns {number}
   */
  get length() {
    bitArrayPrintDeprecationWarning(
      "length",
      "Use BitArray.bitSize or BitArray.byteSize instead"
    );
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error(
        "BitArray.length does not support unaligned bit arrays"
      );
    }
    return this.rawBuffer.length;
  }
};
function bitArrayByteAt(buffer, bitOffset, index3) {
  if (bitOffset === 0) {
    return buffer[index3] ?? 0;
  } else {
    const a = buffer[index3] << bitOffset & 255;
    const b = buffer[index3 + 1] >> 8 - bitOffset;
    return a | b;
  }
}
var isBitArrayDeprecationMessagePrinted = {};
function bitArrayPrintDeprecationWarning(name, message) {
  if (isBitArrayDeprecationMessagePrinted[name]) {
    return;
  }
  console.warn(
    `Deprecated BitArray.${name} property used in JavaScript FFI code. ${message}.`
  );
  isBitArrayDeprecationMessagePrinted[name] = true;
}
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
  return !(a instanceof BitArray) && a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
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

// build/dev/javascript/lustre/lustre/vdom/attribute.mjs
var attribute_kind = 0;
var property_kind = 1;
var event_kind = 2;

// build/dev/javascript/lustre/lustre/vdom/node.mjs
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var unsafe_inner_html_kind = 3;

// build/dev/javascript/lustre/lustre/vdom/patch.mjs
var replace_text_kind = 0;
var replace_inner_html_kind = 1;
var update_kind = 2;
var move_kind = 3;
var remove_key_kind = 4;
var replace_kind = 5;
var insert_kind = 6;
var remove_kind = 7;

// build/dev/javascript/lustre/lustre/runtime/client/reconciler.ffi.mjs
var Reconciler = class {
  #root = null;
  #dispatch = () => {
  };
  #useServerEvents = false;
  constructor(root, dispatch, { useServerEvents = false } = {}) {
    this.#root = root;
    this.#dispatch = dispatch;
    this.#useServerEvents = useServerEvents;
  }
  mount(vdom) {
    this.#root.appendChild(this.#createElement(vdom));
  }
  #stack = [];
  push(patch) {
    this.#stack.push({ node: this.#root, patch });
    this.#reconcile();
  }
  // PATCHING ------------------------------------------------------------------
  #reconcile() {
    while (this.#stack.length) {
      const { node, patch } = this.#stack.pop();
      iterate(patch.changes, (change) => {
        switch (change.kind) {
          case insert_kind:
            this.#insert(node, change.children, change.before);
            break;
          case move_kind:
            this.#move(node, change.key, change.before, change.count);
            break;
          case remove_key_kind:
            this.#removeKey(node, change.key, change.count);
            break;
          case remove_kind:
            this.#remove(node, change.from, change.count);
            break;
          case replace_kind:
            this.#replace(node, change.from, change.count, change.with);
            break;
          case replace_text_kind:
            this.#replaceText(node, change.content);
            break;
          case replace_inner_html_kind:
            this.#replaceInnerHtml(node, change.inner_html);
            break;
          case update_kind:
            this.#update(node, change.added, change.removed);
            break;
        }
      });
      this.#remove(node, node.childNodes.length - patch.removed, patch.removed);
      iterate(patch.children, (child) => {
        this.#stack.push({
          node: node.childNodes[child.index],
          patch: child
        });
      });
    }
  }
  // CHANGES -------------------------------------------------------------------
  #insert(node, children, before) {
    const fragment3 = document.createDocumentFragment();
    iterate(children, (child) => {
      const el = this.#createElement(child);
      addKeyedChild(node, el);
      fragment3.appendChild(el);
    });
    node.insertBefore(fragment3, node.childNodes[before] ?? null);
  }
  #move(node, key, before, count) {
    let el = node[meta].keyedChildren.get(key).deref();
    if (count > 1) {
      const fragment3 = document.createDocumentFragment();
      for (let i = 0; i < count && el !== null; ++i) {
        let next = el.nextSibling;
        fragment3.append(el);
        el = next;
      }
      el = fragment3;
    }
    node.insertBefore(el, node.childNodes[before] ?? null);
  }
  #removeKey(node, key, count) {
    this.#removeFromChild(
      node,
      node[meta].keyedChildren.get(key).deref(),
      count
    );
  }
  #remove(node, from, count) {
    this.#removeFromChild(node, node.childNodes[from], count);
  }
  #removeFromChild(parent, child, count) {
    while (count-- > 0 && child !== null) {
      const next = child.nextSibling;
      const key = child[meta].key;
      if (key) {
        parent[meta].keyedChildren.delete(key);
      }
      parent.removeChild(child);
      child = next;
    }
  }
  #replace(parent, from, count, child) {
    this.#remove(parent, from, count);
    const el = this.#createElement(child);
    addKeyedChild(parent, el);
    parent.insertBefore(el, parent.childNodes[from] ?? null);
  }
  #replaceText(node, content) {
    node.data = content;
  }
  #replaceInnerHtml(node, inner_html) {
    node.innerHTML = inner_html;
  }
  #update(node, added, removed) {
    iterate(removed, (attribute3) => {
      const name = attribute3.name;
      if (node[meta].handlers.has(name)) {
        node.removeEventListener(name, handleEvent);
        node[meta].handlers.delete(name);
      } else {
        node.removeAttribute(name);
        ATTRIBUTE_HOOKS[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute3) => {
      this.#createAttribute(node, attribute3);
    });
  }
  // CONSTRUCTORS --------------------------------------------------------------
  #createElement(vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
        initialiseMetadata(node, vnode.key);
        iterate(vnode.attributes, (attribute3) => {
          this.#createAttribute(node, attribute3);
        });
        this.#insert(node, vnode.children, 0);
        return node;
      }
      case text_kind: {
        const node = document.createTextNode(vnode.content);
        initialiseMetadata(node, vnode.key);
        return node;
      }
      case fragment_kind: {
        const node = document.createDocumentFragment();
        const head = document.createTextNode("");
        initialiseMetadata(head, vnode.key, true);
        node.appendChild(head);
        iterate(vnode.children, (child) => {
          node.appendChild(this.#createElement(child));
        });
        return node;
      }
      case unsafe_inner_html_kind: {
        const node = vnode.namespace ? document.createElementNS(vnode.namespace, vnode.tag) : document.createElement(vnode.tag);
        initialiseMetadata(node, vnode.key);
        iterate(vnode.attributes, (attribute3) => {
          this.#createAttribute(node, attribute3);
        });
        this.#replaceInnerHtml(node, vnode.inner_html);
        return node;
      }
    }
  }
  #createAttribute(node, attribute3) {
    switch (attribute3.kind) {
      case attribute_kind: {
        const name = attribute3.name;
        const value = attribute3.value;
        if (value !== node.getAttribute(name)) {
          node.setAttribute(name, value);
        }
        ATTRIBUTE_HOOKS[name]?.added?.(node, value);
        break;
      }
      case property_kind:
        node[attribute3.name] = attribute3.value;
        break;
      case event_kind: {
        if (!node[meta].handlers.has(attribute3.name)) {
          node.addEventListener(attribute3.name, handleEvent, {
            passive: !attribute3.prevent_default
          });
        }
        const prevent = attribute3.prevent_default;
        const stop = attribute3.stop_propagation;
        const immediate = attribute3.immediate || IMMEDIATE_EVENTS.includes(attribute3.name);
        const include = Array.isArray(attribute3.include) ? attribute3.include : [];
        node[meta].handlers.set(attribute3.name, (event2) => {
          if (prevent)
            event2.preventDefault();
          if (stop)
            event2.stopPropagation();
          let path = [];
          let node2 = event2.currentTarget;
          while (node2 !== this.#root) {
            const key = node2[meta].key;
            if (key) {
              path.push(key);
            } else {
              const index3 = [].indexOf.call(node2.parentNode.childNodes, node2);
              path.push(index3.toString());
            }
            node2 = node2.parentNode;
          }
          path.reverse();
          const data = this.#useServerEvents ? createServerEvent(event2, include) : event2;
          this.#dispatch(data, path, event2.type, immediate);
        });
        break;
      }
    }
  }
};
function iterate(list3, callback) {
  if (Array.isArray(list3)) {
    for (let i = 0; i < list3.length; i++) {
      callback(list3[i]);
    }
  } else {
    for (list3; list3.tail; list3 = list3.tail) {
      callback(list3.head);
    }
  }
}
var meta = Symbol("metadata");
function initialiseMetadata(node, key = "", fragment3 = false) {
  if (node.nodeType === Node.ELEMENT_NODE || fragment3) {
    node[meta] = {
      key,
      keyedChildren: /* @__PURE__ */ new Map(),
      handlers: /* @__PURE__ */ new Map()
    };
    return;
  }
  node[meta] = { key };
}
function addKeyedChild(node, child) {
  if (child.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
    for (child = child.firstChild; child; child = child.nextSibling) {
      addKeyedChild(node, child);
    }
    return;
  }
  const key = child[meta].key;
  if (key) {
    node[meta].keyedChildren.set(key, new WeakRef(child));
  }
}
function handleEvent(event2) {
  const target = event2.currentTarget;
  const handler = target[meta].handlers.get(event2.type);
  handler(event2);
}
function createServerEvent(event2, include = []) {
  const data = {};
  if (event2.type === "input" || event2.type === "change") {
    include.push("target.value");
  }
  for (const property2 of include) {
    const path = property2.split(".");
    for (let i = 0, input = event2, output = data; i < path.length; i++) {
      if (i === path.length - 1) {
        output[path[i]] = input[path[i]];
        break;
      }
      output = output[path[i]] ??= {};
      input = input[path[i]];
    }
  }
  return data;
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
      try {
        node.play?.();
      } catch (e) {
        console.error(e);
      }
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
  if (!shadowRoot.host.isConnected) {
    return [];
  }
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

// build/dev/javascript/lustre/lustre/runtime/transport.mjs
var mount_kind = 0;
var reconcile_kind = 1;
var emit_kind = 2;
var attributes_changed_kind = 0;
var event_fired_kind = 1;

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
    this.#reconciler = new Reconciler(
      this.shadowRoot,
      (event2, path, name) => {
        this.#transport?.send({ kind: event_fired_kind, path, name, event: event2 });
      },
      {
        useServerEvents: true
      }
    );
    this.#observer = new MutationObserver((mutations) => {
      const attributes = [];
      for (const mutation of mutations) {
        if (mutation.type !== "attributes")
          continue;
        const name = mutation.attributeName;
        if (!this.#remoteObservedAttributes.includes(name))
          continue;
        attributes.push([name, this.getAttribute(name)]);
      }
      if (attributes.length) {
        this.#transport?.send({ kind: attributes_changed_kind, attributes });
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
  eventReceivedCallback(event2, path, name) {
    this.#transport?.send("hi!");
  }
  messageReceivedCallback(data) {
    console.log(data);
    switch (data.kind) {
      case mount_kind: {
        while (this.shadowRoot.children[this.#adoptedStyleNodes.length]) {
          this.shadowRoot.children[this.#adoptedStyleNodes.length].remove();
        }
        this.#reconciler.mount(data.vdom);
        break;
      }
      case reconcile_kind: {
        this.#reconciler.push(data.patch);
        break;
      }
      case emit_kind: {
        this.dispatchEvent(new CustomEvent(data.name, { detail: data.data }));
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
