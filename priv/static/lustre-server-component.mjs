// build/dev/javascript/houdini/houdini.ffi.mjs
function do_escape(string5) {
  return string5.replaceAll(/[><&"']/g, (replaced) => {
    switch (replaced) {
      case ">":
        return "&gt;";
      case "<":
        return "&lt;";
      case "'":
        return "&#39;";
      case "&":
        return "&amp;";
      case '"':
        return "&quot;";
      default:
        return replaced;
    }
  });
}

// build/dev/javascript/houdini/houdini/internal/escape_js.mjs
function escape(text3) {
  return do_escape(text3);
}

// build/dev/javascript/houdini/houdini.mjs
function escape2(string5) {
  return escape(string5);
}

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
  atLeastLength(desired) {
    let current = this;
    while (desired-- > 0 && current) current = current.tail;
    return current !== void 0;
  }
  hasLength(desired) {
    let current = this;
    while (desired-- > 0 && current) current = current.tail;
    return desired === -1 && current instanceof Empty;
  }
  countLength() {
    let current = this;
    let length2 = 0;
    while (current) {
      current = current.tail;
      length2++;
    }
    return length2 - 1;
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
var List$NonEmpty = (head, tail) => new NonEmpty(head, tail);
var List$NonEmpty$first = (value) => value.head;
var List$NonEmpty$rest = (value) => value.tail;
var Result = class _Result extends CustomType {
  static isResult(data2) {
    return data2 instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
  isOk() {
    return true;
  }
};
var Result$Ok = (value) => new Ok(value);
var Result$isOk = (value) => value instanceof Ok;
var Result$Ok$0 = (value) => value[0];
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  isOk() {
    return false;
  }
};
var Result$Error = (detail) => new Error(detail);

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Order$Lt = () => new Lt();
var Eq = class extends CustomType {
};
var Order$Eq = () => new Eq();
var Gt = class extends CustomType {
};
var Order$Gt = () => new Gt();

// build/dev/javascript/gleam_stdlib/dict.mjs
var bits = 5;
var mask = (1 << bits) - 1;
var noElementMarker = Symbol();
var generationKey = Symbol();

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
var Ascending = class extends CustomType {
};
var Descending = class extends CustomType {
};
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix instanceof Empty) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function reverse(list4) {
  return reverse_and_prepend(list4, toList([]));
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list22 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list4 = list22;
      return reverse_and_prepend(list4, acc);
    } else if (list22 instanceof Empty) {
      let list4 = list1;
      return reverse_and_prepend(list4, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list22.head;
      let rest2 = list22.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = rest1;
        loop$list2 = list22;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list22;
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
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let descending1 = sequences2.head;
        let descending2 = $.head;
        let rest$1 = $.tail;
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
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list22 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list4 = list22;
      return reverse_and_prepend(list4, acc);
    } else if (list22 instanceof Empty) {
      let list4 = list1;
      return reverse_and_prepend(list4, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list22.head;
      let rest2 = list22.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list22;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Eq) {
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
    if (sequences2 instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let ascending1 = sequences2.head;
        let ascending2 = $.head;
        let rest$1 = $.tail;
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
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences2 = loop$sequences;
    let direction = loop$direction;
    let compare4 = loop$compare;
    if (sequences2 instanceof Empty) {
      return sequences2;
    } else if (direction instanceof Ascending) {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return sequence;
      } else {
        let sequences$1 = merge_ascending_pairs(sequences2, compare4, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Descending();
        loop$compare = compare4;
      }
    } else {
      let $ = sequences2.tail;
      if ($ instanceof Empty) {
        let sequence = sequences2.head;
        return reverse(sequence);
      } else {
        let sequences$1 = merge_descending_pairs(sequences2, compare4, toList([]));
        loop$sequences = sequences$1;
        loop$direction = new Ascending();
        loop$compare = compare4;
      }
    }
  }
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list4 = loop$list;
    let compare4 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list4 instanceof Empty) {
      if (direction instanceof Ascending) {
        return prepend(reverse(growing$1), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list4.head;
      let rest$1 = list4.tail;
      let $ = compare4(prev, new$1);
      if (direction instanceof Ascending) {
        if ($ instanceof Lt) {
          loop$list = rest$1;
          loop$compare = compare4;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else if ($ instanceof Eq) {
          loop$list = rest$1;
          loop$compare = compare4;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else {
          let _block;
          if (direction instanceof Ascending) {
            _block = prepend(reverse(growing$1), acc);
          } else {
            _block = prepend(growing$1, acc);
          }
          let acc$1 = _block;
          if (rest$1 instanceof Empty) {
            return prepend(toList([new$1]), acc$1);
          } else {
            let next = rest$1.head;
            let rest$2 = rest$1.tail;
            let _block$1;
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              _block$1 = new Ascending();
            } else if ($1 instanceof Eq) {
              _block$1 = new Ascending();
            } else {
              _block$1 = new Descending();
            }
            let direction$1 = _block$1;
            loop$list = rest$2;
            loop$compare = compare4;
            loop$growing = toList([new$1]);
            loop$direction = direction$1;
            loop$prev = next;
            loop$acc = acc$1;
          }
        }
      } else if ($ instanceof Lt) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare4(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending();
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending();
          } else {
            _block$1 = new Descending();
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Eq) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare4(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = new Ascending();
          } else if ($1 instanceof Eq) {
            _block$1 = new Ascending();
          } else {
            _block$1 = new Descending();
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      }
    }
  }
}
function sort(list4, compare4) {
  if (list4 instanceof Empty) {
    return list4;
  } else {
    let $ = list4.tail;
    if ($ instanceof Empty) {
      return list4;
    } else {
      let x = list4.head;
      let y = $.head;
      let rest$1 = $.tail;
      let _block;
      let $1 = compare4(x, y);
      if ($1 instanceof Lt) {
        _block = new Ascending();
      } else if ($1 instanceof Eq) {
        _block = new Ascending();
      } else {
        _block = new Descending();
      }
      let direction = _block;
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
}

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
var trim_start_regex = /* @__PURE__ */ new RegExp(
  `^[${unicode_whitespaces}]*`
);
var trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);

// build/dev/javascript/gleam_stdlib/gleam/function.mjs
function identity3(x) {
  return x;
}

// build/dev/javascript/agnostic/agnostic/internals/constants.mjs
var empty_list = /* @__PURE__ */ toList([]);

// build/dev/javascript/agnostic/agnostic/internals/mutable_map.ffi.mjs
function empty() {
  return null;
}
function has_key(map7, key) {
  return map7 && map7.has(key);
}
function insert2(map7, key, value) {
  map7 ??= /* @__PURE__ */ new Map();
  map7.set(key, value);
  return map7;
}

// build/dev/javascript/agnostic/agnostic/vdom/vattr.ffi.mjs
var GT = /* @__PURE__ */ Order$Gt();
var LT = /* @__PURE__ */ Order$Lt();
var EQ = /* @__PURE__ */ Order$Eq();
function compare3(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}

// build/dev/javascript/agnostic/agnostic/vdom/vattr.mjs
var Attribute = class extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
};
var attribute_kind = 0;
var property_kind = 1;
var event_kind = 2;
var never_kind = 0;
var always_kind = 2;
function attribute(name, value) {
  return new Attribute(attribute_kind, name, value);
}
function merge(loop$attributes, loop$merged) {
  while (true) {
    let attributes = loop$attributes;
    let merged = loop$merged;
    if (attributes instanceof Empty) {
      return merged;
    } else {
      let $ = attributes.head;
      if ($ instanceof Attribute) {
        let $1 = $.name;
        if ($1 === "") {
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = merged;
        } else if ($1 === "class") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "class") {
                  let kind = $.kind;
                  let class1 = $2;
                  let rest = $3.tail;
                  let class2 = $4.value;
                  let value = class1 + " " + class2;
                  let attribute$1 = new Attribute(kind, "class", value);
                  loop$attributes = prepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = prepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else if ($1 === "style") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute) {
                let $5 = $4.name;
                if ($5 === "style") {
                  let kind = $.kind;
                  let style1 = $2;
                  let rest = $3.tail;
                  let style2 = $4.value;
                  let value = style1 + ";" + style2;
                  let attribute$1 = new Attribute(kind, "style", value);
                  loop$attributes = prepend(attribute$1, rest);
                  loop$merged = merged;
                } else {
                  let attribute$1 = $;
                  let rest = $3;
                  loop$attributes = rest;
                  loop$merged = prepend(attribute$1, merged);
                }
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else {
          let attribute$1 = $;
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = prepend(attribute$1, merged);
        }
      } else {
        let attribute$1 = $;
        let rest = attributes.tail;
        loop$attributes = rest;
        loop$merged = prepend(attribute$1, merged);
      }
    }
  }
}
function prepare(attributes) {
  if (attributes instanceof Empty) {
    return attributes;
  } else {
    let $ = attributes.tail;
    if ($ instanceof Empty) {
      return attributes;
    } else {
      let _pipe = attributes;
      let _pipe$1 = sort(_pipe, (a, b) => {
        return compare3(b, a);
      });
      return merge(_pipe$1, empty_list);
    }
  }
}

// build/dev/javascript/agnostic/agnostic/vdom/vnode.mjs
var Fragment = class extends CustomType {
  constructor(kind, key, children, keyed_children) {
    super();
    this.kind = kind;
    this.key = key;
    this.children = children;
    this.keyed_children = keyed_children;
  }
};
var Element = class extends CustomType {
  constructor(kind, key, namespace, tag, attributes, children, keyed_children) {
    super();
    this.kind = kind;
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
  }
};
var Text = class extends CustomType {
  constructor(kind, key, content) {
    super();
    this.kind = kind;
    this.key = key;
    this.content = content;
  }
};
var RawContainer = class extends CustomType {
  constructor(kind, key, namespace, tag, attributes, content, compare4) {
    super();
    this.kind = kind;
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.content = content;
    this.compare = compare4;
  }
};
var RawNode = class extends CustomType {
  constructor(kind, key, content, compare4) {
    super();
    this.kind = kind;
    this.key = key;
    this.content = content;
    this.compare = compare4;
  }
};
var Map2 = class extends CustomType {
  constructor(kind, key, mapper, child2) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.child = child2;
  }
};
var Memo = class extends CustomType {
  constructor(kind, key, dependencies, view) {
    super();
    this.kind = kind;
    this.key = key;
    this.dependencies = dependencies;
    this.view = view;
  }
};
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var raw_container_kind = 3;
var raw_node_kind = 6;
var map_kind = 4;
var memo_kind = 5;
function fragment(key, children, keyed_children) {
  return new Fragment(fragment_kind, key, children, keyed_children);
}
function element(key, namespace, tag, attributes, children, keyed_children) {
  return new Element(
    element_kind,
    key,
    namespace,
    tag,
    prepare(attributes),
    children,
    keyed_children
  );
}
function text(key, content) {
  return new Text(text_kind, key, content);
}
function map4(element3, mapper) {
  if (element3 instanceof Map2) {
    let child_mapper = element3.mapper;
    return new Map2(
      map_kind,
      element3.key,
      (handler) => {
        return identity3(mapper)(child_mapper(handler));
      },
      identity3(element3.child)
    );
  } else {
    return new Map2(map_kind, element3.key, identity3(mapper), identity3(element3));
  }
}
function memo(key, dependencies, view) {
  return new Memo(memo_kind, key, dependencies, view);
}
function to_keyed(key, node) {
  if (node instanceof Fragment) {
    return new Fragment(node.kind, key, node.children, node.keyed_children);
  } else if (node instanceof Element) {
    return new Element(
      node.kind,
      key,
      node.namespace,
      node.tag,
      node.attributes,
      node.children,
      node.keyed_children
    );
  } else if (node instanceof Text) {
    return new Text(node.kind, key, node.content);
  } else if (node instanceof RawContainer) {
    return new RawContainer(
      node.kind,
      key,
      node.namespace,
      node.tag,
      node.attributes,
      node.content,
      node.compare
    );
  } else if (node instanceof RawNode) {
    return new RawNode(node.kind, key, node.content, node.compare);
  } else if (node instanceof Map2) {
    let child2 = node.child;
    return new Map2(node.kind, key, node.mapper, to_keyed(key, child2));
  } else {
    let view = node.view;
    return new Memo(
      node.kind,
      key,
      node.dependencies,
      () => {
        return to_keyed(key, view());
      }
    );
  }
}

// build/dev/javascript/agnostic/agnostic/vdom/patch.mjs
var replace_text_kind = 0;
var replace_raw_content_kind = 1;
var replace_raw_node_kind = 7;
var update_kind = 2;
var move_kind = 3;
var remove_kind = 4;
var replace_kind = 5;
var insert_kind = 6;

// build/dev/javascript/agnostic/agnostic/vdom/path.mjs
var separator_subtree = "\r";
var separator_element = "	";

// build/dev/javascript/agnostic/agnostic/internals/list.ffi.mjs
var toList2 = (arr) => arr.reduceRight((xs, x) => List$NonEmpty(x, xs), empty_list);
var iterate = (list4, callback) => {
  if (Array.isArray(list4)) {
    for (let i = 0; i < list4.length; i++) {
      callback(list4[i]);
    }
  } else if (list4) {
    for (list4; List$NonEmpty$rest(list4); list4 = List$NonEmpty$rest(list4)) {
      callback(List$NonEmpty$first(list4));
    }
  }
};

// build/dev/javascript/agnostic/agnostic/internals/constants.ffi.mjs
var NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
var ELEMENT_NODE = 1;
var TEXT_NODE = 3;
var COMMENT_NODE = 8;
var SUPPORTS_MOVE_BEFORE = !!globalThis.HTMLElement?.prototype?.moveBefore;

// build/dev/javascript/agnostic/agnostic/vdom/reconciler.ffi.mjs
var setTimeout2 = globalThis.setTimeout;
var clearTimeout = globalThis.clearTimeout;
var wrapRef = (ref2) => ref2 != null ? Result$Ok(ref2) : Result$Error(void 0);
var debugWriteLine = () => {
};
var debugLogPath = globalThis.process?.env?.AGNOSTIC_DEBUG_LOG;
if (debugLogPath) {
  try {
    const fs = await import(
      /* @vite-ignore */
      "node:fs"
    );
    debugWriteLine = (line) => {
      try {
        fs.appendFileSync(debugLogPath, line + "\n");
      } catch {
      }
    };
  } catch {
  }
}
var gleamListToArray = (list4) => {
  const out = [];
  let cur = list4;
  while (cur && cur.head !== void 0) {
    out.push(cur.head);
    cur = cur.tail;
  }
  return out;
};
var meta = Symbol("lustre");
var MetadataNode = class {
  constructor(kind, parent, node, key) {
    this.kind = kind;
    this.key = key;
    this.parent = parent;
    this.children = [];
    this.node = node;
    this.endNode = null;
    this.handlers = /* @__PURE__ */ new Map();
    this.throttles = /* @__PURE__ */ new Map();
    this.debouncers = /* @__PURE__ */ new Map();
  }
  get isVirtual() {
    return this.kind === fragment_kind || this.kind === map_kind;
  }
  get parentNode() {
    return this.isVirtual ? this.node.parentNode : this.node;
  }
};
var insertMetadataChild = (kind, parent, node, index2, key) => {
  const child2 = new MetadataNode(kind, parent, node, key);
  node[meta] = child2;
  parent?.children.splice(index2, 0, child2);
  return child2;
};
var getPath = (node) => {
  let path = "";
  for (let current = node[meta]; current.parent; current = current.parent) {
    const separator = current.parent && current.parent.kind === map_kind ? separator_subtree : separator_element;
    if (current.key) {
      path = `${separator}${current.key}${path}`;
    } else {
      const index2 = current.parent.children.indexOf(current);
      path = `${separator}${index2}${path}`;
    }
  }
  return path.slice(1);
};
var Reconciler = class {
  #root = null;
  #decodeEvent;
  #dispatch;
  #platform;
  #debug = false;
  constructor(root2, decodeEvent, dispatch2, platform, { debug = false } = {}) {
    this.#root = root2;
    this.#decodeEvent = decodeEvent;
    this.#dispatch = dispatch2;
    this.#platform = platform;
    this.#debug = debug;
  }
  mount(vdom) {
    insertMetadataChild(element_kind, null, this.#root, 0, null);
    this.#insertChild(this.#root, null, this.#root[meta], 0, vdom);
  }
  push(patch, memos2 = null) {
    this.#memos = memos2;
    this.#stack.push({ node: this.#root[meta], patch });
    this.#reconcile();
  }
  // PATCHING ------------------------------------------------------------------
  #memos;
  #stack = [];
  #reconcile() {
    const stack = this.#stack;
    while (stack.length) {
      let { node, patch } = stack.pop();
      const { path, changes, removed, children: childPatches } = patch;
      iterate(path, (index2) => {
        if (node === void 0 || node.children?.[index2] === void 0) {
          const changes2 = gleamListToArray(patch.changes).map((c) => {
            const summary = { kind: c.kind };
            if (c.index !== void 0) summary.index = c.index;
            if (c.before !== void 0) summary.before = c.before;
            if (c.key !== void 0) summary.key = c.key;
            if (c.added !== void 0) {
              summary.added = gleamListToArray(c.added).map((a) => a.name);
            }
            if (c.removed !== void 0) {
              summary.removed = gleamListToArray(c.removed).map((a) => a.name);
            }
            return summary;
          });
          const childPatches2 = gleamListToArray(patch.children).map((p) => ({
            index: p.index,
            pathLen: gleamListToArray(p.path).length,
            changesLen: gleamListToArray(p.changes).length
          }));
          const ancestors = [];
          for (let cur = node; cur; cur = cur.parent) {
            ancestors.push({
              kind: cur.kind,
              key: cur.key,
              childrenLen: cur.children?.length
            });
          }
          const stackSnapshot = stack.map((entry) => ({
            patchPath: gleamListToArray(entry.patch.path),
            patchIndex: entry.patch.index,
            patchChangeKinds: gleamListToArray(entry.patch.changes).map(
              (c) => c.kind
            ),
            patchChildrenCount: gleamListToArray(entry.patch.children).length,
            nodeKind: entry.node?.kind,
            nodeKey: entry.node?.key
          }));
          debugWriteLine(
            `[lustre] path descent overruns metadata ${JSON.stringify({
              patchPath: gleamListToArray(patch.path),
              patchIndex: patch.index,
              patchChanges: changes2,
              patchChildren: childPatches2,
              stepIndex: index2,
              nodeKind: node?.kind,
              nodeKey: node?.key,
              childrenLen: node?.children?.length,
              nodeChildKinds: node?.children?.map((c) => c?.kind),
              ancestors,
              pendingStack: stackSnapshot
            })}`
          );
        }
        node = node.children[index2];
      });
      const { children: childNodes } = node;
      iterate(changes, (change) => this.#patch(node, change));
      if (removed) {
        this.#removeChildren(node, childNodes.length - removed, removed);
      }
      iterate(childPatches, (childPatch) => {
        const idx = childPatch.index | 0;
        const child2 = childNodes[idx];
        if (child2 === void 0) {
          debugWriteLine(
            `[lustre] child patch index resolves to undefined ${JSON.stringify({
              patchPath: patch.path,
              index: idx,
              childNodesLength: childNodes.length
            })}`
          );
        }
        this.#stack.push({ node: child2, patch: childPatch });
      });
    }
  }
  #patch(node, change) {
    switch (change.kind) {
      case replace_text_kind:
        this.#replaceText(node, change);
        break;
      case replace_raw_content_kind:
        this.#replaceRawContent(node, change);
        break;
      case replace_raw_node_kind:
        this.#replaceRawNode(node, change);
        break;
      case update_kind:
        this.#update(node, change);
        break;
      case move_kind:
        this.#move(node, change);
        break;
      case remove_kind:
        this.#remove(node, change);
        break;
      case replace_kind:
        this.#replace(node, change);
        break;
      case insert_kind:
        this.#insert(node, change);
        break;
    }
  }
  // CHANGES -------------------------------------------------------------------
  #insert(parent, { children, before }) {
    const fragment3 = this.#platform.create_fragment();
    const beforeEl = this.#getReference(parent, before);
    this.#insertChildren(fragment3, null, parent, before | 0, children);
    this.#platform.insert_before(
      parent.parentNode,
      fragment3,
      wrapRef(beforeEl)
    );
  }
  #replace(parent, { index: index2, with: child2 }) {
    const beforeEl = this.#getReference(parent, (index2 | 0) + 1);
    this.#removeChildren(parent, index2 | 0, 1);
    this.#insertChild(parent.parentNode, beforeEl, parent, index2 | 0, child2);
  }
  #getReference(node, index2) {
    index2 = index2 | 0;
    const { children } = node;
    const childCount = children.length;
    if (index2 < childCount) return children[index2].node;
    if (node.endNode) return node.endNode;
    if (!node.isVirtual) return null;
    while (node.isVirtual && node.children.length) {
      if (node.endNode) {
        const sibling2 = this.#platform.next_sibling(node.endNode);
        return Result$isOk(sibling2) ? Result$Ok$0(sibling2) : null;
      }
      node = node.children[node.children.length - 1];
    }
    const sibling = this.#platform.next_sibling(node.node);
    return Result$isOk(sibling) ? Result$Ok$0(sibling) : null;
  }
  #move(parent, { key, before }) {
    before = before | 0;
    const { children, parentNode } = parent;
    const beforeEl = children[before].node;
    let prev = children[before];
    for (let i = before + 1; i < children.length; ++i) {
      const next = children[i];
      children[i] = prev;
      prev = next;
      if (next.key === key) {
        children[before] = next;
        break;
      }
    }
    this.#moveChild(parentNode, prev, beforeEl);
  }
  #moveChildren(domParent, children, beforeEl) {
    for (let i = 0; i < children.length; ++i) {
      this.#moveChild(domParent, children[i], beforeEl);
    }
  }
  #moveChild(domParent, child2, beforeEl) {
    this.#platform.move_before(domParent, child2.node, wrapRef(beforeEl));
    if (child2.isVirtual) {
      this.#moveChildren(domParent, child2.children, beforeEl);
    }
    if (child2.endNode) {
      this.#platform.move_before(domParent, child2.endNode, wrapRef(beforeEl));
    }
  }
  #remove(parent, { index: index2 }) {
    this.#removeChildren(parent, index2, 1);
  }
  #removeChildren(parent, index2, count) {
    const { children, parentNode } = parent;
    const deleted = children.splice(index2, count);
    for (let i = 0; i < deleted.length; ++i) {
      const child2 = deleted[i];
      const { node, endNode, isVirtual, children: nestedChildren } = child2;
      this.#platform.remove_child(parentNode, node);
      if (endNode) {
        this.#platform.remove_child(parentNode, endNode);
      }
      this.#removeDebouncers(child2);
      if (isVirtual) {
        deleted.push(...nestedChildren);
      }
    }
  }
  #removeDebouncers(node) {
    const { debouncers, children } = node;
    for (const { timeout } of debouncers.values()) {
      if (timeout) {
        clearTimeout(timeout);
      }
    }
    debouncers.clear();
    iterate(children, (child2) => this.#removeDebouncers(child2));
  }
  #update({ node, handlers, throttles, debouncers }, { added, removed }) {
    iterate(removed, ({ name }) => {
      if (handlers.delete(name)) {
        this.#platform.remove_event_listener(node, name, handleEvent);
        this.#updateDebounceThrottle(throttles, name, 0);
        this.#updateDebounceThrottle(debouncers, name, 0);
      } else {
        this.#platform.remove_attribute(node, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute3) => this.#createAttribute(node, attribute3));
  }
  #replaceText({ node }, { content }) {
    this.#platform.set_text(node, content ?? "");
  }
  #replaceRawContent({ node }, { content }) {
    this.#platform.set_raw_content(node, content ?? "");
  }
  #replaceRawNode(metaNode, { with: vnode }) {
    const parentNode = metaNode.parent.parentNode;
    const oldNode = metaNode.node;
    const newNode = this.#platform.create_raw_node(vnode.content);
    this.#platform.insert_before(parentNode, newNode, wrapRef(oldNode));
    this.#platform.remove_child(parentNode, oldNode);
    newNode[meta] = metaNode;
    metaNode.node = newNode;
  }
  // INSERT --------------------------------------------------------------------
  #insertChildren(domParent, beforeEl, metaParent, index2, children) {
    iterate(
      children,
      (child2) => this.#insertChild(domParent, beforeEl, metaParent, index2++, child2)
    );
  }
  #insertChild(domParent, beforeEl, metaParent, index2, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(metaParent, index2, vnode);
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        this.#insertChildren(node, null, node[meta], 0, vnode.children);
        break;
      }
      case text_kind: {
        const node = this.#createTextNode(metaParent, index2, vnode);
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        break;
      }
      case fragment_kind: {
        const marker = "lustre:fragment";
        const head = this.#createHead(marker, metaParent, index2, vnode);
        this.#platform.insert_before(domParent, head, wrapRef(beforeEl));
        this.#insertChildren(
          domParent,
          beforeEl,
          head[meta],
          0,
          vnode.children
        );
        if (this.#debug) {
          head[meta].endNode = this.#platform.create_comment(` /${marker} `);
          this.#platform.insert_before(
            domParent,
            head[meta].endNode,
            wrapRef(beforeEl)
          );
        }
        break;
      }
      case raw_container_kind: {
        const node = this.#createElement(metaParent, index2, vnode);
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        this.#replaceRawContent({ node }, vnode);
        break;
      }
      case raw_node_kind: {
        const node = this.#platform.create_raw_node(vnode.content);
        insertMetadataChild(raw_node_kind, metaParent, node, index2, vnode.key);
        this.#platform.insert_before(domParent, node, wrapRef(beforeEl));
        break;
      }
      case map_kind: {
        const head = this.#createHead("lustre:map", metaParent, index2, vnode);
        this.#platform.insert_before(domParent, head, wrapRef(beforeEl));
        this.#insertChild(domParent, beforeEl, head[meta], 0, vnode.child);
        break;
      }
      case memo_kind: {
        const child2 = this.#memos?.get(vnode.view) ?? vnode.view();
        this.#insertChild(domParent, beforeEl, metaParent, index2, child2);
        break;
      }
    }
  }
  #createElement(parent, index2, { kind, key, tag, namespace, attributes }) {
    const node = this.#platform.create_element(
      namespace || NAMESPACE_HTML,
      tag
    );
    insertMetadataChild(kind, parent, node, index2, key);
    if (this.#debug && key) {
      this.#platform.set_attribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute3) => this.#createAttribute(node, attribute3));
    return node;
  }
  #createTextNode(parent, index2, { kind, key, content }) {
    const node = this.#platform.create_text_node(content ?? "");
    insertMetadataChild(kind, parent, node, index2, key);
    return node;
  }
  #createHead(marker, parent, index2, { kind, key }) {
    const node = this.#debug ? this.#platform.create_comment(markerComment(marker, key)) : this.#platform.create_text_node("");
    insertMetadataChild(kind, parent, node, index2, key);
    return node;
  }
  #createAttribute(node, attribute3) {
    const { debouncers, handlers, throttles } = node[meta];
    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      debounce: debounceDelay,
      throttle: throttleDelay
    } = attribute3;
    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? "";
        if (name === "virtual:defaultValue") {
          this.#platform.set_property(node, "defaultValue", valueOrDefault);
          return;
        } else if (name === "virtual:defaultChecked") {
          this.#platform.set_property(node, "defaultChecked", true);
          return;
        } else if (name === "virtual:defaultSelected") {
          this.#platform.set_property(node, "defaultSelected", true);
          return;
        }
        const current = this.#platform.get_attribute(node, name);
        const currentValue = Result$isOk(current) ? Result$Ok$0(current) : null;
        if (valueOrDefault !== currentValue) {
          this.#platform.set_attribute(node, name, valueOrDefault);
        }
        SYNCED_ATTRIBUTES[name]?.added?.(node, valueOrDefault);
        break;
      }
      case property_kind:
        this.#platform.set_property(node, name, value);
        break;
      case event_kind: {
        if (handlers.has(name)) {
          this.#platform.remove_event_listener(node, name, handleEvent);
        }
        const passive = prevent.kind === never_kind;
        this.#platform.add_event_listener(node, name, handleEvent, passive);
        this.#updateDebounceThrottle(throttles, name, throttleDelay);
        this.#updateDebounceThrottle(debouncers, name, debounceDelay);
        handlers.set(name, (event2) => this.#handleEvent(attribute3, event2));
        break;
      }
    }
  }
  #updateDebounceThrottle(map7, name, delay) {
    const debounceOrThrottle = map7.get(name);
    if (delay > 0) {
      if (debounceOrThrottle) {
        debounceOrThrottle.delay = delay;
      } else {
        map7.set(name, { delay });
      }
    } else if (debounceOrThrottle) {
      const { timeout } = debounceOrThrottle;
      if (timeout) {
        clearTimeout(timeout);
      }
      map7.delete(name);
    }
  }
  #handleEvent(attribute3, event2) {
    const { currentTarget, type } = event2;
    const { debouncers, throttles } = currentTarget[meta];
    const path = getPath(currentTarget);
    const {
      prevent_default: prevent,
      stop_propagation: stop,
      include
    } = attribute3;
    if (prevent.kind === always_kind) event2.preventDefault();
    if (stop.kind === always_kind) event2.stopPropagation();
    if (type === "submit") {
      event2.detail ??= {};
      event2.detail.formData = [
        ...new FormData(event2.target, event2.submitter).entries()
      ];
    }
    const data2 = this.#decodeEvent(event2, path, type, include);
    const throttle = throttles.get(type);
    if (throttle) {
      const now = Date.now();
      const last = throttle.last || 0;
      if (now > last + throttle.delay) {
        throttle.last = now;
        throttle.lastEvent = event2;
        this.#dispatch(event2, data2);
      }
    }
    const debounce = debouncers.get(type);
    if (debounce) {
      clearTimeout(debounce.timeout);
      debounce.timeout = setTimeout2(() => {
        if (event2 === throttles.get(type)?.lastEvent) return;
        this.#dispatch(event2, data2);
      }, debounce.delay);
    }
    if (!throttle && !debounce) {
      this.#dispatch(event2, data2);
    }
  }
};
var markerComment = (marker, key) => {
  if (key) {
    return ` ${marker} key="${escape2(key)}" `;
  } else {
    return ` ${marker} `;
  }
};
var handleEvent = (event2) => {
  const { currentTarget, type } = event2;
  const handler = currentTarget[meta].handlers.get(type);
  handler(event2);
};
var syncedBooleanAttribute = /* @__NO_SIDE_EFFECTS__ */ (name) => {
  return {
    added(node) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
};
var syncedAttribute = /* @__NO_SIDE_EFFECTS__ */ (name) => {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
};
var SYNCED_ATTRIBUTES = {
  checked: /* @__PURE__ */ syncedBooleanAttribute("checked"),
  selected: /* @__PURE__ */ syncedBooleanAttribute("selected"),
  value: /* @__PURE__ */ syncedAttribute("value"),
  autofocus: {
    added(node) {
      queueMicrotask(() => {
        node.focus?.();
      });
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

// build/dev/javascript/agnostic/agnostic/runtime/platform/base.ffi.mjs
var copiedStyleSheets = /* @__PURE__ */ new WeakMap();
async function adoptStylesheets(shadowRoot) {
  const pendingParentStylesheets = [];
  for (const node of globalThis.document.querySelectorAll(
    "link[rel=stylesheet], style"
  )) {
    if (node.sheet) continue;
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
  for (const sheet of globalThis.document.styleSheets) {
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
var ContextRequestEvent = class extends Event {
  constructor(context, callback, subscribe) {
    super("context-request", { bubbles: true, composed: true });
    this.context = context;
    this.callback = callback;
    this.subscribe = subscribe;
  }
};
var LustreEvent = class extends CustomEvent {
  // We can't rely on `instanceof` checks because the server component client
  // runtime is bundled on its own and thus will have its own copy of this class.
  isLustreEvent = true;
  constructor(name, detail) {
    super(name, { detail, bubbles: true, composed: true });
  }
};

// build/dev/javascript/agnostic/agnostic/attribute.mjs
function attribute2(name, value) {
  return attribute(name, value);
}

// build/dev/javascript/agnostic/agnostic/element.mjs
function text2(content) {
  return text("", content);
}
function none() {
  return text("", "");
}
function memo2(dependencies, view) {
  return memo("", dependencies, view);
}
function ref(value) {
  return identity3(value);
}
function map6(element3, f) {
  return map4(element3, f);
}

// build/dev/javascript/agnostic/agnostic/platform.mjs
var Phase = class extends CustomType {
  constructor(name, schedule) {
    super();
    this.name = name;
    this.schedule = schedule;
  }
};
var Phase$Phase = (name, schedule) => new Phase(name, schedule);
var Platform = class extends CustomType {
  constructor(target, mount, create_element2, create_text_node2, create_fragment2, create_comment2, insert_before2, move_before2, remove_child3, next_sibling2, get_attribute2, set_attribute2, remove_attribute2, set_property2, set_text2, set_raw_content2, create_raw_node2, add_event_listener2, remove_event_listener2, schedule_render2, after_render2, phases2) {
    super();
    this.target = target;
    this.mount = mount;
    this.create_element = create_element2;
    this.create_text_node = create_text_node2;
    this.create_fragment = create_fragment2;
    this.create_comment = create_comment2;
    this.insert_before = insert_before2;
    this.move_before = move_before2;
    this.remove_child = remove_child3;
    this.next_sibling = next_sibling2;
    this.get_attribute = get_attribute2;
    this.set_attribute = set_attribute2;
    this.remove_attribute = remove_attribute2;
    this.set_property = set_property2;
    this.set_text = set_text2;
    this.set_raw_content = set_raw_content2;
    this.create_raw_node = create_raw_node2;
    this.add_event_listener = add_event_listener2;
    this.remove_event_listener = remove_event_listener2;
    this.schedule_render = schedule_render2;
    this.after_render = after_render2;
    this.phases = phases2;
  }
};
function new$4(target, mount, create_element2, create_text_node2, create_fragment2, create_comment2, insert_before2, move_before2, remove_child3, next_sibling2, get_attribute2, set_attribute2, remove_attribute2, set_property2, set_text2, set_raw_content2, create_raw_node2, add_event_listener2, remove_event_listener2, schedule_render2, after_render2, phases2) {
  return new Platform(
    target,
    mount,
    create_element2,
    create_text_node2,
    create_fragment2,
    create_comment2,
    insert_before2,
    move_before2,
    remove_child3,
    next_sibling2,
    get_attribute2,
    set_attribute2,
    remove_attribute2,
    set_property2,
    set_text2,
    set_raw_content2,
    create_raw_node2,
    add_event_listener2,
    remove_event_listener2,
    schedule_render2,
    after_render2,
    phases2
  );
}

// build/dev/javascript/agnostic/agnostic/element/keyed.mjs
function do_extract_keyed_children(loop$reversed_pairs, loop$keyed_children, loop$children) {
  while (true) {
    let reversed_pairs = loop$reversed_pairs;
    let keyed_children = loop$keyed_children;
    let children = loop$children;
    if (reversed_pairs instanceof Empty) {
      return [keyed_children, children];
    } else {
      let rest = reversed_pairs.tail;
      let key = reversed_pairs.head[0];
      let element$1 = reversed_pairs.head[1];
      let keyed_element = to_keyed(key, element$1);
      if (key === "") {
        loop$reversed_pairs = rest;
        loop$keyed_children = keyed_children;
        loop$children = prepend(keyed_element, children);
      } else {
        let $ = has_key(keyed_children, key);
        if ($) {
          loop$reversed_pairs = rest;
          loop$keyed_children = keyed_children;
          loop$children = children;
        } else {
          loop$reversed_pairs = rest;
          loop$keyed_children = insert2(
            keyed_children,
            key,
            keyed_element
          );
          loop$children = prepend(keyed_element, children);
        }
      }
    }
  }
}
function extract_keyed_children(children) {
  return do_extract_keyed_children(
    reverse(children),
    empty(),
    empty_list
  );
}
function element2(tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return element("", "", tag, attributes, children$1, keyed_children);
}
function namespaced(namespace, tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return element(
    "",
    namespace,
    tag,
    attributes,
    children$1,
    keyed_children
  );
}
function fragment2(children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return fragment("", children$1, keyed_children);
}

// build/dev/javascript/agnostic/agnostic/vdom/virtualise.ffi.mjs
var virtualise = (root2) => {
  const rootMeta = insertMetadataChild(element_kind, null, root2, 0, null);
  const { children } = virtualiseChildren(rootMeta, root2, root2.firstChild);
  if (children.length > 1) {
    const rootNodeMeta = insertMetadataChild(element_kind, null, root2, 0, null);
    rootMeta.kind = fragment_kind;
    rootMeta.node = globalThis.document.createTextNode("");
    rootMeta.parent = rootNodeMeta;
    rootNodeMeta.children.push(rootMeta);
    root2.insertBefore(rootMeta.node, root2.firstChild);
    return fragment2(toList3(children));
  }
  if (children.length === 1) {
    return children[0][1];
  }
  const placeholder = globalThis.document.createTextNode("");
  insertMetadataChild(text_kind, rootMeta, placeholder, 0, null);
  root2.insertBefore(placeholder, root2.firstChild);
  return none();
};
var virtualiseChild = (meta2, domParent, child2, index2) => {
  if (child2.nodeType === COMMENT_NODE) {
    const data2 = child2.data.trim();
    if (data2.startsWith("lustre:fragment")) {
      return virtualiseFragment(meta2, domParent, child2, index2);
    }
    if (data2.startsWith("lustre:map")) {
      return virtualiseMap(meta2, domParent, child2, index2);
    }
    if (data2.startsWith("lustre:memo")) {
      return virtualiseMemo(meta2, domParent, child2, index2);
    }
    return null;
  }
  if (child2.nodeType === ELEMENT_NODE) {
    return virtualiseElement(meta2, child2, index2);
  }
  if (child2.nodeType === TEXT_NODE) {
    return virtualiseText(meta2, child2, index2);
  }
  return null;
};
var virtualiseElement = (metaParent, node, index2) => {
  const key = node.getAttribute("data-lustre-key") ?? "";
  if (key) {
    node.removeAttribute("data-lustre-key");
  }
  const meta2 = insertMetadataChild(element_kind, metaParent, node, index2, key);
  const tag = node.localName;
  const namespace = node.namespaceURI;
  const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;
  if (isHtmlElement && INPUT_ELEMENTS.includes(tag)) {
    virtualiseInputEvents(tag, node);
  }
  const attributes = virtualiseAttributes(node);
  const { children } = virtualiseChildren(meta2, node, node.firstChild);
  const vnode = isHtmlElement ? element2(tag, attributes, toList3(children)) : namespaced(namespace, tag, attributes, toList3(children));
  return childResult(key, vnode, node.nextSibling);
};
var virtualiseChildren = (meta2, domParent, childNode) => {
  const children = [];
  while (childNode && (childNode.nodeType !== COMMENT_NODE || childNode.data.trim() !== "/lustre:fragment")) {
    const child2 = virtualiseChild(meta2, domParent, childNode, children.length);
    if (child2) {
      children.push([child2.key, child2.vnode]);
      childNode = child2.next;
    } else {
      childNode = childNode.nextSibling;
    }
  }
  return { children, end: childNode };
};
var virtualiseText = (meta2, node, index2) => {
  insertMetadataChild(text_kind, meta2, node, index2, null);
  return childResult("", text2(node.data), node.nextSibling);
};
var virtualiseFragment = (metaParent, domParent, node, index2) => {
  const key = parseKey(node.data);
  const meta2 = insertMetadataChild(fragment_kind, metaParent, node, index2, key);
  const { children, end } = virtualiseChildren(meta2, domParent, node.nextSibling);
  meta2.endNode = end;
  const vnode = fragment2(toList3(children));
  return childResult(key, vnode, end?.nextSibling);
};
var virtualiseMap = (metaParent, domParent, node, index2) => {
  const key = parseKey(node.data);
  const meta2 = insertMetadataChild(map_kind, metaParent, node, index2, key);
  const child2 = virtualiseNextChild(meta2, domParent, node, 0);
  if (!child2) return null;
  const vnode = map6(child2.vnode, (x) => x);
  return childResult(key, vnode, child2.next);
};
var virtualiseMemo = (meta2, domParent, node, index2) => {
  const key = parseKey(node.data);
  const child2 = virtualiseNextChild(meta2, domParent, node, index2);
  if (!child2) return null;
  domParent.removeChild(node);
  const vnode = memo2(toList3([ref({})]), () => child2.vnode);
  return childResult(key, vnode, child2.next);
};
var virtualiseNextChild = (meta2, domParent, node, index2) => {
  while (true) {
    node = node.nextSibling;
    if (!node) return null;
    const child2 = virtualiseChild(meta2, domParent, node, index2);
    if (child2) return child2;
  }
};
var childResult = (key, vnode, next) => {
  return { key, vnode, next };
};
var virtualiseAttributes = (node) => {
  const attributes = [];
  for (let i = 0; i < node.attributes.length; i++) {
    const attr = node.attributes[i];
    if (attr.name !== "xmlns") {
      attributes.push(attribute2(attr.localName, attr.value));
    }
  }
  return toList3(attributes);
};
var INPUT_ELEMENTS = ["input", "select", "textarea"];
var virtualiseInputEvents = (tag, node) => {
  const value = node.value;
  const checked = node.checked;
  if (tag === "input" && node.type === "checkbox" && !checked) return;
  if (tag === "input" && node.type === "radio" && !checked) return;
  if (node.type !== "checkbox" && node.type !== "radio" && !value) return;
  queueMicrotask(() => {
    node.value = value;
    node.checked = checked;
    node.dispatchEvent(new Event("input", { bubbles: true }));
    node.dispatchEvent(new Event("change", { bubbles: true }));
    if (globalThis.document.activeElement !== node) {
      node.dispatchEvent(new Event("blur", { bubbles: true }));
    }
  });
};
var parseKey = (data2) => {
  const keyMatch = data2.match(/key="([^"]*)"/);
  if (!keyMatch) return "";
  return unescapeKey(keyMatch[1]);
};
var unescapeKey = (key) => {
  return key.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&amp;/g, "&").replace(/&#39;/g, "'");
};
var toList3 = (arr) => arr.reduceRight((xs, x) => List$NonEmpty(x, xs), empty_list);

// build/dev/javascript/agnostic/agnostic/platform/dom.ffi.mjs
var unwrapResult = (result) => Result$isOk(result) ? Result$Ok$0(result) : null;
var wrapResult = (value) => value != null ? Result$Ok(value) : Result$Error(void 0);
var mount_strict = (root2) => {
  const initialVdom = virtualise(root2);
  return [root2, initialVdom];
};
var create_element = (ns, tag) => globalThis.document.createElementNS(ns || NAMESPACE_HTML, tag);
var create_text_node = (content) => globalThis.document.createTextNode(content ?? "");
var create_fragment = () => globalThis.document.createDocumentFragment();
var create_comment = (data2) => globalThis.document.createComment(data2);
var insert_before = (parent, node, ref2) => parent.insertBefore(node, unwrapResult(ref2));
var move_before = SUPPORTS_MOVE_BEFORE ? (parent, node, ref2) => parent.moveBefore(node, unwrapResult(ref2)) : (parent, node, ref2) => parent.insertBefore(node, unwrapResult(ref2));
var remove_child2 = (parent, child2) => parent.removeChild(child2);
var next_sibling = (node) => {
  const sibling = node.nextSibling;
  return sibling ? Result$Ok(sibling) : Result$Error(void 0);
};
var get_attribute = (node, name) => wrapResult(node.getAttribute(name));
var set_attribute = (node, name, value) => node.setAttribute(name, value ?? "");
var remove_attribute = (node, name) => node.removeAttribute(name);
var set_property = (node, name, value) => {
  node[name] = value;
};
var set_text = (node, content) => {
  node.data = content ?? "";
};
var set_raw_content = (node, content) => {
  node.innerHTML = content ?? "";
};
var create_raw_node = (content) => content;
var add_event_listener = (node, name, handler, passive) => node.addEventListener(name, handler, { passive });
var remove_event_listener = (node, name, handler) => node.removeEventListener(name, handler);
var schedule_render = (callback) => {
  const id = window.requestAnimationFrame(callback);
  return () => window.cancelAnimationFrame(id);
};
var after_render = () => {
};
var schedule_before_paint = (callback) => {
  queueMicrotask(callback);
};
var schedule_after_paint = (callback) => {
  window.requestAnimationFrame(callback);
};
var phases = () => toList2([
  Phase$Phase(before_paint_phase, schedule_before_paint),
  Phase$Phase(after_paint_phase, schedule_after_paint)
]);
var dom_strict = (root2) => {
  return new$4(
    root2,
    mount_strict,
    create_element,
    create_text_node,
    create_fragment,
    create_comment,
    insert_before,
    move_before,
    remove_child2,
    next_sibling,
    get_attribute,
    set_attribute,
    remove_attribute,
    set_property,
    set_text,
    set_raw_content,
    create_raw_node,
    add_event_listener,
    remove_event_listener,
    schedule_render,
    after_render,
    phases()
  );
};

// build/dev/javascript/agnostic/agnostic/platform/dom.mjs
var before_paint_phase = "before_paint";
var after_paint_phase = "after_paint";

// build/dev/javascript/agnostic/agnostic/runtime/transport.mjs
var mount_kind = 0;
var reconcile_kind = 1;
var emit_kind = 2;
var provide_kind = 3;
var subscribe_kind = 4;
var unsubscribe_kind = 5;
var attribute_changed_kind = 0;
var event_fired_kind = 1;
var property_changed_kind = 2;
var batch_kind = 3;
var context_provided_kind = 4;

// src/agnostic/runtime/server_component.ffi.mjs
var ServerComponent = class extends HTMLElement {
  static get observedAttributes() {
    return ["route", "method", "csrf-token"];
  }
  #shadowRoot;
  #method = "ws";
  #route = null;
  #csrfToken = null;
  #transport = null;
  #adoptedStyleNodes = [];
  #reconciler;
  #remoteObservedAttributes = /* @__PURE__ */ new Set();
  #remoteObservedProperties = /* @__PURE__ */ new Set();
  #connected = false;
  #changedAttributesQueue = [];
  #contexts = /* @__PURE__ */ new Map();
  #contextSubscriptions = /* @__PURE__ */ new Map();
  #observer = new MutationObserver((mutations) => {
    const attributes = [];
    for (const mutation of mutations) {
      if (mutation.type !== "attributes") continue;
      const name = mutation.attributeName;
      if (!this.#connected || this.#remoteObservedAttributes.has(name)) {
        attributes.push([name, this.getAttribute(name)]);
      }
    }
    if (attributes.length === 1) {
      const [name, value] = attributes[0];
      this.#transport?.send({ kind: attribute_changed_kind, name, value });
    } else if (attributes.length) {
      this.#transport?.send({
        kind: batch_kind,
        messages: attributes.map(([name, value]) => ({
          kind: attribute_changed_kind,
          name,
          value
        }))
      });
    } else {
      this.#changedAttributesQueue.push(...attributes);
    }
  });
  constructor() {
    super();
    this.internals = this.attachInternals();
    this.#observer.observe(this, {
      attributes: true
    });
  }
  connectedCallback() {
    for (const attribute3 of this.attributes) {
      this.#changedAttributesQueue.push([attribute3.name, attribute3.value]);
    }
  }
  attributeChangedCallback(name, prev, next) {
    switch (name) {
      case (prev !== next && "route"): {
        this.#route = new URL(next, location.href);
        this.#csrfToken = this.#getCsrfToken();
        this.#route.searchParams.set("csrf-token", this.#csrfToken);
        this.#connect();
        return;
      }
      case "method": {
        const normalised = next.toLowerCase();
        if (normalised == this.#method) return;
        if (["ws", "sse", "polling"].includes(normalised)) {
          this.#method = normalised;
          if (this.#method == "ws") {
            if (this.#route.protocol == "https:") this.#route.protocol = "wss:";
            if (this.#route.protocol == "http:") this.#route.protocol = "ws:";
          }
          this.#connect();
        }
        return;
      }
      case "csrf-token": {
        if (prev !== next && this.#connected) {
          this.#transport?.close();
        }
        this.#csrfToken = this.#getCsrfToken();
        if (this.#route) {
          this.#route.searchParams.set("csrf-token", this.#csrfToken);
        }
        if (this.#connected) {
          this.#connect();
        }
      }
    }
  }
  async messageReceivedCallback(data2) {
    switch (data2.kind) {
      case mount_kind: {
        this.#shadowRoot ??= this.attachShadow({
          mode: data2.open_shadow_root ? "open" : "closed"
        });
        while (this.#shadowRoot.firstChild) {
          this.#shadowRoot.firstChild.remove();
        }
        const decodeEvent = (event2, path, name, include) => {
          const data3 = this.#createServerEvent(event2, include ?? []);
          return {
            kind: event_fired_kind,
            path,
            name,
            event: data3
          };
        };
        const dispatch2 = (event2, data3) => {
          this.#transport?.send(data3);
        };
        const platform = dom_strict(this.#shadowRoot);
        this.#reconciler = new Reconciler(
          this.#shadowRoot,
          decodeEvent,
          dispatch2,
          platform
        );
        this.#remoteObservedAttributes = new Set(data2.observed_attributes);
        const filteredQueuedAttributes = this.#changedAttributesQueue.filter(
          ([name]) => this.#remoteObservedAttributes.has(name)
        );
        const messages = filteredQueuedAttributes.map(([name, value]) => ({
          kind: attribute_changed_kind,
          name,
          value
        }));
        this.#changedAttributesQueue = [];
        this.#remoteObservedProperties = new Set(data2.observed_properties);
        for (const name of this.#remoteObservedProperties) {
          Object.defineProperty(this, name, {
            get() {
              return this[`_${name}`];
            },
            set(value) {
              this[`_${name}`] = value;
              this.#transport?.send({
                kind: property_changed_kind,
                name,
                value
              });
            }
          });
        }
        for (const [key, value] of Object.entries(data2.provided_contexts)) {
          this.provide(key, value);
        }
        for (const key of [...new Set(data2.requested_contexts)]) {
          this.subscribe(key);
        }
        if (messages.length) {
          this.#transport.send({
            kind: batch_kind,
            messages
          });
        }
        if (data2.will_adopt_styles) {
          await this.#adoptStyleSheets();
        }
        this.#shadowRoot.addEventListener("context-request", (event2) => {
          if (!event2.context || !event2.callback) return;
          if (!this.#contexts.has(event2.context)) return;
          event2.stopImmediatePropagation();
          const context = this.#contexts.get(event2.context);
          if (event2.subscribe) {
            const unsubscribe = () => {
              context.subscribers = context.subscribers.filter(
                (subscriber) => subscriber !== event2.callback
              );
            };
            context.subscribers.push([event2.callback, unsubscribe]);
            event2.callback(context.value, unsubscribe);
          } else {
            event2.callback(context.value);
          }
        });
        this.#reconciler.mount(data2.vdom);
        this.dispatchEvent(new CustomEvent("lustre:mount"));
        break;
      }
      case reconcile_kind: {
        this.#reconciler.push(data2.patch);
        break;
      }
      case emit_kind: {
        this.dispatchEvent(new LustreEvent(data2.name, data2.data));
        break;
      }
      case provide_kind: {
        this.provide(data2.key, data2.value);
        break;
      }
      case subscribe_kind: {
        this.subscribe(data2.key);
        break;
      }
      case unsubscribe_kind: {
        this.unsubscribe(data2.key);
        break;
      }
    }
  }
  //
  disconnectedCallback() {
    this.unsubscribeAll();
    if (this.#transport) {
      this.#transport.close();
      this.#transport = null;
    }
  }
  // Context provider method
  provide(key, value) {
    if (!this.#contexts.has(key)) {
      this.#contexts.set(key, { value, subscribers: [] });
    } else {
      const context = this.#contexts.get(key);
      context.value = value;
      for (let i = context.subscribers.length - 1; i >= 0; i--) {
        const [subscriber, unsubscribe] = context.subscribers[i];
        if (!subscriber) {
          context.subscribers.splice(i, 1);
          continue;
        }
        subscriber(value, unsubscribe);
      }
    }
  }
  subscribe(key) {
    if (!key) return;
    this.#contextSubscriptions.get(key)?.();
    this.dispatchEvent(
      new ContextRequestEvent(key, (value, unsubscribe) => {
        this.#transport?.send({
          kind: context_provided_kind,
          key,
          value
        });
        this.#contextSubscriptions.get(key)?.();
        this.#contextSubscriptions.set(unsubscribe);
      })
    );
  }
  unsubscribe(key) {
    this.#contextSubscriptions.get(key)?.();
    this.#contextSubscriptions.delete(key);
  }
  unsubscribeAll() {
    for (const [_, unsubscribe] of this.#contextSubscriptions) {
      unsubscribe?.();
    }
    this.#contextSubscriptions.clear();
  }
  #getCsrfToken() {
    if (this.hasAttribute("csrf-token")) {
      return this.getAttribute("csrf-token") || null;
    } else {
      const meta2 = document.querySelector('meta[name="csrf-token"]');
      const token = meta2?.getAttribute("content");
      return token || null;
    }
  }
  #connect() {
    if (!this.#route || !this.#method) return;
    if (this.#transport) this.#transport.close();
    const onConnect = () => {
      this.#connected = true;
      this.dispatchEvent(new CustomEvent("lustre:connect"), {
        detail: {
          route: this.#route,
          method: this.#method
        }
      });
    };
    const onMessage = (data2) => {
      this.messageReceivedCallback(data2);
    };
    const onClose = () => {
      this.#connected = false;
      this.dispatchEvent(
        new CustomEvent("lustre:close", {
          detail: {
            route: this.#route,
            method: this.#method
          }
        })
      );
    };
    const options = {
      onConnect,
      onMessage,
      onClose,
      csrfToken: this.#csrfToken
    };
    switch (this.#method) {
      case "ws":
        this.#transport = new WebsocketTransport(this.#route, options);
        break;
      case "sse":
        this.#transport = new SseTransport(this.#route, options);
        break;
      case "polling":
        this.#transport = new PollingTransport(this.#route, options);
        break;
    }
  }
  //
  async #adoptStyleSheets() {
    while (this.#adoptedStyleNodes.length) {
      this.#adoptedStyleNodes.pop().remove();
      this.#shadowRoot.firstChild.remove();
    }
    this.#adoptedStyleNodes = await adoptStylesheets(this.#shadowRoot);
  }
  /** Server components send the event data as a JSON object over the network to
   *  the server component runtime. Out of the box this would effectively do nothing
   *  because the event object is not serialisable: almost every property is
   *  non-enumerable.
   *
   *  To counter this, users can provide a list of properties they'd like the runtime
   *  to include in the event data. Each property is a dot-separated string that
   *  represents the traversal path to the desired property.
   *
   */
  #createServerEvent(event2, include = []) {
    const data2 = {};
    if (event2.isLustreEvent) {
      include.push("detail");
    }
    if (event2.type === "input" || event2.type === "change") {
      if (event2.target.type === "checkbox") {
        include.push("target.checked");
      } else {
        include.push("target.value");
      }
    }
    if (event2.type === "keydown" || event2.type === "keyup" || event2.type === "keypress") {
      include.push("key");
    }
    if (event2.type === "submit") {
      include.push("detail.formData");
    }
    for (const property2 of include) {
      const path = property2.split(".");
      for (let i = 0, input = event2, output = data2; i < path.length; i++) {
        if (i === path.length - 1) {
          output[path[i]] = input[path[i]];
          break;
        }
        output = output[path[i]] ??= {};
        input = input[path[i]];
      }
    }
    return data2;
  }
};
var WebsocketTransport = class {
  #url;
  #socket;
  #waitingForResponse = false;
  #queue = [];
  #shouldReconnect = true;
  #reconnectDelay = 500;
  #maxReconnectDelay = 1e4;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose }) {
    this.#url = url;
    this.#onConnect = onConnect;
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#connect();
  }
  #connect() {
    this.#socket = new WebSocket(this.#url);
    this.#shouldReconnect = true;
    this.#queue = [];
    this.#socket.onopen = () => {
      this.#reconnectDelay = 500;
      this.#onConnect();
    };
    this.#socket.onmessage = ({ data: data2 }) => {
      try {
        this.#onMessage(JSON.parse(data2));
      } finally {
        if (this.#queue.length) {
          this.#socket.send(
            JSON.stringify({
              kind: batch_kind,
              messages: this.#queue
            })
          );
        } else {
          this.#waitingForResponse = false;
        }
        this.#queue = [];
      }
    };
    this.#socket.onclose = (event2) => {
      this.#onClose();
      if (event2.code !== 1e3 && this.#shouldReconnect) {
        this.#attemptReconnect();
      }
    };
  }
  #attemptReconnect() {
    const reconnect = () => {
      if (!this.#shouldReconnect) return;
      this.#connect();
      this.#reconnectDelay = Math.min(
        this.#reconnectDelay * 2,
        this.#maxReconnectDelay
      );
    };
    if (document.hidden) {
      const handleVisibilityChange = () => {
        if (!document.hidden && this.#shouldReconnect) {
          document.removeEventListener(
            "visibilitychange",
            handleVisibilityChange
          );
          reconnect();
        }
      };
      document.addEventListener("visibilitychange", handleVisibilityChange);
    } else {
      setTimeout(reconnect, this.#reconnectDelay);
    }
  }
  send(data2) {
    if (!this.#socket || this.#socket.readyState !== WebSocket.OPEN) return;
    if (this.#waitingForResponse) {
      this.#queue.push(data2);
      return;
    } else {
      this.#socket.send(JSON.stringify(data2));
      this.#waitingForResponse = true;
    }
  }
  close() {
    this.#shouldReconnect = false;
    this.#socket.close(1e3);
    this.#socket = null;
  }
};
var SseTransport = class {
  #url;
  #eventSource;
  #shouldReconnect = true;
  #reconnectDelay = 500;
  #maxReconnectDelay = 1e4;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose }) {
    this.#url = url;
    this.#onConnect = onConnect;
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#connect();
  }
  #connect() {
    this.#eventSource = new EventSource(this.#url);
    this.#reconnectDelay = 500;
    this.#shouldReconnect = true;
    this.#eventSource.onopen = () => {
      this.#onConnect();
    };
    this.#eventSource.onmessage = ({ data: data2 }) => {
      try {
        this.#onMessage(JSON.parse(data2));
      } catch {
      }
    };
    this.#eventSource.onerror = () => {
      this.#eventSource.close();
      this.#onClose();
      if (this.#shouldReconnect) {
        this.#attemptReconnect();
      }
    };
  }
  #attemptReconnect() {
    const reconnect = () => {
      if (!this.#shouldReconnect) return;
      this.#connect();
      this.#reconnectDelay = Math.min(
        this.#reconnectDelay * 2,
        this.#maxReconnectDelay
      );
    };
    if (document.hidden) {
      const handleVisibilityChange = () => {
        if (!document.hidden && this.#shouldReconnect) {
          document.removeEventListener(
            "visibilitychange",
            handleVisibilityChange
          );
          reconnect();
        }
      };
      document.addEventListener("visibilitychange", handleVisibilityChange);
    } else {
      setTimeout(reconnect, this.#reconnectDelay);
    }
  }
  send(data2) {
  }
  close() {
    this.#shouldReconnect = false;
    this.#eventSource.close();
    this.#onClose();
  }
};
var PollingTransport = class {
  #url;
  #csrfToken;
  #interval;
  #timer;
  #onConnect;
  #onMessage;
  #onClose;
  constructor(url, { onConnect, onMessage, onClose, csrfToken, interval }) {
    this.#url = url;
    this.#csrfToken = csrfToken;
    this.#interval = interval ?? 5e3;
    this.#onConnect = onConnect;
    this.#onMessage = onMessage;
    this.#onClose = onClose;
    this.#fetch().finally(() => {
      this.#onConnect();
      this.#timer = setInterval(() => this.#fetch(), this.#interval);
    });
  }
  async send(data2) {
  }
  close() {
    clearInterval(this.#timer);
    this.#onClose();
  }
  #fetch() {
    const headers = Object.assign(
      {},
      // If a CSRF token is provided, include it as a request header as checking
      // headers is more common than query param for CSRF protection in traditional
      // HTTP requests.
      this.#csrfToken && { "x-csrf-token": this.#csrfToken }
    );
    return fetch(this.#url, { headers }).then((response) => response.json()).then(this.#onMessage).catch(console.error);
  }
};
customElements.define("lustre-server-component", ServerComponent);
export {
  ServerComponent
};
