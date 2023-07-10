// This module is vendored and lightly modified from the original diffhtml source
// available at:
//
//   https://github.com/tbranyen/diffhtml/
//
// You can read a copy of the original license, unchanged, at:
//
//   https://github.com/tbranyen/diffhtml/blob/master/LICENSE
//

function t(e, t) {
  var n = Object.keys(e);
  if (Object.getOwnPropertySymbols) {
    var r = Object.getOwnPropertySymbols(e);
    t &&
      (r = r.filter(function (t) {
        return Object.getOwnPropertyDescriptor(e, t).enumerable;
      })),
      n.push.apply(n, r);
  }
  return n;
}
function n(e) {
  for (var n = 1; n < arguments.length; n++) {
    var r = null != arguments[n] ? arguments[n] : {};
    n % 2
      ? t(Object(r), !0).forEach(function (t) {
          i(e, t, r[t]);
        })
      : Object.getOwnPropertyDescriptors
      ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(r))
      : t(Object(r)).forEach(function (t) {
          Object.defineProperty(e, t, Object.getOwnPropertyDescriptor(r, t));
        });
  }
  return e;
}
function r(e, t) {
  if (!(e instanceof t))
    throw new TypeError("Cannot call a class as a function");
}
function a(e, t) {
  for (var n = 0; n < t.length; n++) {
    var r = t[n];
    (r.enumerable = r.enumerable || !1),
      (r.configurable = !0),
      "value" in r && (r.writable = !0),
      Object.defineProperty(e, r.key, r);
  }
}
function o(e, t, n) {
  return (
    t && a(e.prototype, t),
    n && a(e, n),
    Object.defineProperty(e, "prototype", { writable: !1 }),
    e
  );
}
function i(e, t, n) {
  return (
    t in e
      ? Object.defineProperty(e, t, {
          value: n,
          enumerable: !0,
          configurable: !0,
          writable: !0,
        })
      : (e[t] = n),
    e
  );
}
function s(e) {
  return c(e) || d(e) || l(e) || f();
}
function c(e) {
  if (Array.isArray(e)) return u(e);
}
function d(e) {
  if (
    ("undefined" != typeof Symbol && null != e[Symbol.iterator]) ||
    null != e["@@iterator"]
  )
    return Array.from(e);
}
function l(e, t) {
  if (e) {
    if ("string" == typeof e) return u(e, t);
    var n = Object.prototype.toString.call(e).slice(8, -1);
    return (
      "Object" === n && e.constructor && (n = e.constructor.name),
      "Map" === n || "Set" === n
        ? Array.from(e)
        : "Arguments" === n ||
          /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)
        ? u(e, t)
        : void 0
    );
  }
}
function u(e, t) {
  (null == t || t > e.length) && (t = e.length);
  for (var n = 0, r = new Array(t); n < t; n++) r[n] = e[n];
  return r;
}
function f() {
  throw new TypeError(
    "Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."
  );
}
function h(e, t) {
  var n = String(e);
  switch (t) {
    case "boolean":
      return "false" !== n;
    case "string":
      return n;
    case "number":
      return te(n, 10);
    case "object":
      return ne(n);
  }
}
function v(e, t) {
  var n =
      arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : typeof t,
    r = arguments.length > 3 ? arguments[3] : void 0,
    a = ee.location,
    o = ee.URLSearchParams,
    i = void 0 !== o,
    s = void 0 !== a,
    c = i && s,
    d = W.env;
  if (r && e in r) return r[e];
  var l = "DIFF_".concat(e.replace(/[^a-zA-Z0-9]/, ""));
  if (c) {
    var u = new o(a.search),
      f = l.toLowerCase();
    if (u.has(f)) return h(decodeURIComponent(String(u.get(f))), n);
  }
  var v = l.toUpperCase();
  return d && v in W.env ? h(W.env[v.toUpperCase()], n) : t;
}
function p(e) {
  for (
    var t = arguments.length > 1 && void 0 !== arguments[1] ? arguments[1] : [],
      n = 0;
    n < e.length;
    n++
  ) {
    var r = e[n];
    r && r.rawNodeName === ve ? p(r.childNodes, t) : r && t.push(r);
  }
  return t;
}
function m(e, t, r) {
  var a = null;
  if (he.protected.has(e) || he.allocated.has(e)) a = e;
  else if (!e || fe(e)) {
    var o = e ? e.length : 0;
    r = [];
    for (var i = 0; i < o; i++) {
      var c = e && !e[i];
      c || (e && r.push(e[i]));
    }
    a = m(ve, null, r);
  }
  if (a) return a;
  var d = "object" == typeof e,
    l = e;
  if (e && d && "ownerDocument" in l) {
    if (l.nodeType === B.TEXT) {
      var u = m(pe, l.nodeValue);
      return X.set(u, l), u;
    }
    (t = {}), (r = []);
    var f = l.attributes;
    if (l.nodeType === B.ELEMENT && f && f.length)
      for (var h = 0; h < f.length; h++) {
        var v = f[h],
          T = v.name,
          g = v.value;
        g === U.STR && T in l ? (t[T] = e[T]) : (t[T] = g);
      }
    if (l.nodeType === B.ELEMENT || l.nodeType === B.FRAGMENT) {
      r = [];
      for (var b = 0; b < l.childNodes.length; b++) {
        var N = l.childNodes[b];
        r.push(m(N));
      }
    }
    return (
      X.forEach(function (t, n) {
        t === e && (a = n);
      }),
      (a = a || m(l.nodeName, t, r)),
      (a.attributes = n(n({}, a.attributes), t)),
      (a.childNodes = r),
      X.set(a, l),
      a
    );
  }
  if (d) {
    var y = e.rawNodeName,
      E = e.nodeName,
      w = e.nodeValue,
      S = e.attributes,
      O = e.childNodes,
      k = e.children,
      R = y || E,
      C = m(R, S || null, k || O);
    return w && (C.nodeValue = w), C;
  }
  for (
    var M = arguments.length, A = new Array(M > 3 ? M - 3 : 0), x = 3;
    x < M;
    x++
  )
    A[x - 3] = arguments[x];
  A.length && (r = [r].concat(A)), (a = ue.get());
  var L = e === pe;
  "string" == typeof e
    ? ((a.rawNodeName = e), (a.nodeName = a.rawNodeName.toLowerCase()))
    : ((a.rawNodeName = e), (a.nodeName = ve)),
    (a.nodeValue = U.STR),
    (a.key = U.STR),
    (a.childNodes.length = 0),
    (a.attributes = {});
  var D = fe(t) || "object" != typeof t,
    I = D ? t : r,
    V = p(fe(I) ? I : [I]);
  if (L) {
    var j = V.join(U.STR);
    return (a.nodeType = B.TEXT), (a.nodeValue = String(j)), a;
  }
  if (
    (a.nodeName === ve
      ? (a.nodeType = B.FRAGMENT)
      : (a.nodeType = "#comment" === e ? B.COMMENT : B.ELEMENT),
    I && V.length && (!t || !t.childNodes))
  )
    for (var _ = 0; _ < V.length; _++) {
      var P = V[_];
      if (fe(P)) {
        var H;
        (H = a.childNodes).push.apply(H, s(P));
      } else {
        if (!P) continue;
        if (P.nodeType === B.FRAGMENT && "string" == typeof P.rawNodeName) {
          var F;
          (F = a.childNodes).push.apply(F, s(P.childNodes));
        } else
          P && "object" == typeof P
            ? a.childNodes.push(m(P))
            : a.childNodes.push(m(pe, null, P));
      }
    }
  if (
    t &&
    "object" == typeof t &&
    !fe(t) &&
    ((a.attributes = n({}, t)), t.childNodes)
  ) {
    var z = "object" == typeof t.childNodes;
    a.childNodes.push(z ? m(t.childNodes) : m("#text", t.childNodes));
  }
  return (
    "script" === a.nodeName &&
      a.attributes.src &&
      (a.key = String(a.attributes.src)),
    a.attributes && "key" in a.attributes && (a.key = String(a.attributes.key)),
    Y.size &&
      Y.forEach(function (e, t) {
        (t = e(a)) && (a = m(t));
      }),
    a
  );
}
function T(e) {
  var t = e.mount,
    n = e.input,
    r = n,
    a = ge++;
  return v("collectMetrics", !1)
    ? function (e) {
        e = "[".concat(a, "] ").concat(e);
        var n = t.host;
        t && n
          ? (e = "".concat(n.constructor.name, " ").concat(e))
          : r &&
            "function" == typeof r.rawNodeName &&
            (e = "".concat(r.rawNodeName.name, " ").concat(e));
        var o = "".concat(e, "-end");
        if (Te.has(e)) {
          var i = Te.get(e) || 0,
            s = (performance.now() - i).toFixed(3);
          Te.delete(e),
            performance.mark(o),
            performance.measure(
              "".concat(me, " ").concat(e, " (").concat(s, "ms)"),
              e,
              o
            );
        } else Te.set(e, performance.now()), performance.mark(e);
      }
    : U.FUN;
}
function g(e) {
  if ((be(e), e.childNodes.length))
    for (var t = 0; t < e.childNodes.length; t++) g(e.childNodes[t]);
}
function b(e) {
  if (e.childNodes.length)
    for (var t = 0; t < e.childNodes.length; t++) b(e.childNodes[t]);
  X.delete(e), Ne(e);
}
function N() {
  ye.allocated.forEach(function (e) {
    (e.attributes = {}),
      (e.childNodes.length = 0),
      ye.free.add(e),
      ye.allocated.delete(e),
      X.delete(e);
  });
}
function y(e) {
  var t = e.state,
    n = e.state.isRendering;
  t.measure("schedule"),
    G.forEach(function (r) {
      var a = r.activeTransaction && r.activeTransaction.mount,
        o = e.mount;
      a &&
        o &&
        r.isRendering &&
        ((a.contains && a.contains(o)) || (o.contains && o.contains(a))
          ? ((t = r), (n = !0))
          : a === o && ((t = r), (n = !0)));
    });
  var r = t,
    a = r.activeTransaction,
    o = r.nextTransaction;
  if (n) {
    var i = e.tasks;
    (t.nextTransaction = e), e.abort();
    var s = (o && o.promise) || a.promise || Promise.resolve();
    return (e.promise = s.then(function () {
      return (
        (e.aborted = !1),
        (e.state.isRendering = !0),
        (e.state.activeTransaction = e),
        t.measure("schedule"),
        Je.flow(e, i.slice(1))
      );
    }));
  }
  (t.isRendering = !0), (t.activeTransaction = e), t.measure("schedule");
}
function E(e) {
  var t = e.mount,
    n = e.input,
    r = e.state.measure,
    a = e.config,
    o = a.inner ? "innerHTML" : "outerHTML";
  r("should update");
  var i = t;
  if ("string" == typeof n && i[o] === n) return e.abort(!0);
  r("should update");
}
function w(e) {
  if (G.has(e)) {
    var t = G.get(e),
      n = t.mutationObserver,
      r = t.oldTree;
    n && n.disconnect(),
      r &&
        !X.has(r) &&
        (K.forEach(function (e) {
          return e(r);
        }),
        b(r)),
      G.delete(e);
  }
  if (e) {
    var a = e;
    if (a.childNodes && a.childNodes.length)
      for (var o = 0; o < a.childNodes.length; o++) w(a.childNodes[o]);
    a.shadowRoot && w(a.shadowRoot),
      X.forEach(function (e, t) {
        e === a &&
          (K.forEach(function (e) {
            return e(t);
          }),
          b(t));
      }),
      ke(Se),
      (Se = Oe(N));
  }
}
function S(e) {
  var t = e.state,
    n = e.mount,
    r = e.input,
    a = e.config,
    o = a.inner,
    i = n;
  t.mutationObserver && !t.isDirty
    ? (t.isDirty = Boolean(t.mutationObserver.takeRecords().length))
    : t.mutationObserver || (t.isDirty = !1),
    (!t.isDirty && t.oldTree) ||
      (w(i),
      i.ownerDocument &&
        t.mutationObserver &&
        t.mutationObserver.observe(i, {
          subtree: !0,
          childList: !0,
          attributes: !0,
          characterData: !0,
        }),
      (t.oldTree = m(i)),
      g(t.oldTree),
      G.set(n, t));
  var s = t.oldTree,
    c = s.nodeName,
    d = s.attributes;
  e.newTree || (e.newTree = m(r));
  var l = e.newTree;
  if (!o && l.nodeType === B.FRAGMENT && t.oldTree.nodeType !== B.FRAGMENT) {
    for (var u = [], f = 0; f < l.childNodes.length; f++) {
      var h = l.childNodes[f];
      (h.nodeType === B.TEXT && !h.nodeValue.trim()) || u.push(h);
    }
    1 === u.length
      ? (e.newTree = u[0])
      : u.length > 1 && (e.newTree = m(l.childNodes));
  }
  e.oldTree = t.oldTree;
  var v = e.oldTree,
    p = e.newTree;
  if (o && v && p) {
    var T = "string" != typeof p.rawNodeName,
      b = p.nodeType === B.FRAGMENT,
      N = b && !T ? p.childNodes : p;
    e.newTree = m(c, d, N);
  }
}
function O(e, t) {
  var n = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : [],
    r = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : {},
    a = arguments.length > 4 && void 0 !== arguments[4] ? arguments[4] : U.OBJ,
    o = arguments.length > 5 ? arguments[5] : void 0;
  e || (e = U.OBJ), t || (t = U.OBJ);
  var i = r.svgElements,
    s = void 0 === i ? new Set() : i,
    c = e.nodeName,
    d = t.nodeName,
    l = e === U.OBJ || o,
    u = "svg" === d || s.has(t),
    f = null;
  if (
    ($.size &&
      $.forEach(function (r) {
        var o = r(e, t, a);
        o && o === e ? (f = n) : !1 === o ? (f = !1) : o && (t = o);
      }),
    null !== f || !t)
  )
    return f;
  if (d === Me) {
    if (c === Me && e.nodeValue !== t.nodeValue)
      return (
        n.push(F.NODE_VALUE, e, t.nodeValue, e.nodeValue),
        (e.nodeValue = t.nodeValue),
        n
      );
    if (l) return n.push(F.NODE_VALUE, t, t.nodeValue, null), n;
  }
  var h = t.childNodes || [];
  if (t.nodeType === B.ELEMENT) {
    var v = l ? U.OBJ : e.attributes,
      p = t.attributes || {};
    for (var m in p) {
      var T = p[m];
      (m in v && v[m] === p[m]) ||
        (l || (v[m] = T),
        ((e && "script" === e.nodeName) ||
          "script" !== t.nodeName ||
          "type" !== m) &&
          n.push(F.SET_ATTRIBUTE, l ? t : e, m, T));
    }
    if (!l)
      for (var g in v)
        g in p || (n.push(F.REMOVE_ATTRIBUTE, e, g), delete v[g]);
  }
  if (o) {
    for (var b = 0; b < h.length; b++)
      u && s.add(h[b]), O(null, h[b], n, r, a, !0);
    return n;
  }
  for (var N = { old: new Map(), new: new Map() }, y = 0; y < Ce.length; y++) {
    var E = Ce[y],
      w = N[E],
      S = arguments[y],
      k = S && S.childNodes;
    if (k && k.length)
      for (var R = 0; R < k.length; R++) {
        var C = k[R];
        C.key && w.set(C.key, C);
      }
  }
  for (
    var M = e.childNodes || [], A = Re(h.length, M.length), x = 0;
    x < A;
    x++
  ) {
    var L = M && M[x],
      D = h[x];
    if (((u || (D && "svg" === D.nodeName)) && s.add(D), D))
      if (L) {
        var I = D.key,
          V = L.key,
          j = N.new.has(V),
          _ = N.old.has(I);
        if (V || I) {
          if (!j && !_) {
            M.splice(M.indexOf(L), 1, D),
              O(null, D, n, r, a, !0),
              n.push(F.REPLACE_CHILD, D, L),
              (x -= 1);
            continue;
          }
          if (!j) {
            n.push(F.REMOVE_CHILD, L), M.splice(M.indexOf(L), 1), (x -= 1);
            continue;
          }
          if (I !== V) {
            var P = D;
            I && _ ? ((P = N.old.get(I)), M.splice(M.indexOf(P), 1)) : (P = D),
              O(null, P, n, r, a, !0),
              n.push(F.INSERT_BEFORE, e, P, L),
              M.splice(x, 0, P);
            continue;
          }
        }
        var H = L.nodeName === D.nodeName,
          z = O(L, D, n, r, a, !H);
        if (!1 !== z) {
          if (!H) {
            M[x] = D;
            var J = M.lastIndexOf(D);
            J > x && M.splice(J, 1), n.push(F.REPLACE_CHILD, D, L);
          }
        } else h.splice(x, 0, L), (A += 1);
      } else
        M.push(D), O(null, D, n, r, a, !0), n.push(F.INSERT_BEFORE, e, D, null);
    else !1 === O(L, null, n, r, a, !0) && h.splice(x, 0, L);
  }
  if (M.length !== h.length) {
    for (var G = h.length; G < M.length; G++) n.push(F.REMOVE_CHILD, M[G]);
    M.length = h.length;
  }
  return n;
}
function k(e) {
  var t =
      arguments.length > 1 && void 0 !== arguments[1]
        ? arguments[1]
        : ee.document,
    n = arguments.length > 2 ? arguments[2] : void 0,
    r = m(e),
    a = X.get(r);
  if (a) return a;
  var o = r.nodeName,
    i = r.rawNodeName,
    s = void 0 === i ? o : i,
    c = r.childNodes,
    d = void 0 === c ? [] : c;
  n = n || "svg" === o;
  var l = null,
    u = null;
  if (
    (Z.forEach(function (e) {
      (u = e(r)) && (l = u);
    }),
    !t)
  )
    return l;
  var f = l;
  f ||
    ((f =
      "#text" === o
        ? t.createTextNode(r.nodeValue || U.STR)
        : "#document-fragment" === o
        ? t.createDocumentFragment()
        : n
        ? t.createElementNS(Ae, s)
        : t.createElement(s)),
    "script" === o && (f.type = "no-execute")),
    X.set(r, f);
  for (var h = 0; h < d.length; h++) {
    var v = k(d[h], t, n);
    f && v && f.appendChild(v);
  }
  return f;
}
function R(e) {
  var t = e.state,
    n = e.state.measure,
    r = e.oldTree,
    a = e.newTree,
    o = e.mount;
  if (
    (n("sync trees"),
    r && a && r.nodeName !== a.nodeName && a.nodeType !== B.FRAGMENT)
  ) {
    (e.patches = [F.REPLACE_CHILD, a, r]), (e.oldTree = t.oldTree = a);
    var i = k(a);
    G.delete(o),
      G.set(i, t),
      (e.mount = i),
      "script" === a.nodeName &&
        t.scriptsToExecute.set(a, a.attributes.type || U.STR);
  } else e.patches = O(r || null, a || null, [], t, e);
  n("sync trees");
}
function C(e, t) {
  var n;
  null === (n = J.get(e)) || void 0 === n || n.add(t);
}
function M(e, t) {
  if (!t && e) {
    var n;
    null === (n = J.get(e)) || void 0 === n || n.clear();
  } else if (e && t) {
    var r;
    null === (r = J.get(e)) || void 0 === r || r.delete(t);
  } else
    for (var a = 0; a < z.length; a++) {
      var o;
      null === (o = J.get(z[a])) || void 0 === o || o.clear();
    }
}
function A(e) {
  for (
    var t = arguments.length, n = new Array(t > 1 ? t - 1 : 0), r = 1;
    r < t;
    r++
  )
    n[r - 1] = arguments[r];
  var a = J.get(e),
    o = [];
  if (!a) return o;
  var i = n[0],
    c = i.nodeType === B.ELEMENT;
  return !a.size || ("textChanged" !== e && !c)
    ? o
    : (a.forEach(function (e) {
        var t = n.map(function (e) {
            return X.get(e) || e;
          }),
          r = e.apply(void 0, s(t));
        "object" == typeof r && r.then && o.push(r);
      }),
      ("attached" !== e && "detached" !== e) ||
        i.childNodes.forEach(function (t) {
          o.push.apply(o, s(A.apply(void 0, [e, t].concat(s(n.slice(1))))));
        }),
      o);
}
function x(e) {
  return xe && e && e.indexOf && e.includes("&")
    ? ((xe.innerHTML = e), xe.textContent || U.STR)
    : e;
}
function L(e) {
  for (
    var t =
        arguments.length > 1 && void 0 !== arguments[1] ? arguments[1] : U.OBJ,
      n = [],
      r = t.ownerDocument,
      a = t.svgElements,
      o = void 0 === a ? new Set() : a,
      i = e.length,
      c = 0;
    ;

  ) {
    var d = e[c];
    if (c === i) break;
    switch (d) {
      case F.REMOVE_ATTRIBUTE:
      case F.SET_ATTRIBUTE:
        if (
          "break" ===
          (function () {
            var t = d === F.SET_ATTRIBUTE,
              a = e[c + 1],
              i = e[c + 2],
              l = t ? x(e[c + 3]) : null;
            c += t ? 4 : 3;
            var u = o.has(a),
              f = k(a, r, u),
              h = f.getAttribute(i),
              v = A("attributeChanged", a, i, h, l);
            g(a);
            var p = t ? _e : Pe;
            return (
              v.length
                ? (Promise.all(v).then(function () {
                    return p(a, f, i, l);
                  }),
                  n.push.apply(n, s(v)))
                : p(a, f, i, l),
              "break"
            );
          })()
        )
          break;
      case F.NODE_VALUE:
        if (
          "break" ===
          (function () {
            var t = e[c + 1],
              a = e[c + 2],
              i = e[c + 3],
              d = o.has(t);
            c += 4;
            var l = k(t, r, d);
            g(t);
            var u = A("textChanged", t, i, a);
            return (
              u.length
                ? (Promise.all(u).then(function () {
                    return He(l, a);
                  }),
                  n.push.apply(n, s(u)))
                : He(l, a),
              "break"
            );
          })()
        )
          break;
      case F.INSERT_BEFORE:
        var l = e[c + 1],
          u = e[c + 2],
          f = e[c + 3];
        if (((c += 4), !X.has(l) && l !== Le)) continue;
        var h = X.get(l);
        if (l === Le) {
          var v = X.get(f);
          v && ((h = v.parentNode), (f = v.nextSibling ? v.nextSibling : null));
        }
        var p = o.has(u);
        g(u);
        var m = f && k(f, r, p),
          T = k(u, r, p);
        h.insertBefore(T, m || null), n.push.apply(n, s(A("attached", u)));
        break;
      case F.REPLACE_CHILD:
        if (
          "break" ===
          (function () {
            var t,
              a,
              i,
              d = e[c + 1],
              l = e[c + 2];
            c += 3;
            var u = o.has(d),
              f = X.get(l),
              h = k(d, r, u);
            if (!f || !f.parentNode) return "break";
            g(d);
            var v =
                null === (t = J.get("attached")) || void 0 === t
                  ? void 0
                  : t.size,
              p =
                null === (a = J.get("detached")) || void 0 === a
                  ? void 0
                  : a.size,
              m =
                null === (i = J.get("replaced")) || void 0 === i
                  ? void 0
                  : i.size;
            if (!v && !p && !m)
              return f.parentNode.replaceChild(h, f), b(l), "break";
            f.parentNode.insertBefore(h, f);
            var T = [].concat(
              s((v && A("attached", d)) || U.ARR),
              s((p && A("detached", l)) || U.ARR),
              s((m && A("replaced", l, d)) || U.ARR)
            );
            return (
              T.length
                ? (Promise.all(T).then(function () {
                    f.parentNode && f.parentNode.removeChild(f), b(l);
                  }),
                  n.push.apply(n, s(T)))
                : (f.parentNode.removeChild(f), b(l)),
              "break"
            );
          })()
        )
          break;
      case F.REMOVE_CHILD:
        if (
          "break" ===
          (function () {
            var t = e[c + 1];
            c += 2;
            var r = X.get(t);
            if (!r || !r.parentNode) return "break";
            var a = A("detached", t);
            return (
              a.length
                ? (Promise.all(a).then(function () {
                    r.parentNode && r.parentNode.removeChild(r), b(t);
                  }),
                  n.push.apply(n, s(a)))
                : (r.parentNode.removeChild(r), b(t)),
              "break"
            );
          })()
        )
          break;
    }
  }
  return n;
}
function D(e) {
  var t = e.mount,
    n = e.state,
    r = e.patches,
    a = n.mutationObserver,
    o = n.measure,
    i = n.scriptsToExecute;
  o("patch node");
  var c = t.ownerDocument,
    d = e.promises || [];
  (n.ownerDocument = c || ee.document), a && a.disconnect();
  var l = function (e) {
    "script" === e.nodeName && i.set(e, e.attributes.type);
  };
  Z.add(l),
    n.ownerDocument && d.push.apply(d, s(L(r, n))),
    Z.delete(l),
    (e.promises = d),
    o("patch node");
}
function I(e) {
  var t = e.promises;
  return t && t.length
    ? (e.promise = Promise.all(t).then(function () {
        return e.end();
      }))
    : (e.promise = Promise.resolve(e.end()));
}
function V() {
  return Boolean(Be && "noModule" in Be);
}
function j(e) {
  return e.replace(/[&<>]/g, function (e) {
    return "&#".concat(e.charCodeAt(0), ";");
  });
}
function _(e) {
  var t =
      arguments.length > 1 && void 0 !== arguments[1] ? arguments[1] : U.STR,
    n = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {};
  return (
    (n.inner = !0),
    (n.executeScripts = !("executeScripts" in n) || n.executeScripts),
    (n.tasks = n.tasks || Fe),
    Je.create(e, t, n).start()
  );
}
function P(e) {
  var t =
      arguments.length > 1 && void 0 !== arguments[1] ? arguments[1] : U.STR,
    n = arguments.length > 2 && void 0 !== arguments[2] ? arguments[2] : {};
  return (
    (n.inner = !1),
    (n.executeScripts = !("executeScripts" in n) || n.executeScripts),
    (n.tasks = n.tasks || Fe),
    Je.create(e, t, n).start()
  );
}
function H(e) {
  var t = "function" == typeof e,
    n = e.subscribe,
    r = e.unsubscribe,
    a = e.createTreeHook,
    o = e.createNodeHook,
    i = e.syncTreeHook,
    s = e.releaseHook,
    c = e.parseHook;
  return (
    t && q.add(e),
    n && n(Xe),
    a && Y.add(a),
    o && Z.add(o),
    i && $.add(i),
    s && K.add(s),
    c && Q.add(c),
    function () {
      t && q.delete(e),
        r && r(Xe),
        a && Y.delete(a),
        o && Z.delete(o),
        i && $.delete(i),
        s && K.delete(s),
        c && Q.delete(c);
    }
  );
}
var B = { ELEMENT: 1, ATTR: 2, TEXT: 3, COMMENT: 8, FRAGMENT: 11 },
  U = {
    STR: "",
    NUM: 1,
    OBJ: {},
    ARR: [],
    MAP: new Map(),
    SET: new Set(),
    DOM: {},
    FUN: function () {},
  },
  F = {
    SET_ATTRIBUTE: 0,
    REMOVE_ATTRIBUTE: 1,
    NODE_VALUE: 2,
    INSERT_BEFORE: 3,
    REPLACE_CHILD: 4,
    REMOVE_CHILD: 5,
  },
  z = ["attached", "detached", "replaced", "attributeChanged", "textChanged"],
  J = new Map([
    ["attached", new Set()],
    ["detached", new Set()],
    ["replaced", new Set()],
    ["attributeChanged", new Set()],
    ["textChanged", new Set()],
  ]),
  G = new Map(),
  X = new Map(),
  q = new Set(),
  Y = new Set(),
  Z = new Set(),
  $ = new Set(),
  K = new Set(),
  Q = new Set(),
  W = { env: { NODE_ENV: "production" } },
  ee =
    "object" == typeof global
      ? global
      : ("object" == typeof window ? window : self) || {},
  te = Number.parseInt,
  ne = JSON.parse,
  re = { collectMetrics: !0, executeScripts: !0 },
  ae = v("initialPoolSize", 5e3),
  oe = new Set(),
  ie = new Set(),
  se = new Set(),
  ce = function () {
    return {
      rawNodeName: U.STR,
      nodeName: U.STR,
      nodeValue: U.STR,
      nodeType: B.ELEMENT,
      key: U.STR,
      childNodes: [],
      attributes: {},
    };
  },
  de = { free: oe, allocated: ie, protected: se },
  le = oe.values(),
  ue = {
    size: ae,
    memory: de,
    fill: function () {
      for (var e = this, t = oe.size; t < this.size; t++) oe.add(ce());
      this.size < oe.size &&
        oe.forEach(function (t) {
          oe.size !== e.size && oe.delete(t);
        });
    },
    get: function () {
      var e = le.next(),
        t = e.value,
        n = void 0 === t ? ce() : t;
      return e.done && (le = oe.values()), oe.delete(n), ie.add(n), n;
    },
    protect: function (e) {
      ie.delete(e), se.add(e);
    },
    unprotect: function (e) {
      (se.has(e) || ie.has(e)) && (se.delete(e), ie.delete(e), oe.add(e));
    },
  };
ue.fill();
var fe = Array.isArray,
  he = ue.memory,
  ve = "#document-fragment",
  pe = "#text",
  me = "diffHTML",
  Te = new Map(),
  ge = 0,
  be = ue.protect,
  Ne = ue.unprotect,
  ye = ue.memory,
  Ee = Object.freeze({
    __proto__: null,
    protectVTree: g,
    unprotectVTree: b,
    gc: N,
  }),
  we = "undefined" != typeof requestIdleCallback,
  Se = -1,
  Oe = function (e) {
    return (we ? requestIdleCallback : setTimeout)(e);
  },
  ke = function (e) {
    return (we ? cancelIdleCallback : clearTimeout)(e);
  },
  Re = Math.max,
  Ce = ["old", "new"],
  Me = "#text",
  Ae = "http://www.w3.org/2000/svg",
  xe = ee.document ? document.createElement("div") : null,
  Le = Symbol.for("diff.after"),
  De = Symbol.for("diffHTML"),
  Ie = Object.keys,
  Ve = new Set(),
  je = new Set(),
  _e = function (e, t, n, r) {
    var a = "object" == typeof r && r,
      o = "function" == typeof r,
      i = "symbol" == typeof r,
      s = 0 === n.indexOf("on"),
      c = t,
      d = s ? n.toLowerCase() : n,
      l = "s-" + e.nodeName + "-" + d,
      u = t;
    if (je.has(l)) c[d] = r;
    else if (!Ve.has(l))
      try {
        (c[d] = r), je.add(l);
      } catch (e) {
        Ve.add(l);
      }
    if (a || o || i) {
      if (a && "style" === d)
        for (var f = Ie(r), h = 0; h < f.length; h++) u.style[f[h]] = r[f[h]];
    } else {
      var v = null === r || void 0 === r || !0 === r;
      u.setAttribute(d, v ? U.STR : r);
    }
  },
  Pe = function (e, t, n) {
    var r = "r-" + e.nodeName + "-" + n,
      a = t;
    if (je.has(r)) (a[n] = void 0), delete a[n];
    else if (!Ve.has(r))
      try {
        (a[n] = void 0), delete a[n], je.add(r);
      } catch (e) {
        Ve.add(r);
      }
    t.removeAttribute(n);
  },
  He = function (e, t) {
    var n = e;
    t.includes("&") ? (n.nodeValue = x(t)) : (n.nodeValue = t);
  },
  Be = ee.document ? document.createElement("script") : null,
  Ue = Object.assign,
  Fe = [y, E, S, R, D, I],
  ze = {
    schedule: y,
    shouldUpdate: E,
    reconcileTrees: S,
    syncTrees: R,
    patchNode: D,
    endAsPromise: I,
  },
  Je = (function () {
    function e(t, n, a) {
      r(this, e),
        i(this, "state", U.OBJ),
        i(this, "mount", U.OBJ),
        i(this, "input", U.OBJ),
        i(this, "oldTree", void 0),
        i(this, "newTree", void 0),
        i(this, "promise", void 0),
        i(this, "promises", void 0),
        i(this, "tasks", []),
        i(this, "patches", []),
        (this.mount = t),
        (this.input = n),
        (this.config = a);
      var o =
        !a.disableMutationObserver &&
        "MutationObserver" in (ee.window || U.OBJ);
      (this.state = G.get(t) || {
        measure: T(this),
        svgElements: new Set(),
        scriptsToExecute: new Map(),
        activeTransaction: this,
        mutationObserver: o && new ee.window.MutationObserver(U.FUN),
      }),
        (this.tasks = v("tasks", Fe, void 0, a).slice()),
        (this.endedCallbacks = new Set()),
        G.set(t, this.state);
    }
    return (
      o(
        e,
        [
          {
            key: "start",
            value: function () {
              var t = this.state.measure,
                n = this.tasks,
                r = n.pop();
              return (
                t("render"),
                (this.aborted = !1),
                e.invokeMiddleware(this),
                r && n.push(r),
                e.flow(this, n)
              );
            },
          },
          {
            key: "abort",
            value: function (e) {
              if (((this.aborted = !0), e))
                return this.tasks[this.tasks.length - 1](this);
            },
          },
          {
            key: "end",
            value: function () {
              var e = this,
                t = this.state,
                n = this.config,
                r = this.mount,
                a = t.mutationObserver,
                o = t.measure,
                i = t.svgElements,
                s = t.scriptsToExecute,
                c = r;
              return (
                o("finalize"),
                (this.completed = !0),
                i.clear(),
                (t.isRendering = !1),
                (t.isDirty = !1),
                c.ownerDocument && a
                  ? a.observe(c, {
                      subtree: !0,
                      childList: !0,
                      attributes: !0,
                      characterData: !0,
                    })
                  : (t.isDirty = !0),
                s.forEach(function (e, r) {
                  var a = X.get(r);
                  if (
                    ((a.type = e),
                    n.executeScripts && (!V() || "nomodule" !== e))
                  ) {
                    var o = Ue(a.ownerDocument.createElement("script"), a);
                    for (var i in r.attributes) {
                      var s = r.attributes[i];
                      o.setAttribute(i, s);
                    }
                    (o.textContent = a.textContent),
                      G.has(a) && (w(a), G.set(o, t)),
                      X.set(r, o),
                      a.parentNode && a.parentNode.replaceChild(o, a);
                  }
                }),
                s.clear(),
                this.endedCallbacks.forEach(function (t) {
                  return t(e);
                }),
                this.endedCallbacks.clear(),
                o("finalize"),
                o("render"),
                t.oldTree && g(t.oldTree),
                this
              );
            },
          },
          {
            key: "onceEnded",
            value: function (e) {
              this.endedCallbacks.add(e);
            },
          },
        ],
        [
          {
            key: "create",
            value: function (t, n, r) {
              return new e(t, n, r);
            },
          },
          {
            key: "flow",
            value: function (e, t) {
              for (var n = e, r = 0; r < t.length; r++) {
                if (e.aborted) return n;
                if (void 0 !== (n = t[r](e)) && n !== e) return n;
              }
              return n;
            },
          },
          { key: "assert", value: function (e) {} },
          {
            key: "invokeMiddleware",
            value: function (e) {
              var t = e.state.measure,
                n = e.tasks;
              q.forEach(function (r) {
                var a = "invoke ".concat(r.name || "anon");
                t(a);
                var o = r(e);
                o && n.push(o), t(a);
              });
            },
          },
        ]
      ),
      e
    );
  })(),
  Ge = {
    StateCache: G,
    NodeCache: X,
    TransitionCache: J,
    MiddlewareCache: q,
    CreateTreeHookCache: Y,
    CreateNodeHookCache: Z,
    SyncTreeHookCache: $,
    ReleaseHookCache: K,
    ParseHookCache: Q,
  },
  Xe = n(
    {
      decodeEntities: x,
      escape: j,
      makeMeasure: T,
      memory: Ee,
      Pool: ue,
      process: W,
      PATCH_TYPE: F,
      globalConfig: re,
      createNode: k,
      syncTree: O,
      Transaction: Je,
      defaultTasks: Fe,
      tasks: ze,
    },
    Ge
  ),
  qe = Object.assign,
  Ye = "".concat("1.0.0-beta.29", "-lite");
qe(Xe, { VERSION: Ye });
var $e = ee;
if (De in ee) {
  var Ke = $e[De];
  Ye !== Ke.VERSION &&
    console.log("Loaded ".concat(Ye, " after ").concat(Ke.VERSION));
}
$e.devTools && ($e.unsubscribeDevTools = H($e.devTools(Xe)));

export const Internals = Xe;
export const VERSION = Ye;
export const addTransitionState = C;
export const createTree = m;
export const html = m;
export const innerHTML = _;
export const outerHTML = P;
export const release = w;
export const removeTransitionState = M;
export const use = H;

export default {
  VERSION: Ye,
  addTransitionState: C,
  removeTransitionState: M,
  release: w,
  createTree: m,
  use: H,
  outerHTML: P,
  innerHTML: _,
  html: m,
  Internals: Xe,
};
