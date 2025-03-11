//// > **Note**: server components are currently only supported on the **Erlang**
//// > target. If it's important to you that they work on the JavaScript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (<20kb!) client runtime that patches the DOM and events are sent back to the
//// server component in real-time.
////
//// ```text
//// -- SERVER -----------------------------------------------------------------
////
////                  Msg                            Element(Msg)
//// +--------+        v        +----------------+        v        +------+
//// |        | <-------------- |                | <-------------- |      |
//// | update |                 | Lustre runtime |                 | view |
//// |        | --------------> |                | --------------> |      |
//// +--------+        ^        +----------------+        ^        +------+
////         #(model, Effect(msg))  |        ^          Model
////                                |        |
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                        +-----------------------+
////                        |                       |
////                        | Your WebSocket server |
////                        |                       |
////                        +-----------------------+
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
//// -- BROWSER ----------------------------------------------------------------
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                            +----------------+
////                            |                |
////                            | Client runtime |
////                            |                |
////                            +----------------+
//// ```
////
//// **Note**: Lustre's server component runtime is separate from your application's
//// WebSocket server. You're free to bring your own stack, connect multiple
//// clients to the same Lustre instance, or keep the application alive even when
//// no clients are connected.
////
//// Lustre server components run next to the rest of your backend code, your
//// services, your database, etc. Real-time applications like chat services, games,
//// or components that can benefit from direct access to your backend services
//// like an admin dashboard or data table are excellent candidates for server
//// components.
////
//// ## Examples
////
//// Server components are a new feature in Lustre and we're still working on the
//// best ways to use them and show them off. For now, you can find a simple
//// undocumented example in the `examples/` directory:
////
//// - [`99-server-components`](https://github.com/lustre-labs/lustre/tree/main/examples/99-server-components)
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Selector, type Subject}
import gleam/json.{type Json}
import lustre.{type Error, type Runtime, type RuntimeMessage}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/vdom/attribute.{Event} as _

// TYPES -----------------------------------------------------------------------

/// A type representing the messages sent to the server component _client_
/// runtime. This instruct the client runtime to do things like update the DOM
/// or emit an event from the element.
///
pub type ClientMessage(msg) =
  transport.ClientMessage(msg)

// ELEMENTS --------------------------------------------------------------------

/// Render the Lustre Server Component client runtime. The content of your server
/// component will be rendered inside this element.
///
/// **Note**: you must include the `lustre-server-component.mjs` script found in
/// the `priv/` directory of the Lustre package in your project's HTML or using
/// the [`script`](#script) function.
///
pub fn element(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("lustre-server-component", attrs, children)
}

/// Inline the Lustre Server Component client runtime as a script tag.
///
pub fn script() -> Element(msg) {
  html.script(
    [attribute.type_("module")],
    // <<INJECT RUNTIME>>
    "var c=class{withFields(t){let n=Object.keys(this).map(r=>r in t?t[r]:this[r]);return new this.constructor(...n)}},M=class{static fromArray(t,n){let r=n||new z;for(let i=t.length-1;i>=0;--i)r=new U(t[i],r);return r}[Symbol.iterator](){return new he(this)}toArray(){return[...this]}atLeastLength(t){let n=this;for(;t-- >0&&n;)n=n.tail;return n!==void 0}hasLength(t){let n=this;for(;t-- >0&&n;)n=n.tail;return t===-1&&n instanceof z}countLength(){let t=this,n=0;for(;t;)t=t.tail,n++;return n-1}};var he=class{#t;constructor(t){this.#t=t}next(){if(this.#t instanceof z)return{done:!0};{let{head:t,tail:n}=this.#t;return this.#t=n,{value:t,done:!1}}}},z=class extends M{},U=class extends M{constructor(t,n){super(),this.head=t,this.tail=n}},P=class{bitSize;byteSize;bitOffset;rawBuffer;constructor(t,n,r){if(!(t instanceof Uint8Array))throw globalThis.Error(\"BitArray can only be constructed from a Uint8Array\");if(this.bitSize=n??t.length*8,this.byteSize=Math.trunc((this.bitSize+7)/8),this.bitOffset=r??0,this.bitSize<0)throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);if(this.bitOffset<0||this.bitOffset>7)throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);if(t.length!==Math.trunc((this.bitOffset+this.bitSize+7)/8))throw globalThis.Error(\"BitArray buffer length is invalid\");this.rawBuffer=t}byteAt(t){if(!(t<0||t>=this.byteSize))return F(this.rawBuffer,this.bitOffset,t)}equals(t){if(this.bitSize!==t.bitSize)return!1;let n=Math.trunc(this.bitSize/8);if(this.bitOffset===0&&t.bitOffset===0){for(let i=0;i<n;i++)if(this.rawBuffer[i]!==t.rawBuffer[i])return!1;let r=this.bitSize%8;if(r){let i=8-r;if(this.rawBuffer[n]>>i!==t.rawBuffer[n]>>i)return!1}}else{for(let i=0;i<n;i++){let s=F(this.rawBuffer,this.bitOffset,i),u=F(t.rawBuffer,t.bitOffset,i);if(s!==u)return!1}let r=this.bitSize%8;if(r){let i=F(this.rawBuffer,this.bitOffset,n),s=F(t.rawBuffer,t.bitOffset,n),u=8-r;if(i>>u!==s>>u)return!1}}return!0}get buffer(){if(Xe(\"buffer\",\"Use BitArray.byteAt() or BitArray.rawBuffer instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.buffer does not support unaligned bit arrays\");return this.rawBuffer}get length(){if(Xe(\"length\",\"Use BitArray.bitSize or BitArray.byteSize instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.length does not support unaligned bit arrays\");return this.rawBuffer.length}};function F(e,t,n){if(t===0)return e[n]??0;{let r=e[n]<<t&255,i=e[n+1]>>8-t;return r|i}}var Ye={};function Xe(e,t){Ye[e]||(console.warn(`Deprecated BitArray.${e} property used in JavaScript FFI code. ${t}.`),Ye[e]=!0)}function p(e,t){let n=[e,t];for(;n.length;){let r=n.pop(),i=n.pop();if(r===i)continue;if(!Qe(r)||!Qe(i)||!zr(r,i)||Cr(r,i)||Ar(r,i)||Nr(r,i)||Tr(r,i)||Ir(r,i)||Mr(r,i))return!1;let u=Object.getPrototypeOf(r);if(u!==null&&typeof u.equals==\"function\")try{if(r.equals(i))continue;return!1}catch{}let[o,l]=Br(r);for(let a of o(r))n.push(l(r,a),l(i,a))}return!0}function Br(e){if(e instanceof Map)return[t=>t.keys(),(t,n)=>t.get(n)];{let t=e instanceof globalThis.Error?[\"message\"]:[];return[n=>[...t,...Object.keys(n)],(n,r)=>n[r]]}}function Cr(e,t){return e instanceof Date&&(e>t||e<t)}function Ar(e,t){return!(e instanceof P)&&e.buffer instanceof ArrayBuffer&&e.BYTES_PER_ELEMENT&&!(e.byteLength===t.byteLength&&e.every((n,r)=>n===t[r]))}function Nr(e,t){return Array.isArray(e)&&e.length!==t.length}function Tr(e,t){return e instanceof Map&&e.size!==t.size}function Ir(e,t){return e instanceof Set&&(e.size!=t.size||[...e].some(n=>!t.has(n)))}function Mr(e,t){return e instanceof RegExp&&(e.source!==t.source||e.flags!==t.flags)}function Qe(e){return typeof e==\"object\"&&e!==null}function zr(e,t){return typeof e!=\"object\"&&typeof t!=\"object\"&&(!e||!t)||[Promise,WeakSet,WeakMap,Function].some(r=>e instanceof r)?!1:e.constructor===t.constructor}var S=class extends c{},O=class extends c{},v=class extends c{};var ot=new WeakMap,me=new DataView(new ArrayBuffer(8)),xe=0;function $e(e){let t=ot.get(e);if(t!==void 0)return t;let n=xe++;return xe===2147483647&&(xe=0),ot.set(e,n),n}function we(e,t){return e^t+2654435769+(e<<6)+(e>>2)|0}function ye(e){let t=0,n=e.length;for(let r=0;r<n;r++)t=Math.imul(31,t)+e.charCodeAt(r)|0;return t}function at(e){me.setFloat64(0,e);let t=me.getInt32(0),n=me.getInt32(4);return Math.imul(73244475,t>>16^t)^n}function tn(e){return ye(e.toString())}function rn(e){let t=Object.getPrototypeOf(e);if(t!==null&&typeof t.hashCode==\"function\")try{let r=e.hashCode(e);if(typeof r==\"number\")return r}catch{}if(e instanceof Promise||e instanceof WeakSet||e instanceof WeakMap)return $e(e);if(e instanceof Date)return at(e.getTime());let n=0;if(e instanceof ArrayBuffer&&(e=new Uint8Array(e)),Array.isArray(e)||e instanceof Uint8Array)for(let r=0;r<e.length;r++)n=Math.imul(31,n)+y(e[r])|0;else if(e instanceof Set)e.forEach(r=>{n=n+y(r)|0});else if(e instanceof Map)e.forEach((r,i)=>{n=n+we(y(r),y(i))|0});else{let r=Object.keys(e);for(let i=0;i<r.length;i++){let s=r[i],u=e[s];n=n+we(y(u),ye(s))|0}}return n}function y(e){if(e===null)return 1108378658;if(e===void 0)return 1108378659;if(e===!0)return 1108378657;if(e===!1)return 1108378656;switch(typeof e){case\"number\":return at(e);case\"string\":return ye(e);case\"bigint\":return tn(e);case\"object\":return rn(e);case\"symbol\":return $e(e);case\"function\":return $e(e);default:return 0}}var E=5,be=Math.pow(2,E),nn=be-1,sn=be/2,un=be/4,m=0,j=1,$=2,T=3,ke={type:$,bitmap:0,array:[]};function V(e,t){return e>>>t&nn}function ie(e,t){return 1<<V(e,t)}function on(e){return e-=e>>1&1431655765,e=(e&858993459)+(e>>2&858993459),e=e+(e>>4)&252645135,e+=e>>8,e+=e>>16,e&127}function Le(e,t){return on(e&t-1)}function b(e,t,n){let r=e.length,i=new Array(r);for(let s=0;s<r;++s)i[s]=e[s];return i[t]=n,i}function ln(e,t,n){let r=e.length,i=new Array(r+1),s=0,u=0;for(;s<t;)i[u++]=e[s++];for(i[u++]=n;s<r;)i[u++]=e[s++];return i}function ge(e,t){let n=e.length,r=new Array(n-1),i=0,s=0;for(;i<t;)r[s++]=e[i++];for(++i;i<n;)r[s++]=e[i++];return r}function ct(e,t,n,r,i,s){let u=y(t);if(u===r)return{type:T,hash:u,array:[{type:m,k:t,v:n},{type:m,k:i,v:s}]};let o={val:!1};return K(Oe(ke,e,u,t,n,o),e,r,i,s,o)}function K(e,t,n,r,i,s){switch(e.type){case j:return an(e,t,n,r,i,s);case $:return Oe(e,t,n,r,i,s);case T:return cn(e,t,n,r,i,s)}}function an(e,t,n,r,i,s){let u=V(n,t),o=e.array[u];if(o===void 0)return s.val=!0,{type:j,size:e.size+1,array:b(e.array,u,{type:m,k:r,v:i})};if(o.type===m)return p(r,o.k)?i===o.v?e:{type:j,size:e.size,array:b(e.array,u,{type:m,k:r,v:i})}:(s.val=!0,{type:j,size:e.size,array:b(e.array,u,ct(t+E,o.k,o.v,n,r,i))});let l=K(o,t+E,n,r,i,s);return l===o?e:{type:j,size:e.size,array:b(e.array,u,l)}}function Oe(e,t,n,r,i,s){let u=ie(n,t),o=Le(e.bitmap,u);if(e.bitmap&u){let l=e.array[o];if(l.type!==m){let d=K(l,t+E,n,r,i,s);return d===l?e:{type:$,bitmap:e.bitmap,array:b(e.array,o,d)}}let a=l.k;return p(r,a)?i===l.v?e:{type:$,bitmap:e.bitmap,array:b(e.array,o,{type:m,k:r,v:i})}:(s.val=!0,{type:$,bitmap:e.bitmap,array:b(e.array,o,ct(t+E,a,l.v,n,r,i))})}else{let l=e.array.length;if(l>=sn){let a=new Array(32),d=V(n,t);a[d]=Oe(ke,t+E,n,r,i,s);let C=0,k=e.bitmap;for(let de=0;de<32;de++){if(k&1){let Sr=e.array[C++];a[de]=Sr}k=k>>>1}return{type:j,size:l+1,array:a}}else{let a=ln(e.array,o,{type:m,k:r,v:i});return s.val=!0,{type:$,bitmap:e.bitmap|u,array:a}}}}function cn(e,t,n,r,i,s){if(n===e.hash){let u=ve(e,r);if(u!==-1)return e.array[u].v===i?e:{type:T,hash:n,array:b(e.array,u,{type:m,k:r,v:i})};let o=e.array.length;return s.val=!0,{type:T,hash:n,array:b(e.array,o,{type:m,k:r,v:i})}}return K({type:$,bitmap:ie(e.hash,t),array:[e]},t,n,r,i,s)}function ve(e,t){let n=e.array.length;for(let r=0;r<n;r++)if(p(t,e.array[r].k))return r;return-1}function ne(e,t,n,r){switch(e.type){case j:return fn(e,t,n,r);case $:return pn(e,t,n,r);case T:return dn(e,r)}}function fn(e,t,n,r){let i=V(n,t),s=e.array[i];if(s!==void 0){if(s.type!==m)return ne(s,t+E,n,r);if(p(r,s.k))return s}}function pn(e,t,n,r){let i=ie(n,t);if(!(e.bitmap&i))return;let s=Le(e.bitmap,i),u=e.array[s];if(u.type!==m)return ne(u,t+E,n,r);if(p(r,u.k))return u}function dn(e,t){let n=ve(e,t);if(!(n<0))return e.array[n]}function je(e,t,n,r){switch(e.type){case j:return hn(e,t,n,r);case $:return _n(e,t,n,r);case T:return mn(e,r)}}function hn(e,t,n,r){let i=V(n,t),s=e.array[i];if(s===void 0)return e;let u;if(s.type===m){if(!p(s.k,r))return e}else if(u=je(s,t+E,n,r),u===s)return e;if(u===void 0){if(e.size<=un){let o=e.array,l=new Array(e.size-1),a=0,d=0,C=0;for(;a<i;){let k=o[a];k!==void 0&&(l[d]=k,C|=1<<a,++d),++a}for(++a;a<o.length;){let k=o[a];k!==void 0&&(l[d]=k,C|=1<<a,++d),++a}return{type:$,bitmap:C,array:l}}return{type:j,size:e.size-1,array:b(e.array,i,u)}}return{type:j,size:e.size,array:b(e.array,i,u)}}function _n(e,t,n,r){let i=ie(n,t);if(!(e.bitmap&i))return e;let s=Le(e.bitmap,i),u=e.array[s];if(u.type!==m){let o=je(u,t+E,n,r);return o===u?e:o!==void 0?{type:$,bitmap:e.bitmap,array:b(e.array,s,o)}:e.bitmap===i?void 0:{type:$,bitmap:e.bitmap^i,array:ge(e.array,s)}}return p(r,u.k)?e.bitmap===i?void 0:{type:$,bitmap:e.bitmap^i,array:ge(e.array,s)}:e}function mn(e,t){let n=ve(e,t);if(n<0)return e;if(e.array.length!==1)return{type:T,hash:e.hash,array:ge(e.array,n)}}function ft(e,t){if(e===void 0)return;let n=e.array,r=n.length;for(let i=0;i<r;i++){let s=n[i];if(s!==void 0){if(s.type===m){t(s.v,s.k);continue}ft(s,t)}}}var N=class e{static fromObject(t){let n=Object.keys(t),r=e.new();for(let i=0;i<n.length;i++){let s=n[i];r=r.set(s,t[s])}return r}static fromMap(t){let n=e.new();return t.forEach((r,i)=>{n=n.set(i,r)}),n}static new(){return new e(void 0,0)}constructor(t,n){this.root=t,this.size=n}get(t,n){if(this.root===void 0)return n;let r=ne(this.root,0,y(t),t);return r===void 0?n:r.v}set(t,n){let r={val:!1},i=this.root===void 0?ke:this.root,s=K(i,0,y(t),t,n,r);return s===this.root?this:new e(s,r.val?this.size+1:this.size)}delete(t){if(this.root===void 0)return this;let n=je(this.root,0,y(t),t);return n===this.root?this:n===void 0?e.new():new e(n,this.size-1)}has(t){return this.root===void 0?!1:ne(this.root,0,y(t),t)!==void 0}entries(){if(this.root===void 0)return[];let t=[];return this.forEach((n,r)=>t.push([r,n])),t}forEach(t){ft(this.root,t)}hashCode(){let t=0;return this.forEach((n,r)=>{t=t+we(y(n),y(r))|0}),t}equals(t){if(!(t instanceof e)||this.size!==t.size)return!1;try{return this.forEach((n,r)=>{if(!p(t.get(r,!n),n))throw lt}),!0}catch(n){if(n===lt)return!1;throw n}}},lt=Symbol();var pt=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),cs=new RegExp(`^[${pt}]*`),fs=new RegExp(`[${pt}]*$`);function W(){return N.new()}var Se=class extends c{constructor(t){super(),this.dict=t}};function $t(){return new Se(W())}var ho=N.new();var _o=$t();var So=new v,Bo=new S,Co=new O;var kt=0,Lt=1,Ot=1,vt=1,jt=2;var Et=1,St=2,Bt=0;var Ct=1;var At=0;var Te=2,Nt=1,R=1,Ie=2,Me=3,ze=4,Tt=5,It=2,Mt=1,Ue=2,De=3,qe=4,zt=5,Ut=3;var Dt=1,qt=2,Rt=0;var Re=1,Ft=2,Pt=1;var Wt=1,Gt=2,Ht=2;var X=1,Jt=2,Fe=3,Vt=4,Kt=5,Yt=0,Xt=1,Pe=2,We=3,Qt=0,Zt=1,er=1;var tr=1,rr=2;var nr=1,ir=3;var sr=1,ur=2;var or=5;var lr=1,ar=2,cr=3,fr=6;var pr=1,dr=2,hr=7,_r=1,mr=2,xr=8;var $r=1,wr=2;var h=Symbol(\"metadata\"),fe=class{#t=null;#e=()=>{};#r=[];constructor(t,n){this.#t=t,this.#e=n}mount(t){this.#t.appendChild(pe(t,this.#e,this.#t))}push(t){this.#r.push({node:this.#t,patch:t}),this.#n()}#n(){for(;this.#r.length;){let{node:t,patch:n}=this.#r.pop();for(let r=0;r<n[Pe].length;r++){let i=n[Pe][r];switch(i[0]){case hr:yr(t,i[_r],i[mr],this.#e,this.#t);break;case or:Jn(t,i[lr],i[ar],i[cr]);break;case fr:Vn(t,i[pr],i[dr]);break;case xr:Kn(t,i[$r],i[wr]);break;case Qt:Yn(t,i[Zt],this.#e,this.#t);break;case er:Xn(t,i[tr]);break;case rr:br(t,i[nr]);break;case ir:Qn(t,i[sr],i[ur],this.#e,this.#t);break}}for(let r=0;r<n[Xt];++r){let i=t.lastChild,s=i[h].key;s&&t[h].keyedChildren.delete(s),t.removeChild(i)}for(let r=0;r<n[We].length;r++){let i=n[We][r];this.#r.push({node:t.childNodes[i[Yt]],patch:i})}}}};function yr(e,t,n,r,i){let s=document.createDocumentFragment();for(let u=0;u<t.length;u++){let o=t[u],l=pe(o,r,i);if(o[R]){let a=new WeakRef(kr(l));e[h].keyedChildren.set(o[R],a)}s.appendChild(l)}e.insertBefore(s,e.childNodes[n]??null)}function Jn(e,t,n,r){let i=e[h].keyedChildren.get(t).deref();if(r>1){let s=document.createDocumentFragment();for(let u=0;u<r&&i!==null;++u){let o=i.nextSibling;s.append(i),i=o}i=s}e.insertBefore(i,e.childNodes[n]??null)}function Vn(e,t,n){let r=e[h].keyedChildren.get(t).deref();for(e[h].keyedChildren.delete(t);n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Kn(e,t,n){let r=e.childNodes[t];for(;n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Yn(e,t,n,r){let i=pe(t,n,r),s=e.parentNode;if(t[R]){let u=new WeakRef(kr(i));s[h].keyedChildren.set(t[R],u)}s.replaceChild(i,e)}function Xn(e,t){e.data=t}function br(e,t){e.innerHTML=t}function Qn(e,t,n,r,i){for(let s=0;s<n.length;s++){let u=n[s][Re];e[h].handlers.has(u)?(e.removeEventListener(u,Lr),e[h].handlers.delete(u)):(e.removeAttribute(u),Or[u]?.removed?.(e,u))}for(let s=0;s<t.length;s++)Ge(e,t[s],r,i)}function kr(e){for(;e.nodeType===DocumentFragment.DOCUMENT_FRAGMENT_NODE;)e=e.firstChild;return e}function pe(e,t,n){switch(e[0]){case Nt:{let r=e[Ie]?document.createElementNS(e[Ie],e[Me]):document.createElement(e[Me]);r[h]={key:e[R],keyedChildren:new Map,handlers:new Map};for(let i=0;i<e[ze].length;i++)Ge(r,e[ze][i],t,n);return yr(r,e[Tt],0,t,n),r}case Ut:{let r=document.createTextNode(e[qt]);return r[h]={key:e[Dt]},r}case At:{let r=document.createDocumentFragment();for(let i=0;i<e[Te].length;i++)r.appendChild(pe(e[Te][i],t,n));return r}case It:{let r=e[Ue]?document.createElementNS(e[Ue],e[De]):document.createElement(e[De]);r[h]={key:e[Mt],handlers:new Map};for(let i=0;i<e[qe].length;i++)Ge(r,e[qe][i],t,n);return br(r,e[zt]),r}}}function Ge(e,t,n,r){switch(t[0]){case Rt:{let i=t[Re],s=t[Ft];s!==e.getAttribute(i)&&e.setAttribute(i,s),Or[i]?.added?.(e,s);break}case Pt:e[t[Wt]]=t[Gt];break;case Ht:{e[h].handlers.has(t[X])||e.addEventListener(t[X],Lr,{passive:!t[Fe]});let i=t[Fe],s=t[Vt],u=t[Kt]||ti.includes(t[X]),o=t[Jt];e[h].handlers.set(t[X],l=>{i&&l.preventDefault(),s&&l.stopPropagation();let a=[];for(let d=l.currentTarget;d!==r;d=d.parentNode){let C=d[h].key;if(C)a.push(C);else{let k=[].indexOf.call(d.parentNode.childNodes,d);a.push(k.toString())}}a.reverse(),n(Zn(l,o),a,l.type,u)});break}}}function Zn(e,t=[]){let n={};(e.type===\"input\"||e.type===\"change\")&&t.push(\"target.value\");for(let r of t){let i=r.split(\".\");for(let s=0,u=e,o=n;s<i.length;s++)s===i.length-1?o[i[s]]=u[i[s]]:(o=o[i[s]]??={},u=u[i[s]])}return n}function Lr(e){e.currentTarget[h].handlers.get(e.type)(e)}var Or={checked:gr(\"checked\"),selected:gr(\"selected\"),value:ei(\"value\"),autofocus:{added(e){e.focus?.()}},autoplay:{added(e){e.play?.()}}};function gr(e){return{added(t,n){t[e]=!0},removed(t){t[e]=!1}}}function ei(e){return{added(t,n){t[e]=n}}}var ti=[\"input\",\"change\",\"focusin\",\"focusout\",\"focus\",\"blur\",\"select\"];var sa=Symbol(\"metadata\");var ua={checked:vr(\"checked\"),selected:vr(\"selected\"),value:ri(\"value\"),autofocus:{added(e){e.focus?.()}},autoplay:{added(e){try{e.play?.()}catch(t){console.error(t)}}}};function vr(e){return{added(t,n){t[e]=!0},removed(t){t[e]=!1}}}function ri(e){return{added(t,n){t[e]=n}}}var jr=new WeakMap;async function Er(e){let t=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||t.push(new Promise((i,s)=>{r.addEventListener(\"load\",i),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(t),!e.host.isConnected)return[];e.adoptedStyleSheets=e.host.getRootNode().adoptedStyleSheets;let n=[];for(let r of document.styleSheets)try{e.adoptedStyleSheets.push(r)}catch{try{let i=jr.get(r);if(!i){i=new CSSStyleSheet;for(let s of r.cssRules)i.insertRule(s.cssText,i.cssRules.length);jr.set(r,i)}e.adoptedStyleSheets.push(i)}catch{let i=r.ownerNode.cloneNode();e.prepend(i),n.push(i)}}return n}var He=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new WebSocket(this.#t),this.#e.onmessage=({data:r})=>{try{n(JSON.parse(r))}catch{}}}send(t){this.#e.send(JSON.stringify(t))}close(){this.#e.close()}},Je=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new EventSource(t),this.#e.onmessage=({data:r})=>{try{n(JSON.parse(r))}catch{}}}send(t){}close(){this.#e.close()}},Ve=class{#t;#e;#r;#n;constructor(t,n,r={}){this.#t=t,this.#e=n,this.#r=r.interval??5e3,this.#i().finally(()=>{this.#n=window.setInterval(()=>this.#i(),this.#r)})}async send(t){}close(){clearInterval(this.#n)}#i(){return fetch(this.#t).then(t=>t.json()).then(this.#e).catch(console.error)}},Ke=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u;#l=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new fe(this.shadowRoot,(t,n,r)=>{this.#r?.send([Ct,n,r,t])}),this.#u=new MutationObserver(t=>{let n=[];for(let r of t){if(r.type!==\"attributes\")continue;let i=r.attributeName;this.#l.includes(i)&&n.push([i,this.getAttribute(i)])}n.length&&this.#r?.send([Bt,n])})}connectedCallback(){this.#o(),this.#u.observe(this,{attributes:!0}),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#s())}adoptedCallback(){this.#o()}attributeChangedCallback(t,n,r){switch(t){case n!==r:{this.#e=new URL(r,window.location.href),this.#s();return}case\"method\":{let i=r.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\",\"http\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#s());return}}}eventReceivedCallback(t,n,r){this.#r?.send(\"hi!\")}messageReceivedCallback(t){switch(t[0]){case kt:{for(;this.shadowRoot.children[this.#n.length];)this.shadowRoot.children[this.#n.length].remove();this.#i.mount(t[Lt]);break}case Ot:{this.#i.push(t[vt]);break}case jt:{this.dispatchEvent(new CustomEvent(t[Et],{detail:t[St]}));break}}}#s(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let t=n=>{this.messageReceivedCallback(n)};switch(this.#t){case\"ws\":this.#r=new He(this.#e,t,{});break;case\"sse\":this.#r=new Je(this.#e,t,{});break;case\"polling\":this.#r=new Ve(this.#e,t,{});break}}async#o(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await Er(this.shadowRoot)}};window.customElements.define(\"lustre-server-component\",Ke);export{Ke as ServerComponent};\\n",
  )
}

// ATTRIBUTES ------------------------------------------------------------------

/// The `route` attribute tells the client runtime what route it should use to
/// set up the WebSocket connection to the server. Whenever this attribute is
/// changed (by a clientside Lustre app, for example), the client runtime will
/// destroy the current connection and set up a new one.
///
pub fn route(path: String) -> Attribute(msg) {
  attribute("route", path)
}

/// Ocassionally you may want to attach custom data to an event sent to the server.
/// This could be used to include a hash of the current build to detect if the
/// event was sent from a stale client.
///
/// Your event decoders can access this data by decoding `data` property of the
/// event object.
///
pub fn data(json: Json) -> Attribute(msg) {
  json
  |> json.to_string
  |> attribute("data-lustre-data", _)
}

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to pass them to the server we need to copy them into a new
/// object first.
///
/// This attribute tells Lustre what properties to include. Properties can come
/// from nested objects by using dot notation. For example, you could include the
/// `id` of the target `element` by passing `["target.id"]`.
///
/// ```gleam
/// import gleam/dynamic
/// import gleam/result.{try}
/// import lustre/element.{type Element}
/// import lustre/element/html
/// import lustre/event
/// import lustre/server
///
/// pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {
///   let handler = fn(event) {
///     use target <- try(dynamic.field("target", dynamic.dynamic)(event))
///     use id <- try(dynamic.field("id", dynamic.string)(target))
///
///     Ok(on_click(id))
///   }
///
///   html.button([event.on_click(handler), server.include(["target.id"])], [
///     element.text("Click me!")
///   ])
/// }
/// ```
///
pub fn include(
  event: Attribute(msg),
  properties: List(String),
) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, include: properties)
    _ -> event
  }
}

// ACTIONS ---------------------------------------------------------------------

///
///
pub fn subject(
  runtime: Runtime(msg),
) -> Result(Subject(RuntimeMessage(msg)), Error) {
  do_subject(runtime)
}

@target(erlang)
fn do_subject(
  runtime: Runtime(msg),
) -> Result(Subject(RuntimeMessage(msg)), Error) {
  Ok(coerce(runtime))
}

@target(javascript)
fn do_subject(_: Runtime(msg)) -> Result(Subject(RuntimeMessage(msg)), Error) {
  Error(lustre.NotErlang)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(value: a) -> b

///
///
pub fn register_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_register_subject(runtime, client)
}

@target(erlang)
fn do_register_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  process.send(runtime, runtime.ClientRegisteredSubject(client))
}

@target(javascript)
fn do_register_subject(_, _) -> Nil {
  Nil
}

///
///
pub fn deregister_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_deregister_subject(runtime, client)
}

@target(erlang)
fn do_deregister_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  process.send(runtime, runtime.ClientDeregisteredSubject(client))
}

@target(javascript)
fn do_deregister_subject(_, _) -> Nil {
  Nil
}

///
///
pub fn register_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientRegisteredCallback(callback))
}

///
///
pub fn deregister_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientDeregisteredCallback(callback))
}

// EFFECTS ---------------------------------------------------------------------

/// Instruct any connected clients to emit a DOM event with the given name and
/// data. This lets your server component communicate to frontend the same way
/// any other HTML elements do: you might emit a `"change"` event when some part
/// of the server component's state changes, for example.
///
/// This is a real DOM event and any JavaScript on the page can attach an event
/// listener to the server component element and listen for these events.
///
pub fn emit(event: String, data: Json) -> Effect(msg) {
  effect.event(event, data)
}

/// On the Erlang target, Lustre's server component runtime is an OTP
/// [actor](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html) that can be
/// communicated with using the standard process API and the `Subject` returned
/// when starting the server component.
///
/// Sometimes, you might want to hand a different `Subject` to a process to restrict
/// the type of messages it can send or to distinguish messages from different
/// sources from one another. The `select` effect creates a fresh `Subject` each
/// time it is run. By returning a `Selector` you can teach the Lustre server
/// component runtime how to listen to messages from this `Subject`.
///
/// The `select` effect also gives you the dispatch function passed to `effect.from`.
/// This is useful in case you want to store the provided `Subject` in your model
/// for later use. For example you may subscribe to a pubsub service and later use
/// that same `Subject` to unsubscribe.
///
/// **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s
/// and `Selector`s don't exist, and is the equivalent of returning `effect.none`.
///
pub fn select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  do_select(sel)
}

@target(erlang)
fn do_select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  use dispatch, _, select, _ <- effect.custom
  let self = process.new_subject()
  let selector = sel(dispatch, self)

  select(selector)
}

@target(javascript)
fn do_select(_: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg)) -> Effect(msg) {
  effect.none
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON encoded actions for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
/// Encode a DOM patch as JSON you can send to the client runtime to apply. Whenever
/// the server runtime re-renders, all subscribed clients will receive a patch
/// message they must forward to the client runtime.
///
pub fn runtime_message_decoder() -> Decoder(RuntimeMessage(msg)) {
  decode.map(
    transport.server_message_decoder(),
    runtime.ClientDispatchedMessage,
  )
}

// ENCODERS --------------------------------------------------------------------

pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  transport.client_message_to_json(message)
}
