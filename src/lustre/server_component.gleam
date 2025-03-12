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
    "var c=class{withFields(e){let r=Object.keys(this).map(n=>n in e?e[n]:this[n]);return new this.constructor(...r)}};var P=class{bitSize;byteSize;bitOffset;rawBuffer;constructor(e,r,n){if(!(e instanceof Uint8Array))throw globalThis.Error(\"BitArray can only be constructed from a Uint8Array\");if(this.bitSize=r??e.length*8,this.byteSize=Math.trunc((this.bitSize+7)/8),this.bitOffset=n??0,this.bitSize<0)throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);if(this.bitOffset<0||this.bitOffset>7)throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);if(e.length!==Math.trunc((this.bitOffset+this.bitSize+7)/8))throw globalThis.Error(\"BitArray buffer length is invalid\");this.rawBuffer=e}byteAt(e){if(!(e<0||e>=this.byteSize))return D(this.rawBuffer,this.bitOffset,e)}equals(e){if(this.bitSize!==e.bitSize)return!1;let r=Math.trunc(this.bitSize/8);if(this.bitOffset===0&&e.bitOffset===0){for(let i=0;i<r;i++)if(this.rawBuffer[i]!==e.rawBuffer[i])return!1;let n=this.bitSize%8;if(n){let i=8-n;if(this.rawBuffer[r]>>i!==e.rawBuffer[r]>>i)return!1}}else{for(let i=0;i<r;i++){let s=D(this.rawBuffer,this.bitOffset,i),u=D(e.rawBuffer,e.bitOffset,i);if(s!==u)return!1}let n=this.bitSize%8;if(n){let i=D(this.rawBuffer,this.bitOffset,r),s=D(e.rawBuffer,e.bitOffset,r),u=8-n;if(i>>u!==s>>u)return!1}}return!0}get buffer(){if(ze(\"buffer\",\"Use BitArray.byteAt() or BitArray.rawBuffer instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.buffer does not support unaligned bit arrays\");return this.rawBuffer}get length(){if(ze(\"length\",\"Use BitArray.bitSize or BitArray.byteSize instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.length does not support unaligned bit arrays\");return this.rawBuffer.length}};function D(t,e,r){if(e===0)return t[r]??0;{let n=t[r]<<e&255,i=t[r+1]>>8-e;return n|i}}var Ie={};function ze(t,e){Ie[t]||(console.warn(`Deprecated BitArray.${t} property used in JavaScript FFI code. ${e}.`),Ie[t]=!0)}function p(t,e){let r=[t,e];for(;r.length;){let n=r.pop(),i=r.pop();if(n===i)continue;if(!Me(n)||!Me(i)||!Ft(n,i)||Mt(n,i)||Ut(n,i)||qt(n,i)||Dt(n,i)||Pt(n,i)||Rt(n,i))return!1;let u=Object.getPrototypeOf(n);if(u!==null&&typeof u.equals==\"function\")try{if(n.equals(i))continue;return!1}catch{}let[o,a]=zt(n);for(let l of o(n))r.push(a(n,l),a(i,l))}return!0}function zt(t){if(t instanceof Map)return[e=>e.keys(),(e,r)=>e.get(r)];{let e=t instanceof globalThis.Error?[\"message\"]:[];return[r=>[...e,...Object.keys(r)],(r,n)=>r[n]]}}function Mt(t,e){return t instanceof Date&&(t>e||t<e)}function Ut(t,e){return!(t instanceof P)&&t.buffer instanceof ArrayBuffer&&t.BYTES_PER_ELEMENT&&!(t.byteLength===e.byteLength&&t.every((r,n)=>r===e[n]))}function qt(t,e){return Array.isArray(t)&&t.length!==e.length}function Dt(t,e){return t instanceof Map&&t.size!==e.size}function Pt(t,e){return t instanceof Set&&(t.size!=e.size||[...t].some(r=>!e.has(r)))}function Rt(t,e){return t instanceof RegExp&&(t.source!==e.source||t.flags!==e.flags)}function Me(t){return typeof t==\"object\"&&t!==null}function Ft(t,e){return typeof t!=\"object\"&&typeof e!=\"object\"&&(!t||!e)||[Promise,WeakSet,WeakMap,Function].some(n=>t instanceof n)?!1:t.constructor===e.constructor}var E=class extends c{},O=class extends c{},j=class extends c{};var Ge=new WeakMap,de=new DataView(new ArrayBuffer(8)),he=0;function _e(t){let e=Ge.get(t);if(e!==void 0)return e;let r=he++;return he===2147483647&&(he=0),Ge.set(t,r),r}function me(t,e){return t^e+2654435769+(t<<6)+(t>>2)|0}function xe(t){let e=0,r=t.length;for(let n=0;n<r;n++)e=Math.imul(31,e)+t.charCodeAt(n)|0;return e}function Je(t){de.setFloat64(0,t);let e=de.getInt32(0),r=de.getInt32(4);return Math.imul(73244475,e>>16^e)^r}function or(t){return xe(t.toString())}function lr(t){let e=Object.getPrototypeOf(t);if(e!==null&&typeof e.hashCode==\"function\")try{let n=t.hashCode(t);if(typeof n==\"number\")return n}catch{}if(t instanceof Promise||t instanceof WeakSet||t instanceof WeakMap)return _e(t);if(t instanceof Date)return Je(t.getTime());let r=0;if(t instanceof ArrayBuffer&&(t=new Uint8Array(t)),Array.isArray(t)||t instanceof Uint8Array)for(let n=0;n<t.length;n++)r=Math.imul(31,r)+g(t[n])|0;else if(t instanceof Set)t.forEach(n=>{r=r+g(n)|0});else if(t instanceof Map)t.forEach((n,i)=>{r=r+me(g(n),g(i))|0});else{let n=Object.keys(t);for(let i=0;i<n.length;i++){let s=n[i],u=t[s];r=r+me(g(u),xe(s))|0}}return r}function g(t){if(t===null)return 1108378658;if(t===void 0)return 1108378659;if(t===!0)return 1108378657;if(t===!1)return 1108378656;switch(typeof t){case\"number\":return Je(t);case\"string\":return xe(t);case\"bigint\":return or(t);case\"object\":return lr(t);case\"symbol\":return _e(t);case\"function\":return _e(t);default:return 0}}var S=5,we=Math.pow(2,S),ar=we-1,cr=we/2,fr=we/4,h=0,v=1,$=2,z=3,ge={type:$,bitmap:0,array:[]};function J(t,e){return t>>>e&ar}function se(t,e){return 1<<J(t,e)}function pr(t){return t-=t>>1&1431655765,t=(t&858993459)+(t>>2&858993459),t=t+(t>>4)&252645135,t+=t>>8,t+=t>>16,t&127}function ye(t,e){return pr(t&e-1)}function y(t,e,r){let n=t.length,i=new Array(n);for(let s=0;s<n;++s)i[s]=t[s];return i[e]=r,i}function dr(t,e,r){let n=t.length,i=new Array(n+1),s=0,u=0;for(;s<e;)i[u++]=t[s++];for(i[u++]=r;s<n;)i[u++]=t[s++];return i}function $e(t,e){let r=t.length,n=new Array(r-1),i=0,s=0;for(;i<e;)n[s++]=t[i++];for(++i;i<r;)n[s++]=t[i++];return n}function Ve(t,e,r,n,i,s){let u=g(e);if(u===n)return{type:z,hash:u,array:[{type:h,k:e,v:r},{type:h,k:i,v:s}]};let o={val:!1};return V(be(ge,t,u,e,r,o),t,n,i,s,o)}function V(t,e,r,n,i,s){switch(t.type){case v:return hr(t,e,r,n,i,s);case $:return be(t,e,r,n,i,s);case z:return _r(t,e,r,n,i,s)}}function hr(t,e,r,n,i,s){let u=J(r,e),o=t.array[u];if(o===void 0)return s.val=!0,{type:v,size:t.size+1,array:y(t.array,u,{type:h,k:n,v:i})};if(o.type===h)return p(n,o.k)?i===o.v?t:{type:v,size:t.size,array:y(t.array,u,{type:h,k:n,v:i})}:(s.val=!0,{type:v,size:t.size,array:y(t.array,u,Ve(e+S,o.k,o.v,r,n,i))});let a=V(o,e+S,r,n,i,s);return a===o?t:{type:v,size:t.size,array:y(t.array,u,a)}}function be(t,e,r,n,i,s){let u=se(r,e),o=ye(t.bitmap,u);if(t.bitmap&u){let a=t.array[o];if(a.type!==h){let b=V(a,e+S,r,n,i,s);return b===a?t:{type:$,bitmap:t.bitmap,array:y(t.array,o,b)}}let l=a.k;return p(n,l)?i===a.v?t:{type:$,bitmap:t.bitmap,array:y(t.array,o,{type:h,k:n,v:i})}:(s.val=!0,{type:$,bitmap:t.bitmap,array:y(t.array,o,Ve(e+S,l,a.v,r,n,i))})}else{let a=t.array.length;if(a>=cr){let l=new Array(32),b=J(r,e);l[b]=be(ge,e+S,r,n,i,s);let A=0,k=t.bitmap;for(let ae=0;ae<32;ae++){if(k&1){let It=t.array[A++];l[ae]=It}k=k>>>1}return{type:v,size:a+1,array:l}}else{let l=dr(t.array,o,{type:h,k:n,v:i});return s.val=!0,{type:$,bitmap:t.bitmap|u,array:l}}}}function _r(t,e,r,n,i,s){if(r===t.hash){let u=ke(t,n);if(u!==-1)return t.array[u].v===i?t:{type:z,hash:r,array:y(t.array,u,{type:h,k:n,v:i})};let o=t.array.length;return s.val=!0,{type:z,hash:r,array:y(t.array,o,{type:h,k:n,v:i})}}return V({type:$,bitmap:se(t.hash,e),array:[t]},e,r,n,i,s)}function ke(t,e){let r=t.array.length;for(let n=0;n<r;n++)if(p(e,t.array[n].k))return n;return-1}function ie(t,e,r,n){switch(t.type){case v:return mr(t,e,r,n);case $:return $r(t,e,r,n);case z:return xr(t,n)}}function mr(t,e,r,n){let i=J(r,e),s=t.array[i];if(s!==void 0){if(s.type!==h)return ie(s,e+S,r,n);if(p(n,s.k))return s}}function $r(t,e,r,n){let i=se(r,e);if(!(t.bitmap&i))return;let s=ye(t.bitmap,i),u=t.array[s];if(u.type!==h)return ie(u,e+S,r,n);if(p(n,u.k))return u}function xr(t,e){let r=ke(t,e);if(!(r<0))return t.array[r]}function Le(t,e,r,n){switch(t.type){case v:return wr(t,e,r,n);case $:return gr(t,e,r,n);case z:return yr(t,n)}}function wr(t,e,r,n){let i=J(r,e),s=t.array[i];if(s===void 0)return t;let u;if(s.type===h){if(!p(s.k,n))return t}else if(u=Le(s,e+S,r,n),u===s)return t;if(u===void 0){if(t.size<=fr){let o=t.array,a=new Array(t.size-1),l=0,b=0,A=0;for(;l<i;){let k=o[l];k!==void 0&&(a[b]=k,A|=1<<l,++b),++l}for(++l;l<o.length;){let k=o[l];k!==void 0&&(a[b]=k,A|=1<<l,++b),++l}return{type:$,bitmap:A,array:a}}return{type:v,size:t.size-1,array:y(t.array,i,u)}}return{type:v,size:t.size,array:y(t.array,i,u)}}function gr(t,e,r,n){let i=se(r,e);if(!(t.bitmap&i))return t;let s=ye(t.bitmap,i),u=t.array[s];if(u.type!==h){let o=Le(u,e+S,r,n);return o===u?t:o!==void 0?{type:$,bitmap:t.bitmap,array:y(t.array,s,o)}:t.bitmap===i?void 0:{type:$,bitmap:t.bitmap^i,array:$e(t.array,s)}}return p(n,u.k)?t.bitmap===i?void 0:{type:$,bitmap:t.bitmap^i,array:$e(t.array,s)}:t}function yr(t,e){let r=ke(t,e);if(r<0)return t;if(t.array.length!==1)return{type:z,hash:t.hash,array:$e(t.array,r)}}function He(t,e){if(t===void 0)return;let r=t.array,n=r.length;for(let i=0;i<n;i++){let s=r[i];if(s!==void 0){if(s.type===h){e(s.v,s.k);continue}He(s,e)}}}var N=class t{static fromObject(e){let r=Object.keys(e),n=t.new();for(let i=0;i<r.length;i++){let s=r[i];n=n.set(s,e[s])}return n}static fromMap(e){let r=t.new();return e.forEach((n,i)=>{r=r.set(i,n)}),r}static new(){return new t(void 0,0)}constructor(e,r){this.root=e,this.size=r}get(e,r){if(this.root===void 0)return r;let n=ie(this.root,0,g(e),e);return n===void 0?r:n.v}set(e,r){let n={val:!1},i=this.root===void 0?ge:this.root,s=V(i,0,g(e),e,r,n);return s===this.root?this:new t(s,n.val?this.size+1:this.size)}delete(e){if(this.root===void 0)return this;let r=Le(this.root,0,g(e),e);return r===this.root?this:r===void 0?t.new():new t(r,this.size-1)}has(e){return this.root===void 0?!1:ie(this.root,0,g(e),e)!==void 0}entries(){if(this.root===void 0)return[];let e=[];return this.forEach((r,n)=>e.push([n,r])),e}forEach(e){He(this.root,e)}hashCode(){let e=0;return this.forEach((r,n)=>{e=e+me(g(r),g(n))|0}),e}equals(e){if(!(e instanceof t)||this.size!==e.size)return!1;try{return this.forEach((r,n)=>{if(!p(e.get(n,!r),r))throw We}),!0}catch(r){if(r===We)return!1;throw r}}},We=Symbol();var Ke=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),si=new RegExp(`^[${Ke}]*`),ui=new RegExp(`[${Ke}]*$`);function R(){return N.new()}var je=class extends c{constructor(e){super(),this.dict=e}};function tt(){return new je(R())}var lu=N.new();var au=tt();var ku=new j,Lu=new E,Ou=new O;var nt=0;var it=1;var st=2;var ut=0;var ot=1;var lt=2;var at=3;var ct=0;var ft=1;var pt=2;var dt=3;var ht=4;var _t=5;var mt=6;var $t=7;var Z=class{#t=null;#e=()=>{};#r=!1;constructor(e,r,{useServerEvents:n=!1}={}){this.#t=e,this.#e=r,this.#r=n}mount(e){this.#t.appendChild(this.#l(e))}#n=[];push(e){this.#n.push({node:this.#t,patch:e}),this.#i()}#i(){for(;this.#n.length;){let{node:e,patch:r}=this.#n.pop();T(r.changes,n=>{switch(n.kind){case mt:this.#u(e,n.children,n.before);break;case dt:this.#a(e,n.key,n.before,n.count);break;case ht:this.#o(e,n.key,n.count);break;case $t:this.#s(e,n.from,n.count);break;case _t:this.#d(e,n.from,n.count,n.with);break;case ct:this.#h(e,n.content);break;case ft:this.#p(e,n.inner_html);break;case pt:this.#_(e,n.added,n.removed);break}}),this.#s(e,e.childNodes.length-r.removed,r.removed),T(r.children,n=>{this.#n.push({node:e.childNodes[n.index],patch:n})})}}#u(e,r,n){let i=document.createDocumentFragment();T(r,s=>{let u=this.#l(s);Be(e,u),i.appendChild(u)}),e.insertBefore(i,e.childNodes[n]??null)}#a(e,r,n,i){let s=e[_].keyedChildren.get(r).deref();if(i>1){let u=document.createDocumentFragment();for(let o=0;o<i&&s!==null;++o){let a=s.nextSibling;u.append(s),s=a}s=u}e.insertBefore(s,e.childNodes[n]??null)}#o(e,r,n){this.#f(e,e[_].keyedChildren.get(r).deref(),n)}#s(e,r,n){this.#f(e,e.childNodes[r],n)}#f(e,r,n){for(;n-- >0&&r!==null;){let i=r.nextSibling,s=r[_].key;s&&e[_].keyedChildren.delete(s),e.removeChild(r),r=i}}#d(e,r,n,i){this.#s(e,r,n);let s=this.#l(i);Be(e,s),e.insertBefore(s,e.childNodes[r]??null)}#h(e,r){e.data=r}#p(e,r){e.innerHTML=r}#_(e,r,n){T(n,i=>{let s=i.name;e[_].handlers.has(s)?(e.removeEventListener(s,wt),e[_].handlers.delete(s)):(e.removeAttribute(s),gt[s]?.removed?.(e,s))}),T(r,i=>{this.#c(e,i)})}#l(e){switch(e.kind){case ot:{let r=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return Q(r,e.key),T(e.attributes,n=>{this.#c(r,n)}),this.#u(r,e.children,0),r}case lt:{let r=document.createTextNode(e.content);return Q(r,e.key),r}case ut:{let r=document.createDocumentFragment(),n=document.createTextNode(\"\");return Q(n,e.key,!0),r.appendChild(n),T(e.children,i=>{r.appendChild(this.#l(i))}),r}case at:{let r=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return Q(r,e.key),T(e.attributes,n=>{this.#c(r,n)}),this.#p(r,e.inner_html),r}}}#c(e,r){switch(r.kind){case nt:{let n=r.name,i=r.value;i!==e.getAttribute(n)&&e.setAttribute(n,i),gt[n]?.added?.(e,i);break}case it:e[r.name]=r.value;break;case st:{e[_].handlers.has(r.name)||e.addEventListener(r.name,wt,{passive:!r.prevent_default});let n=r.prevent_default,i=r.stop_propagation,s=r.immediate||Pr.includes(r.name),u=Array.isArray(r.include)?r.include:[];e[_].handlers.set(r.name,o=>{n&&o.preventDefault(),i&&o.stopPropagation();let a=[],l=o.currentTarget;for(;l!==this.#t;){let A=l[_].key;if(A)a.push(A);else{let k=[].indexOf.call(l.parentNode.childNodes,l);a.push(k.toString())}l=l.parentNode}a.reverse();let b=this.#r?qr(o,u):o;this.#e(b,a,o.type,s)});break}}}};function T(t,e){if(Array.isArray(t))for(let r=0;r<t.length;r++)e(t[r]);else for(t;t.tail;t=t.tail)e(t.head)}var _=Symbol(\"metadata\");function Q(t,e=\"\",r=!1){if(t.nodeType===Node.ELEMENT_NODE||r){t[_]={key:e,keyedChildren:new Map,handlers:new Map};return}t[_]={key:e}}function Be(t,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)Be(t,e);return}let r=e[_].key;r&&t[_].keyedChildren.set(r,new WeakRef(e))}function wt(t){t.currentTarget[_].handlers.get(t.type)(t)}function qr(t,e=[]){let r={};(t.type===\"input\"||t.type===\"change\")&&e.push(\"target.value\");for(let n of e){let i=n.split(\".\");for(let s=0,u=t,o=r;s<i.length;s++){if(s===i.length-1){o[i[s]]=u[i[s]];break}o=o[i[s]]??={},u=u[i[s]]}}return r}var gt={checked:yt(\"checked\"),selected:yt(\"selected\"),value:Dr(\"value\"),autofocus:{added(t){t.focus?.()}},autoplay:{added(t){try{t.play?.()}catch(e){console.error(e)}}}};function yt(t){return{added(e,r){e[t]=!0},removed(e){e[t]=!1}}}function Dr(t){return{added(e,r){e[t]=r}}}var Pr=[\"input\",\"change\",\"focusin\",\"focusout\",\"focus\",\"blur\",\"select\"];var St=new WeakMap;async function Et(t){let e=[];for(let n of document.querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((i,s)=>{n.addEventListener(\"load\",i),n.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!t.host.isConnected)return[];t.adoptedStyleSheets=t.host.getRootNode().adoptedStyleSheets;let r=[];for(let n of document.styleSheets)try{t.adoptedStyleSheets.push(n)}catch{try{let i=St.get(n);if(!i){i=new CSSStyleSheet;for(let s of n.cssRules)i.insertRule(s.cssText,i.cssRules.length);St.set(n,i)}t.adoptedStyleSheets.push(i)}catch{let i=n.ownerNode.cloneNode();t.prepend(i),r.push(i)}}return r}var Bt=0;var At=1;var Ct=2;var Nt=0;var Tt=1;var Ae=class{#t;#e;constructor(e,r,{}){this.#t=e,this.#e=new WebSocket(this.#t),this.#e.onmessage=({data:n})=>{try{r(JSON.parse(n))}catch{}}}send(e){this.#e.send(JSON.stringify(e))}close(){this.#e.close()}},Ce=class{#t;#e;constructor(e,r,{}){this.#t=e,this.#e=new EventSource(e),this.#e.onmessage=({data:n})=>{try{r(JSON.parse(n))}catch{}}}send(e){}close(){this.#e.close()}},Ne=class{#t;#e;#r;#n;constructor(e,r,n={}){this.#t=e,this.#e=r,this.#r=n.interval??5e3,this.#i().finally(()=>{this.#n=window.setInterval(()=>this.#i(),this.#r)})}async send(e){}close(){clearInterval(this.#n)}#i(){return fetch(this.#t).then(e=>e.json()).then(this.#e).catch(console.error)}},Te=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u;#a=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new Z(this.shadowRoot,(e,r,n)=>{this.#r?.send({kind:Tt,path:r,name:n,event:e})},{useServerEvents:!0}),this.#u=new MutationObserver(e=>{let r=[];for(let n of e){if(n.type!==\"attributes\")continue;let i=n.attributeName;this.#a.includes(i)&&r.push([i,this.getAttribute(i)])}r.length&&this.#r?.send({kind:Nt,attributes:r})})}connectedCallback(){this.#s(),this.#u.observe(this,{attributes:!0}),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#o())}adoptedCallback(){this.#s()}attributeChangedCallback(e,r,n){switch(e){case r!==n:{this.#e=new URL(n,window.location.href),this.#o();return}case\"method\":{let i=n.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\",\"http\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#o());return}}}eventReceivedCallback(e,r,n){this.#r?.send(\"hi!\")}messageReceivedCallback(e){switch(console.log(e),e.kind){case Bt:{for(;this.shadowRoot.children[this.#n.length];)this.shadowRoot.children[this.#n.length].remove();this.#i.mount(e.vdom);break}case At:{this.#i.push(e.patch);break}case Ct:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#o(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let e=r=>{this.messageReceivedCallback(r)};switch(this.#t){case\"ws\":this.#r=new Ae(this.#e,e,{});break;case\"sse\":this.#r=new Ce(this.#e,e,{});break;case\"polling\":this.#r=new Ne(this.#e,e,{});break}}async#s(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await Et(this.shadowRoot)}};window.customElements.define(\"lustre-server-component\",Te);export{Te as ServerComponent};\\n",
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
