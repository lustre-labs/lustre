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

pub type TransportMethod {
  WebSocket
  ServerSentEvents
  Polling
}

// ELEMENTS --------------------------------------------------------------------

/// Render the server component custom element. This element acts as the thin
/// client runtime for a server component running remotely.
///
/// **Note**: the server component runtime bundle must be included and sent to
/// the client for this to work correctly. You can do this by including the
/// JavaScript bundle found in Lustre's `priv/static` directory or by inlining
/// the script source directly with the [`script`](#script) element below.
///
pub fn element(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("lustre-server-component", attrs, children)
}

/// Inline the server component client runtime as a `<script>` tag. Where possible
/// you should prefer serving the pre-built client runtime from Lustre's `priv/static`
/// directory, but this inline script can be useful for development or scenarios
/// where you don't control the HTML document.
///
pub fn script() -> Element(msg) {
  html.script(
    [attribute.type_("module")],
    // <<INJECT RUNTIME>>
    "var c=class{withFields(e){let r=Object.keys(this).map(n=>n in e?e[n]:this[n]);return new this.constructor(...r)}};var D=class{bitSize;byteSize;bitOffset;rawBuffer;constructor(e,r,n){if(!(e instanceof Uint8Array))throw globalThis.Error(\"BitArray can only be constructed from a Uint8Array\");if(this.bitSize=r??e.length*8,this.byteSize=Math.trunc((this.bitSize+7)/8),this.bitOffset=n??0,this.bitSize<0)throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);if(this.bitOffset<0||this.bitOffset>7)throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);if(e.length!==Math.trunc((this.bitOffset+this.bitSize+7)/8))throw globalThis.Error(\"BitArray buffer length is invalid\");this.rawBuffer=e}byteAt(e){if(!(e<0||e>=this.byteSize))return q(this.rawBuffer,this.bitOffset,e)}equals(e){if(this.bitSize!==e.bitSize)return!1;let r=Math.trunc(this.bitSize/8);if(this.bitOffset===0&&e.bitOffset===0){for(let i=0;i<r;i++)if(this.rawBuffer[i]!==e.rawBuffer[i])return!1;let n=this.bitSize%8;if(n){let i=8-n;if(this.rawBuffer[r]>>i!==e.rawBuffer[r]>>i)return!1}}else{for(let i=0;i<r;i++){let s=q(this.rawBuffer,this.bitOffset,i),u=q(e.rawBuffer,e.bitOffset,i);if(s!==u)return!1}let n=this.bitSize%8;if(n){let i=q(this.rawBuffer,this.bitOffset,r),s=q(e.rawBuffer,e.bitOffset,r),u=8-n;if(i>>u!==s>>u)return!1}}return!0}get buffer(){if(qe(\"buffer\",\"Use BitArray.byteAt() or BitArray.rawBuffer instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.buffer does not support unaligned bit arrays\");return this.rawBuffer}get length(){if(qe(\"length\",\"Use BitArray.bitSize or BitArray.byteSize instead\"),this.bitOffset!==0||this.bitSize%8!==0)throw new globalThis.Error(\"BitArray.length does not support unaligned bit arrays\");return this.rawBuffer.length}};function q(t,e,r){if(e===0)return t[r]??0;{let n=t[r]<<e&255,i=t[r+1]>>8-e;return n|i}}var Ue={};function qe(t,e){Ue[t]||(console.warn(`Deprecated BitArray.${t} property used in JavaScript FFI code. ${e}.`),Ue[t]=!0)}function p(t,e){let r=[t,e];for(;r.length;){let n=r.pop(),i=r.pop();if(n===i)continue;if(!De(n)||!De(i)||!Vt(n,i)||Pt(n,i)||Ft(n,i)||Rt(n,i)||Gt(n,i)||Wt(n,i)||Jt(n,i))return!1;let u=Object.getPrototypeOf(n);if(u!==null&&typeof u.equals==\"function\")try{if(n.equals(i))continue;return!1}catch{}let[o,a]=Dt(n);for(let l of o(n))r.push(a(n,l),a(i,l))}return!0}function Dt(t){if(t instanceof Map)return[e=>e.keys(),(e,r)=>e.get(r)];{let e=t instanceof globalThis.Error?[\"message\"]:[];return[r=>[...e,...Object.keys(r)],(r,n)=>r[n]]}}function Pt(t,e){return t instanceof Date&&(t>e||t<e)}function Ft(t,e){return!(t instanceof D)&&t.buffer instanceof ArrayBuffer&&t.BYTES_PER_ELEMENT&&!(t.byteLength===e.byteLength&&t.every((r,n)=>r===e[n]))}function Rt(t,e){return Array.isArray(t)&&t.length!==e.length}function Gt(t,e){return t instanceof Map&&t.size!==e.size}function Wt(t,e){return t instanceof Set&&(t.size!=e.size||[...t].some(r=>!e.has(r)))}function Jt(t,e){return t instanceof RegExp&&(t.source!==e.source||t.flags!==e.flags)}function De(t){return typeof t==\"object\"&&t!==null}function Vt(t,e){return typeof t!=\"object\"&&typeof e!=\"object\"&&(!t||!e)||[Promise,WeakSet,WeakMap,Function].some(n=>t instanceof n)?!1:t.constructor===e.constructor}var B=class extends c{},O=class extends c{},j=class extends c{};var Ye=new WeakMap,pe=new DataView(new ArrayBuffer(8)),de=0;function he(t){let e=Ye.get(t);if(e!==void 0)return e;let r=de++;return de===2147483647&&(de=0),Ye.set(t,r),r}function _e(t,e){return t^e+2654435769+(t<<6)+(t>>2)|0}function $e(t){let e=0,r=t.length;for(let n=0;n<r;n++)e=Math.imul(31,e)+t.charCodeAt(n)|0;return e}function Ke(t){pe.setFloat64(0,t);let e=pe.getInt32(0),r=pe.getInt32(4);return Math.imul(73244475,e>>16^e)^r}function fr(t){return $e(t.toString())}function pr(t){let e=Object.getPrototypeOf(t);if(e!==null&&typeof e.hashCode==\"function\")try{let n=t.hashCode(t);if(typeof n==\"number\")return n}catch{}if(t instanceof Promise||t instanceof WeakSet||t instanceof WeakMap)return he(t);if(t instanceof Date)return Ke(t.getTime());let r=0;if(t instanceof ArrayBuffer&&(t=new Uint8Array(t)),Array.isArray(t)||t instanceof Uint8Array)for(let n=0;n<t.length;n++)r=Math.imul(31,r)+g(t[n])|0;else if(t instanceof Set)t.forEach(n=>{r=r+g(n)|0});else if(t instanceof Map)t.forEach((n,i)=>{r=r+_e(g(n),g(i))|0});else{let n=Object.keys(t);for(let i=0;i<n.length;i++){let s=n[i],u=t[s];r=r+_e(g(u),$e(s))|0}}return r}function g(t){if(t===null)return 1108378658;if(t===void 0)return 1108378659;if(t===!0)return 1108378657;if(t===!1)return 1108378656;switch(typeof t){case\"number\":return Ke(t);case\"string\":return $e(t);case\"bigint\":return fr(t);case\"object\":return pr(t);case\"symbol\":return he(t);case\"function\":return he(t);default:return 0}}var E=5,xe=Math.pow(2,E),dr=xe-1,hr=xe/2,_r=xe/4,h=0,v=1,$=2,I=3,we={type:$,bitmap:0,array:[]};function W(t,e){return t>>>e&dr}function se(t,e){return 1<<W(t,e)}function mr(t){return t-=t>>1&1431655765,t=(t&858993459)+(t>>2&858993459),t=t+(t>>4)&252645135,t+=t>>8,t+=t>>16,t&127}function ge(t,e){return mr(t&e-1)}function y(t,e,r){let n=t.length,i=new Array(n);for(let s=0;s<n;++s)i[s]=t[s];return i[e]=r,i}function $r(t,e,r){let n=t.length,i=new Array(n+1),s=0,u=0;for(;s<e;)i[u++]=t[s++];for(i[u++]=r;s<n;)i[u++]=t[s++];return i}function me(t,e){let r=t.length,n=new Array(r-1),i=0,s=0;for(;i<e;)n[s++]=t[i++];for(++i;i<r;)n[s++]=t[i++];return n}function Qe(t,e,r,n,i,s){let u=g(e);if(u===n)return{type:I,hash:u,array:[{type:h,k:e,v:r},{type:h,k:i,v:s}]};let o={val:!1};return J(ye(we,t,u,e,r,o),t,n,i,s,o)}function J(t,e,r,n,i,s){switch(t.type){case v:return xr(t,e,r,n,i,s);case $:return ye(t,e,r,n,i,s);case I:return wr(t,e,r,n,i,s)}}function xr(t,e,r,n,i,s){let u=W(r,e),o=t.array[u];if(o===void 0)return s.val=!0,{type:v,size:t.size+1,array:y(t.array,u,{type:h,k:n,v:i})};if(o.type===h)return p(n,o.k)?i===o.v?t:{type:v,size:t.size,array:y(t.array,u,{type:h,k:n,v:i})}:(s.val=!0,{type:v,size:t.size,array:y(t.array,u,Qe(e+E,o.k,o.v,r,n,i))});let a=J(o,e+E,r,n,i,s);return a===o?t:{type:v,size:t.size,array:y(t.array,u,a)}}function ye(t,e,r,n,i,s){let u=se(r,e),o=ge(t.bitmap,u);if(t.bitmap&u){let a=t.array[o];if(a.type!==h){let b=J(a,e+E,r,n,i,s);return b===a?t:{type:$,bitmap:t.bitmap,array:y(t.array,o,b)}}let l=a.k;return p(n,l)?i===a.v?t:{type:$,bitmap:t.bitmap,array:y(t.array,o,{type:h,k:n,v:i})}:(s.val=!0,{type:$,bitmap:t.bitmap,array:y(t.array,o,Qe(e+E,l,a.v,r,n,i))})}else{let a=t.array.length;if(a>=hr){let l=new Array(32),b=W(r,e);l[b]=ye(we,e+E,r,n,i,s);let A=0,k=t.bitmap;for(let ce=0;ce<32;ce++){if(k&1){let qt=t.array[A++];l[ce]=qt}k=k>>>1}return{type:v,size:a+1,array:l}}else{let l=$r(t.array,o,{type:h,k:n,v:i});return s.val=!0,{type:$,bitmap:t.bitmap|u,array:l}}}}function wr(t,e,r,n,i,s){if(r===t.hash){let u=be(t,n);if(u!==-1)return t.array[u].v===i?t:{type:I,hash:r,array:y(t.array,u,{type:h,k:n,v:i})};let o=t.array.length;return s.val=!0,{type:I,hash:r,array:y(t.array,o,{type:h,k:n,v:i})}}return J({type:$,bitmap:se(t.hash,e),array:[t]},e,r,n,i,s)}function be(t,e){let r=t.array.length;for(let n=0;n<r;n++)if(p(e,t.array[n].k))return n;return-1}function ie(t,e,r,n){switch(t.type){case v:return gr(t,e,r,n);case $:return yr(t,e,r,n);case I:return br(t,n)}}function gr(t,e,r,n){let i=W(r,e),s=t.array[i];if(s!==void 0){if(s.type!==h)return ie(s,e+E,r,n);if(p(n,s.k))return s}}function yr(t,e,r,n){let i=se(r,e);if(!(t.bitmap&i))return;let s=ge(t.bitmap,i),u=t.array[s];if(u.type!==h)return ie(u,e+E,r,n);if(p(n,u.k))return u}function br(t,e){let r=be(t,e);if(!(r<0))return t.array[r]}function ke(t,e,r,n){switch(t.type){case v:return kr(t,e,r,n);case $:return Lr(t,e,r,n);case I:return Or(t,n)}}function kr(t,e,r,n){let i=W(r,e),s=t.array[i];if(s===void 0)return t;let u;if(s.type===h){if(!p(s.k,n))return t}else if(u=ke(s,e+E,r,n),u===s)return t;if(u===void 0){if(t.size<=_r){let o=t.array,a=new Array(t.size-1),l=0,b=0,A=0;for(;l<i;){let k=o[l];k!==void 0&&(a[b]=k,A|=1<<l,++b),++l}for(++l;l<o.length;){let k=o[l];k!==void 0&&(a[b]=k,A|=1<<l,++b),++l}return{type:$,bitmap:A,array:a}}return{type:v,size:t.size-1,array:y(t.array,i,u)}}return{type:v,size:t.size,array:y(t.array,i,u)}}function Lr(t,e,r,n){let i=se(r,e);if(!(t.bitmap&i))return t;let s=ge(t.bitmap,i),u=t.array[s];if(u.type!==h){let o=ke(u,e+E,r,n);return o===u?t:o!==void 0?{type:$,bitmap:t.bitmap,array:y(t.array,s,o)}:t.bitmap===i?void 0:{type:$,bitmap:t.bitmap^i,array:me(t.array,s)}}return p(n,u.k)?t.bitmap===i?void 0:{type:$,bitmap:t.bitmap^i,array:me(t.array,s)}:t}function Or(t,e){let r=be(t,e);if(r<0)return t;if(t.array.length!==1)return{type:I,hash:t.hash,array:me(t.array,r)}}function Ze(t,e){if(t===void 0)return;let r=t.array,n=r.length;for(let i=0;i<n;i++){let s=r[i];if(s!==void 0){if(s.type===h){e(s.v,s.k);continue}Ze(s,e)}}}var T=class t{static fromObject(e){let r=Object.keys(e),n=t.new();for(let i=0;i<r.length;i++){let s=r[i];n=n.set(s,e[s])}return n}static fromMap(e){let r=t.new();return e.forEach((n,i)=>{r=r.set(i,n)}),r}static new(){return new t(void 0,0)}constructor(e,r){this.root=e,this.size=r}get(e,r){if(this.root===void 0)return r;let n=ie(this.root,0,g(e),e);return n===void 0?r:n.v}set(e,r){let n={val:!1},i=this.root===void 0?we:this.root,s=J(i,0,g(e),e,r,n);return s===this.root?this:new t(s,n.val?this.size+1:this.size)}delete(e){if(this.root===void 0)return this;let r=ke(this.root,0,g(e),e);return r===this.root?this:r===void 0?t.new():new t(r,this.size-1)}has(e){return this.root===void 0?!1:ie(this.root,0,g(e),e)!==void 0}entries(){if(this.root===void 0)return[];let e=[];return this.forEach((r,n)=>e.push([n,r])),e}forEach(e){Ze(this.root,e)}hashCode(){let e=0;return this.forEach((r,n)=>{e=e+_e(g(r),g(n))|0}),e}equals(e){if(!(e instanceof t)||this.size!==e.size)return!1;try{return this.forEach((r,n)=>{if(!p(e.get(n,!r),r))throw Xe}),!0}catch(r){if(r===Xe)return!1;throw r}}},Xe=Symbol();var tt=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),si=new RegExp(`^[${tt}]*`),ui=new RegExp(`[${tt}]*$`);function F(){return T.new()}var Oe=class extends c{constructor(e){super(),this.dict=e}};function ut(){return new Oe(F())}var lu=T.new();var au=ut();var Eu=new j,Su=new B,Bu=new O;var pt=0;var dt=1;var ht=2;var _t=0;var mt=1;var $t=2;var xt=3;var Ot=0;var jt=1;var vt=2;var Ee=3;var Et=4;var Se=5;var Be=6;var Ce=7;var Q=class{#t=null;#e=()=>{};#r=!1;constructor(e,r,{useServerEvents:n=!1}={}){this.#t=e,this.#e=r,this.#r=n}mount(e){this.#t.appendChild(this.#f(e))}#n=[];push(e,r=0){r&&(S(e.changes,n=>{switch(n.kind){case Be:n.before+=r;break;case Ee:n.before+=r;break;case Ce:n.from+=r;break;case Se:n.from+=r;break}}),S(e.children,n=>{n.index+=r})),this.#n.push({node:this.#t,patch:e}),this.#i()}#i(){for(;this.#n.length;){let{node:e,patch:r}=this.#n.pop();S(r.changes,n=>{switch(n.kind){case Be:this.#u(e,n.children,n.before);break;case Ee:this.#o(e,n.key,n.before,n.count);break;case Et:this.#a(e,n.key,n.count);break;case Ce:this.#s(e,n.from,n.count);break;case Se:this.#c(e,n.from,n.count,n.with);break;case Ot:this.#h(e,n.content);break;case jt:this.#d(e,n.inner_html);break;case vt:this.#_(e,n.added,n.removed);break}}),this.#s(e,e.childNodes.length-r.removed,r.removed),S(r.children,n=>{this.#n.push({node:e.childNodes[n.index],patch:n})})}}#u(e,r,n){let i=document.createDocumentFragment();S(r,s=>{let u=this.#f(s);Ae(e,u),i.appendChild(u)}),e.insertBefore(i,e.childNodes[n]??null)}#o(e,r,n,i){let s=e[_].keyedChildren.get(r).deref();if(i>1){let u=document.createDocumentFragment();for(let o=0;o<i&&s!==null;++o){let a=s.nextSibling;u.append(s),s=a}s=u}e.insertBefore(s,e.childNodes[n]??null)}#a(e,r,n){this.#l(e,e[_].keyedChildren.get(r).deref(),n)}#s(e,r,n){this.#l(e,e.childNodes[r],n)}#l(e,r,n){for(;n-- >0&&r!==null;){let i=r.nextSibling,s=r[_].key;s&&e[_].keyedChildren.delete(s),e.removeChild(r),r=i}}#c(e,r,n,i){this.#s(e,r,n);let s=this.#f(i);Ae(e,s),e.insertBefore(s,e.childNodes[r]??null)}#h(e,r){e.data=r}#d(e,r){e.innerHTML=r}#_(e,r,n){S(n,i=>{let s=i.name;e[_].handlers.has(s)?(e.removeEventListener(s,Bt),e[_].handlers.delete(s)):(e.removeAttribute(s),Ct[s]?.removed?.(e,s))}),S(r,i=>{this.#p(e,i)})}#f(e){switch(e.kind){case mt:{let r=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return K(r,e.key),S(e.attributes,n=>{this.#p(r,n)}),this.#u(r,e.children,0),r}case $t:{let r=document.createTextNode(e.content);return K(r,e.key),r}case _t:{let r=document.createDocumentFragment(),n=document.createTextNode(\"\");return K(n,e.key),r.appendChild(n),S(e.children,i=>{r.appendChild(this.#f(i))}),r}case xt:{let r=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return K(r,e.key),S(e.attributes,n=>{this.#p(r,n)}),this.#d(r,e.inner_html),r}}}#p(e,r){switch(r.kind){case pt:{let n=r.name,i=r.value;i!==e.getAttribute(n)&&e.setAttribute(n,i),Ct[n]?.added?.(e,i);break}case dt:e[r.name]=r.value;break;case ht:{e[_].handlers.has(r.name)||e.addEventListener(r.name,Bt,{passive:!r.prevent_default});let n=r.prevent_default,i=r.stop_propagation,s=r.immediate||Vr.includes(r.name),u=Array.isArray(r.include)?r.include:[];e[_].handlers.set(r.name,o=>{n&&o.preventDefault(),i&&o.stopPropagation();let a=[],l=o.currentTarget;for(;l!==this.#t;){let A=l[_].key;if(A)a.push(A);else{let k=[].indexOf.call(l.parentNode.childNodes,l);a.push(k.toString())}l=l.parentNode}a.reverse();let b=this.#r?Wr(o,u):o;this.#e(b,a,o.type,s)});break}}}};function S(t,e){if(Array.isArray(t))for(let r=0;r<t.length;r++)e(t[r]);else for(t;t.tail;t=t.tail)e(t.head)}var _=Symbol(\"metadata\");function K(t,e=\"\"){switch(t.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:t[_]={key:e,keyedChildren:new Map,handlers:new Map};break;case Node.TEXT_NODE:t[_]={key:e};break}}function Ae(t,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)Ae(t,e);return}let r=e[_].key;r&&t[_].keyedChildren.set(r,new WeakRef(e))}function Bt(t){t.currentTarget[_].handlers.get(t.type)(t)}function Wr(t,e=[]){let r={};(t.type===\"input\"||t.type===\"change\")&&e.push(\"target.value\");for(let n of e){let i=n.split(\".\");for(let s=0,u=t,o=r;s<i.length;s++){if(s===i.length-1){o[i[s]]=u[i[s]];break}o=o[i[s]]??={},u=u[i[s]]}}return r}var Ct={checked:At(\"checked\"),selected:At(\"selected\"),value:Jr(\"value\"),autofocus:{added(t){t.focus?.()}},autoplay:{added(t){try{t.play?.()}catch(e){console.error(e)}}}};function At(t){return{added(e,r){e[t]=!0},removed(e){e[t]=!1}}}function Jr(t){return{added(e,r){e[t]=r}}}var Vr=[\"input\",\"change\",\"focusin\",\"focusout\",\"focus\",\"blur\",\"select\"];var Nt=new WeakMap;async function Tt(t){let e=[];for(let n of document.querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((i,s)=>{n.addEventListener(\"load\",i),n.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!t.host.isConnected)return[];t.adoptedStyleSheets=t.host.getRootNode().adoptedStyleSheets;let r=[];for(let n of document.styleSheets)try{t.adoptedStyleSheets.push(n)}catch{try{let i=Nt.get(n);if(!i){i=new CSSStyleSheet;for(let s of n.cssRules)i.insertRule(s.cssText,i.cssRules.length);Nt.set(n,i)}t.adoptedStyleSheets.push(i)}catch{let i=n.ownerNode.cloneNode();t.prepend(i),r.push(i)}}return r}var It=0;var Mt=1;var zt=2;var Ne=0;var Ut=1;var Te=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u=new MutationObserver(e=>{let r=[];for(let n of e){if(n.type!==\"attributes\")continue;let i=n.attributeName;this.#o.includes(i)&&r.push([i,this.getAttribute(i)])}r.length&&this.#a?this.#r?.send({kind:Ne,attributes:r}):this.#s.push(...r)});#o=[];#a=!1;#s=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new Q(this.shadowRoot,(e,r,n)=>{this.#r?.send({kind:Ut,path:r,name:n,event:e})},{useServerEvents:!0}),this.#u.observe(this,{attributes:!0})}connectedCallback(){this.#c(),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#l())}adoptedCallback(){this.#c()}attributeChangedCallback(e,r,n){switch(e){case r!==n:{this.#e=new URL(n,window.location.href),this.#l();return}case\"method\":{let i=n.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#l());return}}}messageReceivedCallback(e){switch(e.kind){case It:{this.#i.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Mt:{this.#i.push(e.patch,this.#n.length);break}case zt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#l(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let i={onConnect:()=>{this.#a=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#e,method:this.#t}}),this.#s.length&&(this.#r.send({kind:Ne,attributes:this.#s}),this.#s=[])},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#a=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#e,method:this.#t}})}};switch(this.#t){case\"ws\":this.#r=new Ie(this.#e,i);break;case\"sse\":this.#r=new Me(this.#e,i);break;case\"polling\":this.#r=new ze(this.#e,i);break}}async#c(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await Tt(this.shadowRoot)}},Ie=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:r,onMessage:n,onClose:i}){this.#t=e,this.#e=new WebSocket(this.#t),this.#r=r,this.#n=n,this.#i=i,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}},this.#e.onclose=()=>{this.#i()}}send(e){this.#e.send(JSON.stringify(e))}close(){this.#e.close()}},Me=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:r,onMessage:n,onClose:i}){this.#t=e,this.#e=new EventSource(this.#t),this.#n=n,this.#i=i,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},ze=class{#t;#e;#r;#n;#i;#u;constructor(e,{onConnect:r,onMessage:n,onClose:i,...s}){this.#t=e,this.#n=r,this.#i=n,this.#u=i,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#n(),this.#r=window.setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#u()}#o(){return fetch(this.#t).then(e=>e.json()).then(this.#i).catch(console.error)}};window.customElements.define(\"lustre-server-component\",Te);export{Te as ServerComponent};\\n",
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

///
///
pub fn method(value: TransportMethod) -> Attribute(msg) {
  attribute("method", case value {
    WebSocket -> "ws"
    ServerSentEvents -> "sse"
    Polling -> "polling"
  })
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
