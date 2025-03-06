//// > **Note**: server components are currently only supported on the **erlang**
//// > target. If it's important to you that they work on the javascript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run entire
//// Lustre applications on the server. DOM changes are broadcasted to a small
//// client runtime and browser events are sent back to the server.
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
import lustre.{type RuntimeMessage}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/vdom/attribute.{Event} as _

// TYPES -----------------------------------------------------------------------

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
    "var c=class{withFields(t){let n=Object.keys(this).map(r=>r in t?t[r]:this[r]);return new this.constructor(...n)}},M=class{static fromArray(t,n){let r=n||new G;for(let i=t.length-1;i>=0;--i)r=new B(t[i],r);return r}[Symbol.iterator](){return new le(this)}toArray(){return[...this]}atLeastLength(t){for(let n of this){if(t<=0)return!0;t--}return t<=0}hasLength(t){for(let n of this){if(t<=0)return!1;t--}return t===0}countLength(){let t=0;for(let n of this)t++;return t}};var le=class{#t;constructor(t){this.#t=t}next(){if(this.#t instanceof G)return{done:!0};{let{head:t,tail:n}=this.#t;return this.#t=n,{value:t,done:!1}}}},G=class extends M{},B=class extends M{constructor(t,n){super(),this.head=t,this.tail=n}};function p(e,t){let n=[e,t];for(;n.length;){let r=n.pop(),i=n.pop();if(r===i)continue;if(!Be(r)||!Be(i)||!mr(r,i)||cr(r,i)||fr(r,i)||pr(r,i)||_r(r,i)||dr(r,i)||hr(r,i))return!1;let l=Object.getPrototypeOf(r);if(l!==null&&typeof l.equals==\"function\")try{if(r.equals(i))continue;return!1}catch{}let[u,o]=ar(r);for(let a of u(r))n.push(o(r,a),o(i,a))}return!0}function ar(e){if(e instanceof Map)return[t=>t.keys(),(t,n)=>t.get(n)];{let t=e instanceof globalThis.Error?[\"message\"]:[];return[n=>[...t,...Object.keys(n)],(n,r)=>n[r]]}}function cr(e,t){return e instanceof Date&&(e>t||e<t)}function fr(e,t){return e.buffer instanceof ArrayBuffer&&e.BYTES_PER_ELEMENT&&!(e.byteLength===t.byteLength&&e.every((n,r)=>n===t[r]))}function pr(e,t){return Array.isArray(e)&&e.length!==t.length}function _r(e,t){return e instanceof Map&&e.size!==t.size}function dr(e,t){return e instanceof Set&&(e.size!=t.size||[...e].some(n=>!t.has(n)))}function hr(e,t){return e instanceof RegExp&&(e.source!==t.source||e.flags!==t.flags)}function Be(e){return typeof e==\"object\"&&e!==null}function mr(e,t){return typeof e!=\"object\"&&typeof t!=\"object\"&&(!e||!t)||[Promise,WeakSet,WeakMap,Function].some(r=>e instanceof r)?!1:e.constructor===t.constructor}var A=class extends c{},L=class extends c{},O=class extends c{};var Ye=new WeakMap,ae=new DataView(new ArrayBuffer(8)),ce=0;function fe(e){let t=Ye.get(e);if(t!==void 0)return t;let n=ce++;return ce===2147483647&&(ce=0),Ye.set(e,n),n}function pe(e,t){return e^t+2654435769+(e<<6)+(e>>2)|0}function de(e){let t=0,n=e.length;for(let r=0;r<n;r++)t=Math.imul(31,t)+e.charCodeAt(r)|0;return t}function Ke(e){ae.setFloat64(0,e);let t=ae.getInt32(0),n=ae.getInt32(4);return Math.imul(73244475,t>>16^t)^n}function Mr(e){return de(e.toString())}function zr(e){let t=Object.getPrototypeOf(e);if(t!==null&&typeof t.hashCode==\"function\")try{let r=e.hashCode(e);if(typeof r==\"number\")return r}catch{}if(e instanceof Promise||e instanceof WeakSet||e instanceof WeakMap)return fe(e);if(e instanceof Date)return Ke(e.getTime());let n=0;if(e instanceof ArrayBuffer&&(e=new Uint8Array(e)),Array.isArray(e)||e instanceof Uint8Array)for(let r=0;r<e.length;r++)n=Math.imul(31,n)+y(e[r])|0;else if(e instanceof Set)e.forEach(r=>{n=n+y(r)|0});else if(e instanceof Map)e.forEach((r,i)=>{n=n+pe(y(r),y(i))|0});else{let r=Object.keys(e);for(let i=0;i<r.length;i++){let s=r[i],l=e[s];n=n+pe(y(l),de(s))|0}}return n}function y(e){if(e===null)return 1108378658;if(e===void 0)return 1108378659;if(e===!0)return 1108378657;if(e===!1)return 1108378656;switch(typeof e){case\"number\":return Ke(e);case\"string\":return de(e);case\"bigint\":return Mr(e);case\"object\":return zr(e);case\"symbol\":return fe(e);case\"function\":return fe(e);default:return 0}}var j=5,he=Math.pow(2,j),qr=he-1,Dr=he/2,Br=he/4,h=0,v=1,m=2,I=3,me={type:m,bitmap:0,array:[]};function R(e,t){return e>>>t&qr}function ee(e,t){return 1<<R(e,t)}function Ur(e){return e-=e>>1&1431655765,e=(e&858993459)+(e>>2&858993459),e=e+(e>>4)&252645135,e+=e>>8,e+=e>>16,e&127}function xe(e,t){return Ur(e&t-1)}function b(e,t,n){let r=e.length,i=new Array(r);for(let s=0;s<r;++s)i[s]=e[s];return i[t]=n,i}function Rr(e,t,n){let r=e.length,i=new Array(r+1),s=0,l=0;for(;s<t;)i[l++]=e[s++];for(i[l++]=n;s<r;)i[l++]=e[s++];return i}function _e(e,t){let n=e.length,r=new Array(n-1),i=0,s=0;for(;i<t;)r[s++]=e[i++];for(++i;i<n;)r[s++]=e[i++];return r}function He(e,t,n,r,i,s){let l=y(t);if(l===r)return{type:I,hash:l,array:[{type:h,k:t,v:n},{type:h,k:i,v:s}]};let u={val:!1};return P(ge(me,e,l,t,n,u),e,r,i,s,u)}function P(e,t,n,r,i,s){switch(e.type){case v:return Pr(e,t,n,r,i,s);case m:return ge(e,t,n,r,i,s);case I:return Fr(e,t,n,r,i,s)}}function Pr(e,t,n,r,i,s){let l=R(n,t),u=e.array[l];if(u===void 0)return s.val=!0,{type:v,size:e.size+1,array:b(e.array,l,{type:h,k:r,v:i})};if(u.type===h)return p(r,u.k)?i===u.v?e:{type:v,size:e.size,array:b(e.array,l,{type:h,k:r,v:i})}:(s.val=!0,{type:v,size:e.size,array:b(e.array,l,He(t+j,u.k,u.v,n,r,i))});let o=P(u,t+j,n,r,i,s);return o===u?e:{type:v,size:e.size,array:b(e.array,l,o)}}function ge(e,t,n,r,i,s){let l=ee(n,t),u=xe(e.bitmap,l);if(e.bitmap&l){let o=e.array[u];if(o.type!==h){let x=P(o,t+j,n,r,i,s);return x===o?e:{type:m,bitmap:e.bitmap,array:b(e.array,u,x)}}let a=o.k;return p(r,a)?i===o.v?e:{type:m,bitmap:e.bitmap,array:b(e.array,u,{type:h,k:r,v:i})}:(s.val=!0,{type:m,bitmap:e.bitmap,array:b(e.array,u,He(t+j,a,o.v,n,r,i))})}else{let o=e.array.length;if(o>=Dr){let a=new Array(32),x=R(n,t);a[x]=ge(me,t+j,n,r,i,s);let T=0,E=e.bitmap;for(let se=0;se<32;se++){if(E&1){let or=e.array[T++];a[se]=or}E=E>>>1}return{type:v,size:o+1,array:a}}else{let a=Rr(e.array,u,{type:h,k:r,v:i});return s.val=!0,{type:m,bitmap:e.bitmap|l,array:a}}}}function Fr(e,t,n,r,i,s){if(n===e.hash){let l=$e(e,r);if(l!==-1)return e.array[l].v===i?e:{type:I,hash:n,array:b(e.array,l,{type:h,k:r,v:i})};let u=e.array.length;return s.val=!0,{type:I,hash:n,array:b(e.array,u,{type:h,k:r,v:i})}}return P({type:m,bitmap:ee(e.hash,t),array:[e]},t,n,r,i,s)}function $e(e,t){let n=e.array.length;for(let r=0;r<n;r++)if(p(t,e.array[r].k))return r;return-1}function Z(e,t,n,r){switch(e.type){case v:return Wr(e,t,n,r);case m:return Gr(e,t,n,r);case I:return Jr(e,r)}}function Wr(e,t,n,r){let i=R(n,t),s=e.array[i];if(s!==void 0){if(s.type!==h)return Z(s,t+j,n,r);if(p(r,s.k))return s}}function Gr(e,t,n,r){let i=ee(n,t);if(!(e.bitmap&i))return;let s=xe(e.bitmap,i),l=e.array[s];if(l.type!==h)return Z(l,t+j,n,r);if(p(r,l.k))return l}function Jr(e,t){let n=$e(e,t);if(!(n<0))return e.array[n]}function we(e,t,n,r){switch(e.type){case v:return Yr(e,t,n,r);case m:return Vr(e,t,n,r);case I:return Kr(e,r)}}function Yr(e,t,n,r){let i=R(n,t),s=e.array[i];if(s===void 0)return e;let l;if(s.type===h){if(!p(s.k,r))return e}else if(l=we(s,t+j,n,r),l===s)return e;if(l===void 0){if(e.size<=Br){let u=e.array,o=new Array(e.size-1),a=0,x=0,T=0;for(;a<i;){let E=u[a];E!==void 0&&(o[x]=E,T|=1<<a,++x),++a}for(++a;a<u.length;){let E=u[a];E!==void 0&&(o[x]=E,T|=1<<a,++x),++a}return{type:m,bitmap:T,array:o}}return{type:v,size:e.size-1,array:b(e.array,i,l)}}return{type:v,size:e.size,array:b(e.array,i,l)}}function Vr(e,t,n,r){let i=ee(n,t);if(!(e.bitmap&i))return e;let s=xe(e.bitmap,i),l=e.array[s];if(l.type!==h){let u=we(l,t+j,n,r);return u===l?e:u!==void 0?{type:m,bitmap:e.bitmap,array:b(e.array,s,u)}:e.bitmap===i?void 0:{type:m,bitmap:e.bitmap^i,array:_e(e.array,s)}}return p(r,l.k)?e.bitmap===i?void 0:{type:m,bitmap:e.bitmap^i,array:_e(e.array,s)}:e}function Kr(e,t){let n=$e(e,t);if(n<0)return e;if(e.array.length!==1)return{type:I,hash:e.hash,array:_e(e.array,n)}}function Xe(e,t){if(e===void 0)return;let n=e.array,r=n.length;for(let i=0;i<r;i++){let s=n[i];if(s!==void 0){if(s.type===h){t(s.v,s.k);continue}Xe(s,t)}}}var C=class e{static fromObject(t){let n=Object.keys(t),r=e.new();for(let i=0;i<n.length;i++){let s=n[i];r=r.set(s,t[s])}return r}static fromMap(t){let n=e.new();return t.forEach((r,i)=>{n=n.set(i,r)}),n}static new(){return new e(void 0,0)}constructor(t,n){this.root=t,this.size=n}get(t,n){if(this.root===void 0)return n;let r=Z(this.root,0,y(t),t);return r===void 0?n:r.v}set(t,n){let r={val:!1},i=this.root===void 0?me:this.root,s=P(i,0,y(t),t,n,r);return s===this.root?this:new e(s,r.val?this.size+1:this.size)}delete(t){if(this.root===void 0)return this;let n=we(this.root,0,y(t),t);return n===this.root?this:n===void 0?e.new():new e(n,this.size-1)}has(t){return this.root===void 0?!1:Z(this.root,0,y(t),t)!==void 0}entries(){if(this.root===void 0)return[];let t=[];return this.forEach((n,r)=>t.push([r,n])),t}forEach(t){Xe(this.root,t)}hashCode(){let t=0;return this.forEach((n,r)=>{t=t+pe(y(n),y(r))|0}),t}equals(t){if(!(t instanceof e)||this.size!==t.size)return!1;try{return this.forEach((n,r)=>{if(!p(t.get(r,!n),n))throw Ve}),!0}catch(n){if(n===Ve)return!1;throw n}}},Ve=Symbol();var Qe=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Fi=new RegExp(`^[${Qe}]*`),Wi=new RegExp(`[${Qe}]*$`);function U(){return C.new()}var ke=class extends c{constructor(t){super(),this.dict=t}};function nt(){return new ke(U())}var Yl=C.new();var Vl=nt();var ou=new O,au=new A,cu=new L;var at=0,ct=1,ft=1,pt=1,_t=2;var dt=1,ht=2,mt=0;var xt=1;var gt=0;var je=2,$t=1,q=1,Ee=2,Ae=3,Se=4,wt=5,yt=2;var bt=1,kt=2,Lt=0;var D=1,ne=2,Ot=1;var vt=1,jt=2,Et=2;var F=1;var Ne=3,At=4,St=5,Nt=0,Ct=1,Ce=2,Te=3,Tt=0,It=1,Mt=1;var zt=1,qt=2;var Dt=1,Bt=2,Ut=3,Rt=1,Pt=2,Ft=4;var Wt=1,Gt=2,Jt=3,Yt=5;var Vt=1,Kt=2,Ht=6,Xt=1,Qt=2,Zt=7;var er=1,tr=2;var f=Symbol(\"metadata\"),ie=class{#t=null;#e=()=>{};#r=[];constructor(t,n){this.#t=t,this.#e=n}mount(t){this.#t.appendChild(W(t,this.#e,this.#t))}push(t){this.#r.push({node:this.#t,patch:t}),this.#n()}#n(){for(;this.#r.length;){let{node:t,patch:n}=this.#r.pop();for(let r=0;r<n[Ce].length;r++){let i=n[Ce][r];switch(i[0]){case Ht:rr(t,i[Xt],i[Qt],this.#e,this.#t);break;case Ut:En(t,i[Rt],i[Pt],this.#e,this.#t);break;case Ft:An(t,i[Wt],i[Gt],i[Jt]);break;case Yt:Sn(t,i[Vt],i[Kt]);break;case Zt:Nn(t,i[er],i[tr]);break;case Tt:Cn(t,i[It],this.#e,this.#t);break;case Mt:Tn(t,i[zt]);break;case qt:In(t,i[Dt],i[Bt],this.#e,this.#t);break}}for(;n[Ct]-- >0;){let r=t.lastChild,i=r[f].key;i&&t[f].keyedChildren.delete(i),t.removeChild(r)}for(let r=0;r<n[Te].length;r++){let i=n[Te][r];this.#r.push({node:t.childNodes[i[Nt]],patch:i})}}}};function rr(e,t,n,r,i){let s=document.createDocumentFragment();for(let l=0;l<t.length;l++){let u=t[l],o=W(u,r,i);o[f].key&&e[f].keyedChildren.set(o[f].key,new WeakRef(Ie(o))),s.appendChild(o)}e.insertBefore(s,e.childNodes[n])}function En(e,t,n,r,i){let s=W(t,r,i);t[q]&&e[f].keyedChildren.set(t[q],new WeakRef(Ie(s))),e.insertBefore(s,e.childNodes[n])}function An(e,t,n,r){let i=e[f].keyedChildren.get(t).deref();if(r>1){let s=document.createDocumentFragment();for(let l=0;l<r&&i!==null;++l){let u=i.nextSibling;s.append(i),i=u}i=s}e.insertBefore(i,e.childNodes[n])}function Sn(e,t,n){let r=e[f].keyedChildren.get(t).deref();for(e[f].keyedChildren.delete(t);n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Nn(e,t,n){let r=e.childNodes[t];for(;n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Cn(e,t,n,r){let i=W(t,n,r),s=e.parentNode;t[q]&&s[f].keyedChildren.set(t[q],new WeakRef(Ie(i))),s.replaceChild(i,e)}function Tn(e,t){e.data=t}function In(e,t,n,r,i){for(let s=0;s<n.length;s++){let l=n[s][D];e[f].handlers.has(l)?(e.removeEventListener(l,ir),e[f].handlers.delete(l)):e.removeAttribute(l)}for(let s=0;s<t.length;s++)nr(e,t[s],r,i)}function Ie(e){for(;e.nodeType===DocumentFragment.DOCUMENT_FRAGMENT_NODE;)e=e.firstChild;return e}function W(e,t,n){switch(e[0]){case $t:{let r=e[Ee]?document.createElementNS(e[Ee],e[Ae]):document.createElement(e[Ae]);r[f]={key:e[q],keyedChildren:new Map,handlers:new Map};for(let i=0;i<e[Se].length;i++)nr(r,e[Se][i],t,n);return rr(r,e[wt],0,t,n),r}case yt:{let r=document.createTextNode(e[kt]);return r[f]={key:e[bt]},r}case gt:{let r=document.createDocumentFragment();for(let i=0;i<e[je].length;i++)r.appendChild(W(e[je][i],t,n));return r}}}function nr(e,t,n,r){switch(t[0]){case Lt:t[ne]!==e.getAttribute(t[D])&&(e.setAttribute(t[D],t[ne]),Mn.includes(t[D])&&(e[t[D]]=t[ne]));break;case Ot:e[t[vt]]=t[jt];break;case Et:e[f].handlers.has(t[F])||e.addEventListener(t[F],ir,{passive:!t[Ne]});let i=t[Ne],s=t[At],l=t[St]||zn.includes(t[F]);e[f].handlers.set(t[F],u=>{i&&u.preventDefault(),s&&u.stopPropagation();let o=u.target,a=o[f].key||Array.from(o.parentNode.childNodes).indexOf(o);for(o=o.parentNode;o!==r;){let x=o[f].key,T=Array.from(o.parentNode.childNodes).indexOf(o);a=x?`${x}.${a}`:`${T}.${a}`,o=o.parentNode}n(u,a,u.type,l)});break}}function ir(e){e.currentTarget[f].handlers.get(e.type)(e)}var Mn=[\"checked\",\"disabled\",\"selected\",\"value\"],zn=[\"input\",\"change\",\"focusin\",\"focusout\",\"focus\",\"blur\",\"select\"];var Yo=Symbol(\"metadata\");var sr=new WeakMap;async function lr(e){let t=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||t.push(new Promise((i,s)=>{r.addEventListener(\"load\",i),r.addEventListener(\"error\",s)}));await Promise.allSettled(t),e.adoptedStyleSheets=e.host.getRootNode().adoptedStyleSheets;let n=[];for(let r of document.styleSheets)try{e.adoptedStyleSheets.push(r)}catch{try{let i=sr.get(r);if(!i){i=new CSSStyleSheet;for(let s of r.cssRules)i.insertRule(s.cssText,i.cssRules.length);sr.set(r,i)}e.adoptedStyleSheets.push(i)}catch{let i=r.ownerNode.cloneNode();e.prepend(i),n.push(i)}}return n}var ur=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#s;#o=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new ie(this.shadowRoot,(t,n,r)=>{this.#r?.send([xt,n,r,t])}),this.#s=new MutationObserver(t=>{let n=[];for(let r of t){if(r.type!==\"attributes\")continue;let i=r.attributeName;this.#o.includes(i)&&n.push([i,this.getAttribute(i)])}n.length&&this.#r?.send([mt,n])})}connectedCallback(){this.#u(),this.#s.observe(this,{attributes:!0})}adoptedCallback(){this.#u()}attributeChangedCallback(t,n,r){switch(t){case n!==r:{this.#e=new URL(r,window.location.href),this.#l();return}case\"method\":{let i=r.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\",\"http\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#l());return}}}eventReceivedCallback(t,n,r){this.#r?.send(\"hi!\")}messageReceivedCallback(t){switch(t[0]){case at:{for(;this.shadowRoot.children[this.#n.length];)this.shadowRoot.children[this.#n.length].remove();this.#i.mount(t[ct]);break}case ft:{this.#i.push(t[pt]);break}case _t:{this.dispatchEvent(new CustomEvent(t[dt],{detail:t[ht]}));break}}}#l(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let t=n=>{this.messageReceivedCallback(n)};switch(this.#t){case\"ws\":this.#r=new Me(this.#e,t,{});break;case\"sse\":this.#r=new ze(this.#e,t,{});break;case\"polling\":this.#r=new qe(this.#e,t,{});break;case\"http\":this.#r=new De(this.#e,t);break}}async#u(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await lr(this.shadowRoot)}},Me=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new WebSocket(this.#t),this.#e.onmessage=n}send(t){this.#e.send(JSON.stringify(t))}close(){this.#e.close()}},ze=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new EventSource(t),this.#e.onmessage=n}send(t){fetch(this.#t,{method:\"POST\",headers:{\"Content-Type\":\"application/json\"},body:JSON.stringify(t)})}close(){this.#e.close()}},qe=class{#t;#e;#r;#n;constructor(t,n,r={}){this.#t=t,this.#e=n,this.#r=r.interval??1e3,this.#n=window.setInterval(()=>{fetch(this.#t).then(i=>i.json()).then(this.#e)},this.#r)}async send(t){let r=await(await fetch(this.#t,{method:\"POST\",headers:{\"Content-Type\":\"application/json\"},body:JSON.stringify(t)})).json();window.clearInterval(this.#n),this.#e(r),this.#n=window.setInterval(()=>{fetch(this.#t).then(i=>i.json()).then(this.#e)},this.#r)}close(){clearInterval(this.#n)}},De=class{#t;#e;constructor(t,n){this.#t=t,this.#e=n}send(t){fetch(this.#t,{method:\"POST\",headers:{\"Content-Type\":\"application/json\"},body:JSON.stringify(t)}).then(n=>n.json()).then(n=>this.#e(n))}close(){}};export{ur as ServerComponent};\\n",
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

/// A server component broadcasts patches to be applied to the DOM to any connected
/// clients. This action is used to add a new client to a running server component.
///
// pub fn subscribe(
//   id: String,
//   renderer: fn(Patch(msg)) -> Nil,
// ) -> Action(msg, ServerComponent) {
//   runtime.Subscribe(id, renderer)
// }

/// Remove a registered renderer from a server component. If no renderer with the
/// given id is found, this action has no effect.
///
// pub fn unsubscribe(id: String) -> Action(msg, ServerComponent) {
//   runtime.Unsubscribe(id)
// }

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
