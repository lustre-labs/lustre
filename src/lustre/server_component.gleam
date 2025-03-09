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
import lustre.{type Error, type Runtime, type RuntimeMessage}
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
    "var c=class{withFields(t){let n=Object.keys(this).map(r=>r in t?t[r]:this[r]);return new this.constructor(...n)}},q=class{static fromArray(t,n){let r=n||new F;for(let i=t.length-1;i>=0;--i)r=new z(t[i],r);return r}[Symbol.iterator](){return new pe(this)}toArray(){return[...this]}atLeastLength(t){for(let n of this){if(t<=0)return!0;t--}return t<=0}hasLength(t){for(let n of this){if(t<=0)return!1;t--}return t===0}countLength(){let t=0;for(let n of this)t++;return t}};var pe=class{#t;constructor(t){this.#t=t}next(){if(this.#t instanceof F)return{done:!0};{let{head:t,tail:n}=this.#t;return this.#t=n,{value:t,done:!1}}}},F=class extends q{},z=class extends q{constructor(t,n){super(),this.head=t,this.tail=n}};function f(e,t){let n=[e,t];for(;n.length;){let r=n.pop(),i=n.pop();if(r===i)continue;if(!Je(r)||!Je(i)||!Tr(r,i)||jr(r,i)||Er(r,i)||Ar(r,i)||Nr(r,i)||Cr(r,i)||Sr(r,i))return!1;let u=Object.getPrototypeOf(r);if(u!==null&&typeof u.equals==\"function\")try{if(r.equals(i))continue;return!1}catch{}let[l,o]=Or(r);for(let a of l(r))n.push(o(r,a),o(i,a))}return!0}function Or(e){if(e instanceof Map)return[t=>t.keys(),(t,n)=>t.get(n)];{let t=e instanceof globalThis.Error?[\"message\"]:[];return[n=>[...t,...Object.keys(n)],(n,r)=>n[r]]}}function jr(e,t){return e instanceof Date&&(e>t||e<t)}function Er(e,t){return e.buffer instanceof ArrayBuffer&&e.BYTES_PER_ELEMENT&&!(e.byteLength===t.byteLength&&e.every((n,r)=>n===t[r]))}function Ar(e,t){return Array.isArray(e)&&e.length!==t.length}function Nr(e,t){return e instanceof Map&&e.size!==t.size}function Cr(e,t){return e instanceof Set&&(e.size!=t.size||[...e].some(n=>!t.has(n)))}function Sr(e,t){return e instanceof RegExp&&(e.source!==t.source||e.flags!==t.flags)}function Je(e){return typeof e==\"object\"&&e!==null}function Tr(e,t){return typeof e!=\"object\"&&typeof t!=\"object\"&&(!e||!t)||[Promise,WeakSet,WeakMap,Function].some(r=>e instanceof r)?!1:e.constructor===t.constructor}var A=class extends c{},L=class extends c{},v=class extends c{};var rt=new WeakMap,_e=new DataView(new ArrayBuffer(8)),he=0;function me(e){let t=rt.get(e);if(t!==void 0)return t;let n=he++;return he===2147483647&&(he=0),rt.set(e,n),n}function xe(e,t){return e^t+2654435769+(e<<6)+(e>>2)|0}function we(e){let t=0,n=e.length;for(let r=0;r<n;r++)t=Math.imul(31,t)+e.charCodeAt(r)|0;return t}function it(e){_e.setFloat64(0,e);let t=_e.getInt32(0),n=_e.getInt32(4);return Math.imul(73244475,t>>16^t)^n}function Xr(e){return we(e.toString())}function Qr(e){let t=Object.getPrototypeOf(e);if(t!==null&&typeof t.hashCode==\"function\")try{let r=e.hashCode(e);if(typeof r==\"number\")return r}catch{}if(e instanceof Promise||e instanceof WeakSet||e instanceof WeakMap)return me(e);if(e instanceof Date)return it(e.getTime());let n=0;if(e instanceof ArrayBuffer&&(e=new Uint8Array(e)),Array.isArray(e)||e instanceof Uint8Array)for(let r=0;r<e.length;r++)n=Math.imul(31,n)+y(e[r])|0;else if(e instanceof Set)e.forEach(r=>{n=n+y(r)|0});else if(e instanceof Map)e.forEach((r,i)=>{n=n+xe(y(r),y(i))|0});else{let r=Object.keys(e);for(let i=0;i<r.length;i++){let s=r[i],u=e[s];n=n+xe(y(u),we(s))|0}}return n}function y(e){if(e===null)return 1108378658;if(e===void 0)return 1108378659;if(e===!0)return 1108378657;if(e===!1)return 1108378656;switch(typeof e){case\"number\":return it(e);case\"string\":return we(e);case\"bigint\":return Xr(e);case\"object\":return Qr(e);case\"symbol\":return me(e);case\"function\":return me(e);default:return 0}}var j=5,ge=Math.pow(2,j),Zr=ge-1,en=ge/2,tn=ge/4,h=0,O=1,x=2,T=3,ye={type:x,bitmap:0,array:[]};function J(e,t){return e>>>t&Zr}function re(e,t){return 1<<J(e,t)}function rn(e){return e-=e>>1&1431655765,e=(e&858993459)+(e>>2&858993459),e=e+(e>>4)&252645135,e+=e>>8,e+=e>>16,e&127}function be(e,t){return rn(e&t-1)}function b(e,t,n){let r=e.length,i=new Array(r);for(let s=0;s<r;++s)i[s]=e[s];return i[t]=n,i}function nn(e,t,n){let r=e.length,i=new Array(r+1),s=0,u=0;for(;s<t;)i[u++]=e[s++];for(i[u++]=n;s<r;)i[u++]=e[s++];return i}function $e(e,t){let n=e.length,r=new Array(n-1),i=0,s=0;for(;i<t;)r[s++]=e[i++];for(++i;i<n;)r[s++]=e[i++];return r}function st(e,t,n,r,i,s){let u=y(t);if(u===r)return{type:T,hash:u,array:[{type:h,k:t,v:n},{type:h,k:i,v:s}]};let l={val:!1};return V(ke(ye,e,u,t,n,l),e,r,i,s,l)}function V(e,t,n,r,i,s){switch(e.type){case O:return sn(e,t,n,r,i,s);case x:return ke(e,t,n,r,i,s);case T:return un(e,t,n,r,i,s)}}function sn(e,t,n,r,i,s){let u=J(n,t),l=e.array[u];if(l===void 0)return s.val=!0,{type:O,size:e.size+1,array:b(e.array,u,{type:h,k:r,v:i})};if(l.type===h)return f(r,l.k)?i===l.v?e:{type:O,size:e.size,array:b(e.array,u,{type:h,k:r,v:i})}:(s.val=!0,{type:O,size:e.size,array:b(e.array,u,st(t+j,l.k,l.v,n,r,i))});let o=V(l,t+j,n,r,i,s);return o===l?e:{type:O,size:e.size,array:b(e.array,u,o)}}function ke(e,t,n,r,i,s){let u=re(n,t),l=be(e.bitmap,u);if(e.bitmap&u){let o=e.array[l];if(o.type!==h){let $=V(o,t+j,n,r,i,s);return $===o?e:{type:x,bitmap:e.bitmap,array:b(e.array,l,$)}}let a=o.k;return f(r,a)?i===o.v?e:{type:x,bitmap:e.bitmap,array:b(e.array,l,{type:h,k:r,v:i})}:(s.val=!0,{type:x,bitmap:e.bitmap,array:b(e.array,l,st(t+j,a,o.v,n,r,i))})}else{let o=e.array.length;if(o>=en){let a=new Array(32),$=J(n,t);a[$]=ke(ye,t+j,n,r,i,s);let M=0,E=e.bitmap;for(let fe=0;fe<32;fe++){if(E&1){let vr=e.array[M++];a[fe]=vr}E=E>>>1}return{type:O,size:o+1,array:a}}else{let a=nn(e.array,l,{type:h,k:r,v:i});return s.val=!0,{type:x,bitmap:e.bitmap|u,array:a}}}}function un(e,t,n,r,i,s){if(n===e.hash){let u=Le(e,r);if(u!==-1)return e.array[u].v===i?e:{type:T,hash:n,array:b(e.array,u,{type:h,k:r,v:i})};let l=e.array.length;return s.val=!0,{type:T,hash:n,array:b(e.array,l,{type:h,k:r,v:i})}}return V({type:x,bitmap:re(e.hash,t),array:[e]},t,n,r,i,s)}function Le(e,t){let n=e.array.length;for(let r=0;r<n;r++)if(f(t,e.array[r].k))return r;return-1}function te(e,t,n,r){switch(e.type){case O:return ln(e,t,n,r);case x:return on(e,t,n,r);case T:return an(e,r)}}function ln(e,t,n,r){let i=J(n,t),s=e.array[i];if(s!==void 0){if(s.type!==h)return te(s,t+j,n,r);if(f(r,s.k))return s}}function on(e,t,n,r){let i=re(n,t);if(!(e.bitmap&i))return;let s=be(e.bitmap,i),u=e.array[s];if(u.type!==h)return te(u,t+j,n,r);if(f(r,u.k))return u}function an(e,t){let n=Le(e,t);if(!(n<0))return e.array[n]}function ve(e,t,n,r){switch(e.type){case O:return cn(e,t,n,r);case x:return fn(e,t,n,r);case T:return pn(e,r)}}function cn(e,t,n,r){let i=J(n,t),s=e.array[i];if(s===void 0)return e;let u;if(s.type===h){if(!f(s.k,r))return e}else if(u=ve(s,t+j,n,r),u===s)return e;if(u===void 0){if(e.size<=tn){let l=e.array,o=new Array(e.size-1),a=0,$=0,M=0;for(;a<i;){let E=l[a];E!==void 0&&(o[$]=E,M|=1<<a,++$),++a}for(++a;a<l.length;){let E=l[a];E!==void 0&&(o[$]=E,M|=1<<a,++$),++a}return{type:x,bitmap:M,array:o}}return{type:O,size:e.size-1,array:b(e.array,i,u)}}return{type:O,size:e.size,array:b(e.array,i,u)}}function fn(e,t,n,r){let i=re(n,t);if(!(e.bitmap&i))return e;let s=be(e.bitmap,i),u=e.array[s];if(u.type!==h){let l=ve(u,t+j,n,r);return l===u?e:l!==void 0?{type:x,bitmap:e.bitmap,array:b(e.array,s,l)}:e.bitmap===i?void 0:{type:x,bitmap:e.bitmap^i,array:$e(e.array,s)}}return f(r,u.k)?e.bitmap===i?void 0:{type:x,bitmap:e.bitmap^i,array:$e(e.array,s)}:e}function pn(e,t){let n=Le(e,t);if(n<0)return e;if(e.array.length!==1)return{type:T,hash:e.hash,array:$e(e.array,n)}}function ut(e,t){if(e===void 0)return;let n=e.array,r=n.length;for(let i=0;i<r;i++){let s=n[i];if(s!==void 0){if(s.type===h){t(s.v,s.k);continue}ut(s,t)}}}var S=class e{static fromObject(t){let n=Object.keys(t),r=e.new();for(let i=0;i<n.length;i++){let s=n[i];r=r.set(s,t[s])}return r}static fromMap(t){let n=e.new();return t.forEach((r,i)=>{n=n.set(i,r)}),n}static new(){return new e(void 0,0)}constructor(t,n){this.root=t,this.size=n}get(t,n){if(this.root===void 0)return n;let r=te(this.root,0,y(t),t);return r===void 0?n:r.v}set(t,n){let r={val:!1},i=this.root===void 0?ye:this.root,s=V(i,0,y(t),t,n,r);return s===this.root?this:new e(s,r.val?this.size+1:this.size)}delete(t){if(this.root===void 0)return this;let n=ve(this.root,0,y(t),t);return n===this.root?this:n===void 0?e.new():new e(n,this.size-1)}has(t){return this.root===void 0?!1:te(this.root,0,y(t),t)!==void 0}entries(){if(this.root===void 0)return[];let t=[];return this.forEach((n,r)=>t.push([r,n])),t}forEach(t){ut(this.root,t)}hashCode(){let t=0;return this.forEach((n,r)=>{t=t+xe(y(n),y(r))|0}),t}equals(t){if(!(t instanceof e)||this.size!==t.size)return!1;try{return this.forEach((n,r)=>{if(!f(t.get(r,!n),n))throw nt}),!0}catch(n){if(n===nt)return!1;throw n}}},nt=Symbol();var lt=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),ns=new RegExp(`^[${lt}]*`),is=new RegExp(`[${lt}]*$`);function P(){return S.new()}var je=class extends c{constructor(t){super(),this.dict=t}};function _t(){return new je(P())}var ul=S.new();var ll=_t();var yl=new v,bl=new A,kl=new L;var gt=0,yt=1,bt=1,kt=1,Lt=2;var vt=1,Ot=2,jt=0;var Et=1;var At=0;var Ce=2,Nt=1,R=1,Se=2,Te=3,Ie=4,Ct=5,St=2,Tt=1,Me=2,qe=3,ze=4,It=5,Mt=3;var qt=1,zt=2,Dt=0;var De=1,Bt=2,Ut=1;var Rt=1,Ft=2,Pt=2;var Y=1;var Be=3,Gt=4,Ht=5,Wt=0,Jt=1,Ue=2,Re=3,Vt=0,Yt=1,Kt=1;var Xt=1,Qt=2;var Zt=1,er=3;var tr=1,rr=2,nr=4;var ir=1,sr=2,ur=3,lr=5;var or=1,ar=2,cr=6,fr=1,pr=2,dr=7;var _r=1,hr=2;var d=Symbol(\"metadata\"),ae=class{#t=null;#e=()=>{};#r=[];constructor(t,n){this.#t=t,this.#e=n}mount(t){this.#t.appendChild(ce(t,this.#e,this.#t))}push(t){this.#r.push({node:this.#t,patch:t}),this.#n()}#n(){for(;this.#r.length;){let{node:t,patch:n}=this.#r.pop();for(let r=0;r<n[Ue].length;r++){let i=n[Ue][r];switch(i[0]){case cr:xr(t,i[fr],i[pr],this.#e,this.#t);break;case nr:Rn(t,i[ir],i[sr],i[ur]);break;case lr:Fn(t,i[or],i[ar]);break;case dr:Pn(t,i[_r],i[hr]);break;case Vt:Gn(t,i[Yt],this.#e,this.#t);break;case Kt:Hn(t,i[Xt]);break;case Qt:$r(t,i[Zt]);break;case er:Wn(t,i[tr],i[rr],this.#e,this.#t);break}}for(let r=0;r<n[Jt];++r){let i=t.lastChild,s=i[d].key;s&&t[d].keyedChildren.delete(s),t.removeChild(i)}for(let r=0;r<n[Re].length;r++){let i=n[Re][r];this.#r.push({node:t.childNodes[i[Wt]],patch:i})}}}};function xr(e,t,n,r,i){let s=document.createDocumentFragment();for(let u=0;u<t.length;u++){let l=t[u],o=ce(l,r,i);if(l[R]){let a=new WeakRef(wr(o));e[d].keyedChildren.set(l[R],a)}s.appendChild(o)}e.insertBefore(s,e.childNodes[n]??null)}function Rn(e,t,n,r){let i=e[d].keyedChildren.get(t).deref();if(r>1){let s=document.createDocumentFragment();for(let u=0;u<r&&i!==null;++u){let l=i.nextSibling;s.append(i),i=l}i=s}e.insertBefore(i,e.childNodes[n]??null)}function Fn(e,t,n){let r=e[d].keyedChildren.get(t).deref();for(e[d].keyedChildren.delete(t);n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Pn(e,t,n){let r=e.childNodes[t];for(;n-- >0&&r!==null;){let i=r.nextSibling;e.removeChild(r),r=i}}function Gn(e,t,n,r){let i=ce(t,n,r),s=e.parentNode;if(t[R]){let u=new WeakRef(wr(i));s[d].keyedChildren.set(t[R],u)}s.replaceChild(i,e)}function Hn(e,t){e.data=t}function $r(e,t){e.innerHTML=t}function Wn(e,t,n,r,i){for(let s=0;s<n.length;s++){let u=n[s][De];e[d].handlers.has(u)?(e.removeEventListener(u,gr),e[d].handlers.delete(u)):(e.removeAttribute(u),yr[u]?.removed?.(e,u))}for(let s=0;s<t.length;s++)Fe(e,t[s],r,i)}function wr(e){for(;e.nodeType===DocumentFragment.DOCUMENT_FRAGMENT_NODE;)e=e.firstChild;return e}function ce(e,t,n){switch(e[0]){case Nt:{let r=e[Se]?document.createElementNS(e[Se],e[Te]):document.createElement(e[Te]);r[d]={key:e[R],keyedChildren:new Map,handlers:new Map};for(let i=0;i<e[Ie].length;i++)Fe(r,e[Ie][i],t,n);return xr(r,e[Ct],0,t,n),r}case Mt:{let r=document.createTextNode(e[zt]);return r[d]={key:e[qt]},r}case At:{let r=document.createDocumentFragment();for(let i=0;i<e[Ce].length;i++)r.appendChild(ce(e[Ce][i],t,n));return r}case St:{let r=e[Me]?document.createElementNS(e[Me],e[qe]):document.createElement(e[qe]);r[d]={key:e[Tt],handlers:new Map};for(let i=0;i<e[ze].length;i++)Fe(r,e[ze][i],t,n);return $r(r,e[It]),r}}}function Fe(e,t,n,r){switch(t[0]){case Dt:{let i=t[De],s=t[Bt];s!==e.getAttribute(i)&&e.setAttribute(i,s),yr[i]?.added?.(e,s)}break;case Ut:e[t[Rt]]=t[Ft];break;case Pt:{e[d].handlers.has(t[Y])||e.addEventListener(t[Y],gr,{passive:!t[Be]});let i=t[Be],s=t[Gt],u=t[Ht]||Vn.includes(t[Y]);e[d].handlers.set(t[Y],l=>{i&&l.preventDefault(),s&&l.stopPropagation();let o=l.target,a=o[d].key||[].indexOf.call(o.parentNode.childNodes,o).toString();for(o=o.parentNode;o!==r;){let $=o[d].key;$?a=`${$}.${a}`:a=`${[].indexOf.call(o.parentNode.childNodes,o)}.${a}`,o=o.parentNode}n(l,a,l.type,u)})}break}}function gr(e){e.currentTarget[d].handlers.get(e.type)(e)}var yr={checked:mr(\"checked\"),selected:mr(\"selected\"),value:Jn(\"value\"),autofocus:{added(e){e.focus?.()}},autoplay:{added(e){e.play?.()}}};function mr(e){return{added(t,n){t[e]=!0},removed(t){t[e]=!1}}}function Jn(e){return{added(t,n){t[e]=n}}}var Vn=[\"input\",\"change\",\"focusin\",\"focusout\",\"focus\",\"blur\",\"select\"];var sa=Symbol(\"metadata\");var ua={checked:br(\"checked\"),selected:br(\"selected\"),value:Yn(\"value\"),autofocus:{added(e){e.focus?.()}},autoplay:{added(e){e.play?.()}}};function br(e){return{added(t,n){t[e]=!0},removed(t){t[e]=!1}}}function Yn(e){return{added(t,n){t[e]=n}}}var kr=new WeakMap;async function Lr(e){let t=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||t.push(new Promise((i,s)=>{r.addEventListener(\"load\",i),r.addEventListener(\"error\",s)}));await Promise.allSettled(t),e.adoptedStyleSheets=e.host.getRootNode().adoptedStyleSheets;let n=[];for(let r of document.styleSheets)try{e.adoptedStyleSheets.push(r)}catch{try{let i=kr.get(r);if(!i){i=new CSSStyleSheet;for(let s of r.cssRules)i.insertRule(s.cssText,i.cssRules.length);kr.set(r,i)}e.adoptedStyleSheets.push(i)}catch{let i=r.ownerNode.cloneNode();e.prepend(i),n.push(i)}}return n}var Pe=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new WebSocket(this.#t),this.#e.onmessage=({data:r})=>{try{n(JSON.parse(r))}catch{}}}send(t){this.#e.send(JSON.stringify(t))}close(){this.#e.close()}},Ge=class{#t;#e;constructor(t,n,{}){this.#t=t,this.#e=new EventSource(t),this.#e.onmessage=({data:r})=>{try{n(JSON.parse(r))}catch{}}}send(t){}close(){this.#e.close()}},He=class{#t;#e;#r;#n;constructor(t,n,r={}){this.#t=t,this.#e=n,this.#r=r.interval??5e3,this.#i().finally(()=>{this.#n=window.setInterval(()=>this.#i(),this.#r)})}async send(t){}close(){clearInterval(this.#n)}#i(){return fetch(this.#t).then(t=>t.json()).then(this.#e).catch(console.error)}},We=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u;#o=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new ae(this.shadowRoot,(t,n,r)=>{this.#r?.send([Et,n,r,t])}),this.#u=new MutationObserver(t=>{let n=[];for(let r of t){if(r.type!==\"attributes\")continue;let i=r.attributeName;this.#o.includes(i)&&n.push([i,this.getAttribute(i)])}n.length&&this.#r?.send([jt,n])})}connectedCallback(){this.#l(),this.#u.observe(this,{attributes:!0}),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#s())}adoptedCallback(){this.#l()}attributeChangedCallback(t,n,r){switch(t){case n!==r:{this.#e=new URL(r,window.location.href),this.#s();return}case\"method\":{let i=r.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\",\"http\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#s());return}}}eventReceivedCallback(t,n,r){this.#r?.send(\"hi!\")}messageReceivedCallback(t){switch(t[0]){case gt:{for(;this.shadowRoot.children[this.#n.length];)this.shadowRoot.children[this.#n.length].remove();this.#i.mount(t[yt]);break}case bt:{this.#i.push(t[kt]);break}case Lt:{this.dispatchEvent(new CustomEvent(t[vt],{detail:t[Ot]}));break}}}#s(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let t=n=>{this.messageReceivedCallback(n)};switch(this.#t){case\"ws\":this.#r=new Pe(this.#e,t,{});break;case\"sse\":this.#r=new Ge(this.#e,t,{});break;case\"polling\":this.#r=new He(this.#e,t,{});break}}async#l(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await Lr(this.shadowRoot)}};window.customElements.define(\"lustre-server-component\",We);export{We as ServerComponent};\\n",
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
@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
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
