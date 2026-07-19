//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (10kb!) client runtime that patches the DOM and events are sent back to the
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
////         #(model, Effect(message))  |        ^          Model
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
//// > **Note**: Lustre's server component runtime is separate from your application's
//// > WebSocket server. You're free to bring your own stack, connect multiple
//// > clients to the same Lustre instance, or keep the application alive even when
//// > no clients are connected.
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
//// best ways to use them and show them off. Here are a few examples we've
//// developed so far:
////
//// - [Basic setup](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)
////
//// - [Custom attributes and events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/02-attributes-and-events)
////
//// - [Decoding DOM events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/03-event-include)
////
//// - [Connecting more than one client](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/04-multiple-clients)
////
//// - [Adding publish-subscribe](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/05-publish-subscribe)
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import agnostic/attribute.{type Attribute, attribute}
import agnostic/effect.{type Effect}
import agnostic/element.{type Element}
import agnostic/element/html
import agnostic/runtime/headless
import agnostic/runtime/transport
import agnostic/serializer.{type Serializer}
import agnostic/vdom/vattr.{Event}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}

@target(erlang)
import agnostic.{type Runtime, type RuntimeMessage}
@target(erlang)
import gleam/erlang/process.{type Pid, type Selector, type Subject}

// We don't want users of the JavaScript target to see warnings about an unused
// `Pid` type so we use target-specific imports to only pull in the types we need
// for each target.
@target(javascript)
import agnostic.{type RuntimeMessage}
@target(javascript)
import gleam/erlang/process.{type Selector, type Subject}

// TYPES -----------------------------------------------------------------------

/// A type representing the messages sent to the server component _client_
/// runtime. This instruct the client runtime to do things like update the DOM
/// or emit an event from the element.
///
pub type ClientMessage(message) =
  transport.ClientMessage(message)

/// The type of transport the client runtime should use to communicate with your
/// server component. This is set by the [`method`](#method) attribute on the
/// server component element.
///
pub type TransportMethod {
  WebSocket
  ServerSentEvents
  Polling
}

// ELEMENTS --------------------------------------------------------------------

/// Render the server component custom element. This element acts as the thin
/// client runtime for a server component running remotely. There are a handful
/// of attributes you should provide to configure the client runtime:
///
/// - [`route`](#route) is the URL your server component should connect to. This
///   **must** be provided before the client runtime will do anything. The route
///   can be a relative URL, in which case it will be resolved against the current
///   page URL.
///
/// - [`method`](#method) is the transport method the client runtime should use.
///   This defaults to `WebSocket` enabling duplex communication between the client
///   and server runtime. Other options include `ServerSentEvents` and `Polling`
///   which are unidirectional transports.
///
/// > **Note**: the server component runtime bundle must be included and sent to
/// > the client for this to work correctly. You can do this by including the
/// > JavaScript bundle found in Lustre's `priv/static` directory or by inlining
/// > the script source directly with the [`script`](#script) element below.
///
pub fn element(
  attributes: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("lustre-server-component", attributes, children)
}

/// Inline the server component client runtime as a `<script>` tag. Where possible
/// you should prefer serving the pre-built client runtime from Lustre's `priv/static`
/// directory, but this inline script can be useful for development or scenarios
/// where you don't control the HTML document.
///
pub fn script() -> Element(message) {
  html.script(
    [attribute.type_("module")],
    // <<INJECT RUNTIME>>
    "function kt(n){return n.replaceAll(/[><&\"']/g,e=>{switch(e){case\">\":return\"&gt;\";case\"<\":return\"&lt;\";case\"'\":return\"&#39;\";case\"&\":return\"&amp;\";case'\"':return\"&quot;\";default:return e}})}function vt(n){return kt(n)}function Le(n){return vt(n)}var m=class{withFields(e){let t=Object.keys(this).map(r=>r in e?e[r]:this[r]);return new this.constructor(...t)}},oe=class{static fromArray(e,t){let r=t||new p;for(let i=e.length-1;i>=0;--i)r=new ce(e[i],r);return r}[Symbol.iterator](){return new Ue(this)}toArray(){return[...this]}atLeastLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return t!==void 0}hasLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return e===-1&&t instanceof p}countLength(){let e=this,t=0;for(;e;)e=e.tail,t++;return t-1}};function f(n,e){return new ce(n,e)}function $(n,e){return oe.fromArray(n,e)}var Ue=class{#i;constructor(e){this.#i=e}next(){if(this.#i instanceof p)return{done:!0};{let{head:e,tail:t}=this.#i;return this.#i=t,{value:e,done:!1}}}},p=class extends oe{};var ce=class extends oe{constructor(e,t){super(),this.head=e,this.tail=t}},Se=(n,e)=>new ce(n,e);var Fe=n=>n.head,Me=n=>n.tail;var de=class n extends m{static isResult(e){return e instanceof n}},w=class extends de{constructor(e){super(),this[0]=e}isOk(){return!0}},K=n=>new w(n),Q=n=>n instanceof w,X=n=>n[0],j=class extends de{constructor(e){super(),this[0]=e}isOk(){return!1}},Y=n=>new j(n);var v=class extends m{},Et=()=>new v;var E=class extends m{},jt=()=>new E;var Z=class extends m{},Ct=()=>new Z;var Yn=5,Fi=(1<<Yn)-1,Vi=Symbol(),Hi=Symbol();var k=class extends m{},ee=class extends m{};function me(n,e){for(;;){let t=n,r=e;if(t instanceof p)return r;{let i=t.head;n=t.tail,e=f(i,r)}}}function S(n){return me(n,$([]))}function rr(n,e,t,r){for(;;){let i=n,s=e,c=t,o=r;if(i instanceof p)return me(s,o);if(s instanceof p)return me(i,o);{let a=i.head,l=i.tail,d=s.head,_=s.tail,h=c(a,d);h instanceof v?(n=i,e=_,t=c,r=f(d,o)):h instanceof E?(n=l,e=s,t=c,r=f(a,o)):(n=l,e=s,t=c,r=f(a,o))}}}function ir(n,e,t){for(;;){let r=n,i=e,s=t;if(r instanceof p)return S(s);{let c=r.tail;if(c instanceof p){let o=r.head;return S(f(S(o),s))}else{let o=r.head,a=c.head,l=c.tail,d=rr(o,a,i,$([]));n=l,e=i,t=f(d,s)}}}}function sr(n,e,t,r){for(;;){let i=n,s=e,c=t,o=r;if(i instanceof p)return me(s,o);if(s instanceof p)return me(i,o);{let a=i.head,l=i.tail,d=s.head,_=s.tail,h=c(a,d);h instanceof v?(n=l,e=s,t=c,r=f(a,o)):h instanceof E?(n=i,e=_,t=c,r=f(d,o)):(n=i,e=_,t=c,r=f(d,o))}}}function or(n,e,t){for(;;){let r=n,i=e,s=t;if(r instanceof p)return S(s);{let c=r.tail;if(c instanceof p){let o=r.head;return S(f(S(o),s))}else{let o=r.head,a=c.head,l=c.tail,d=sr(o,a,i,$([]));n=l,e=i,t=f(d,s)}}}}function cr(n,e,t){for(;;){let r=n,i=e,s=t;if(r instanceof p)return r;if(i instanceof k){if(r.tail instanceof p)return r.head;n=or(r,s,$([])),e=new ee,t=s}else if(r.tail instanceof p){let o=r.head;return S(o)}else n=ir(r,s,$([])),e=new k,t=s}}function ar(n,e,t,r,i,s){for(;;){let c=n,o=e,a=t,l=r,d=i,_=s,h=f(d,a);if(c instanceof p)return l instanceof k?f(S(h),_):f(h,_);{let x=c.head,u=c.tail,g=o(d,x);if(l instanceof k)if(g instanceof v)n=u,e=o,t=h,r=l,i=x,s=_;else if(g instanceof E)n=u,e=o,t=h,r=l,i=x,s=_;else{let b;l instanceof k?b=f(S(h),_):b=f(h,_);let B=b;if(u instanceof p)return f($([x]),B);{let N=u.head,L=u.tail,y,z=o(x,N);z instanceof v?y=new k:z instanceof E?y=new k:y=new ee;let J=y;n=L,e=o,t=$([x]),r=J,i=N,s=B}}else if(g instanceof v){let b;l instanceof k?b=f(S(h),_):b=f(h,_);let B=b;if(u instanceof p)return f($([x]),B);{let N=u.head,L=u.tail,y,z=o(x,N);z instanceof v?y=new k:z instanceof E?y=new k:y=new ee;let J=y;n=L,e=o,t=$([x]),r=J,i=N,s=B}}else if(g instanceof E){let b;l instanceof k?b=f(S(h),_):b=f(h,_);let B=b;if(u instanceof p)return f($([x]),B);{let N=u.head,L=u.tail,y,z=o(x,N);z instanceof v?y=new k:z instanceof E?y=new k:y=new ee;let J=y;n=L,e=o,t=$([x]),r=J,i=N,s=B}}else n=u,e=o,t=h,r=l,i=x,s=_}}}function Tt(n,e){if(n instanceof p)return n;{let t=n.tail;if(t instanceof p)return n;{let r=n.head,i=t.head,s=t.tail,c,o=e(r,i);o instanceof v?c=new k:o instanceof E?c=new k:c=new ee;let a=c,l=ar(s,e,$([r]),a,i,$([]));return cr(l,new k,e)}}}var Ot=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Ds=new RegExp(`^[${Ot}]*`),Ps=new RegExp(`[${Ot}]*$`);var M=$([]);function te(){return null}function ge(n,e){return n&&n.has(e)}function be(n,e,t){return n??=new Map,n.set(e,t),n}var Ar=Ct(),Tr=Et(),Br=jt();function Qe(n,e){return n.name===e.name?Br:n.name<e.name?Tr:Ar}var D=class extends m{constructor(e,t,r){super(),this.kind=e,this.name=t,this.value=r}};var Xe=0,Qt=1,Xt=2,Yt=0;var Ye=2;function Ze(n,e){return new D(Xe,n,e)}function zr(n,e){for(;;){let t=n,r=e;if(t instanceof p)return r;{let i=t.head;if(i instanceof D){let s=i.name;if(s===\"\")n=t.tail,e=r;else if(s===\"class\"){let c=i.value;if(c===\"\")n=t.tail,e=r;else{let o=t.tail;if(o instanceof p){let a=i;n=o,e=f(a,r)}else{let a=o.head;if(a instanceof D)if(a.name===\"class\"){let d=i.kind,_=c,h=o.tail,x=a.value,u=_+\" \"+x,g=new D(d,\"class\",u);n=f(g,h),e=r}else{let d=i;n=o,e=f(d,r)}else{let l=i;n=o,e=f(l,r)}}}}else if(s===\"style\"){let c=i.value;if(c===\"\")n=t.tail,e=r;else{let o=t.tail;if(o instanceof p){let a=i;n=o,e=f(a,r)}else{let a=o.head;if(a instanceof D)if(a.name===\"style\"){let d=i.kind,_=c,h=o.tail,x=a.value,u=_+\";\"+x,g=new D(d,\"style\",u);n=f(g,h),e=r}else{let d=i;n=o,e=f(d,r)}else{let l=i;n=o,e=f(l,r)}}}}else{let c=i;n=t.tail,e=f(c,r)}}else{let s=i;n=t.tail,e=f(s,r)}}}}function Zt(n){if(n instanceof p)return n;if(n.tail instanceof p)return n;{let r=Tt(n,(i,s)=>Qe(s,i));return zr(r,M)}}var F=class extends m{constructor(e,t,r,i){super(),this.kind=e,this.key=t,this.children=r,this.keyed_children=i}};var V=class extends m{constructor(e,t,r,i,s,c,o){super(),this.kind=e,this.key=t,this.namespace=r,this.tag=i,this.attributes=s,this.children=c,this.keyed_children=o}};var H=class extends m{constructor(e,t,r){super(),this.kind=e,this.key=t,this.content=r}};var ne=class extends m{constructor(e,t,r,i,s,c,o){super(),this.kind=e,this.key=t,this.namespace=r,this.tag=i,this.attributes=s,this.content=c,this.compare=o}};var re=class extends m{constructor(e,t,r,i){super(),this.kind=e,this.key=t,this.content=r,this.compare=i}};var P=class extends m{constructor(e,t,r,i){super(),this.kind=e,this.key=t,this.mapper=r,this.child=i}};var ue=class extends m{constructor(e,t,r,i){super(),this.kind=e,this.key=t,this.dependencies=r,this.view=i}};var ie=0,W=1,se=2,tn=3,et=6,G=4,tt=5;function nt(n,e,t){return new F(ie,n,e,t)}function ve(n,e,t,r,i,s){return new V(W,n,e,t,Zt(r),i,s)}function rt(n,e){return new H(se,n,e)}function nn(n,e){if(n instanceof P){let t=n.mapper;return new P(G,n.key,r=>e(t(r)),n.child)}else return new P(G,n.key,e,n)}function rn(n,e,t){return new ue(tt,n,e,t)}function Re(n,e){if(e instanceof F)return new F(e.kind,n,e.children,e.keyed_children);if(e instanceof V)return new V(e.kind,n,e.namespace,e.tag,e.attributes,e.children,e.keyed_children);if(e instanceof H)return new H(e.kind,n,e.content);if(e instanceof ne)return new ne(e.kind,n,e.namespace,e.tag,e.attributes,e.content,e.compare);if(e instanceof re)return new re(e.kind,n,e.content,e.compare);if(e instanceof P){let t=e.child;return new P(e.kind,n,e.mapper,Re(n,t))}else{let t=e.view;return new ue(e.kind,n,e.dependencies,()=>Re(n,t()))}}var sn=0,on=1,cn=7,an=2,ln=3,un=4,fn=5,pn=6;var hn=\"\\r\",_n=\"	\";var it=n=>n.reduceRight((e,t)=>Se(t,e),M),I=(n,e)=>{if(Array.isArray(n))for(let t=0;t<n.length;t++)e(n[t]);else if(n)for(n;Me(n);n=Me(n))e(Fe(n))};var fe=\"http://www.w3.org/1999/xhtml\";var xn=!!globalThis.HTMLElement?.prototype?.moveBefore;var Dr=globalThis.setTimeout,st=globalThis.clearTimeout,R=n=>n!=null?K(n):Y(void 0),ct=()=>{},$n=globalThis.process?.env?.AGNOSTIC_DEBUG_LOG;if($n)try{let n=await import(\"node:fs\");ct=e=>{try{n.appendFileSync($n,e+`\\n`)}catch{}}}catch{}var q=n=>{let e=[],t=n;for(;t&&t.head!==void 0;)e.push(t.head),t=t.tail;return e},A=Symbol(\"lustre\"),at=class{constructor(e,t,r,i){this.kind=e,this.key=i,this.parent=t,this.children=[],this.node=r,this.endNode=null,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get isVirtual(){return this.kind===ie||this.kind===G}get parentNode(){return this.isVirtual?this.node.parentNode:this.node}};var T=(n,e,t,r,i)=>{let s=new at(n,e,t,i);return t[A]=s,e?.children.splice(r,0,s),s},Pr=n=>{let e=\"\";for(let t=n[A];t.parent;t=t.parent){let r=t.parent&&t.parent.kind===G?hn:_n;if(t.key)e=`${r}${t.key}${e}`;else{let i=t.parent.children.indexOf(t);e=`${r}${i}${e}`}}return e.slice(1)},je=class{#i=null;#t;#n;#e;#r=!1;constructor(e,t,r,i,{debug:s=!1}={}){this.#i=e,this.#t=t,this.#n=r,this.#e=i,this.#r=s}mount(e){T(W,null,this.#i,0,null),this.#m(this.#i,null,this.#i[A],0,e)}push(e,t=null){this.#s=t,this.#o.push({node:this.#i[A],patch:e}),this.#c()}#s;#o=[];#c(){let e=this.#o;for(;e.length;){let{node:t,patch:r}=e.pop(),{path:i,changes:s,removed:c,children:o}=r;I(i,l=>{if(t===void 0||t.children?.[l]===void 0){let d=q(r.changes).map(u=>{let g={kind:u.kind};return u.index!==void 0&&(g.index=u.index),u.before!==void 0&&(g.before=u.before),u.key!==void 0&&(g.key=u.key),u.added!==void 0&&(g.added=q(u.added).map(b=>b.name)),u.removed!==void 0&&(g.removed=q(u.removed).map(b=>b.name)),g}),_=q(r.children).map(u=>({index:u.index,pathLen:q(u.path).length,changesLen:q(u.changes).length})),h=[];for(let u=t;u;u=u.parent)h.push({kind:u.kind,key:u.key,childrenLen:u.children?.length});let x=e.map(u=>({patchPath:q(u.patch.path),patchIndex:u.patch.index,patchChangeKinds:q(u.patch.changes).map(g=>g.kind),patchChildrenCount:q(u.patch.children).length,nodeKind:u.node?.kind,nodeKey:u.node?.key}));ct(`[lustre] path descent overruns metadata ${JSON.stringify({patchPath:q(r.path),patchIndex:r.index,patchChanges:d,patchChildren:_,stepIndex:l,nodeKind:t?.kind,nodeKey:t?.key,childrenLen:t?.children?.length,nodeChildKinds:t?.children?.map(u=>u?.kind),ancestors:h,pendingStack:x})}`)}t=t.children[l]});let{children:a}=t;I(s,l=>this.#l(t,l)),c&&this.#_(t,a.length-c,c),I(o,l=>{let d=l.index|0,_=a[d];_===void 0&&ct(`[lustre] child patch index resolves to undefined ${JSON.stringify({patchPath:r.path,index:d,childNodesLength:a.length})}`),this.#o.push({node:_,patch:l})})}}#l(e,t){switch(t.kind){case sn:this.#j(e,t);break;case on:this.#w(e,t);break;case cn:this.#C(e,t);break;case an:this.#E(e,t);break;case ln:this.#p(e,t);break;case un:this.#h(e,t);break;case fn:this.#f(e,t);break;case pn:this.#a(e,t);break}}#a(e,{children:t,before:r}){let i=this.#e.create_fragment(),s=this.#u(e,r);this.#b(i,null,e,r|0,t),this.#e.insert_before(e.parentNode,i,R(s))}#f(e,{index:t,with:r}){let i=this.#u(e,(t|0)+1);this.#_(e,t|0,1),this.#m(e.parentNode,i,e,t|0,r)}#u(e,t){t=t|0;let{children:r}=e,i=r.length;if(t<i)return r[t].node;if(e.endNode)return e.endNode;if(!e.isVirtual)return null;for(;e.isVirtual&&e.children.length;){if(e.endNode){let c=this.#e.next_sibling(e.endNode);return Q(c)?X(c):null}e=e.children[e.children.length-1]}let s=this.#e.next_sibling(e.node);return Q(s)?X(s):null}#p(e,{key:t,before:r}){r=r|0;let{children:i,parentNode:s}=e,c=i[r].node,o=i[r];for(let a=r+1;a<i.length;++a){let l=i[a];if(i[a]=o,o=l,l.key===t){i[r]=l;break}}this.#d(s,o,c)}#g(e,t,r){for(let i=0;i<t.length;++i)this.#d(e,t[i],r)}#d(e,t,r){this.#e.move_before(e,t.node,R(r)),t.isVirtual&&this.#g(e,t.children,r),t.endNode&&this.#e.move_before(e,t.endNode,R(r))}#h(e,{index:t}){this.#_(e,t,1)}#_(e,t,r){let{children:i,parentNode:s}=e,c=i.splice(t,r);for(let o=0;o<c.length;++o){let a=c[o],{node:l,endNode:d,isVirtual:_,children:h}=a;this.#e.remove_child(s,l),d&&this.#e.remove_child(s,d),this.#x(a),_&&c.push(...h)}}#x(e){let{debouncers:t,children:r}=e;for(let{timeout:i}of t.values())i&&st(i);t.clear(),I(r,i=>this.#x(i))}#E({node:e,handlers:t,throttles:r,debouncers:i},{added:s,removed:c}){I(c,({name:o})=>{t.delete(o)?(this.#e.remove_event_listener(e,o,ot),this.#$(r,o,0),this.#$(i,o,0)):(this.#e.remove_attribute(e,o),bn[o]?.removed?.(e,o))}),I(s,o=>this.#v(e,o))}#j({node:e},{content:t}){this.#e.set_text(e,t??\"\")}#w({node:e},{content:t}){this.#e.set_raw_content(e,t??\"\")}#C(e,{with:t}){let r=e.parent.parentNode,i=e.node,s=this.#e.create_raw_node(t.content);this.#e.insert_before(r,s,R(i)),this.#e.remove_child(r,i),s[A]=e,e.node=s}#b(e,t,r,i,s){I(s,c=>this.#m(e,t,r,i++,c))}#m(e,t,r,i,s){switch(s.kind){case W:{let c=this.#y(r,i,s);this.#e.insert_before(e,c,R(t)),this.#b(c,null,c[A],0,s.children);break}case se:{let c=this.#S(r,i,s);this.#e.insert_before(e,c,R(t));break}case ie:{let c=\"lustre:fragment\",o=this.#k(c,r,i,s);this.#e.insert_before(e,o,R(t)),this.#b(e,t,o[A],0,s.children),this.#r&&(o[A].endNode=this.#e.create_comment(` /${c} `),this.#e.insert_before(e,o[A].endNode,R(t)));break}case tn:{let c=this.#y(r,i,s);this.#e.insert_before(e,c,R(t)),this.#w({node:c},s);break}case et:{let c=this.#e.create_raw_node(s.content);T(et,r,c,i,s.key),this.#e.insert_before(e,c,R(t));break}case G:{let c=this.#k(\"lustre:map\",r,i,s);this.#e.insert_before(e,c,R(t)),this.#m(e,t,c[A],0,s.child);break}case tt:{let c=this.#s?.get(s.view)??s.view();this.#m(e,t,r,i,c);break}}}#y(e,t,{kind:r,key:i,tag:s,namespace:c,attributes:o}){let a=this.#e.create_element(c||fe,s);return T(r,e,a,t,i),this.#r&&i&&this.#e.set_attribute(a,\"data-lustre-key\",i),I(o,l=>this.#v(a,l)),a}#S(e,t,{kind:r,key:i,content:s}){let c=this.#e.create_text_node(s??\"\");return T(r,e,c,t,i),c}#k(e,t,r,{kind:i,key:s}){let c=this.#r?this.#e.create_comment(Ir(e,s)):this.#e.create_text_node(\"\");return T(i,t,c,r,s),c}#v(e,t){let{debouncers:r,handlers:i,throttles:s}=e[A],{kind:c,name:o,value:a,prevent_default:l,debounce:d,throttle:_}=t;switch(c){case Xe:{let h=a??\"\";if(o===\"virtual:defaultValue\"){this.#e.set_property(e,\"defaultValue\",h);return}else if(o===\"virtual:defaultChecked\"){this.#e.set_property(e,\"defaultChecked\",!0);return}else if(o===\"virtual:defaultSelected\"){this.#e.set_property(e,\"defaultSelected\",!0);return}let x=this.#e.get_attribute(e,o),u=Q(x)?X(x):null;h!==u&&this.#e.set_attribute(e,o,h),bn[o]?.added?.(e,h);break}case Qt:this.#e.set_property(e,o,a);break;case Xt:{i.has(o)&&this.#e.remove_event_listener(e,o,ot);let h=l.kind===Yt;this.#e.add_event_listener(e,o,ot,h),this.#$(s,o,_),this.#$(r,o,d),i.set(o,x=>this.#M(t,x));break}}}#$(e,t,r){let i=e.get(t);if(r>0)i?i.delay=r:e.set(t,{delay:r});else if(i){let{timeout:s}=i;s&&st(s),e.delete(t)}}#M(e,t){let{currentTarget:r,type:i}=t,{debouncers:s,throttles:c}=r[A],o=Pr(r),{prevent_default:a,stop_propagation:l,include:d}=e;a.kind===Ye&&t.preventDefault(),l.kind===Ye&&t.stopPropagation(),i===\"submit\"&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let _=this.#t(t,o,i,d),h=c.get(i);if(h){let u=Date.now(),g=h.last||0;u>g+h.delay&&(h.last=u,h.lastEvent=t,this.#n(t,_))}let x=s.get(i);x&&(st(x.timeout),x.timeout=Dr(()=>{t!==c.get(i)?.lastEvent&&this.#n(t,_)},x.delay)),!h&&!x&&this.#n(t,_)}},Ir=(n,e)=>e?` ${n} key=\"${Le(e)}\" `:` ${n} `,ot=n=>{let{currentTarget:e,type:t}=n;e[A].handlers.get(t)(n)},gn=n=>({added(e){e[n]=!0},removed(e){e[n]=!1}}),qr=n=>({added(e,t){e[n]=t}}),bn={checked:gn(\"checked\"),selected:gn(\"selected\"),value:qr(\"value\"),autofocus:{added(n){queueMicrotask(()=>{n.focus?.()})}},autoplay:{added(n){try{n.play?.()}catch(e){console.error(e)}}}};var vn=new WeakMap;async function En(n){let e=[];for(let r of globalThis.document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((i,s)=>{r.addEventListener(\"load\",i),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!n.host.isConnected)return[];n.adoptedStyleSheets=n.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of globalThis.document.styleSheets)try{n.adoptedStyleSheets.push(r)}catch{try{let i=vn.get(r);if(!i){i=new CSSStyleSheet;for(let s of r.cssRules)i.insertRule(s.cssText,i.cssRules.length);vn.set(r,i)}n.adoptedStyleSheets.push(i)}catch{let i=r.ownerNode.cloneNode();n.prepend(i),t.push(i)}}return t}var De=class extends Event{constructor(e,t,r){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=r}},Pe=class extends CustomEvent{isLustreEvent=!0;constructor(e,t){super(e,{detail:t,bubbles:!0,composed:!0})}};function jn(n,e){return Ze(n,e)}function Cn(n){return rt(\"\",n)}function Sn(){return rt(\"\",\"\")}function Mn(n,e){return rn(\"\",n,e)}function An(n){return n}function Tn(n,e){return nn(n,e)}var ut=class extends m{constructor(e,t){super(),this.name=e,this.schedule=t}},pt=(n,e)=>new ut(n,e);var ft=class extends m{constructor(e,t,r,i,s,c,o,a,l,d,_,h,x,u,g,b,B,N,L,y,z,J){super(),this.target=e,this.mount=t,this.create_element=r,this.create_text_node=i,this.create_fragment=s,this.create_comment=c,this.insert_before=o,this.move_before=a,this.remove_child=l,this.next_sibling=d,this.get_attribute=_,this.set_attribute=h,this.remove_attribute=x,this.set_property=u,this.set_text=g,this.set_raw_content=b,this.create_raw_node=B,this.add_event_listener=N,this.remove_event_listener=L,this.schedule_render=y,this.after_render=z,this.phases=J}};function Bn(n,e,t,r,i,s,c,o,a,l,d,_,h,x,u,g,b,B,N,L,y,z){return new ft(n,e,t,r,i,s,c,o,a,l,d,_,h,x,u,g,b,B,N,L,y,z)}function Zr(n,e,t){for(;;){let r=n,i=e,s=t;if(r instanceof p)return[i,s];{let c=r.tail,o=r.head[0],a=r.head[1],l=Re(o,a);o===\"\"?(n=c,e=i,t=f(l,s)):ge(i,o)?(n=c,e=i,t=s):(n=c,e=be(i,o,l),t=f(l,s))}}}function dt(n){return Zr(S(n),te(),M)}function Nn(n,e,t){let r=dt(t),i=r[0],s=r[1];return ve(\"\",\"\",n,e,s,i)}function zn(n,e,t,r){let i=dt(r),s=i[0],c=i[1];return ve(\"\",n,e,t,c,s)}function ht(n){let e=dt(n),t=e[0],r=e[1];return nt(\"\",r,t)}var Rn=n=>{let e=T(W,null,n,0,null),{children:t}=_t(e,n,n.firstChild);if(t.length>1){let i=T(W,null,n,0,null);return e.kind=ie,e.node=globalThis.document.createTextNode(\"\"),e.parent=i,i.children.push(e),n.insertBefore(e.node,n.firstChild),ht(pe(t))}if(t.length===1)return t[0][1];let r=globalThis.document.createTextNode(\"\");return T(se,e,r,0,null),n.insertBefore(r,n.firstChild),Sn()},Dn=(n,e,t,r)=>{if(t.nodeType===8){let i=t.data.trim();return i.startsWith(\"lustre:fragment\")?ii(n,e,t,r):i.startsWith(\"lustre:map\")?si(n,e,t,r):i.startsWith(\"lustre:memo\")?oi(n,e,t,r):null}return t.nodeType===1?ni(n,t,r):t.nodeType===3?ri(n,t,r):null},ni=(n,e,t)=>{let r=e.getAttribute(\"data-lustre-key\")??\"\";r&&e.removeAttribute(\"data-lustre-key\");let i=T(W,n,e,t,r),s=e.localName,c=e.namespaceURI,o=!c||c===fe;o&&ai.includes(s)&&li(s,e);let a=ci(e),{children:l}=_t(i,e,e.firstChild),d=o?Nn(s,a,pe(l)):zn(c,s,a,pe(l));return Ce(r,d,e.nextSibling)},_t=(n,e,t)=>{let r=[];for(;t&&(t.nodeType!==8||t.data.trim()!==\"/lustre:fragment\");){let i=Dn(n,e,t,r.length);i?(r.push([i.key,i.vnode]),t=i.next):t=t.nextSibling}return{children:r,end:t}},ri=(n,e,t)=>(T(se,n,e,t,null),Ce(\"\",Cn(e.data),e.nextSibling)),ii=(n,e,t,r)=>{let i=mt(t.data),s=T(ie,n,t,r,i),{children:c,end:o}=_t(s,e,t.nextSibling);s.endNode=o;let a=ht(pe(c));return Ce(i,a,o?.nextSibling)},si=(n,e,t,r)=>{let i=mt(t.data),s=T(G,n,t,r,i),c=Pn(s,e,t,0);if(!c)return null;let o=Tn(c.vnode,a=>a);return Ce(i,o,c.next)},oi=(n,e,t,r)=>{let i=mt(t.data),s=Pn(n,e,t,r);if(!s)return null;e.removeChild(t);let c=Mn(pe([An({})]),()=>s.vnode);return Ce(i,c,s.next)},Pn=(n,e,t,r)=>{for(;;){if(t=t.nextSibling,!t)return null;let i=Dn(n,e,t,r);if(i)return i}},Ce=(n,e,t)=>({key:n,vnode:e,next:t}),ci=n=>{let e=[];for(let t=0;t<n.attributes.length;t++){let r=n.attributes[t];r.name!==\"xmlns\"&&e.push(jn(r.localName,r.value))}return pe(e)},ai=[\"input\",\"select\",\"textarea\"],li=(n,e)=>{let t=e.value,r=e.checked;n===\"input\"&&e.type===\"checkbox\"&&!r||n===\"input\"&&e.type===\"radio\"&&!r||e.type!==\"checkbox\"&&e.type!==\"radio\"&&!t||queueMicrotask(()=>{e.value=t,e.checked=r,e.dispatchEvent(new Event(\"input\",{bubbles:!0})),e.dispatchEvent(new Event(\"change\",{bubbles:!0})),globalThis.document.activeElement!==e&&e.dispatchEvent(new Event(\"blur\",{bubbles:!0}))})},mt=n=>{let e=n.match(/key=\"([^\"]*)\"/);return e?ui(e[1]):\"\"},ui=n=>n.replace(/&lt;/g,\"<\").replace(/&gt;/g,\">\").replace(/&quot;/g,'\"').replace(/&amp;/g,\"&\").replace(/&#39;/g,\"'\"),pe=n=>n.reduceRight((e,t)=>Se(t,e),M);var xt=n=>Q(n)?X(n):null,fi=n=>n!=null?K(n):Y(void 0);var pi=n=>{let e=Rn(n);return[n,e]};var di=(n,e)=>globalThis.document.createElementNS(n||fe,e),hi=n=>globalThis.document.createTextNode(n??\"\"),_i=()=>globalThis.document.createDocumentFragment(),mi=n=>globalThis.document.createComment(n),xi=(n,e,t)=>n.insertBefore(e,xt(t)),$i=xn?(n,e,t)=>n.moveBefore(e,xt(t)):(n,e,t)=>n.insertBefore(e,xt(t)),gi=(n,e)=>n.removeChild(e),bi=n=>{let e=n.nextSibling;return e?K(e):Y(void 0)},wi=(n,e)=>fi(n.getAttribute(e)),yi=(n,e,t)=>n.setAttribute(e,t??\"\"),ki=(n,e)=>n.removeAttribute(e),vi=(n,e,t)=>{n[e]=t},Ei=(n,e)=>{n.data=e??\"\"},ji=(n,e)=>{n.innerHTML=e??\"\"},Ci=n=>n,Si=(n,e,t,r)=>n.addEventListener(e,t,{passive:r}),Mi=(n,e,t)=>n.removeEventListener(e,t),Ai=n=>{let e=window.requestAnimationFrame(n);return()=>window.cancelAnimationFrame(e)},Ti=()=>{},Bi=n=>{queueMicrotask(n)},Ni=n=>{window.requestAnimationFrame(n)},zi=()=>it([pt(In,Bi),pt(qn,Ni)]),$t=n=>Bn(n,pi,di,hi,_i,mi,xi,$i,gi,bi,wi,yi,ki,vi,Ei,ji,Ci,Si,Mi,Ai,Ti,zi());var In=\"before_paint\",qn=\"after_paint\";var Ln=0,Un=1,Fn=2,Vn=3,Hn=4,Gn=5,Ie=0,Wn=1,Jn=2,qe=3,Kn=4;var gt=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\",\"csrf-token\"]}#i;#t=\"ws\";#n=null;#e=null;#r=null;#s=[];#o;#c=new Set;#l=new Set;#a=!1;#f=[];#u=new Map;#p=new Map;#g=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let i=r.attributeName;(!this.#a||this.#c.has(i))&&t.push([i,this.getAttribute(i)])}if(t.length===1){let[r,i]=t[0];this.#r?.send({kind:Ie,name:r,value:i})}else t.length?this.#r?.send({kind:qe,messages:t.map(([r,i])=>({kind:Ie,name:r,value:i}))}):this.#f.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#g.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#f.push([e.name,e.value])}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#n=new URL(r,location.href),this.#e=this.#d(),this.#n.searchParams.set(\"csrf-token\",this.#e),this.#h();return}case\"method\":{let i=r.toLowerCase();if(i==this.#t)return;[\"ws\",\"sse\",\"polling\"].includes(i)&&(this.#t=i,this.#t==\"ws\"&&(this.#n.protocol==\"https:\"&&(this.#n.protocol=\"wss:\"),this.#n.protocol==\"http:\"&&(this.#n.protocol=\"ws:\")),this.#h());return}case\"csrf-token\":t!==r&&this.#a&&this.#r?.close(),this.#e=this.#d(),this.#n&&this.#n.searchParams.set(\"csrf-token\",this.#e),this.#a&&this.#h()}}async messageReceivedCallback(e){switch(e.kind){case Ln:{for(this.#i??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#i.firstChild;)this.#i.firstChild.remove();let t=(o,a,l,d)=>{let _=this.#x(o,d??[]);return{kind:Wn,path:a,name:l,event:_}},r=(o,a)=>{this.#r?.send(a)},i=$t(this.#i);this.#o=new je(this.#i,t,r,i),this.#c=new Set(e.observed_attributes);let c=this.#f.filter(([o])=>this.#c.has(o)).map(([o,a])=>({kind:Ie,name:o,value:a}));this.#f=[],this.#l=new Set(e.observed_properties);for(let o of this.#l)Object.defineProperty(this,o,{get(){return this[`_${o}`]},set(a){this[`_${o}`]=a,this.#r?.send({kind:Jn,name:o,value:a})}});for(let[o,a]of Object.entries(e.provided_contexts))this.provide(o,a);for(let o of[...new Set(e.requested_contexts)])this.subscribe(o);c.length&&this.#r.send({kind:qe,messages:c}),e.will_adopt_styles&&await this.#_(),this.#i.addEventListener(\"context-request\",o=>{if(!o.context||!o.callback||!this.#u.has(o.context))return;o.stopImmediatePropagation();let a=this.#u.get(o.context);if(o.subscribe){let l=()=>{a.subscribers=a.subscribers.filter(d=>d!==o.callback)};a.subscribers.push([o.callback,l]),o.callback(a.value,l)}else o.callback(a.value)}),this.#o.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Un:{this.#o.push(e.patch);break}case Fn:{this.dispatchEvent(new Pe(e.name,e.data));break}case Vn:{this.provide(e.key,e.value);break}case Hn:{this.subscribe(e.key);break}case Gn:{this.unsubscribe(e.key);break}}}disconnectedCallback(){this.unsubscribeAll(),this.#r&&(this.#r.close(),this.#r=null)}provide(e,t){if(!this.#u.has(e))this.#u.set(e,{value:t,subscribers:[]});else{let r=this.#u.get(e);r.value=t;for(let i=r.subscribers.length-1;i>=0;i--){let[s,c]=r.subscribers[i];if(!s){r.subscribers.splice(i,1);continue}s(t,c)}}}subscribe(e){e&&(this.#p.get(e)?.(),this.dispatchEvent(new De(e,(t,r)=>{this.#r?.send({kind:Kn,key:e,value:t}),this.#p.get(e)?.(),this.#p.set(r)})))}unsubscribe(e){this.#p.get(e)?.(),this.#p.delete(e)}unsubscribeAll(){for(let[e,t]of this.#p)t?.();this.#p.clear()}#d(){return this.hasAttribute(\"csrf-token\")?this.getAttribute(\"csrf-token\")||null:document.querySelector('meta[name=\"csrf-token\"]')?.getAttribute(\"content\")||null}#h(){if(!this.#n||!this.#t)return;this.#r&&this.#r.close();let i={onConnect:()=>{this.#a=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#n,method:this.#t}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#a=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#n,method:this.#t}}))},csrfToken:this.#e};switch(this.#t){case\"ws\":this.#r=new bt(this.#n,i);break;case\"sse\":this.#r=new wt(this.#n,i);break;case\"polling\":this.#r=new yt(this.#n,i);break}}async#_(){for(;this.#s.length;)this.#s.pop().remove(),this.#i.firstChild.remove();this.#s=await En(this.#i)}#x(e,t=[]){let r={};e.isLustreEvent&&t.push(\"detail\"),(e.type===\"input\"||e.type===\"change\")&&(e.target.type===\"checkbox\"?t.push(\"target.checked\"):t.push(\"target.value\")),(e.type===\"keydown\"||e.type===\"keyup\"||e.type===\"keypress\")&&t.push(\"key\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let i of t){let s=i.split(\".\");for(let c=0,o=e,a=r;c<s.length;c++){if(c===s.length-1){a[s[c]]=o[s[c]];break}a=a[s[c]]??={},o=o[s[c]]}}return r}},bt=class{#i;#t;#n=!1;#e=[];#r=!0;#s=500;#o=1e4;#c;#l;#a;constructor(e,{onConnect:t,onMessage:r,onClose:i}){this.#i=e,this.#c=t,this.#l=r,this.#a=i,this.#f()}#f(){this.#t=new WebSocket(this.#i),this.#r=!0,this.#e=[],this.#t.onopen=()=>{this.#s=500,this.#c()},this.#t.onmessage=({data:e})=>{try{this.#l(JSON.parse(e))}finally{this.#e.length?this.#t.send(JSON.stringify({kind:qe,messages:this.#e})):this.#n=!1,this.#e=[]}},this.#t.onclose=e=>{this.#a(),e.code!==1e3&&this.#r&&this.#u()}}#u(){let e=()=>{this.#r&&(this.#f(),this.#s=Math.min(this.#s*2,this.#o))};if(document.hidden){let t=()=>{!document.hidden&&this.#r&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#s)}send(e){if(!(!this.#t||this.#t.readyState!==WebSocket.OPEN))if(this.#n){this.#e.push(e);return}else this.#t.send(JSON.stringify(e)),this.#n=!0}close(){this.#r=!1,this.#t.close(1e3),this.#t=null}},wt=class{#i;#t;#n=!0;#e=500;#r=1e4;#s;#o;#c;constructor(e,{onConnect:t,onMessage:r,onClose:i}){this.#i=e,this.#s=t,this.#o=r,this.#c=i,this.#l()}#l(){this.#t=new EventSource(this.#i),this.#e=500,this.#n=!0,this.#t.onopen=()=>{this.#s()},this.#t.onmessage=({data:e})=>{try{this.#o(JSON.parse(e))}catch{}},this.#t.onerror=()=>{this.#t.close(),this.#c(),this.#n&&this.#a()}}#a(){let e=()=>{this.#n&&(this.#l(),this.#e=Math.min(this.#e*2,this.#r))};if(document.hidden){let t=()=>{!document.hidden&&this.#n&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#e)}send(e){}close(){this.#n=!1,this.#t.close(),this.#c()}},yt=class{#i;#t;#n;#e;#r;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:i,csrfToken:s,interval:c}){this.#i=e,this.#t=s,this.#n=c??5e3,this.#r=t,this.#s=r,this.#o=i,this.#c().finally(()=>{this.#r(),this.#e=setInterval(()=>this.#c(),this.#n)})}async send(e){}close(){clearInterval(this.#e),this.#o()}#c(){let e=Object.assign({},this.#t&&{\"x-csrf-token\":this.#t});return fetch(this.#i,{headers:e}).then(t=>t.json()).then(this.#s).catch(console.error)}};customElements.define(\"lustre-server-component\",gt);export{gt as ServerComponent};",
  )
}

// ATTRIBUTES ------------------------------------------------------------------

/// The `route` attribute tells the client runtime what route it should use to
/// set up the WebSocket connection to the server. Whenever this attribute is
/// changed (by a clientside Lustre app, for example), the client runtime will
/// destroy the current connection and set up a new one.
///
pub fn route(path: String) -> Attribute(message) {
  attribute("route", path)
}

///
///
pub fn method(value: TransportMethod) -> Attribute(message) {
  attribute("method", case value {
    WebSocket -> "ws"
    ServerSentEvents -> "sse"
    Polling -> "polling"
  })
}

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to send them to the server we need to make a copy of any
/// fields we want to decode first.
///
/// This attribute tells Lustre what properties to include from an event. Properties
/// can come from nested fields by using dot notation. For example, you could include
/// the
/// `id` of the target `element` by passing `["target.id"]`.
///
/// ```gleam
/// import gleam/dynamic/decode
/// import agnostic/element.{type Element}
/// import agnostic/element/html
/// import agnostic/event
/// import agnostic/server_component
///
/// pub fn custom_button(on_click: fn(String) -> message) -> Element(message) {
///   let handler = fn(event) {
///     use id <- decode.at(["target", "id"], decode.string)
///     decode.success(on_click(id))
///   }
///
///   html.button(
///     [server_component.include(["target.id"]), event.on("click", handler)],
///     [html.text("Click me!")],
///   )
/// }
/// ```
///
pub fn include(
  event: Attribute(message),
  properties: List(String),
) -> Attribute(message) {
  case event {
    Event(..) -> Event(..event, include: properties)
    _ -> event
  }
}

/// If this attribute is present, the client runtime will include the CSRF token
/// in the query parameters when connecting to the server. This can be used to
/// mitigate cross-site request forgery attacks, by ensuring that only clients
/// that have access to the token can connect to the server component runtime.
///
/// *Note**: When this attribute is not provided, the client runtime will look for
/// a `<meta name="csrf-token" content="...">` tag in the page and use the content
/// of that tag as the CSRF token. We recommend using this approach over the
/// `csrf_token` attribute where possible.
///
pub fn csrf_token(token: String) -> Attribute(message) {
  attribute("csrf-token", token)
}

// ACTIONS ---------------------------------------------------------------------

@target(erlang)
/// Recover the `Subject` of the server component runtime so that it can be used
/// in supervision trees or passed to other processes. If you want to hand out
/// different `Subject`s to send messages to your application, take a look at the
/// [`select`](#select) effect.
///
/// > **Note**: this function is not available on the JavaScript target.
///
pub fn subject(runtime: Runtime(message)) -> Subject(RuntimeMessage(message)) {
  coerce(runtime)
}

@target(erlang)
/// Recover the `Pid` of the server component runtime so that it can be used in
/// supervision trees or passed to other processes. If you want to hand out
/// different `Subject`s to send messages to your application, take a look at the
/// [`select`](#select) effect.
///
/// > **Note**: this function is not available on the JavaScript target.
///
pub fn pid(runtime: Runtime(message)) -> Pid {
  let assert Ok(pid) = process.subject_owner(subject(runtime))

  pid
}

@target(erlang)
@external(erlang, "gleam@function", "identity")
fn coerce(value: a) -> b

/// Register a `Subject` to receive messages and updates from Lustre's server
/// component runtime. The process that owns this will be monitored and the
/// subject will be gracefully removed if the process dies.
///
/// > **Note**: if you are developing a server component for the JavaScript runtime,
/// > you should use [`register_callback`](#register_callback) instead.
///
pub fn register_subject(
  client: Subject(ClientMessage(message)),
) -> RuntimeMessage(message) {
  headless.ClientRegisteredSubject(client)
}

/// Deregister a `Subject` to stop receiving messages and updates from Lustre's
/// server component runtime. The subject should first have been registered with
/// [`register_subject`](#register_subject) otherwise this will do nothing.
///
pub fn deregister_subject(
  client: Subject(ClientMessage(message)),
) -> RuntimeMessage(message) {
  headless.ClientDeregisteredSubject(client)
}

/// Register a callback to be called whenever the server component runtime
/// produces a message. Avoid using anonymous functions with this function, as
/// they cannot later be removed using [`deregister_callback`](#deregister_callback).
///
/// > **Note**: server components running on the Erlang target are **strongly**
/// > encouraged to use [`register_subject`](#register_subject) instead of this
/// > function.
///
pub fn register_callback(
  callback: fn(ClientMessage(message)) -> Nil,
) -> RuntimeMessage(message) {
  headless.ClientRegisteredCallback(callback)
}

/// Deregister a callback to be called whenever the server component runtime
/// produces a message. The callback to remove is determined by function equality
/// and must be the same function that was passed to [`register_callback`](#register_callback).
///
/// > **Note**: server components running on the Erlang target are **strongly**
/// > encouraged to use [`register_subject`](#register_subject) instead of this
/// > function.
///
pub fn deregister_callback(
  callback: fn(ClientMessage(message)) -> Nil,
) -> RuntimeMessage(message) {
  headless.ClientDeregisteredCallback(callback)
}

// EFFECTS ---------------------------------------------------------------------

/// Instruct any connected clients to emit a DOM event with the given name and
/// data. This lets your server component communicate to the frontend the same way
/// any other HTML elements do: you might emit a `"change"` event when some part
/// of the server component's state changes, for example.
///
/// This is a real DOM event and any JavaScript on the page can attach an event
/// listener to the server component element and listen for these events.
///
pub fn emit(event: String, data: Json) -> Effect(message) {
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
/// > **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s
/// > and `Selector`s don't exist, and is the equivalent of returning `effect.none()`.
///
pub fn select(
  sel: fn(fn(message) -> Nil, Subject(a)) -> Selector(message),
) -> Effect(message) {
  effect.select(sel)
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON-encoded messages for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
pub fn runtime_message_decoder() -> Decoder(RuntimeMessage(message)) {
  decode.map(
    transport.server_message_decoder(),
    headless.ClientDispatchedMessage,
  )
}

// ENCODERS --------------------------------------------------------------------

/// Encode a message you can send to the client runtime to respond to. The server
/// component runtime will send messages to any registered clients to instruct
/// them to update their DOM or emit events, for example.
///
/// Because your WebSocket server sits between the two parts of the runtime, you
/// need to encode these actions and send them to the client runtime yourself.
///
pub fn client_message_to_json(
  message: ClientMessage(message),
  serializer: Serializer(message),
) -> Json {
  transport.client_message_to_json(message, serializer)
}
