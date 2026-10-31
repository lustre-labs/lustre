/**
MIT License

Copyright (c) 2023 Zebulon Piasecki

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

var o=class{promise;resolve;constructor(){this.promise=new Promise(e=>this.resolve=e)}};var c=class{#e;#r;#s;#t;bodyUsed=!1;constructor(e,t,r,s){this.#e=e,this.#r=t,this.#s=s,this.#t=r.length>0?r:void 0}get name(){return this.#e.name}get fileSize(){return this.#e.fileSize}get#n(){let e=Math.ceil(this.#e.fileSize/512)*512;return new ReadableStream({start:()=>{if(this.bodyUsed)throw new Error("Body already used");this.bodyUsed=!0},pull:async t=>{if(e===0){t.close();return}let r;if(this.#t)r=this.#t,this.#t=void 0;else{let i=await this.#r.read();if(i.done){t.error(new Error("Unexpected end of stream"));return}r=i.value}let s=Math.min(e,r.length);t.enqueue(r.slice(0,s)),e-=s,e===0&&(this.#s(r.slice(s)),t.close())}})}get body(){let e=this.#e.fileSize,t=this.#n.getReader();return new ReadableStream({pull:async r=>{if(e===0){r.close();return}let s=await t.read();if(s.done){r.error(new Error("Unexpected end of stream"));return}let i=s.value,n=Math.min(e,i.length);if(r.enqueue(i.slice(0,n)),e-=n,e===0){for(;!(await t.read()).done;);r.close()}}})}async skip(){let e=this.#n.getReader();for(;!(await e.read()).done;);}async arrayBuffer(){let e=this.body.getReader(),t=new Uint8Array(this.#e.fileSize),r=0;for(;r<t.length;){let s=await e.read();if(s.done)throw new Error("Unexpected end of stream");t.set(s.value,r),r+=s.value.length}return t.buffer}async json(e="utf-8"){let t=await this.text(e);return JSON.parse(t)}async text(e="utf-8"){let t=await this.arrayBuffer();return new TextDecoder(e).decode(t)}};var u=new TextDecoder,h=class{#e;constructor(e){this.#e=e}get name(){let e=this.#e.slice(0,100),t=0;for(let r=0;r<e.length&&e[r]!==0;r++)t=r;return u.decode(e.slice(0,t+1))}get fileSize(){let e=this.#e.slice(124,136);return parseInt(u.decode(e),8)}get checksum(){let e=this.#e.slice(148,156);return parseInt(u.decode(e),8)}};var y=8*32;function g(a){let e=y;for(let t=0;t<512;t++)t>=148&&t<156||(e+=a[t]);return e}async function*v(a){let e=a.getReader(),t=new Uint8Array(512),r=0;for(;;){let{done:s,value:i}=await e.read();if(s)break;let n=i;for(;n.length>0;){let l=512-r,f=n.slice(0,l);if(n=n.slice(l),t.set(f,r),r+=f.length,r===512){let d=new h(t);if(g(t)!==d.checksum)return;let{resolve:w,promise:m}=new o;yield new c(d,e,n,w),d.fileSize>0&&(n=await m),r=0}}}}export{v as entries};
