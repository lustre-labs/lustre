# 01-basics/04-keyed-elements

This example demonstrates how to work with keyed elements
in Lustre, and what their benefits are over unkeyed elements.

## What are keyed elements?

Keyed elements are different to a regular list of elements. When inspecting elements,
the html returned identical between the two lists: 

```html
<div>
  <h2 class="text-2xl py-2">Unkeyed</h2>
  <ul class="grid grid-cols-3 gap-2">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/bm2.jpg">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/8pg.jpg">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/9ev.jpg">
  </ul>
</div>
```

vs. 

```html
<div>
  <h2 class="text-2xl py-2">Keyed</h2>
  <ul class="grid grid-cols-3 gap-2">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/bm2.jpg">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/8pg.jpg">
    <img class="aspect-square rounded" src="https://cdn2.thecatapi.com/images/9ev.jpg">
  </ul>
</div>
```

However, viewing the page in the browser, you'll notice that only the last image in the Keyed list flashes.
This is because Lustre is able to recognise that the first two images are the same as last render, and so they can be reused!

The Unkeyed list the DOM is tasked with rendering every time, so every image flashes.

```gleam
fn view_cat(id: String) -> Element(message) {
  html.img([
    attribute.class("aspect-square rounded"),
    attribute.src("https://cdn2.thecatapi.com/images/" <> id <> ".jpg"),
  ])
}
```

This view function creates an image element for a cat, with the `src` attribute pointing to a cat image 
from the cat api.
