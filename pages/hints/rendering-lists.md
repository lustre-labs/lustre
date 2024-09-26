# Rendering lists

Items in a list can change position, for example when sorting or filtering, or
when adding or removing items. When this happens, the view will re-render, resulting
in a new virtual DOM which is then diffed with the existing DOM to determine what
changes are required to be made. As a result of this process you might see some
unexpected visual issues. These issues are not unique to Lustre and can be reproduced
in other frameworks that use a virtual DOM. For example, here is what React has to
[say about this](https://react.dev/learn/rendering-lists#keeping-list-items-in-order-with-key).

We'll explore a visual issue example below, but the quick solution to this issue
is to use the [`element.keyed`](https://hexdocs.pm/lustre/lustre/element.html#keyed)
function so Lustre can more accurately track which items in a list have changed.

## Seeing double

One example of a visual issue can be seen in the [quickstart program](../guide/01-quickstart.md)
where it is possible to briefly see a duplicated cat image before the new image has
completely loaded. Let's walk through why this happens.

When you first click the increment button, the http side effect will eventually
cause a cat image slug, let's call it `a`, to be added the model's `cat` list.
The view will render a single image tag for this list, it will be added to the DOM,
and the browser will make a request for this image, displaying it once it has completed
downloading.

When the increment button is clicked a second time, the http side effect will
eventually cause a cat image slug, let's call it `b`, to be [added to the front](https://tour.gleam.run/basics/lists/)
of the model's `cat` list. So this list has changed from `[a]` to `[b, a]`. The view
will now render two image tags, first for `b` and then for `a`, as it walks the list.
When this virtual DOM is diffed with the current DOM, it looks like the image tag
that used to show `a` has been changed to show `b`, and a new image tag for `a` has
been added as a sibling element. This causes the browser to request image `b`, but while
that is downloading, the previous image shown on screen for this tag remains visible.
Also while `b` is downloading, the browser needs to show the image `a` for new tag.
This can be pulled from the browser cache so is immediately displayed without the user
experiencing any delay. The visual issue presents as a duplicate `a` cat image until
image `b` is downloaded, at which point it replaces the first `a` image.

> **Note:** Because the cat API returns a random image, it is possible for the final
> result to be a duplicate cat image. This is a different situation from the one
> described above. Using the slugs from above, this situation would produce
> a list `[a, a]`.

To try and represent this in a different way by showing how the DOM might change:

```
Increment ->                 Increment ->
             <img href="a">  --changes-->  <img href="b">
                             -- added -->  <img href="a">
```

With `element.keyed`, these changes might look more like:
    
```
Increment ->                 Increment ->
                             -- added -->  <img href="b">
             <img href="a">  -no change->  <img href="a">
```


### Fixing the quickstart program

As mentioned in the first section above, the way to fix this issue is to [key the
elements](https://hexdocs.pm/lustre/lustre/element.html#keyed). This allows Lustre
to better track which items have changed, moved, been removed, etc. Keyed elements
were intentionally not used in the quickstart tutorial in an attempt to focus on
the fundamentals.

Here is one possible way to use `element.keyed` in the `view` function, where the
image slug is used as the key:

```gleam
element.keyed(
  html.div([], _),
  list.map(model.cats, fn(cat) {
    #(cat, html.img([attribute.src("https://cataas.com/cat/" <> cat)]))
  }),
),
```
