A user on the Elixir Forum noted a performance issue with LiveView when rendering
a particularly large table. The issue stems from LiveView not performing keyed
diffs when rendering lists and collections, which often results in sending patches
much larger than could be achieved when a keyed diff.

The user goes on to provide a client side version of the same app, using petite-vue:
a stripped down version of vue based on alpine.js.

This demo app reproduces the petite-vue example in Lustre, with both a keyed and
non-keyed version.

- https://elixirforum.com/t/liveview-performance-problems-morphdom-taking-up-to-1500ms-to-patch-updates/69408
- https://codepen.io/Tiago-Barroso-the-builder/pen/gbOarMY
