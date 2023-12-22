# Lustre examples

Each of these examples is a complete Gleam project that contains a Lustre app
that demos or tests a particular feature of Lustre. To run any of them, navigate
to the example's directory and first run:

```sh
$ gleam build
```

Then serve the app using `lustre/try`:

```sh
$ gleam run -m lustre/try
```

If you do not specify a target, this will attempt to serve the app using an Erlang
HTTP server. If you'd prefer to serve using Node, you can specify the JavaScript
target instead:

```sh
$ gleam run -m lustre/try --target javascript
```

Or you may additionally supply the `--runtime deno` flag to serve using Deno rather
than Node.
