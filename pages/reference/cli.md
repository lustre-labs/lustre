# Lustre CLI reference

Lustre includes a CLI (Command Line Interface) that provides a set of commands
for running and building Lustre projects. If you're familiar with frontend Web
development, you could consider the Lustre CLI as something similar to
[vite](https://vitejs.dev) but built right into the framework! If you're not
familiar with what these tools are used for... then read on.

The Lustre CLI is written in Gleam and requires **Erlang** to be installed even
if you are only building a JavaScript project. Most methods of installing Gleam
will guide you through installing Erlang too, but make sure you have it installed
before you try to use the Lustre CLI!

Because the CLI is written in Gleam, you will run it using the `gleam` command.
As an example, starting a development server looks like this:

```sh
gleam run -m lustre dev
```

<h2 id="lustre-add" class="member-name">
  <a href="#lustre-add">lustre add</a>
</h2>

This command lets you install development-specific binaries and tools from outside
the Gleam ecosystem. Lustre tries to be smart about the executables it understands:
if you try to build a project without esbuild it will grab it, if it finds a
tailwind.config.js it will use tailwind, and so on. All binaries are added to
`build/.lustre/bin` in case you need to execute them manually.

### `lustre add esbuild`

[Esbuild](https://esbuild.github.io) is a bundler and build tool for JavaScript
projects. This is the backbone of a lot of Lustre's build tooling and will be
installed automatically if you use `lustre build` or `lustre dev`.

Example:

```sh
gleam run -m lustre add esbuild
```

### `lustre add tailwind`

[Tailwind CSS](https://tailwindcss.com) is a utility-first CSS framework popular
among devs that want to quickly iterate on designs without touching CSS directly.
This will be installed automatically if Lustre detects a `tailwind.config.js` file
in your project.

Example:

```sh
gleam run -m lustre add tailwind
```

<h2 id="lustre-build" class="member-name">
  <a href="#lustre-build">lustre build</a>
</h2>

Gleam projects can be compiled to JavaScript but this output is not always
desirable for frontend projects where many individual modules can cause HTTP
bottlenecks and slower load times. The `lustre build` command produces different
_bundles_ that are single JavaScript files containing all the code needed for an
application to run.

If a `lustre build` subcommand is run without the necessary tooling installed,
Lustre will attempt to install it automatically.

### `lustre build app`

Bundle a Gleam application into a single JavaScript file. This requires a Gleam
module in your project with the same name as the project itself, and a public
`main` function that will be called when the application starts.

_This can be any Gleam program_, but if your `main` function returns an
`App(Nil, model, msg)` then Lustre will automatically generate some boilerplate
to mount the app onto an element with the id `"app"` and start it.

Flags:

- `--minify` - Reduce the size of the output bundle by removing whitespace and
  renaming variables. This is useful for production builds.

Example:

```sh
gleam run -m lustre build app
```

### `lustre build component`

Lustre components are based on standard Web Components. This means they should
be usable outside of Lustre and Gleam! The `lustre build component` command takes
a module and bundles it into a single JavaScript file that can be included in
_any_ Web app to register a new Custom Element that can be used like native HTML
elements.

For a module to be bundled as a component, it must adhere to the following rules:

- There must be a `pub const name` that is a string representing the name of the
  component to register. Remember that it must always contain a hyphen and follow
  [these rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names).

- There must be a `pub fn` that has the type `fn() -> App(Nil, model, msg)`. It's
  name is not important but in cases where multiple functions in a module fit this
  type, the _first_ one will be used.

Arguments:

- `<module_path>` - The path to the Lustre component you want to bundle. This should
  be in the same format that you would write to import the module in a Gleam file,
  e.g. `ui/my_componnt` and **not** `src/ui/my_component.gleam`.

Flags:

- `--minify` - Reduce the size of the output bundle by removing whitespace and
  renaming variables. This is useful for production builds.

Example:

```sh
gleam run -m lustre build component ui/counter
```

<h2 id="lustre-dev" class="member-name">
  <a href="#lustre-dev">lustre dev</a>
</h2>

The `lustre dev` command starts a development server that builds and serves your
project. This lets you focus on development without having to worry about a backend
or additional tooling.

Currently the server does _not_ watch for changes to your Gleam source files. You
must **restart the server** for changes to take effect. There have been discussions
about including a `watch` command in the Gleam compiler that would make this
possible.

Flags:

- `--port` - The port to serve the project on. Defaults to `1234`.

- `--host` - The host to serve the project on. Defaults to `0.0.0.0` and is
  accessible on localhosts and any other devices on the same network.

- `--include-styles` - Include the stylesheet from
  [lustre/ui](https://hexdocs.pm/lustre_ui/). This is mainly used in the example
  projects, but you may use this option to quickly experiment with the library
  yourself.

Example:

```sh
gleam run -m lustre dev --port=8080 --include-styles
```

## Getting help

The Lustre CLI is still an experimental work in progress. If you run in to issues
or have ideas for how it could be improved we'd love to hear from you, either by
[opening an issue](https://github.com/lustre-labs/lustre/issues) or reaching out
on the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
