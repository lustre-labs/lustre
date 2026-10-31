# esgleam

[![Package Version](https://img.shields.io/hexpm/v/esgleam)](https://hex.pm/packages/esgleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/esgleam/)

*esbuild for Gleam that works on Erlang & JavaScript.*

## [Quick start](#quick-start)
<span id="quick-start"></span>
1. Create a Gleam project as you would normally and make sure you have a file in `src` with the name of your project (as specified in `gleam.toml`)

2. Install `esbuild`
```sh
gleam run -m esgleam/install
```

3.
```sh
gleam run -m esgleam/bundle
```
See `/dist` for your bundled code

4. To start a development server
```sh
gleam run -m esgleam/serve
```


## [Advanced Usage](#advanced-usage)
<span id="advanced-usage"></span>

(Follow steps 1-2)

3. Create `/src/build.gleam` with the following
```gleam
import esgleam

pub fn main() {
   esgleam.new("./dist/static")
   |> esgleam.entry("main.gleam")
   |> esgleam.bundle
}
```

See [esgleam](https://hexdocs.pm/esgleam/esgleam.html) for all config options and their default values.

5. Run your build script
```sh
gleam run -m build
```

## [CLI overview](#cli-overview)
<span id="cli-overview"></span>

### Install
Install esbuild.
```sh
gleam run -m esgleam/install
```

### Bundle
Bundle the project into a library with `src/{project_name}.gleam` as your entry point and `./dist/{project_name}.js` as your output file.
```sh
gleam run -m esgleam/bundle
```

### App
Bundle the project into a single file to run with `src/{project_name}.gleam` as your entry point and `./dist/{project_name}.js` as your output file.
Similar to just running `gleam run`.
```sh
gleam run -m esgleam/app
```

### Serve
Starts a dev server, serving the `dist` directory as `/`.
```sh
gleam run -m esgleam/serve
```


## [Installation](#installation)
<span id="installation"></span>

If available on Hex this package can be added to your Gleam project:

```sh
gleam add esgleam
```

and its documentation can be found at <https://hexdocs.pm/esgleam>.

## Roadmap
- [ ] Create file watcher for `*.gleam` which runs on both Erlang and JavaScript
- [ ] Fix bug with STDOUT/STDERR not being shown when running `esbuild` in the erlang target
- [ ] Write tests for config options
- [ ] Add more config options if needed
- [ ] Pseudo-SourceMaps until [#1341](https://github.com/gleam-lang/gleam/discussions/1341) merges
- [ ] Consider creating a way to use plugins
- [ ] Hot Module Reloading?????
