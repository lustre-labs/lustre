# Client testing for Lustre runtime

A configured app for testing client side implementations.

Depends on:
- `esbuild` - bundle gleam generated helpers along with ffi (vdom in this case)
- `linkedom` - headless DOM testing
- `npm-run-all` - run watch in parallel 
- `vitest` - execute tests


### Commands

Run from the `test-apps/client_test` directory
Each command will run a `build` command to package dependencies

#### Benchmark

- `npm run bench`

#### Test

- ##### Single

  - `npm run test`

- ##### Watch

  - `npm run test:watch`
