# Client testing for Lustre runtime

1. Build and test example projects
2. Build and test vdom

Depends on:
- `linkedom` - headless DOM testing
- `npm-run-all` - run watch in parallel 
- `vitest` - execute tests


### Commands

Run from the `test` directory
Each command will run a `build` command to build project dependencies

#### Benchmark

- `npm run bench`

#### Test

- ##### Single

  - `npm run test`

- ##### Watch

  - `npm run test:watch`
