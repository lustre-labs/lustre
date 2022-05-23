# lustre_counter

This is an example Lustre project. It uses [watchexec](https://github.com/watchexec/watchexec) to rebuild the Gleam project on save.

## Quick start

Initial setup:
```sh
npm install
gleam build
```

Run dev server on `localhost:3000`:
```sh
make dev
```

Make a production build under `dist` folder:
```sh
make
```
