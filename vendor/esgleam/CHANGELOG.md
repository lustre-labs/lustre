## 0.8.0
- [`d9ffa4b`](https://github.com/Enderchief/esgleam/commit/d9ffa4b15e588862ec1bee919e42a347b5af59a7) Create `config.autoinstall`
- [`c262c13`](https://github.com/Enderchief/esgleam/commit/c262c136a640278f90c40407e1ddfb8d1e83581d) Only output `.mjs` if `config.format=Esm`
- [`d490afc`](https://github.com/Enderchief/esgleam/commit/d490afc1e6b7e8aa5c737132678c513321cf42e1) Update broken dependencies
- [`b02d617`](https://github.com/Enderchief/esgleam/commit/b02d617a1c8631e95c69f5826bc0324aeeae8cbe) Update package version
- [`4796983`](https://github.com/Enderchief/esgleam/commit/4796983a9c1f5d66ddb4b8f886b8ad414b77fd2d) Add Android arm64 support

## 0.7.0 (2024-10-03)
- [`75f0356`](https://github.com/Enderchief/esgleam/commit/75f03563239fe220be6a41b43cb2c3a1f6fc5c40) Updated simplifile

## 0.6.0 (2024-02-17)
- [`b90a06e`](https://github.com/Enderchief/esgleam/commit/b90a06ec26f00e741344173fba7e3650e0e107f5) Now installs predefined version of esbuild.
- [`b90a06e`](https://github.com/Enderchief/esgleam/commit/b90a06ec26f00e741344173fba7e3650e0e107f5) Remove thoas as a dependency
- [`fa07ede`](https://github.com/Enderchief/esgleam/commit/fa07ede7f2098bb61d2dfb0eb89cf2d10758f89b) Location of esbuild binary is now outside of the `priv` folder and hidden in the `build` folder.
- [`cecd4b9`](https://github.com/Enderchief/esgleam/commit/cecd4b94e1fd08bbac69a23db3d4aa54a77e2cd1) esbuild autoinstallation works with Deno.
- [`d8e41e8`](https://github.com/Enderchief/esgleam/commit/d8e41e8da7b544c842ff60a792d9298a0542d4fd) CLI no longer defaults to using Deno.

## 0.5.0 (2024-01-23)
- [`8f3a72de`](https://github.com/Enderchief/esgleam/commit/8f3a72deb125a544d125b29ffbf8b772cd3c636a) Command output when using Erlang target works.

## 0.4.7 (2024-01-21)
- [`8f06df5b`](https://github.com/Enderchief/esgleam/commit/8f06df5b61886f404f77a1a4e6ef3c46e6c3fb39) New config option to set esbuild platform.

## 0.4.5 (2024-01-21)
- [`685bffe3`](https://github.com/Enderchief/esgleam/commit/685bffe381745d3e3bc69a1b82ec88bd7f8fc32b) Fix issue with os and arch checking not working on Deno leading to an error.

## 0.4.6 (2024-01-16)
- [`0b071047`](https://github.com/Enderchief/esgleam/commit/0b071047051c29b6dadc62fb5b69799c80ff3ec9) Use correct esbuild executable on Windows. (#5 thanks to @xhh)
## 0.4.5 (2024-01-15)
- [`ddf6037`](https://github.com/Enderchief/esgleam/commit/ddf6037ac7edc87e2e7f05675512749242b39f2c) Readded esbuild autoinstall.
- [`ddf6037`](https://github.com/Enderchief/esgleam/commit/ddf6037ac7edc87e2e7f05675512749242b39f2c) Install interface in Gleam is now synchronous.

## 0.4.3 (2024-01-14)
- [`800dcf5`](https://github.com/Enderchief/esgleam/commit/800dcf540a7fde8f6b2728a478545e54c6500355) Replace esbuild auto install with a warning.
- [`800dcf5`](https://github.com/Enderchief/esgleam/commit/800dcf540a7fde8f6b2728a478545e54c6500355) Change bundle to synchronous for the JS target by using `spawnSync`.

## 0.4.2 (2024-01-14)
- [`b7a1ab8`](https://github.com/Enderchief/esgleam/commit/b7a1ab8bbb89f89154981cd735b50411081933e8) Install esbuild automatically if not installed.

## 0.4.1 (2024-01-13)
- [`00069ae`](https://github.com/Enderchief/esgleam/commit/00069ae870f63d16c54bd6320225b62d28390309) Rename `fetch_version` to `fetch_from_version`.
- [`00069ae`](https://github.com/Enderchief/esgleam/commit/00069ae870f63d16c54bd6320225b62d28390309) Add `fetch_latest` as shorthand for `fetch_from_version(platform.get_package_name(), _)`.

## 0.4.0 (2024-01-13)
- [`6b044f3`](https://github.com/Enderchief/esgleam/commit/6b044f3a494b595e2d16daf6f5a63219a587ce1e) Config option `Kind`. Select between `Script` (call the `main` function in the bundle) and `Library` (standard bundling).
- [`6b044f3`](https://github.com/Enderchief/esgleam/commit/6b044f3a494b595e2d16daf6f5a63219a587ce1e) `esgleam/app` CLI module to bundle with kind `Script`.
- [`37ce214`](https://github.com/Enderchief/esgleam/commit/37ce214c501d62e646b8e7e9f360d33362d609f2) Expose internal api for installing esbuild
- [`37ce214`](https://github.com/Enderchief/esgleam/commit/37ce214c501d62e646b8e7e9f360d33362d609f2) Expose internal api for selecting package version
- [`37ce214`](https://github.com/Enderchief/esgleam/commit/37ce214c501d62e646b8e7e9f360d33362d609f2) Downgrade `Thoas` to be compatable with `gleam_json`
