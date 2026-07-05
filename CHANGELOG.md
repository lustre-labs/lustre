# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

This project is a soft fork of [Lustre](https://github.com/lustre-labs/lustre);
see [FORKING.md](./FORKING.md) for how it tracks upstream. Upstream's changelog
is preserved in [CHANGELOG_UPSTREAM.md](./CHANGELOG_UPSTREAM.md).

## [Unreleased]

Based on upstream Lustre v5.7.0.

### Added

- [lustre/platform] Added the `Platform(node, target, value, event, message, raw)`
  type and the `headless` and `new` constructors.
- [lustre/platform/dom] Added the browser DOM platform via `platform(onto:)`.
- [lustre/serializer] Added a configurable HTML serializer API.
- [lustre/element] Added `unsafe_raw` and `unsafe_raw_content` for embedding
  un-wrapped platform-native raw content.
- [lustre/vdom] Added reconciler diagnostics gated behind the
  `LUSTRE_DEBUG_LOG` environment variable.
- Added deprecated shims for `lustre_dev_tools` compatibility.
- Added a Nix flake development environment.
- Enabled TypeScript declarations in the JavaScript build output.

### Changed

- Renamed the package to `lustre_platform`.
- [lustre] `start` now takes `on platform:` instead of `onto selector:`.
- [lustre/runtime] The reconciler and runtime now perform all DOM operations
  through a `Platform`; browser DOM operations moved to
  `lustre/platform/dom.ffi.mjs`, render scheduling and post-render hooks are
  delegated to the platform.
- [lustre/runtime] Moved `lustre/runtime/server/runtime` to
  `lustre/runtime/headless` and `runtime/client/spa.ffi.mjs` to
  `runtime/platform.ffi.mjs`.
- [lustre/runtime] Reordered the reconciler's replace operation for
  marker-based targets such as OpenTUI.
- [lustre/element] Moved `to_string`, `to_document_string`, and
  `to_readable_string` to `lustre/platform/dom`.
- [lustre/element] Deprecated `unsafe_raw_html` in favour of
  `unsafe_raw_content`.

### Fixed

- [lustre/element/keyed] Duplicate keys are deduplicated in
  `extract_keyed_children`.
- [lustre/vdom] Fixed nested-memo cache eviction when an ancestor entry is
  still held.

### Removed

- [lustre] Removed `start_server_component`; use
  `start(app, on: platform.headless(), with: flags)` instead.
