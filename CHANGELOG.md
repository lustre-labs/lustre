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

- [agnostic/platform] Added the `Platform(node, target, value, event, message, raw)`
  type and the `headless` and `new` constructors.
- [agnostic/platform/opentui] Merged the `lustre_platform_opentui` package: an
  OpenTUI platform for building terminal UIs, with `element`, `attribute`,
  `event`, `effect`, and `portal` modules. JavaScript-only, gated with
  `@target(javascript)`.
- [agnostic/platform/dom] Added the browser DOM platform via `platform(onto:)`.
- [agnostic/serializer] Added a configurable HTML serializer API.
- [agnostic/element] Added `unsafe_raw` and `unsafe_raw_content` for embedding
  un-wrapped platform-native raw content.
- [agnostic/vdom] Added reconciler diagnostics gated behind the
  `AGNOSTIC_DEBUG_LOG` environment variable.
- Added deprecated shims for `lustre_dev_tools` compatibility.
- Added a Nix flake development environment.
- Enabled TypeScript declarations in the JavaScript build output.

### Changed

- Renamed the package from `lustre` to `agnostic`; the module namespace
  `lustre/*` is now `agnostic/*`, and the repository moved to
  [weedonandscott/agnostic](https://github.com/weedonandscott/agnostic).
- [agnostic] `start` now takes `on platform:` instead of `onto selector:`.
- [agnostic/runtime] The reconciler and runtime now perform all DOM operations
  through a `Platform`; browser DOM operations moved to
  `agnostic/platform/dom.ffi.mjs`, render scheduling and post-render hooks are
  delegated to the platform.
- [agnostic/runtime] Moved `lustre/runtime/server/runtime` to
  `agnostic/runtime/headless` and `runtime/client/spa.ffi.mjs` to
  `runtime/platform.ffi.mjs`.
- [agnostic/runtime] Reordered the reconciler's replace operation for
  marker-based targets such as OpenTUI.
- [agnostic/element] Moved `to_string`, `to_document_string`, and
  `to_readable_string` to `agnostic/platform/dom`.
- [agnostic/element] Deprecated `unsafe_raw_html` in favour of
  `unsafe_raw_content`.

### Fixed

- [agnostic/element/keyed] Duplicate keys are deduplicated in
  `extract_keyed_children`.
- [agnostic/vdom] Fixed nested-memo cache eviction when an ancestor entry is
  still held.

### Removed

- [agnostic] Removed `start_server_component`; use
  `start(app, on: platform.headless(), with: flags)` instead.
