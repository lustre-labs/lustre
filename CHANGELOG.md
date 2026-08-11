# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

This project is a soft fork of [Lustre](https://github.com/lustre-labs/lustre);
see [FORKING.md](./FORKING.md) for how it tracks upstream. Upstream's changelog
is preserved in [CHANGELOG_UPSTREAM.md](./CHANGELOG_UPSTREAM.md).

## [Unreleased]

### Added

- [agnostic/platform/opentui] **Breaking:** `KeyEvent` gained a `super` field
  reporting the super modifier (`cmd` on Macs).

## [v1.0.0] - 2026-07-19

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
- [agnostic/effect] Added `deferred(phase, effect)`, the platform-author
  constructor for effects deferred to a platform-declared phase.
- [agnostic/platform] Added the `Phase` type. Platforms declare their own
  deferred effect phases (name + scheduler); the runtime drains pending tasks
  in declaration order at the end of each render pass. Tasks tagged with a
  phase the running platform does not declare are silently dropped — including
  all deferred effects on headless platforms, which declare no phases.
- [agnostic/platform/dom] Added the `before_paint` and `after_paint` effect
  constructors (moved from `agnostic/effect`, upstream-exact semantics) and the
  public phase-name constants `before_paint_phase` / `after_paint_phase` so
  custom platforms can declare compatible phases.
- [agnostic/platform/opentui] Added `effect.after_flush`, anchored on
  OpenTUI's FRAME event — it runs after the frame containing the update has
  been flushed to the terminal (the platform starts OpenTUI's continuous
  render loop, so FRAME fires on every normal tick; blocked ticks drain at
  the next successful flush) — and the public phase-name constant
  `opentui.after_flush_phase`. The callback receives the dispatch function, a
  `Layout` record — post-paint its lookups agree with the renderables' cached
  accessors, and it is the only geometry source reachable from Gleam — and
  the OpenTUI renderer.
- [agnostic/platform/opentui] Added `effect.frame_callbacks`, anchored on
  OpenTUI's frame callbacks slot — it runs after the update has been
  reconciled, in the same render-loop tick, before layout and paint, on every
  loop iteration (blocked frames included) — and the public phase-name
  constant `opentui.frame_callbacks_phase`.
- [agnostic/platform/opentui] Added `effect.after_layout`, anchored on
  OpenTUI's `LAYOUT_CHANGED` event with a frame-callback fallback for frames
  that did not relayout — it runs after yoga layout is final for the frame
  containing the update, before it paints, on every loop iteration — and the
  public phase-name constant `opentui.after_layout_phase`. The callback
  additionally receives a `Layout` record (with the new `effect.Layout`,
  `effect.Rect`, and `effect.ScrollExtents` types) whose `rect` and
  `scroll_extents` lookups read fresh, layout-final geometry straight from
  the layout engine — the only safe geometry source at this timing, since the
  renderables' cached accessors still hold last frame's values.
- [agnostic/platform/opentui] Added `effect.scroll_into_view_with_padding` and
  the `effect.ScrollPadding` type: per-axis optional scroll paddings via the
  labelled `cols:`/`rows:` arguments (`Option(ScrollPadding)` each).
  `OnScroll(by: k)` leaves k cells of clearance at the edge the reveal lands
  the target at, only when that axis actually scrolls; `Always(by: k)`
  enforces k cells of clearance from both viewport edges even for an
  already-visible target (vim-scrolloff behavior). `by` clamps at zero;
  paddings are capped so the target always stays fully in view and clamp
  silently at the content extents.
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
- [agnostic/effect] **Breaking**: removed `effect.before_paint` and
  `effect.after_paint`. Use `dom.before_paint` / `dom.after_paint` from
  `agnostic/platform/dom` — identical callback signature and identical DOM
  semantics. These effects now run only on platforms declaring the DOM phases;
  custom platforms can opt in by declaring phases named
  `dom.before_paint_phase` / `dom.after_paint_phase`.
- [agnostic/platform] **Breaking**: `platform.new` gained a required final
  labelled argument `phases: List(Phase)`. Pass `[]` for no deferred phases.
- [agnostic/platform/opentui] **Breaking**: removed `effect.before_paint` and
  `effect.after_paint`; use `effect.after_flush` (the callback receives
  dispatch, a `Layout`, and the renderer). The callback now runs after the
  flush of the frame containing the update — what the old `before_paint`
  *actually* did (post-flush, one frame late) and what the old `after_paint`
  claimed but didn't reliably do.
- [agnostic/platform/opentui] Retimed `effect.focus`, `effect.focus_next`,
  and `effect.focus_previous` onto the `frame_callbacks` phase
  (post-reconcile, same render-loop tick): elements mounted — or keyed-
  remounted — by the same update can now be focused, and elements the update
  removed are no longer focus targets. Previously these ran pre-reconcile,
  so same-update targets silently no-oped and keyed remounts focused the
  doomed old renderable.
- [agnostic/platform/opentui] Retimed `effect.get_focused_id` and
  `effect.get_focused` from `after_flush` to the `frame_callbacks` phase:
  focus state is final at reconcile, so the read now happens in the same tick
  as the update — one frame earlier — and blocked frames no longer delay it
  to the next successful flush.
- [agnostic/platform/opentui] Retimed `effect.scroll_by` and
  `effect.scroll_to` onto the `after_layout` phase with a fresh-extent
  pre-clamp: elements created by the same update are now resolvable, and
  scroll writes clamp against this frame's content/viewport extents instead
  of last frame's — scrolling toward content grown by the same update no
  longer undershoots.
- [agnostic/platform/opentui] Retimed `effect.set_selection_span` onto the
  `after_flush` phase: OpenTUI's selection API converts the coordinates it is
  given back through geometry cached during the paint walk, so the write must
  run post-paint for its inputs to mean what the API thinks they mean. Ids
  created by the same update are now resolvable, and the selection lands on
  the right cells even when the same update mounts, moves, or scrolls the
  editors — cases that previously dropped or misplaced it silently. The
  selection paints one tick after the frame containing the update.
- [agnostic/platform/opentui] Rebuilt `scroll_into_view` as an intent-holding
  reveal: the platform now applies it inside the next frame, after the
  update's layout is final but before the frame paints, so the first flushed
  frame containing the update already shows the correct scroll position
  (previously it ran post-flush, one frame late). Geometry is now computed
  through OpenTUI's layout engine over the full ancestor chain, fixing
  miscomputed scroll positions for targets nested below the first level of
  the scrollbox content (the old direct-sibling height sum ignored nesting,
  gaps, margins, and padding, and only handled the vertical axis). When the
  target is a textarea/input taller than the container's viewport, the reveal
  now targets the caret row rather than merely an edge of the editor's box.
  The reveal runs on the `after_layout` phase, so platforms that do not
  declare that phase — every non-OpenTUI platform — silently drop the effect
  instead of throwing "Renderer not initialized".

### Fixed

- [agnostic/element/keyed] Duplicate keys are deduplicated in
  `extract_keyed_children`.
- [agnostic/vdom] Fixed nested-memo cache eviction when an ancestor entry is
  still held.
- [agnostic/platform/opentui] Fixed "before paint" effects running one frame
  late: the runtime's `queueMicrotask` drain only ran after OpenTUI's
  synchronous render loop had already flushed the frame, so focus, scroll, and
  selection effects always landed in the *next* frame.
- [agnostic/platform/opentui] Fixed "after paint" effects on event-driven
  renders running *before* the paint of their own frame: the drain went
  through OpenTUI's `requestAnimationFrame`, whose callbacks run at the top of
  the next loop tick — before that tick's flush. `after_flush` anchors on the
  FRAME event instead, which fires only after a genuinely flushed frame.
- [agnostic/platform/opentui] Fixed the inert `scroll_margin` attribute:
  OpenTUI exposes the editor scroll margin only as a constructor option with
  no property setter, so the plain property assignment never reached the
  editor and the default (0.2) always applied. The attribute now forwards to
  the editor view's `setScrollMargin`, and removing it restores the default.
- [agnostic/runtime] Fixed the JavaScript headless runtime's `start` passing
  its arguments shifted by one into the `Runtime` constructor (the unused
  leading name parameter was omitted), which broke
  `start(app, on: platform.headless(), ...)` on the JavaScript target. The
  same latent bug exists upstream.

### Removed

- [agnostic] Removed `start_server_component`; use
  `start(app, on: platform.headless(), with: flags)` instead.
