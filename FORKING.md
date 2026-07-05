# Fork workflow

This repository (`agnostic`) is a rebase-based soft fork of
[lustre-labs/lustre](https://github.com/lustre-labs/lustre). This document is
the canonical description of how the fork tracks upstream.

## Branch scheme

- Development happens on branches named `from-vX.Y.Z`, where `vX.Y.Z` is the
  **upstream release** the fork stack is currently rebased onto.
- There is no `main` branch. The GitHub default branch is always the current
  `from-*` branch.
- Each upstream rebase creates a **new** `from-*` branch; the old one is left
  in place, untouched. Upstream syncs therefore never force-push anything.
  Force pushes only ever happen to the *current* `from-*` branch, during
  interim fork-side history cleanup.

## Versioning

The fork keeps its own semantic versioning, independent of upstream's version
numbers (it started at 1.0.0, based on upstream v5.7.0).

On every upstream rebase, bump the fork version by the same component upstream
bumped — major, minor, or patch — **regardless of the actual effect on the
code**. When absorbing several upstream releases at once, bump by the highest
component changed across all of them (e.g. absorbing 5.7.0 → 5.9.1 is a minor
bump, so fork 1.0.x → 1.1.0). Keep `package.json`'s version in sync with
`gleam.toml`'s.

## Changelog

- `CHANGELOG.md` is the **fork's** changelog and is fork-owned.
- `CHANGELOG_UPSTREAM.md` is upstream's changelog, renamed. Do not edit it by
  hand: during rebases, git's rename tracking routes upstream's edits to their
  `CHANGELOG.md` into this file automatically.
- The rename and the addition of the fork changelog are **two separate
  commits** in the fork stack (a pure rename, then a pure add). Keep them that
  way through any history cleanup — squashing them together breaks git's
  rename detection and turns every future rebase into a changelog conflict.

## The `agnostic` rename

The fork renames the module namespace: `src/lustre/` → `src/agnostic/`, all
imports and `lustre.` qualifiers rewritten. This lives in the fork stack as
**two commits that must never be squashed together**:

- a **pure path-rename commit** (`git mv` only, 100% similarity) — merge-ort
  pairs old and new paths exactly during replays;
- a **content-rewrite commit** (imports, qualifiers, FFI paths, doc links).

The repo has `merge.directoryRenames=true` set locally (re-set it after a
fresh clone: `git config merge.directoryRenames true`), so files upstream adds
under `src/lustre/` are auto-relocated to `src/agnostic/` during replays.

Deliberately **not** renamed (wire/protocol compatibility): the
`lustre-server-component` custom-element tag and bundle filename, the
`lustre:fragment` / `lustre:map` / `lustre:memo` virtualisation markers,
`data-lustre-key`, the `lustre:mount|connect|close` events, and the
`lustre_test` test-runner module (upstream file, not published).

## Deleted upstream content

`pages/` (upstream's guides/announcements docs) is deleted by a dedicated
fork commit. On every rebase, pages upstream touched since the old base come
back as modify/delete conflicts **on that commit** — the resolution is always
"keep deleted": `git rm` the resurrected files and continue. Upstream
additions to the `[documentation]` pages list in `gleam.toml` conflict the
same way; keep only the fork's Changelog entry.

## Upstream rebase procedure

When upstream releases vX.Y.Z:

1. `git fetch upstream --tags`
2. Create the new branch from the current one and switch to it:
   `git branch from-vX.Y.Z from-v<current> && git switch from-vX.Y.Z`
3. `git rebase vX.Y.Z` — rebase onto the upstream **release tag**, not the tip
   of `upstream/main`, so the base is always a released version. When
   resolving conflicts, describe upstream's change first, then decide;
   conflict boundaries are not semantic boundaries. The changelog should merge
   cleanly (see above); pages/ conflicts resolve as "keep deleted" (see
   above); expect real conflicts where the fork diverges structurally
   (`src/agnostic/vdom/reconciler.ffi.mjs`, `src/agnostic/runtime/`).
4. **Post-rebase namespace sweep.** Upstream's new code arrives saying
   `import lustre/...` and `lustre.`, which merges cleanly but is stale under
   the rename. Compiling both targets catches it deterministically:
   `gleam build --target erlang && gleam build --target javascript`, then fix
   every unknown-module error by rewriting the new references to `agnostic`.
   Finish with a residual scan:
   `git grep -n 'import lustre\b' -- '*.gleam'` (should be empty).
5. Bump the fork version in `gleam.toml` (and `package.json`) per the
   versioning rule above, as a new commit.
6. Add a fork `CHANGELOG.md` section for the new fork version: "Rebased onto
   upstream vX.Y.Z (see CHANGELOG_UPSTREAM.md)" plus any fork-side changes
   since the last release.
7. Validate:
   `gleam format --check && gleam test --target erlang && gleam test --target javascript && bunx tsc --noEmit && gleam run -m build`
   (`gleam run -m build` regenerates `priv/static/` and the runtime script
   embedded in `src/agnostic/server_component.gleam` — commit those artifacts.)
8. Push `from-vX.Y.Z` and switch the GitHub default branch to it:
   `gh repo edit weedonandscott/agnostic --default-branch from-vX.Y.Z`
   Leave the old `from-*` branch in place.
9. Release when ready: tag `vN` (fork version) on the new branch, push that
   tag individually (never `git push --tags` — the repo carries inherited
   upstream tags that must not be pushed), and publish manually with
   `gleam publish`. There is no release CI workflow.
