# Fork workflow

This repository (`lustre_platform`) is a rebase-based soft fork of
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
bump, so fork 1.0.x → 1.1.0).

## Changelog

- `CHANGELOG.md` is the **fork's** changelog and is fork-owned.
- `CHANGELOG_UPSTREAM.md` is upstream's changelog, renamed. Do not edit it by
  hand: during rebases, git's rename tracking routes upstream's edits to their
  `CHANGELOG.md` into this file automatically.
- The rename and the addition of the fork changelog are **two separate
  commits** in the fork stack (a pure rename, then a pure add). Keep them that
  way through any history cleanup — squashing them together breaks git's
  rename detection and turns every future rebase into a changelog conflict.

## Upstream rebase procedure

When upstream releases vX.Y.Z:

1. `git fetch upstream --tags`
2. Create the new branch from the current one and switch to it:
   `git branch from-vX.Y.Z from-v<current> && git switch from-vX.Y.Z`
3. `git rebase vX.Y.Z` — rebase onto the upstream **release tag**, not the tip
   of `upstream/main`, so the base is always a released version. When
   resolving conflicts, describe upstream's change first, then decide;
   conflict boundaries are not semantic boundaries. The changelog should merge
   cleanly (see above); expect real conflicts where the fork diverges
   structurally (`src/lustre/vdom/reconciler.ffi.mjs`, `src/lustre/runtime/`).
4. Bump the fork version in `gleam.toml` per the versioning rule above, as a
   new commit.
5. Add a fork `CHANGELOG.md` section for the new fork version: "Rebased onto
   upstream vX.Y.Z (see CHANGELOG_UPSTREAM.md)" plus any fork-side changes
   since the last release.
6. Validate:
   `gleam format --check && gleam test --target erlang && gleam test --target javascript && gleam run -m build`
7. Push `from-vX.Y.Z` and switch the GitHub default branch to it:
   `gh repo edit weedonandscott/lustre --default-branch from-vX.Y.Z`
   Leave the old `from-*` branch in place.
8. Release when ready: tag `vN` (fork version) on the new branch, push that
   tag individually (never `git push --tags` — the repo carries inherited
   upstream tags that must not be pushed), and publish manually with
   `gleam publish`. There is no release CI workflow.
