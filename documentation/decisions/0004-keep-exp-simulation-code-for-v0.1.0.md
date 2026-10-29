# 0004. Keep the simulation code from exp for v0.1.0

- Status: proposed
- Date: 2026-10-29
- Release: v0.1.0
- Discussion: https://github.com/xlai/dosefinderhub/pull/81, https://github.com/xlai/dosefinderhub/issues/85

## Context

There are two versions of the Simulation tab code:

- The version on `exp`, from https://github.com/xlai/dosefinderhub/pull/83. It works, and it includes reworked comparison tables.
- A refactor in https://github.com/xlai/dosefinderhub/pull/81. It is cleaner and fixes how plots are labelled, but it is unfinished.

Merging the refactor into `exp` looks simple, because only one file conflicts. In practice the comparison tables search for plot titles that the refactor renamed, so they would come out empty. The refactor also picks the wrong scenario data for some scenario selections.

Both versions share the same underlying simulation functions, so most known bugs exist in both.

## Options considered

- **Merge the refactor and port the table rework into it.** Cleaner code, but a large change with a known bug, and the comparison plots by scenario are still missing.
- **Keep the `exp` code and fix the known bugs in place.** Smaller, reviewable changes on code that already works.

## Decision

For v0.1.0 we keep the Simulation code from `exp` and fix the known bugs in it with small pull requests. The refactor in https://github.com/xlai/dosefinderhub/pull/81 is not merged for v0.1.0.

## Assumptions

- The bugs listed in `RELEASE.md` can be fixed without restructuring the Simulation tab.

## Consequences

- v0.1.0 keeps some duplicated code in the Simulation tab.
- After v0.1.0, the structure of the app will be reconsidered as a whole, including whether to build on the refactor. https://github.com/xlai/dosefinderhub/pull/81 stays open as input for that.
