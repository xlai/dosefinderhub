# 0001. Two long-lived branches: main and dev

- Status: proposed
- Date: 2026-10-29
- Release: v0.1.0
- Discussion: https://github.com/xlai/dosefinderhub/pull/88

## Context

Work on the app is spread across many branches. Some were merged into each other but never into `main`, and the most complete version of the app lives on `exp`. `main` only holds early prototypes. We need one clear place for ongoing work and one for releases.

## Options considered

- **Only `main`.** Simple, but `main` would hold half-finished work between releases.
- **Keep using `exp`.** It has the latest work, but its name says it is experimental, and it was built up by merging branches into each other without review.

## Decision

We keep two long-lived branches:

- `main` holds stable, tagged releases only. It changes only through a release pull request from `dev`.
- `dev` holds work towards the next release. Every pull request goes to `dev`.

Feature branches are short-lived, branch from `dev`, and are named for what they do.

To start, `dev` was brought up to date with `exp` and then with `main`. Both steps kept all existing history.

## Assumptions

- One maintainer reviews and merges, so a heavier model (release branches, hotfix branches) is not needed yet.

## Consequences

- Open pull requests that targeted other branches have been pointed at `dev`. https://github.com/xlai/dosefinderhub/pull/81 and https://github.com/xlai/dosefinderhub/pull/79 are kept as drafts until after v0.1.0, so they are not merged by mistake.
- Branches whose work is now in `dev` can be retired.
