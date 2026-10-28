# Decisions

Main decisions and assumptions for each release, recorded as short ADRs (architecture decision records).

Plans and working notes don't belong here. Keep them in `plans/`, which is git-ignored.

## Files

One file per decision: `NNNN-short-title.md`, numbered in order (`0001-...`, `0002-...`). Don't renumber or delete old records; mark them superseded instead.

## Status

- **proposed**: open for discussion. Every new record starts here.
- **accepted**: the team has signed off.
- **superseded**: replaced by a later record (link to it).

## Workflow

1. Open a PR that adds the record with `Status: proposed`. A decision doesn't need its own issue. If the question is still open, discuss it in an issue first.
2. The team signs off in the PR review. Change the status to `accepted`, then merge.
3. To change a decision, add a new record and mark the old one superseded.

Each record must make sense without opening its links: state the outcome, the main reasons and what was rejected. Link related issues and PRs under `Discussion` as background. Use full URLs, because `#123` is not linked inside repository files.

## Template

```markdown
# NNNN. Title

- Status: proposed
- Date: YYYY-MM-DD
- Release: v0.1.0
- Discussion: (optional) links to related issues and PRs

## Context
What problem or question this is about, and any constraints.

## Options considered
(Optional) The alternatives and why they were not chosen.

## Decision
What we decided.

## Assumptions
What we take as true for this release. Revisit the decision if any of these change.

## Consequences
What gets easier or harder as a result.
```
