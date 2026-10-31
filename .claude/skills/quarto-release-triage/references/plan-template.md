# Quarto-CLI {{TARGET}} Work Plan & Issue Analysis

**Date**: {{DATE}} · **Previous plan**: {{PREVIOUS_DATE}} ({{PREVIOUS_TARGET}})
**Source**: {{REPO}} · **Open issues**: {{OPEN}} · **{{TARGET}}**: {{N_TARGET}} · **{{DEFERRED}}**: {{N_DEFERRED}}
**Velocity (last 30 d)**: {{CREATED}} created / {{CLOSED}} closed ({{RATIO}}x)

---

## What changed since the previous plan

<!-- From the "Delta vs previous plan" section of the report: which planned items got
closed, which moved milestone, which are still open and unchanged. Two short lists:
"Done since last time" and "Carried over". This is the first thing a reader wants. -->

---

## {{TARGET}} Work Plan — {{USER}}

Each phase is an independent work stream; items are ordered by priority inside a phase.
Effort classes are defined in `references/triage-criteria.md`.

### Phase 1: Early-in-release (do first)

| # | Title | Effort | Notes |
|---|-------|--------|-------|

### Phase 2: Regressions

<!-- Group by shared root cause first ("Code block cluster: 5 issues, 1–2 PRs"), then
singles. Include regressions owned by others with "— track", so nothing is lost. -->

| # | Title | Root cause / Effort | Assignee |
|---|-------|---------------------|----------|

### Phase 3: {{USER}} assigned in {{TARGET}}

| # | Title | Effort | +1 | Notes |
|---|-------|--------|----|-------|

### Phase 4: Owner-area issues left unassigned ({{AREAS}})

**In {{TARGET}}:** …

**In {{DEFERRED}} — candidates to pull back:** …

**Recommendation**: self-assign the recent ones (#…); re-test the 2022–2023 ones (#…) and
close if fixed.

### Phase 5: High-impact {{TARGET}} items without owner

| # | +1 | Title | Effort |
|---|----|-------|--------|

### Phase 6: Strategic / epics

| # | Title | Effort | Notes |
|---|-------|--------|-------|

---

## How to use this plan

1. Work phases roughly in order; quick wins first inside each phase.
2. One sub-task / branch per item; link the PR back to the issue.
3. Re-test old owner-area issues before investing.
4. Track others' regressions without blocking on them.
5. Pull {{DEFERRED}} items into {{TARGET}} as capacity allows — the section below says which.

---

## {{DEFERRED}} Milestone: Triage & Context ({{N_DEFERRED}} issues)

### Pull back into {{TARGET}}

| # | +1 | Title | Why now |
|---|----|-------|---------|

### Maybe — needs a decision or design

| # | +1 | Title | Blocker / decision needed |
|---|----|-------|---------------------------|

### Explicitly deferred — don't pull

| # | +1 | Title | Why deferred (maintainer quote / date) |
|---|----|-------|----------------------------------------|

### {{USER}}'s {{DEFERRED}} backlog ({{N_MINE_DEFERRED}} issues)

| Theme | Count | Key issues |
|-------|-------|------------|

### {{DEFERRED}} thematic clusters (all developers)

| Theme | Count | Top unassigned issues |
|-------|-------|-----------------------|

---
---

## Reference: full issue analysis

<!-- Paste or summarize the remaining report sections: label counts, top wanted, top
discussed, top unassigned, deep-problem candidates, velocity. Keep the issue stories
(Status / Story / decision) for the deferred candidates here so the next triage can diff
them. -->
