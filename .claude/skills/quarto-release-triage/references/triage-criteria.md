# Triage criteria

The scripts rank and group; this file is how to decide. Apply these when reading an issue
(and its comment thread) before placing it in the plan.

## Effort classes

| Class | Typical signals | Examples from past triages |
|---|---|---|
| **Quick** (1–2 h) | Root cause named in the thread; reporter supplies the fix; CSS/SCSS/JS-only; docs-only; `console.log` cleanup; one file touched | #13390 (reporter gave `background-color: inherit`), #12104 (add `uv` to docs), #13583 (`code-copy-outer-scaffold` breaks a querySelector) |
| **Medium** (½ day – few days) | Clear repro, but the fix crosses a filter/pipeline boundary; version range narrowed but cause not found; needs a test matrix | #12890 (monofont broke 1.6.26→1.6.27), #2795 (preview watcher scope), #11727 (lightbox strips caption markdown) |
| **Deep** (days – weeks) | Architecture decision needed; upstream dependency (Deno, Pandoc, jupyter-cache); ≥15 comments of debugging without convergence; `epic`, `needs-design`, `needs-discussion` | #13006 (SQLite on NFS), #4197 (V8 8 GB mmap), #7151 (gt + crossref + docx), #10146 (subfigure stretching) |

Clusters matter more than single issues: five code-block regressions with one CSS root
cause are one Quick PR, not five Medium ones. Look for shared labels + close creation
dates + same component.

## Deferred-milestone triage: pull back, maybe, or leave

Read the thread. The decisive evidence is usually a maintainer sentence, a version
narrowing, or a community workaround — not the reaction count.

**Pull back into the target release** when any of:
- a maintainer wrote "this is a priority" / "we'll do this next release" and nothing happened since
- the root cause is identified in the thread (someone quotes the offending rule/function)
- it is assigned to the user, has zero comments, and is small (nobody objected; it just needs doing)
- a contributor mapped the scope of changes (e.g. mcanouil's analysis on #5301)
- it is already in the target milestone in GitHub but was listed as deferred in the previous plan

**Maybe (needs a decision first)** when:
- a workaround or community extension exists, and the open question is whether to bless
  a built-in (e.g. custom callouts #844, decktape #4677)
- it depends on a dependency bump (Bootstrap 5.3 for paired themes #6741, Pandoc 3.5 for LOF/LOT, a Deno release for #4197)
- a PR was started by someone and stalled (#419)

**Leave deferred** when:
- a maintainer explicitly declined or gave a multi-year horizon (version selector #474, backlinks #1585)
- it needs a project-structure change nobody has designed (books without index.qmd #2556, qmd front matter #2908)
- blocked on an upstream project with no movement (jupyter-cache per-cell #1092)
- well served by community tooling (`quarto create post` #3917)
- ≤1 comment in 3+ years and a cheap workaround (pretty URLs #2065)

Write the reason next to the decision. The next triage will rely on it instead of
re-reading the thread.

## Old issues in owner areas (e.g. Windows)

Issues from 2022–2023 that nobody touched may already be fixed by unrelated work. Before
scheduling them, re-test on the current release and close if gone. Platform-specific
issues are orphaned by default when the only platform developer is not assigned —
list them explicitly and recommend self-assignment for the recent ones.

## Status vocabulary for story summaries

Use one of: `Active discussion`, `Has workaround`, `PR in progress`, `Blocked upstream`,
`Deferred explicitly`, `Stale`, `Verify-fixed`. Combine with a slash when two apply
("Active discussion / Has workaround").

Story format (2–3 sentences): what was asked → what maintainers said / what shipped
partially → what is the latest activity. Then one line: pull back / maybe / leave, and why.
