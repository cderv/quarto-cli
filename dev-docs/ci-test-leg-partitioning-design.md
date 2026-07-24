# Design: practical CI test-leg partitioning (per-suite grouping)

Status: **spec only, post-merge follow-up**. Requires
[#14706](https://github.com/quarto-dev/quarto-cli/pull/14706)
(built-version testing, `test-smokes-built.yml`) and
[#14715](https://github.com/quarto-dev/quarto-cli/pull/14715)
(log grouping, Phases 1–2.x) merged first. No part of this belongs in
either PR. Companion specs: `dev-docs/ci-test-log-grouping-design.md`
(the grouping mechanism this extends),
`dev-docs/ci-run-analysis-helper-design.md` (the verification tooling
that consumes the resulting runs).

## Problem

The grouping work gives the default (non-bucketed) path one group per
*registering test file*. That is the right granularity for `tests/smoke/`
`.ts` files and the wrong one for everything else in the leg:

- **smoke-all**: every `.qmd` document test is registered by the single
  `smoke/smoke-all.test.ts` runner, so the entire corpus (hundreds of
  documents in a full run) collapses into ONE giant group. Failures still
  self-extract (the failure path closes the group before the `FAILED`
  line) and viewer search reaches inside folds, so red-run triage works —
  but per-document navigation through the green bulk does not exist.
- **playwright**: `integration/playwright-tests.test.ts` never enters
  the grouping mechanism at all — it registers with raw `Deno.test`, not
  the harness `test()` wrapper, and renders its fixture corpus at
  module-eval time. In the 2026-07-22 full-serial dev run that meant
  ~17 min of ungrouped output: ~8 min of `Rendering docs/playwright/...`
  lines that Deno's reporter misattributes to the PREVIOUS test's
  post-test-output region (so they sit visually glued to an unrelated
  test's group), then the ~9 min suite run. No grouping invariant is
  violated — the previous file's unload close fires first — but the
  stretch is unnavigable and misleading.
- One serial process for everything also means: one shared 10-annotation
  step budget, one step summary, ~2 h wall-clock, and **cross-suite state
  contamination** — verified 2026-07-22: the `bookCrossrefIndexes` /
  listing failures of run 29841891595 (full serial suite) PASS in
  isolation on the same head (fork run 29914315152). Order-dependent
  in-process state is a real failure class today.

  **Localized 2026-07-23** by comparing upstream runs on the same head
  (0476b338f): full-serial dev run 29952972915 (36 failures/OS, 30 of
  them the book-crossref TypeError) vs built smoke leg of run
  29995036875 (8 failures, book family ABSENT). Mechanism: dev mode
  renders IN-PROCESS (`runDevQuarto`, tests/quarto-cmd.ts:334 — one
  Deno process shared by the whole suite), so unit/integration tests
  leave module state that book post-render later trips over; binary
  mode spawns a fresh built quarto per render, making in-process
  contamination structurally impossible. Consequences: the book-crossref
  family is a dev-serial-environment artifact (users never see it; the
  built daily is inherently free of it), and its triage is narrowed to
  "state left by unit/integration before smoke". The listings family
  ("Unable to read listing item description/preview from <sibling>.html")
  REPRODUCES in built mode — not in-process state; a filesystem-level
  sibling-output dependency of single-document project renders —
  separate triage bucket. Three failures were NEW in built mode, all
  classified: the 2 missing-output failures are the `source=build`
  version stamp `+test.<date>` (test-smokes-built.yml) — it fails
  `pandoc.types.Version` parsing, so `quarto.version` falls back to a
  plain string (datadir init.lua `version()`), and the version
  shortcode's `table.concat(quarto.version, '.')` errors, killing the
  render (fixes: `tostring(quarto.version)` in the shortcode +
  a parseable numeric stamp like `<version>.<date>`); the pdf-compile
  failure is the placeholder shortcode's external svg2png service
  dependency, known issue quarto-dev/quarto-cli#14722.

Purpose: *practical finding of errors* — pick the grouping that matches
each suite's shape, and get isolation + wall-clock + budget headroom as
side effects.

## Decision (2026-07-23): daily testing converges on the three built legs

Daily/nightly testing standardizes on the structure
`test-smokes-built.yml` already has — three parallel legs, all testing
the BUILT quarto:

- **smoke + smoke-all** — the binary-mode default corpus (`smoke/`);
  that subtree contains no `integration/` or `unit/` files, so the built
  smoke leg has no playwright pollution today, and it runs
  harness-owned (no buckets → not orchestrated), so the log-grouping
  work applies to it as-is;
- **ff-matrix** — its own reusable workflow, unchanged;
- **playwright** — its own leg (bucket
  `["integration/playwright-tests.test.ts"]`). Isolating playwright in
  its own leg/workflow is explicitly acceptable for daily testing.

Consequences:

- Legs A/B below *refine* the smoke leg of that structure (splitting
  smoke-all out with per-document groups); Leg C upgrades the playwright
  leg's reporting. Nothing new is needed just to get playwright out of
  the serial harness log — the built structure already does that.
- The full-serial dev-tree mode (`test-smokes.yml` on `schedule` or
  plain dispatch: empty buckets, dev quarto, ALL of `tests/` in one
  process — the mode the 2026-07-22 evidence run used) is the only mode
  with the playwright pollution. **Decided 2026-07-23: retire the
  `schedule:` trigger once daily built testing is verified working.**
  Rationale: per-commit dev coverage already exists —
  `test-smokes-parallel.yml` buckets the FULL corpus
  (`run-parallel-tests.ts` globs `**/*.test.ts`: unit, integration
  incl. playwright, smoke, smoke-all per-document) on every push/PR to
  main — so a daily dev re-run adds nothing the built nightly plus
  per-commit runs don't cover. Keep `workflow_call` (the parallel and
  built workflows depend on it) and `workflow_dispatch` (free; the
  on-demand full-serial run remains the at-scale grouping-evidence and
  order-dependence repro tool). Two checks before flipping: the built
  nightly chain (create-release build → `workflow_run` → smokes) must
  be observed green end-to-end, and note its cadence is gated on the
  nightly build succeeding — a failed build means no smoke coverage
  that day. Retiring the schedule also removes the ungrouped playwright
  stretch from routine CI entirely, and demotes the order-dependent
  failure triage further: those failures only manifest in the serial
  mode, which becomes dispatch-only.

## Design: three parallel legs (in `test-smokes-built.yml`)

The built workflow already runs smoke / playwright / ff-matrix as
parallel legs; this splits the smoke leg in two and upgrades the
playwright leg's reporting. Bucket-mode (`test-smokes.yml` nightly with
`buckets`) is untouched.

### Leg A — `.ts` smoke tests, grouped per test file (existing mechanism)

Everything Phase 2/2.1 already does, minus smoke-all. Exclusion needs no
script changes: `deno test --ignore` via the existing pass-through —

```
QUARTO_DENO_EXTRA_OPTIONS="--ignore=smoke/smoke-all.test.ts" ./run-tests.sh smoke/
```

Verified assumptions to re-check at implementation: `--ignore` accepts a
relative path on the pinned Deno; binary-mode default (`smoke/`) composes
with it.

### Leg B — smoke-all only, grouped per document (group-hint extension)

Run: `./run-tests.sh smoke/smoke-all.test.ts` (full corpus).

Harness extension (**the one new mechanism in this design**): a
registration-level *group hint* that overrides the group key.

- `tests/gha-grouping.ts`: `enterTestFileGroup(fileOrHint)` already
  transitions on key change and chains sibling groups seamlessly
  (verified in fork run 29914315152); no emitter change needed.
- `tests/test.ts` `test()`: accept an optional `groupHint?: string` (via
  `TestContext` or a new descriptor field — implementer's choice, but it
  must survive `mergeTestContexts`). In the test body, prefer
  `enterTestFileGroup(hint)` over the origin-derived file; at
  registration time the stack-based open still uses the file (hints are
  per-test, not known before the body runs — the file-level group opened
  at module eval simply transitions to the first document's group).
- smoke-all's registration site passes the document's repo-relative path
  as the hint → one group per `.qmd`, titled by document.

Semantics that change for this leg, to record in the grouping design
doc's invariants when implemented:

- The count-equality validation gate (`count(::group::) ==
  count(running…from)`) does NOT apply to leg B — groups = documents + 1
  file-level group, not files. Checker invariants 1–3 are key-agnostic
  and still apply unchanged.
- Failure-path closure and unload closure are unchanged (close is
  keyless).

Bonus: legs A and B in separate processes removes cross-suite state
contamination by construction, and shrinks the bisection space for the
remaining within-corpus order dependencies.

### Leg C — playwright, grouped per playwright test (reporter, not harness)

Playwright owns its execution model; the harness must not reach into it.
Two pieces:

- **Annotations**: Playwright's built-in `github` reporter emits per-test
  `::error` annotations — enable it alongside the existing reporter in
  the playwright config when `GITHUB_ACTIONS` is set. GitHub's per-step
  caps apply as usual.
- **Groups**: Playwright runs tests in PARALLEL workers, so inline
  `::group::` emission would interleave and break the no-nesting
  invariant. The correct shape is the buffer-and-flush pattern the
  grouping design's non-goals section prescribes for parallel execution:
  a ~30-line custom reporter that collects each test's output and emits
  `group + body + endgroup` atomically from `onTestEnd` (reporter
  callbacks are serialized). Lives with the playwright config under
  `tests/integration/`; zero interaction with `gha-grouping.ts` or the
  orchestrated gate.

### Setup partitioning (finding, 2026-07-24)

Corpus partitioning landed without setup partitioning: `test-smokes.yml`
gates its six Playwright setup steps on
`runner.os != 'Windows' || schedule || quarto-install not dev`, so every
Linux leg installs the full Playwright stack even when its corpus
contains no Playwright test. Observed cost: the built smoke leg of run
30081294158 failed in `npx playwright install-deps` (transient apt
failure) — a leg that would never run Playwright died in Playwright
setup, while the actual Playwright leg went green on the same step in
the same run. Fix (this design's scope, needs its own trial since the
conditional is shared by all modes): gate those steps on "leg will run
Playwright", computable from workflow inputs — roughly
`contains(inputs.buckets, 'playwright') || (inputs.buckets == '' &&
(inputs.quarto-install == 'dev' || inputs.quarto-install == ''))`
(dev full-serial runs everything; binary-mode empty buckets defaults to
`smoke/`). Side benefit: in `test-smokes-parallel.yml`, every bucket
except the one containing `playwright-tests.test.ts` skips ~2–3 min of
setup on every push. Preserve the current Windows-dev skip semantics
when rewriting the condition, and check whether the MECA/node steps
deserve the same treatment.

## What this buys / what it costs

| | |
|---|---|
| Wall-clock | serial ~2 h → max(leg) (legs already parallel in the built workflow) |
| Annotation budget | one 10-cap step → one per leg |
| Step summaries | one shared file → one per leg (smaller, per-suite) |
| Isolation | cross-suite in-process contamination eliminated by construction |
| Cost | +1 job's setup (~10 runner-minutes warm); one more leg to reason about |

## Verification plan

1. Trial subset runs per leg on a fork branch (the established
   trial-branch technique), checked with the `ci-run` helper if landed,
   else the manual normalize + `checkLog()` pipeline (remember: stored
   completed-run logs double consumed group commands — dedupe pairs).
2. Leg A: count-equality + distinct titles on an all-passing subset.
3. Leg B: groups == documents (+1 file-level opener); seeded failing
   document → `FAILED` outside groups, summary row + annotation carry the
   document path.
4. Leg C: two parallel-running seeded specs → no interleaved/nested
   groups in the log (the buffer-and-flush proof), `github`-reporter
   annotations present.
5. Full built dispatch once, then nightly coverage.

## Non-goals

- Bucket-mode (`test-smokes.yml` with `buckets`) — already navigable via
  YAML groups; unchanged.
- Fixing the order-dependent test failures themselves — separate triage
  track (this design only contains them).
- Per-render sub-grouping inside a document, unit-test legs, or matrix
  re-sharding of the corpus.

## PR chain context (recorded 2026-07-23)

- **PR 1 [#14706] — built-version testing.** Adds binary mode
  (`QUARTO_TEST_BIN`, `tests/quarto-cmd.ts`) and
  `test-smokes-built.yml` with the three legs above. Note the dev-tree
  nightly (`test-smokes.yml` `schedule`, and its plain
  `workflow_dispatch`, which exposes no `quarto-install` input) still
  tests the SOURCE TREE — PR 1 adds built testing alongside dev
  testing, it does not replace it.
- **PR 2 [#14715] — log grouping + failure surfacing.** Harness-owned
  grouping fires in both non-orchestrated modes: the full-serial
  dev run AND the built smoke leg. In the built smoke leg the corpus is
  `smoke/` only, so grouping there is already free of the playwright
  gap; the full-serial dev run is where the gap shows.
- **PR 3 (this spec) — leg refinement.** Depends on both PRs merging.
  Sequencing if merge stalls: develop stacked on the chain for CI
  trials, but open the PR only from post-merge `main`.

## References

- `dev-docs/ci-test-log-grouping-design.md` — grouping mechanism,
  invariants, buffer-and-flush non-goal this design instantiates.
- `dev-docs/ci-run-analysis-helper-design.md` — run verification; its
  `verdict` takes leg shape via flags (deliberately not hardcoded).
- Evidence runs: 29841891595 (full-suite failures),
  29914315152 (same subset green in isolation; seamless group chaining).
- `llm-docs/built-version-testing-architecture.md` — the built workflow
  this partitions.
