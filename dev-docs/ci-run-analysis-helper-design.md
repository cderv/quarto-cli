# Design: `ci-run` — reusable retrieval & analysis of CI test-run evidence

Status: **spec approved, not implemented**. Companion implementation plan:
`dev-docs/ci-run-analysis-helper-plan.md` (working artifact — fold into this
doc or drop before merge). Follow-up to
`dev-docs/ci-test-log-grouping-design.md` (PR
[#14715](https://github.com/quarto-dev/quarto-cli/pull/14715)).

Base: written on top of `test/gha-log-grouping`. The helper imports
`checkLog()`/`stripAnsi` and reasons about the annotation/step-summary
behavior that branch introduces, so it cannot be based on `main` until
#14715 merges. This branch must not be PR'd as a third stack level —
open the PR after #14715 lands (see "Delivery" below).

## Problem

Every debugging or verification session that asks "did that CI run behave?"
rebuilds the same pipeline by hand: find the run, find the job, download a
log, discover it is the *rendered* runner form that
`tests/tools/check-gha-log.ts` cannot read, hand-classify annotations with
raw `gh api` calls, and eyeball the step summary in the UI. The trial-matrix
rounds for #14715 each paid this cost. The evidence surfaces are known and
stable; the retrieval and analysis logic should be written once, live
in-repo, and be discoverable by future sessions (human or agent).

## Evidence surfaces — established facts

Established empirically against real runs and the real API during the
#14715 trials (fork runs 29826031431, 29767185290, 29767190539); the two
items marked *(source-confirmed)* were additionally verified against
`actions/runner` sources. Empirical observation of the live service
outranks documentation here — re-verify on runner/API behavior changes, not
on doc changes.

**Two log forms.**

- Raw harness stdout contains literal `::group::` / `::error` markers —
  the form `check-gha-log.ts` parses (built for
  `GITHUB_ACTIONS=true ./run-tests.sh | tee`).
- Downloaded run logs (`gh api .../actions/runs/ID/logs` zip, or
  `gh run view --job ID --log`) are the **rendered** form: commands the
  runner parsed appear as `##[group]` / `##[error]`; lines are CRLF with an
  ISO-8601 timestamp prefix; the zip form carries a UTF-8 BOM on line 1;
  the `gh run view --log` form prefixes each line with
  `<job>\t<step>\t` fields and may label lines `UNKNOWN STEP` when the zip
  fetch degrades to per-job API calls.
- The step's own log opens with a `##[group]Run <script>` preamble that
  echoes the step's shell **source** — for the bucket loop that source
  literally contains `echo "::group::Testing ..."`, so feeding a rendered
  log to the raw checker false-positives on invariant 2.
- *(source-confirmed)* `ActionCommandManager.TryProcessCommand` matches a
  workflow command only at the start of a line; an unparsed marker survives
  **verbatim** in the stored log. Corollary: in a rendered log, a literal
  `::group::` outside the script-source preamble means the runner did not
  parse it — which *is* an invariant-2 violation.
- *(source-confirmed)* `##[group]` is the runner's *legacy* command
  framing — actively written by the runner itself, but its stability is not
  contractual. Any consumer of the rendered form must fail loudly on
  unrecognized framing rather than degrade silently.
- No raw-stdout artifact is uploaded by `test-smokes.yml`, so finished runs
  currently offer only the rendered form.

**Annotations (the structured surface).**

- `GET repos/O/R/check-runs/CHECK_RUN_ID/annotations` returns each
  annotation as `{annotation_level, path, start_line, title, message}`.
  Default `gh` auth suffices (checks:read); fine-grained PATs get 403.
- `CHECK_RUN_ID` resolution: the actions jobs endpoint
  (`repos/O/R/actions/runs/RUN_ID/jobs`) exposes `check_run_url` per job —
  preferred. Fallback (established during the trials): list
  `repos/O/R/commits/SHA/check-runs --paginate` and filter by `.name` +
  `.started_at` (dispatch runs share the branch head SHA).
- Field usage is a reliable discriminator:
  - harness per-test annotation: `path` = repo-relative test file,
    `title` = test name (emitted by `tests/test.ts` via
    `ghError(msg, {file, title})`, deliberately no line/col);
  - harness aggregate (the 10th): `title` = `"More test failures"`, no
    file;
  - YAML bucket-loop annotation: attributed under `.github` with
    `title` = `"Test Bucket Failed"` (the workflow `echo "::error ..."`
    omits `file=`, so GitHub anchors it to the workflow file);
  - runner-injected: message starts
    `"Process completed with exit code"`.
- This split is what lets a tool assert the design invariants of #14715
  mechanically: ≤ 9 harness + ≤ 1 aggregate per step, and **zero** harness
  annotations on `QUARTO_TESTS_GHA_ORCHESTRATED` legs.

**Step summary.**

- `GITHUB_STEP_SUMMARY` has no REST endpoint (confirmed against `cli/cli`
  and the docs) — UI-only unless a tee + `upload-artifact` step is added.
- Coverage boundary: summary rows are written by the harness only for
  failures it executes and observes. Pre-harness / infra failures (bad
  bucket arg, setup-step death, OOM before the first test) legitimately
  produce a red run with an **empty** summary and zero harness
  annotations. That signature is expected, not a bug — and detecting it is
  itself valuable (see `verdict` below).

## Design decisions

### A. Form: in-repo script + thin project skill (no bundled copy)

One script family under `tests/tools/gha/`, plus a thin project skill at
`.claude/skills/gha-run-analysis/SKILL.md` that references the script **by
repo path**, plus (at implementation time) a one-line cross-reference from
the verification section of `ci-test-log-grouping-design.md`.

Rejected alternatives:

- *Skill with bundled script* (the default Anthropic guidance): targets
  portable/plugin skills. Here the skill and the code share one git tree; a
  bundled copy would be invisible to CI and human devs and would drift from
  the `checkLog`/`stripAnsi` code it imports.
- *Bare `tests/tools/` script only*: solves everything except discovery —
  and discovery is the actual problem (the knowledge existed in a design
  doc during the #14715 work and sessions still rebuilt the pipeline).
- *dev-docs note only*: describes the pipeline; sessions still rebuild it.

Skill-mechanics facts (verified against the Claude Code docs,
code.claude.com/docs/en/skills.md): project skills in
`.claude/skills/<name>/SKILL.md` are auto-discovered; `allowed-tools`
frontmatter pre-approves specific `Bash(...)` patterns for the invoking
turn; pre-approval of a repo file requires variable expansion —
`${CLAUDE_PROJECT_DIR}` (Claude Code ≥ 2.1.196; older versions fall back
to prompting, degraded not broken). Keep `SKILL.md` under ~150 lines; the
`description` + `when_to_use` listing text is truncated at 1,536 chars, so
trigger keywords go first.

### B. Rendered-form support now; raw artifact later

**Now:** a *normalizer stage* (not checker changes) makes downloaded logs
checkable: strip BOM/CRLF/timestamp prefixes (and `gh run view`'s
job/step columns), elide `##[group]Run *` script-source preamble blocks,
map `##[group]`/`##[endgroup]` to `::group::`/`::endgroup::`, then feed
the **untouched** `checkLog()`. This works retroactively on any finished
run, needs no workflow change and no storage. Two semantics to encode:
(a) in rendered form, a surviving literal marker outside the elided
preamble is the invariant-2 violation signal (see source-confirmed facts);
(b) unrecognized framing must be a loud error. The entry point
auto-detects raw vs rendered and **refuses ambiguous input**, closing the
vacuous-pass hazard of feeding a rendered log to the raw checker.

**Later (deferred with D):** a single `if: always()` step in
`test-smokes.yml` uploading tee'd raw stdout + a copy of
`$GITHUB_STEP_SUMMARY` as `test-evidence-<os>-<leg>`. Exact-form evidence,
byte-level diffing, no heuristics — but a workflow edit (review + trial
cost), storage/retention, and the known pwsh `*>` capture gotcha. It rides
along the next PR that touches `test-smokes.yml` anyway; `ci-run` then
prefers the artifact when present. The normalizer stays useful for old
runs and other repos' runs either way.

### C. Annotations API is the canonical verification path

The strongest surface, and the helper's centerpiece (see `annotations`
subcommand below). Minor tradeoffs: pagination, the PAT-403 caveat
(documented in the skill), and asserting per-*step* caps against
per-*check-run* (per-job) data — sound because the path/title
classification separates harness from orchestrator emissions.

### D. Step summary: UI-only now, artifact folded into B-later

Short term the composite report emits the run's summary deep link plus an
explicit eyeball checklist, and uses the annotation data to detect the
pre-harness signature (red run + zero harness annotations + zero
aggregate) — labeling it "infra/pre-harness failure: read the raw job
log", which prevents the most likely future misdiagnosis. Long term the
summary copy in the B evidence artifact makes "summary rows == observed
failures" machine-checkable — worth one workflow change only bundled with
B, not alone.

## Concrete design

### Component 1 — `tests/tools/gha/` (Deno CLI + pure modules)

`ci-run.ts` (entry, `import.meta.main`-guarded like the checker) shells
out to `gh` for auth/transport (`Deno.Command`), defaults
`--repo quarto-dev/quarto-cli`. Subcommands:

- `fetch <run-id> [--job <substr|id>] [--attempt N] [--cache-dir D]` —
  download per-job logs to a cache dir (OS tempdir default, never inside
  the checkout), print paths.
- `check-log <file | run-id --job …> [--step <substr>]` — auto-detect
  form; raw → `checkLog()` directly; rendered → normalize (optionally
  filtered to one step) → `checkLog()`; refuse ambiguous input;
  checker-compatible exit codes (0 ok, 1 violations, 2 usage).
- `annotations <run-id> [--json]` — resolve jobs → check-run IDs → fetch
  annotations; classify harness / aggregate / yaml-orchestrator / runner /
  other; assert: harness ≤ 9 and aggregate ≤ 1 per job, zero harness on
  orchestrated legs, total < 50. Table for humans, `--json` for scripts.
- `verdict <run-id>` — composite PASS/FAIL: run conclusion, per-leg
  `check-log`, annotation assertions, julia-gate `::notice` presence on
  built-mode legs, pre-harness detection, summary deep link + checklist.
  Output formatted to paste into a PR comment (the artifact produced by
  hand for the #14715 trials).

Pure logic lives in `rendered-log.ts` (form detection + normalization) and
`annotations.ts` (classification + budget assertions) so it is unit-testable
without network; `gh.ts` is a thin subprocess wrapper. `checkLog()` and
`check-gha-log.ts` are **not modified**.

### Component 2 — `.claude/skills/gha-run-analysis/SKILL.md`

Thin: third-person description with front-loaded triggers ("analyze CI
run", "check annotations", "smoke test logs", "grouping invariants"); one
command per task; states the script is *executed*, not read; pre-approves
`Bash(deno run --allow-read --allow-write --allow-run=gh
${CLAUDE_PROJECT_DIR}/tests/tools/gha/ci-run.ts *)`; carries the caveats
(raw vs rendered, PAT 403, empty-summary semantics, annotation caps,
`--attempt` for re-runs) and the raw REST endpoints so `gh`-less
environments (e.g. MCP-only remote sessions) can fall back to manual
calls.

### Component 3 — deferred evidence artifact

The B/D workflow step, explicitly out of scope for the first PR.

## Invariants the helper asserts (mapping to #14715)

| #14715 invariant | `ci-run` check |
|---|---|
| 1 — one group open at a time, all closed | `check-log` (raw or normalized) |
| 2 — no harness markers/annotations when orchestrated | `annotations`: zero harness-class on orchestrated legs; `check-log` on the leg |
| 3 — FAILED/ERRORS outside groups | `check-log` |
| 4 — ≤ 9 + 1 aggregate, summary is complete record | `annotations` counts; summary link + (later) artifact row count |
| — | pre-harness signature detection (red + zero harness annotations) |

## Evaluation scenarios (eval-first; baseline = fresh session, no skill)

1. **"Did run N respect the annotation caps?"** Baseline: ad-hoc `gh api`
   spelunking, pagination stumbles, harness/YAML misclassification (~15+
   tool calls). Success: one `annotations` call, correct 9+1 verdict with
   classification table, < 5 calls.
2. **"Verify grouping invariants on finished run N"** (no raw artifact
   exists). Baseline: downloads rendered log, feeds it to the raw checker,
   gets preamble false-positives or a vacuous pass. Success: normalize →
   real checker verdict, preamble excluded, rendered-mode invariant-2
   semantics noted in output.
3. **"Nightly built run is red but the step summary is empty — what
   broke?"** Baseline: session suspects a harness bug and digs into
   `tests/test.ts`. Success: helper reports the pre-harness signature and
   points at the failing step's log lines.
4. **"Confirm bucket-mode logs are byte-identical before/after a harness
   change."** Baseline: manual download + diff drowned in timestamp noise.
   Success: `fetch` + normalize `--strip-timestamps` + diff gives a clean
   verdict.

Baseline measurement: run each scenario once in a session without the
skill; record tool-call count and wrong turns. That is the bar the skill
must beat.

## Verification plan

1. Unit tests for `rendered-log.ts` and `annotations.ts`
   (`tests/unit/gha-*.test.ts`, alongside the existing
   `gha-grouping.test.ts` / `github-actions-reporting.test.ts`), fixtures
   cut from a real downloaded log (BOM, CRLF, timestamps, preamble echo,
   `gh run view` job/step columns, `UNKNOWN STEP`).
2. Ambiguity refusal: a rendered log passed as raw (and vice versa) must
   error, not pass vacuously.
3. Live smoke against a known historical run (e.g. fork run 29826031431:
   expected 9 harness + 1 aggregate on the default leg, zero harness on
   bucket legs) — manual, documented in the skill, not CI.
4. The eval scenarios above, run once with and once without the skill.

## Risks / open questions

- **Rendered-format drift** is the main risk to B-now: legacy framing,
  undocumented. Mitigated by real-log fixtures, loud failure on
  unrecognized framing, and the same "re-check on version bump" discipline
  the checker documents for Deno.
- **Scope creep**: this stays a quarto-cli-specific evidence tool, not a
  general Actions client. YAGNI applies to every subcommand beyond the
  four listed.
- Open: whether `verdict` should learn per-leg expectations from
  `test-smokes-built.yml` job names or take them as flags (start with
  flags; hardcode nothing about matrix shape).

## Delivery

Spec (this doc) and implementation land as **one PR** based on
`test/gha-log-grouping`, opened only after #14715 merges (at which point
the base rebases onto `main` trivially). No third stack level. No workflow
edits in that PR (Component 3 deferred). The implementation plan doc is a
working artifact and is dropped or folded into this doc before merge.

## References

- `dev-docs/ci-test-log-grouping-design.md` — parent design (invariants,
  verification plan, trial evidence).
- PR [#14715](https://github.com/quarto-dev/quarto-cli/pull/14715);
  parent PR [#14706](https://github.com/quarto-dev/quarto-cli/pull/14706).
- `tests/tools/check-gha-log.ts` — the raw-form checker this helper feeds.
- `actions/runner` sources — command parsing (`ActionCommandManager`),
  legacy `##[group]` framing.
- Claude Code skills reference — code.claude.com/docs/en/skills.md
  (project-skill discovery, `allowed-tools`, `${CLAUDE_PROJECT_DIR}`).
- GitHub REST: actions runs/jobs/logs; checks — check-run annotations.
