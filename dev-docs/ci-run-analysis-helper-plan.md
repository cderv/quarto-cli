# Implementation plan: `ci-run` CI evidence helper

> **Working artifact.** Executes the spec in
> `dev-docs/ci-run-analysis-helper-design.md`. Fold anything durable into
> the spec and **delete this file before the PR merges.**

Goal: `tests/tools/gha/` gains a `ci-run` CLI (fetch / check-log /
annotations / verdict) with pure, unit-tested normalization and
classification modules, plus a thin project skill. No changes to
`checkLog()`, `check-gha-log.ts`, any workflow file, or PR #14715's
commits.

Tech: Deno (repo-pinned), plain `Deno.args` parsing (match
`check-gha-log.ts`, no cliffy in tests/tools), `gh` for all transport.

Global constraints:

- Pure modules take data as arguments — **no `Deno.env` reads, no network,
  no default that touches the real environment** (the #14715 unit tests
  broke in CI exactly this way; see the null-sentinel commit c7746dd).
  Environment/subprocess access lives only in `ci-run.ts` / `gh.ts`.
- Imports use explicit `.ts` extensions; new files carry the repo header
  `Copyright (C) 2020-2026 Posit Software, PBC`.
- Unit tests run via the harness: `cd tests && ./run-tests.sh
  unit/gha-rendered-log.test.ts unit/gha-annotations.test.ts` — expect
  `ok` and zero network access.
- One commit per task, message subject in imperative mood.

File map:

| Path | Task | Role |
|---|---|---|
| `tests/tools/gha/rendered-log.ts` | 1 | form detection + normalization (pure) |
| `tests/unit/gha-rendered-log.test.ts` | 1 | fixtures + tests |
| `tests/tools/gha/annotations.ts` | 2 | classification + budget checks (pure) |
| `tests/unit/gha-annotations.test.ts` | 2 | tests |
| `tests/tools/gha/gh.ts` | 3 | `gh` subprocess wrapper |
| `tests/tools/gha/ci-run.ts` | 4 | CLI entry, subcommand dispatch |
| `.claude/skills/gha-run-analysis/SKILL.md` | 5 | discovery skill |
| `dev-docs/ci-test-log-grouping-design.md` | 6 | +1 xref line (verification section) |
| `dev-docs/ci-run-analysis-helper-design.md` | 6 | status flip |

---

## Task 1 — `rendered-log.ts` (+ tests, TDD: tests first)

Interface:

```ts
export type LogForm = "raw" | "rendered-zip" | "rendered-ghview";

export interface NormalizeOptions {
  step?: string;            // ghview only: keep lines whose step field contains this
  stripTimestamps?: boolean; // default true
}

export interface NormalizeResult {
  content: string;   // feedable to checkLog()
  elidedBlocks: number; // count of ##[group]Run * preamble blocks removed
}

export function detectForm(content: string): LogForm; // throws on ambiguous/empty
export function normalizeRendered(content: string, form: LogForm, opts?: NormalizeOptions): NormalizeResult; // throws on form === "raw" or unrecognized framing
```

Implementation (complete):

```ts
/*
 * rendered-log.ts — convert downloaded (rendered) GitHub Actions logs into
 * the raw ::group:: form that tests/tools/check-gha-log.ts parses.
 *
 * Rendered-form facts (see dev-docs/ci-run-analysis-helper-design.md,
 * "Evidence surfaces"): parsed commands appear as ##[group]/##[endgroup];
 * unparsed markers survive verbatim (source-confirmed) — so a literal
 * ::group:: outside the elided script-source preamble is a genuine
 * invariant-2 signal, not noise. Framing is the runner's legacy format:
 * form detection and gh-view line structure are strict (throw, never
 * guess); within a recognized log only group framing is mapped — other
 * ##[...] lines pass through, which checkLog() ignores.
 *
 * Copyright (C) 2020-2026 Posit Software, PBC
 */

const kBom = "\uFEFF";
const kIso = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z ?/;
// gh run view --log: "<job>\t<step>\t<timestamped line>"
const kGhView = /^[^\t\n]*\t[^\t\n]*\t/;
const kPreambleOpen = /^##\[group\]Run /;

const stripBom = (s: string) => s.startsWith(kBom) ? s.slice(1) : s;

export function detectForm(content: string): LogForm {
  const lines = stripBom(content).split(/\r?\n/).filter((l) => l.length > 0);
  if (lines.length === 0) throw new Error("empty log");
  const sample = lines.slice(0, 200);
  const ghview = sample.filter((l) =>
    kGhView.test(l) && kIso.test(l.replace(kGhView, ""))).length;
  const zip = sample.filter((l) => kIso.test(l)).length;
  const ratio = (n: number) => n / sample.length;
  if (ratio(ghview) > 0.9) return "rendered-ghview";
  if (ratio(zip) > 0.9) return "rendered-zip";
  if (zip === 0 && ghview === 0) return "raw";
  throw new Error(
    `ambiguous log form: ${zip}/${sample.length} timestamped, ` +
      `${ghview}/${sample.length} gh-view lines — refusing to guess ` +
      "(a rendered log fed to the raw checker passes vacuously)",
  );
}

export function normalizeRendered(
  content: string,
  form: LogForm,
  opts: NormalizeOptions = {},
): NormalizeResult {
  if (form === "raw") {
    throw new Error("normalizeRendered called on a raw log");
  }
  const stripTs = opts.stripTimestamps ?? true;
  const out: string[] = [];
  let elidedBlocks = 0;
  let eliding = false;
  for (let line of stripBom(content).split(/\r?\n/)) {
    if (form === "rendered-ghview") {
      const m = line.match(kGhView);
      if (m) {
        if (opts.step !== undefined) {
          const step = line.split("\t")[1] ?? "";
          if (!step.includes(opts.step)) continue;
        }
        line = line.replace(kGhView, "");
      } else if (line.length > 0) {
        throw new Error(`unrecognized gh-view framing: ${line}`);
      }
    }
    if (stripTs) line = line.replace(kIso, "");
    if (eliding) {
      if (line === "##[endgroup]") { eliding = false; }
      continue;
    }
    if (kPreambleOpen.test(line)) {
      eliding = true;
      elidedBlocks++;
      continue;
    }
    if (line.startsWith("##[group]")) {
      out.push("::group::" + line.slice("##[group]".length));
    } else if (line === "##[endgroup]") {
      out.push("::endgroup::");
    } else {
      out.push(line);
    }
  }
  if (eliding) {
    throw new Error("log ended inside a ##[group]Run script-source preamble");
  }
  return { content: out.join("\n"), elidedBlocks };
}
```

Tests (`tests/unit/gha-rendered-log.test.ts`, via `unitTest` from
`../test.ts` like `gha-grouping.test.ts`). Cases — build fixtures as
inline template strings with `\r\n` and a leading `"\uFEFF"` where relevant:

1. `detectForm`: raw sample (lines incl. `::group::smoke/a.test.ts`) →
   `"raw"`; zip sample (`"\uFEFF" + "2026-07-21T10:00:00.123Z ##[group]Run ./x`)
   → `"rendered-zip"`; ghview sample (lines like `job\tRun all Smoke
   Tests Linux\t2026-… content`) → `"rendered-ghview"`; 50/50 mixed → throws containing `"ambiguous"`;
   empty → throws.
2. `normalizeRendered` on a zip fixture containing: preamble block
   (`##[group]Run ./run-tests.sh` … `echo "::group::Testing X"` …
   `##[endgroup]`), then `##[group]smoke/a.test.ts`, body, `##[endgroup]`
   → result has `elidedBlocks === 1`, no literal `echo "::group::` line,
   groups mapped to `::group::smoke/a.test.ts` / `::endgroup::`,
   timestamps and `\r` gone.
3. Literal unparsed marker mid-log (line `  ::group::oops` — runner kept
   it verbatim) survives normalization unchanged.
4. ghview + `step: "Run all Smoke Tests"` keeps only that step's lines and
   strips the tab prefix; a ghview line without tabs → throws
   `"unrecognized"`.
5. EOF inside preamble → throws.
6. Round-trip with the real checker: `import { checkLog } from
   "../tools/check-gha-log.ts"` — (a) normalized clean fixture →
   `checkLog(...).length === 0`; (b) normalized fixture where a
   `##[group]` opens before the previous closed → nesting violation
   reported; (c) fixture from case 3 → marker-not-at-column-0 violation.
   This pins the contract without touching the checker.

Verify: `cd tests && ./run-tests.sh unit/gha-rendered-log.test.ts` → `ok`.
Commit: `Add rendered-log normalizer for downloaded GHA logs`.

## Task 2 — `annotations.ts` (+ tests)

Interface + implementation (complete):

```ts
/*
 * annotations.ts — classify check-run annotations from a quarto-cli test
 * run and assert the #14715 annotation invariants. Pure: no network/env.
 *
 * Copyright (C) 2020-2026 Posit Software, PBC
 */

export interface CheckAnnotation {
  annotation_level: string;
  path: string | null;
  start_line: number | null;
  title: string | null;
  message: string;
}

export type AnnotationClass =
  | "harness"           // path=<test file>, title=<test name>
  | "harness-aggregate" // title="More test failures", no file
  | "yaml-orchestrator" // bucket loop's ::error, attributed under .github
  | "runner"            // "Process completed with exit code …"
  | "other";

// Order matters: runner-injected annotations are ALSO attributed under
// .github, so the message check precedes the path check.
export function classify(a: CheckAnnotation): AnnotationClass {
  if (a.title === "More test failures") return "harness-aggregate";
  if (a.message.startsWith("Process completed with exit code")) {
    return "runner";
  }
  if (a.title === "Test Bucket Failed" || (a.path ?? "").startsWith(".github")) {
    return "yaml-orchestrator";
  }
  if (a.path && a.title) return "harness";
  return "other";
}

export interface BudgetReport {
  ok: boolean;
  problems: string[];
  notes: string[];
  counts: Record<AnnotationClass, number>;
}

export const kHarnessCap = 9;     // per-test annotations per step
export const kGitHubJobCap = 50;  // GitHub's hard per-job cap

// `orchestrated`: caller says this job's test steps ran with
// QUARTO_TESTS_GHA_ORCHESTRATED=1 (bucket legs) — harness must be silent.
export function checkBudget(
  annotations: CheckAnnotation[],
  opts: { orchestrated: boolean },
): BudgetReport {
  const counts: Record<AnnotationClass, number> = {
    "harness": 0, "harness-aggregate": 0, "yaml-orchestrator": 0,
    "runner": 0, "other": 0,
  };
  for (const a of annotations) counts[classify(a)]++;
  const problems: string[] = [];
  const notes: string[] = [];
  if (counts["harness"] > kHarnessCap) {
    problems.push(
      `${counts["harness"]} harness annotations > cap ${kHarnessCap}`,
    );
  }
  if (counts["harness-aggregate"] > 1) {
    problems.push(
      `${counts["harness-aggregate"]} aggregate annotations (expected ≤ 1)`,
    );
  }
  if (opts.orchestrated &&
      counts["harness"] + counts["harness-aggregate"] > 0) {
    problems.push(
      "harness annotations present on an orchestrated leg " +
        "(QUARTO_TESTS_GHA_ORCHESTRATED gate failed)",
    );
  }
  if (annotations.length >= kGitHubJobCap) {
    problems.push(
      `${annotations.length} annotations ≥ GitHub job cap ` +
        `${kGitHubJobCap} — GitHub silently dropped the rest; ` +
        "annotation evidence is incomplete",
    );
  }
  return { ok: problems.length === 0, problems, notes, counts };
}
```

Tests (`tests/unit/gha-annotations.test.ts`):

1. `classify`: one literal example per class, **including** a
   runner-injected annotation with `path: ".github/workflows/x.yml"` →
   `"runner"` (not `"yaml-orchestrator"`); aggregate with `path: null` →
   `"harness-aggregate"`; `path: "tests/smoke/a.test.ts"` + title →
   `"harness"`; level-only junk → `"other"`.
2. `checkBudget`: 9 harness + 1 aggregate + 2 yaml, non-orchestrated →
   `ok: true`; 10 harness → problem; 2 aggregates → problem; orchestrated
   + 1 harness → problem mentioning the gate; 50 mixed → `ok: false`
   with an evidence-incomplete problem; counts always returned.

Verify: `cd tests && ./run-tests.sh unit/gha-annotations.test.ts` → `ok`.
Commit: `Add check-run annotation classifier and budget checks`.

## Task 3 — `gh.ts`

```ts
/*
 * gh.ts — minimal wrapper around the gh CLI for transport/auth.
 * Copyright (C) 2020-2026 Posit Software, PBC
 */

export async function gh(args: string[]): Promise<string> {
  const { code, stdout, stderr } = await new Deno.Command("gh", {
    args, stdout: "piped", stderr: "piped",
  }).output();
  if (code !== 0) {
    throw new Error(
      `gh ${args.join(" ")} failed (exit ${code}):\n` +
        new TextDecoder().decode(stderr),
    );
  }
  return new TextDecoder().decode(stdout);
}

export async function ghJson<T>(args: string[]): Promise<T> {
  return JSON.parse(await gh(args)) as T;
}

// gh api --paginate on an array endpoint emits one JSON document per page;
// --slurp wraps them in an outer array. Flatten to a single list.
export async function ghPaginatedArray<T>(path: string): Promise<T[]> {
  const pages = await ghJson<T[][]>(["api", path, "--paginate", "--slurp"]);
  return pages.flat();
}
```

No dedicated unit test (subprocess wrapper; exercised via Task 7 live
smoke). Commit: `Add gh CLI wrapper for ci-run helper`.

## Task 4 — `ci-run.ts`

Behavior contract (implement with plain `Deno.args`; shared flags parsed
first, then subcommand):

| Subcommand | Flags | Behavior |
|---|---|---|
| `fetch <run-id>` | `--repo` (default `quarto-dev/quarto-cli`), `--job <substr\|id>`, `--attempt N`, `--cache-dir D` | list jobs via `repos/{repo}/actions/runs/{id}/jobs` (`/attempts/{n}/jobs` when `--attempt`; page with `ghPaginatedArray` on `.jobs` — use `--jq`-free object pages: fetch `?per_page=100` pages and flatten `.jobs`), filter by name-substring or numeric id, save each `gh run view <run> --repo <repo> --job <jobId> --log` to `<cache>/run-<id>-job-<jobId>.log`, print absolute paths. Cache default: `Deno.makeTempDir({ prefix: "quarto-ci-run-" })` — never inside the checkout. |
| `check-log <path>` or `check-log <run-id> --job …` | `--step <substr>` | read file (or fetch first); `detectForm`; raw → `checkLog` directly; rendered → `normalizeRendered` (pass `--step`) → `checkLog`. Print the same OK/FAIL lines as `check-gha-log.ts`, prefixed with the detected form and `elidedBlocks`. Exit 0 clean / 1 violations / 2 usage or ambiguous form. |
| `annotations <run-id>` | `--repo`, `--json`, `--orchestrated <substr>` | resolve jobs; check-run id = last path segment of each job's `check_run_url`; fetch `repos/{repo}/check-runs/{id}/annotations` via `ghPaginatedArray`; per job: `checkBudget(anns, { orchestrated: name.includes(substr) })` (no `--orchestrated` → false for all, gate check skipped); human table `job / class / count / verdict` or `--json` dump `{job, annotations, report}`. Exit 1 if any `ok === false`. |
| `verdict <run-id>` | `--repo`, `--job`, `--orchestrated`, `--all-jobs` | composite: run `status/conclusion/html_url` from `repos/{repo}/actions/runs/{id}`; `annotations` pass; `check-log` on failed jobs (all with `--all-jobs`); **pre-harness detection**: red conclusion && harness+aggregate totals 0 → print `infra/pre-harness failure — no harness evidence; read the raw job log` with failing job names; end with the run URL, per-job `summary_raw` deep links (`github.com/{repo}/actions/runs/{run}/jobs/{jobId}/summary_raw` — raw markdown, logged-in browser only, never fetched by the tool) + eyeball checklist (step summary render, group collapse). Markdown-flavored output, paste-able into a PR comment; per-failure log deep links `…/job/{jobId}#step:{stepIndex}:{line}` (step index from the jobs API `steps[]`; line = 1-based offset of the failure's `::group::` marker in that step's rendered log). Exit 1 on any failed check. |

Notes: shebang `#!/usr/bin/env -S deno run --allow-read --allow-write
--allow-run=gh`; import `checkLog` from `../check-gha-log.ts`;
`import.meta.main`-guard everything (no side effects at import). Job type:
`{ id: number; name: string; conclusion: string | null; check_run_url:
string; html_url: string }`.

Verify (needs auth; smoke only): `deno run --allow-read --allow-write
--allow-run=gh tests/tools/gha/ci-run.ts annotations 29826031431 --repo
cderv/quarto-cli` → table shows a job with 9 harness + 1 aggregate,
exit 0. Commit: `Add ci-run CLI for CI test-run evidence retrieval`.

## Task 5 — `SKILL.md`

Create `.claude/skills/gha-run-analysis/SKILL.md` exactly:

```markdown
---
name: gha-run-analysis
description: Retrieves and verifies quarto-cli CI smoke-test evidence — run logs, ::error annotations, grouping invariants, step summaries — via the in-repo ci-run helper instead of ad-hoc gh api calls.
when_to_use: Analyzing a GitHub Actions test run; checking annotation caps (9+1); verifying ::group:: log-grouping invariants on a finished run; diagnosing a red run with an empty step summary; comparing bucket-mode logs.
allowed-tools: Bash(deno run --allow-read --allow-write --allow-run=gh ${CLAUDE_PROJECT_DIR}/tests/tools/gha/ci-run.ts *)
---

# Analyzing quarto-cli CI test runs

One executable helper (run it, don't read it):
`tests/tools/gha/ci-run.ts`. Default repo `quarto-dev/quarto-cli`
(`--repo` to override). Requires `gh` (default auth is enough;
fine-grained PATs get 403 on check-run annotations).

| Task | Command |
|---|---|
| Full verdict on a run | `deno run --allow-read --allow-write --allow-run=gh tests/tools/gha/ci-run.ts verdict <run-id>` |
| Annotation caps / gate check | `… ci-run.ts annotations <run-id> [--orchestrated bucket]` |
| Grouping invariants on a finished run | `… ci-run.ts check-log <run-id> --job "Smoke" --step "Run all Smoke Tests"` |
| Grouping invariants on local raw output | `GITHUB_ACTIONS=true ./run-tests.sh <subset> \| tee log.txt` then `… ci-run.ts check-log log.txt` |
| Download job logs | `… ci-run.ts fetch <run-id> [--job <substr>] [--attempt N]` |

Facts that save you an hour (details:
`dev-docs/ci-run-analysis-helper-design.md`):

- Downloaded logs are the RENDERED form (`##[group]`, timestamps, CRLF,
  BOM) — never feed them to `tests/tools/check-gha-log.ts` directly; the
  helper normalizes. A literal `::group::` in a rendered log = the runner
  did not parse it.
- Annotations: harness = path(test file)+title(test name); aggregate title
  "More test failures"; bucket-loop = "Test Bucket Failed" under .github.
  Caps: 9+1 per step, 50/job (GitHub drops the rest silently).
- Red run + EMPTY step summary + zero harness annotations = pre-harness /
  infra failure (expected signature, not a harness bug) — read the raw
  job log.
- Step summaries are UI-only (no REST endpoint; the
  `.../jobs/JOB_ID/summary_raw` web route serves raw markdown but only to
  a logged-in BROWSER — API tokens get 404). `verdict` prints the deep
  links; open them yourself. Re-runs: pass `--attempt N`.
- No gh (MCP-only session)? Endpoints: `repos/O/R/actions/runs/ID/jobs`
  (gives per-job `check_run_url`), `repos/O/R/check-runs/ID/annotations`,
  `gh run view <id> --job <jobId> --log` equivalent =
  `repos/O/R/actions/jobs/ID/logs`.
```

Commit: `Add gha-run-analysis project skill`.

## Task 6 — cross-references + status

1. `dev-docs/ci-test-log-grouping-design.md`, end of "Verification plan":
   add one line: `Retrieval/verification of finished runs is automated by
   tests/tools/gha/ci-run.ts — see dev-docs/ci-run-analysis-helper-design.md.`
   (This file belongs to #14715 — make this edit only when that PR has
   merged, or accept the trivial rebase.)
2. Flip the spec's Status line to
   `Status: **implemented** (tests/tools/gha/, .claude/skills/gha-run-analysis/)`
   and remove the plan-doc pointer.
3. `git rm dev-docs/ci-run-analysis-helper-plan.md` (this file).

Commit: `Wire ci-run helper into design docs; drop implementation plan`.

## Task 7 — live smoke (manual, before opening the PR)

Against known historical fork runs (evidence from the #14715 trials):

```
ci-run.ts annotations 29826031431 --repo cderv/quarto-cli
  → default-path job: 9 harness + 1 aggregate, ok
ci-run.ts annotations 29767185290 --repo cderv/quarto-cli --orchestrated bucket
  → zero harness on bucket jobs, ok (gate confirmed)
ci-run.ts annotations 29767190539 --repo cderv/quarto-cli
  → note fires: ≥ cap, silent dropping observed
ci-run.ts check-log 29826031431 --repo cderv/quarto-cli --job Linux --step "Run all Smoke Tests"
  → OK, elidedBlocks ≥ 1
ci-run.ts verdict 29826031431 --repo cderv/quarto-cli
  → composite report; seeded failures listed; exit 1 (run was red)
```

Record outputs in the PR description. If fork run logs have aged out
(90-day retention), substitute any recent upstream `test-smokes` run and
adjust expectations.

## Execution checklist

- [ ] Task 1 — normalizer + tests green
- [ ] Task 2 — classifier + tests green
- [ ] Task 3 — gh wrapper
- [ ] Task 4 — CLI, smoke-tested locally
- [ ] Task 5 — skill file
- [ ] Task 6 — xrefs, status, plan-doc removal
- [ ] Task 7 — live smoke recorded
- [ ] Full unit suite: `cd tests && ./run-tests.sh unit/` green
- [ ] PR opened only after #14715 merges (base rebases to main)
