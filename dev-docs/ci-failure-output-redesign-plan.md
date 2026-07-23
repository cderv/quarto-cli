# Plan: single-print, audience-aware test-harness failure output

Status: **plan only, post-merge follow-up** to
[#14715](https://github.com/quarto-dev/quarto-cli/pull/14715). All
file:line references below are against that PR's branch (head
`0476b338f`), which is the implementation baseline — `main` lacks the
CI-reporting machinery this composes with. Companion docs:
`dev-docs/ci-test-log-grouping-design.md` (CI surfaces + invariants),
`dev-docs/ci-run-analysis-helper-design.md` (future log consumer).

Motivation (observed, 2026-07-22 full-serial CI run): ONE failing
smoke-all document prints ~60 lines in Deno's end-of-run `ERRORS`
section, of which ~40 are duplicates — the same error + 10-frame quarto
stack trace appears THREE times. Locally the same format makes
`./run-tests.sh` failures far too verbose, and it is poor input for
AI-assisted debugging (duplication burns context for zero signal).

## 1. Inventory: where failure text comes from

Pipeline for a failing dev-mode test:

```
quarto render throws
  → tests/test.ts:405-419 catch: logError(e) → ONE json-stream ERROR
    record (message + stack; stack included because run-tests.sh:61
    exports QUARTO_DEBUG=true)
  → verifiers read the captured log (test.ts:433, readExecuteOutput)
  → noErrorsOrWarnings (verify.ts:151-180) asserts with the FULL record
    text embedded in the assert message between |…| (verify.ts:172-175)
  → test.ts:446 catch builds the "━━━ TEST FAILURE:" banner and calls
    fail(output.join("\n")) at test.ts:637
  → Deno prints that one AssertionError in the ERRORS section
```

Structural fact: the banner is never console.logged — it travels as the
`fail()` message, printed once by Deno. All duplication is INSIDE that
one message. The three copies (banner construction, test.ts:499-522):

| # | Element | Line | Content |
|---|---|---|---|
| A | `ex.message` | test.ts:509 | verify assert message with full captured records embedded |
| B | `ex.stack` | test.ts:510 | **V8 stacks embed the message** — `ex.stack` = `"AssertionError: " + ex.message + frames`, so this line reprints ALL of A plus ~6-10 harness frames with no debugging value. This is a plain bug: pushing both `.message` and `.stack` always double-prints multi-line messages. |
| C | `OUTPUT:` | test.ts:514-522 | every captured log record re-printed wholesale — including the record already inside A and B |

The CI excerpt (`rawExcerpt`, test.ts:545-560) is built from the same
triple, so annotation excerpts and summary cluster excerpts also lead
with duplicated content; `excerptSignature()` (src/tools/github.ts:328)
keys on lines of copy A.

**Wrong terminal repro:** the banner prints `${command} ${relPath}`
(test.ts:465-474) — for smoke-all that is
`./run-tests.sh smoke/smoke-all.test.ts` (reruns ~1600 docs). The CI
path ALREADY extracts the document path from the test name
(test.ts:533-541) and uses it for `annotationFile` and the CI repro;
the terminal banner predates it.

**Verifiers embedding bulk content in assert messages** (verify.ts):
`noErrors` (127-130), `noErrorsOrWarnings` (172-175), `xmlChecker`
(949-961: the ENTIRE XML document), `pptxLayoutChecker` (976-997),
`ensureHtmlElementContents` (439-451), `ensureXmlValidatesWithXsd`
(1314-1317), `ensureMECAValidates` (1340-1343). Existing good patterns
to converge on: `ensurePdfRegexMatches` (856-874, short aggregated
mismatch list) and `ensureSnapshotMatches` (573-604, diff printed once
at verify time, one-line assert).

**Captured-output sources:** dev mode — harness logger json-stream
capture (test.ts:387-392) + `logError` append; binary mode — child
`--log` merged by `mergeChildLog` (quarto-cmd.ts:427-486), which
synthesizes an ERROR record with a 25-line stderr tail on silent
failure (quarto-cmd.ts:399-400, 469-485).

**CI surfaces (compose with, do not redesign):** annotation budget
9+1 (github.ts:208-252), `annotationBody` = repro + 5 excerpt lines +
`Full output: step summary → L-Fn` pointer (github.ts:410-421), summary
rows + per-signature clusters (github.ts:336-404), signature = first 3
non-empty ANSI-stripped excerpt lines (github.ts:328-334). Unit
coverage: `tests/unit/github-actions-reporting.test.ts`.

## 2. Target format: one failure = one record

Each piece of information appears exactly once; still delivered as the
`fail()` message (no reporter change). Mechanics:

1. **Verifiers assert short.** New `tests/failure-report.ts` with
   `failVerify(summary, detail?)` throwing `VerifyError extends
   AssertionError` carrying `.detail`. Log-content verifiers stop
   embedding records (summary e.g. `Errors During Execution (2 error
   records)`); the harness already holds the records and prints the
   flagged ones itself. Non-log verifiers pass evidence via `.detail`;
   detail over ~60 lines spills to a temp file (local modes only — on
   CI prefer capped inline, temp files vanish).
2. **Banner = the single detailed record**: marker, doc-path repro,
   verify summary + one anchor frame (assert site), detail once, then a
   **log tail that set-dedupes records already shown**, capped.
   `OUTPUT:` as a full dump disappears.
3. **Drop `ex.stack` wholesale** for assertion errors (frames-only for
   *unexpected* errors, where frames ARE the information).
4. **Repro fix everywhere**: hoist test.ts:533-541 into a pure exported
   `reproTarget(testName, absPath, relPath)` used by both the terminal
   banner and the CI block.

Mock (local human mode; ~20 lines instead of ~60, error appears once):

```
━━━ TEST FAILURE: [smoke] > quarto render docs/smoke-all/2025/01/issue-9999.qmd --to html
────────────────────────────────────────────────────────────────────────────────
  repro:   ./run-tests.sh docs/smoke-all/2025/01/issue-9999.qmd
  verify:  No Errors or Warnings — 1 flagged record   (tests/verify.ts:172)

  flagged output:
    TypeError: Cannot read properties of undefined (reading 'format')
    Stack trace:
        at renderFormats (file:///…/src/command/render/render-contexts.ts:123:5)
        … 8 more frames (QUARTO_TEST_VERBOSE=true for all)

  log tail (last 6 of 31 records; flagged records excluded above):
    pandoc --to html …
    Output created: issue-9999.html
────────────────────────────────────────────────────────────────────────────────
```

## 3. Audience modes

Existing signals: `isGitHubActions()`, `harnessOwnsStep()`,
`isVerboseMode()` (`RUNNER_DEBUG`/`QUARTO_TEST_VERBOSE`),
`userSession = !runningInCI()`. Only QUARTO_TEST_VERBOSE is
verbosity-related today.

One new env var `QUARTO_TEST_OUTPUT_MODE` ∈ `human | ci | agent`
(unset → derived):

| Mode | Derivation | Format |
|---|---|---|
| `human` | default off-CI | mock above; colored; detail cap ~40, tail cap ~10; ellipses point at QUARTO_TEST_VERBOSE=true |
| `ci` | default on CI | same record, no color, larger caps (the log is CI's only full-fidelity surface; summary excerpt is 20 lines); annotations/summary unchanged |
| `agent` | explicit only | dense field-per-line (`test:`/`repro:`/`verify:`/`error:`/`log-tail:`), no glyphs/color (force NO_COLOR semantics), fixed caps — designed as AI context |

`isVerboseMode()` stays orthogonal (lifts caps in any mode). Mode
resolution is a pure `resolveOutputMode(env)` for unit-testability.
Rejected: auto-detecting agents (e.g. CLAUDECODE env) — implicit format
switches make repro reports non-reproducible; agents export the var
explicitly (document in llm-docs/testing-patterns.md).

## 4. Implementation plan (ordered)

1. **Repro fix** — extract `reproTarget()`, use in banner + CI block;
   unit tests (smoke-all name → doc path; non-smoke-all → file path;
   Windows backslash normalization). Safe relative to CI surfaces
   (repro is not part of `rawExcerpt`, so signatures/excerpts are
   byte-identical; only the terminal line changes). NOTE: `main` lacks
   this machinery — this lands as a follow-up commit on the #14715
   branch or immediately post-merge, never as a PR against `main`.
2. **Kill copy B** (`ex.stack` duplication) — frames-only extraction in
   banner and `rawExcerpt`. ~12 lines saved per failure. Changes
   excerpt content beyond line 3 → after #14715 merges (or as a
   reviewed pre-merge commit updating the unit-test expectations in the
   same commit).
3. **Single-print record + short verifier messages** (the main change;
   after merge). New `tests/failure-report.ts` (`FailureRecord`,
   `formatFailure(record, mode)`, `buildExcerpt(record)`,
   `resolveOutputMode`); convert the seven bulk-embedding verifiers to
   `failVerify`; rebuild the catch block on `FailureRecord`; update
   `github-actions-reporting.test.ts` fixtures; new
   `tests/unit/failure-report.test.ts` (per-mode snapshots: log-verifier
   failure, detail verifier, unexpected error, binary-mode stderr-tail,
   dedupe). Ship as 2 commits (verifiers, then formatter). Validate
   clustering discrimination on the reference workload shape (28-of-36
   identical errors → one cluster) via a seeded fork trial,
   Linux + Windows, default + bucket legs, plus `check-gha-log.ts`.
4. **Modes env var + docs** — mode table, tests/README.md,
   llm-docs/testing-patterns.md (agents export `agent` mode),
   grouping-design-doc note that excerpts derive from the failure
   record. Also CLI sugar in both runner scripts (decided 2026-07-23):
   a general `--output-mode=<human|ci|agent>` plus one alias per mode —
   `--agent`, `--ci`, `--human` — all simply exporting
   `QUARTO_TEST_OUTPUT_MODE` (explicit flag wins over an inherited env
   var; last flag wins if repeated). Flags are discoverable in usage
   text and make the modes equally reachable on Windows, where env-var
   prefixing is clumsy. CAUTION: both scripts classify positional args
   as `.ts` files vs smoke-all documents (with a `--` pass-through);
   the flags must be stripped from the arg list BEFORE that
   classification and must never leak into the deno invocation.

## 5. Risks / open questions

1. **Signature semantics**: signatures are per-run only (clustering
   never persists), so the risk is discrimination quality, not compat.
   Keep record COUNTS out of the excerpt's first line (else
   near-identical failures split into separate clusters); decide and
   state in the step-3 PR.
2. **Banner-format consumers**: repo grep finds only test.ts and the
   design doc referencing `━━━ TEST FAILURE`; keep that marker string
   verbatim (documented Ctrl+F target). `check-gha-log.ts` parses only
   Deno reporter lines — untouched. Humans grepping `OUTPUT:` lose
   that anchor — call out in the PR.
3. **ci-run helper**: its normalizer touches runner framing only, its
   annotation classifier keys on path/title — unaffected. BUT its
   fixtures will be cut from real logs: **land step 3 before ci-run
   fixtures are cut**, or they encode the old triple-print shape. Keep
   the `Full output: step summary → <label>` pointer line verbatim.
4. **Windows**: `run-tests.ps1` command name (no `./`); backslash
   test names normalized inside `reproTarget()`; trial matrix must
   include Windows.
5. **Binary mode**: the synthetic stderr-tail ERROR record must be
   treated as flagged content by the log-tail dedupe (fixture d).
6. **Open — ci-mode tail cap**: 50 lines is a guess; decide from the
   first trial's real sizes.

## Origin

Produced from a planning pass on 2026-07-23 (research-verified against
branch head 0476b338f), triggered by the 2026-07-22 full-serial run's
ERRORS section and the observation that failure output is too verbose
for both local use and AI-assisted debugging.
