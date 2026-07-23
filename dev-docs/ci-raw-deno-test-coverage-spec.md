# Design: per-file CI log grouping for raw `Deno.test` files

Status: **spec only, post-merge follow-up** to
[#14715](https://github.com/quarto-dev/quarto-cli/pull/14715) (log
grouping). Independent of leg partitioning
(`dev-docs/ci-test-leg-partitioning-design.md`) — this closes a coverage
gap inside whatever leg a file runs in. Companion:
`dev-docs/ci-test-log-grouping-design.md` (the mechanism being
extended).

## Problem

The per-file `::group::` mechanism lives in the harness `test()` wrapper
(`tests/test.ts`), so any test file registering with raw `Deno.test`
gets no group — and also none of the harness's failure surfacing
(annotations, step-summary rows, repro commands). Census on the
2026-07-22 full-serial run (complete — verified by grepping every
`.test.ts` in the tree AND the CI-merged subtrees):

| File | Owner | Why raw `Deno.test` |
|---|---|---|
| `integration/playwright-tests.test.ts` | repo | special executor; handled structurally (own leg) |
| `smoke/create/create.test.ts` | repo | uses `t.step`, which the harness does not support |
| `smoke/logging/log-level-and-formats.test.ts` | repo | deliberate — harness helpers mutate the logging config under test |
| `smoke/julia-engine/julia.test.ts` | **external** (PumasAI subtree) | third-party code, must run in its home repo without our harness |
| `smoke/julia-engine/render.test.ts` | **external** (same) | same |

Two structural facts make this a *class*, not a fixed list:

- The julia files are copied in at CI time by
  `.github/actions/merge-extension-tests` from
  `src/resources/extension-subtrees/`; the action is designed to grow
  more subtrees, each likely bringing raw `Deno.test` files. A static
  in-repo census is therefore never complete.
- Binary mode currently skips the julia subtree (spawn-env
  sanitization TODO — see the action), so the built smoke leg is clean
  *today*; the gap joins the built leg the moment that skip is lifted.

Any fix must (a) cover files we do not own and cannot edit, (b) not
require future authors to remember an opt-in, and (c) leave local and
orchestrated (bucket) runs byte-identical, like all grouping work.

## Verified facts (research 2026-07-23, empirical on pinned Deno v2.7.14)

Each claim below was verified by running the exact pinned binary
(v2.7.14) unless marked docs-only.

1. **`deno test --preload <file>`** exists (landed Deno 2.4.0; CLI flag
   only — a `"preload"` key in config files is silently ignored). The
   preload script is evaluated **once per test-file module graph**,
   **before the test file's module eval**, and `Deno.mainModule` is
   already set to the test file's URL at that point. `unload` listeners
   registered there fire once per file graph. This matches the
   per-file scope the harness state already lives in (SCOPE WARNING in
   `tests/test.ts`).
2. **`Deno.test` is a plain writable property**; the runner never
   captures it — registration happens by *calling* it, so a wrapper
   installed pre-eval intercepts every overload form. `@std/testing/bdd`
   also looks it up freshly at registration time. Precedents exist
   (qunitx-cli's `--preload` timeout wrapper; cknight/setupTeardown;
   others). Known wrapper obligations: re-dispatch all overloads, carry
   `.only`/`.ignore` statics via `Object.assign`, cast for typecheck.
3. **`Deno.test.beforeAll/afterAll/beforeEach/afterEach`** (landed
   2.5.0) are per-file-realm and DO apply to raw `Deno.test` tests —
   but `beforeAll` fires at first-test *execution* time, after module
   eval, so module-eval output (e.g. playwright's fixture renders)
   would fall outside a group opened there.
4. **No custom reporter API** exists through 2.7.x (`pretty | dot |
   junit | tap` only). `--junit-path` writes per-file/per-step XML
   *alongside* the pretty stdout reporter, but only at run end —
   useful for the `ci-run` analysis helper someday, useless for live
   grouping.
5. **Custom lint plugins** (landed 2.2.0, API marked "evolving"):
   per-file scoping via `context.filename`, suppression via
   `// deno-lint-ignore <plugin>/<rule>`; denoland/std runs one in
   production. `tests/` is not linted today, and lint cannot see
   CI-merged files at their runtime location.
6. **Convention check scripts** are how denoland/deno and denoland/std
   themselves enforce what lint can't express (`tools/lint.js`,
   `_tools/check_*.ts` + `deno task ok`).

## Options considered

- **A. Explicit helper** (`registerRawTestFileGroup(path)` imported by
  each raw file): simple, but cannot cover external subtree files
  (their home-repo CI lacks the helper) and re-introduces the
  remember-to-opt-in failure mode. Rejected as the primary mechanism.
- **B. Preload-owned per-file grouping — RECOMMENDED.** A tiny
  `--preload` script opens the file's group at preload-eval time and
  closes it at `unload`. Covers every file — harness, raw, subtree,
  future — with zero changes to any test file, and opens *before
  module eval*, so module-eval output (playwright renders) lands inside
  the right group, which even registration-time opening cannot do.
- **C. `Deno.test` wrapper**: not needed for grouping (B suffices) —
  retained as the future vector if raw files should ever get harness
  *failure reporting* (annotations/rows) without adopting the harness.
- **D. Lifecycle hooks for grouping**: rejected — misses module-eval
  output (fact 3); strictly dominated by B.
- **E. Merge-action code injection** (prepend a shim import to copied
  subtree files): workable chokepoint, but superseded by B, which needs
  no mutation of third-party sources.
- **F. Upstream harness adoption**: publish a quarto-provided
  test-helper API that extension repos (e.g. quarto-julia-engine)
  depend on, giving their tests full harness reporting in both repos.
  Complementary long-term track — the only option that closes the
  *reporting* gap for external files — but requires upstream
  coordination; B needs none. Own work track, out of scope here.

## Recommended design (grouping Phase 3)

New file `tests/gha-preload.ts`, passed via `--preload` in BOTH
`run-tests.sh` and `run-tests.ps1` (same `QUARTO_DENO_OPTIONS`
construction; flag is passed unconditionally — the script gates itself
on `harnessOwnsStep()`, so local and orchestrated runs remain
byte-identical). Sketch:

```ts
// tests/gha-preload.ts — evaluated once per test-file module graph,
// BEFORE the file's module eval (Deno >= 2.4 --preload semantics).
import { closeTestFileGroup, enterTestFileGroup } from "./gha-grouping.ts";
import { harnessOwnsStep } from "../src/tools/github.ts";

if (harnessOwnsStep()) {
  enterTestFileGroup(testsRelativePath(Deno.mainModule));
  globalThis.addEventListener("unload", () => closeTestFileGroup());
}
```

Composition with the existing harness (the design's crux): the preload
shares the file graph's module cache, so `gha-grouping.ts`'s emitter
singleton is the SAME instance the harness uses — `test()`'s
registration-time and body-time `enterTestFileGroup` calls become
same-key no-ops, and the harness's failure-path close + reopen keeps
working unchanged. The harness's own unload close and the preload's are
both safe (close is idempotent). **This sharing is an assumption until
spiked — see VERIFY-FIRST.**

Once proven in CI, Phase 2.1's stack-parsing registration-time open
becomes redundant (the preload opens earlier and knows the file
directly); remove it in a follow-up commit with evidence, not in the
same change.

What raw files still do NOT get (documented limitation, not fixed
here): harness failure surfacing. Their inline `FAILED` line lands
inside the file's group; Deno's end-of-run `ERRORS`/`FAILURES` sections
print after the last file's unload close and stay outside all groups.

### VERIFY-FIRST spikes (all must pass before implementation)

1. **Module-cache sharing**: preload's import of `gha-grouping.ts` and
   the test file's (transitive) import resolve to one module instance
   per graph. Probe: counter in a shared module, logged from both
   sides. If NOT shared → fallback: an env flag set by the preload
   makes `test.ts` skip its own opens (preload becomes sole owner).
2. **Ordering vs previous file**: previous file's `unload` (endgroup)
   fires before the next file's preload eval (expected from the
   2026-07-22 log's module-eval ordering; confirm with the probe).
3. **Byte-identity off-CI and in orchestrated mode**: `--preload`
   present but gated off produces zero output difference.
4. **Windows parity**: `run-tests.ps1` flag wiring;
   `Deno.mainModule` → tests-relative path via `fromFileUrl` (reuse
   `testFileFromOrigin`'s logic; note the preload runs with `tests/` as
   cwd from the runner scripts — verify path derivation there).
5. **Raw-file failure rendering**: seeded failure in a raw
   `Deno.test` file — confirm the end-of-run error detail lands outside
   groups and the inline FAILED-inside-group residual is acceptable.
6. **Subtree coverage**: merged julia files get groups with no change
   to subtree sources (dev-mode trial run).
7. **Interaction with `deno test --check`**: preload is typechecked
   with the same config; confirm no conflict with `test-conf.json`
   compiler options on Windows (`--check` is in the ps1 options).

## Guardrail (recurrence control)

With B in place, grouping is automatic, so the guardrail's job shifts
to the *reporting* gap: new raw `Deno.test` files silently lose
annotations/summary/repro. Chosen guard: a convention check script
(std/deno `_tools` pattern — fact 6), NOT a lint plugin for now
(`tests/` is unlinted today; plugin API still "evolving"; lint can't
see CI-merged paths; the script's central allowlist doubles as the
intentional-bypass registry):

- `tests/_tools/check-raw-deno-test.ts`: walk `tests/**/*.test.ts` AND
  `src/resources/extension-subtrees/*/tests/**/*.test.ts`, flag
  `Deno.test` call/property use outside the allowlist (match call
  position, not comments — `tests/test.ts` mentions it in prose).
  Allowlist = the five files above + `tests/test.ts` itself.
- CI step + doc entry ("Don't call `Deno.test` directly") in
  `.claude/rules/testing/` anti-patterns, stating what a bypassing file
  loses and pointing here.
- Upgrade path if wanted later: the same rule as a `deno lint` plugin
  for in-editor feedback (std precedent), suppressions replacing the
  allowlist.

## Phasing

Small standalone PR after #14706/#14715 merge (not part of either, not
coupled to leg partitioning): spikes 1–7 on a fork trial branch →
implementation (preload + runner-script flags + check script + doc
entry + unit tests for the path derivation) → seeded-failure trial run
→ Phase 2.1 removal follow-up with evidence.

## Non-goals

- Harness failure reporting for raw files (future: option C wrapper or
  option F upstream API).
- Publishing a quarto test-helper API for extension repos (option F —
  separate track).
- bdd migration, custom reporters, node:test, `--parallel` (grouping
  requires serial output; unchanged).

## References

- `dev-docs/ci-test-log-grouping-design.md` — mechanism, invariants.
- `dev-docs/ci-test-leg-partitioning-design.md` — structural context.
- `.github/actions/merge-extension-tests/action.yml` — subtree merge +
  binary-mode skip.
- Research sources: Deno 2.4 / 2.5 release notes (`--preload`,
  lifecycle hooks), denoland/deno#8550 (no custom reporters),
  docs.deno.com lint-plugin reference, denoland/std
  `_tools/lint_plugin.ts` + `_tools/check_*.ts`, qunitx-cli
  `deno-test-timeout.ts` (preload wrapper precedent).
