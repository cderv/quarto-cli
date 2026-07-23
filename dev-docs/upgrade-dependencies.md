Change version numbers in `./configuration` to correspond to new versions.

Update hardcoded version strings in `src/command/check/check.ts` (`versionConstraints` array, ~line 249) so that they match the new versions in `configuration`. The `configuration` file warns about this in a comment.

## Upgrade deno

### Upgrade standard library

- run `./configure.sh` (Linux/macOS) or `./configure.cmd` (Windows) to locally install all dependencies against the new Deno binary.

- `src/import_map.json` has migrated to JSR (`jsr:/@std/<package>@<version>` entries). If `configure` errors with `Module not found: jsr:/@std/...`, bump only the specific `@std` package(s) named in the error to a compatible version on <https://jsr.io/@std>. Otherwise, leave `src/import_map.json` alone — historical pattern is reactive (no pre-emptive bumps).

- run `./configure.sh` / `./configure.cmd` again.

Bumping a version in `src/import_map.json` (or any of the other keyed files) automatically invalidates the CI Deno cache on next run. See [ci-deno-caching.md](ci-deno-caching.md) for the key composition and how to force invalidation manually.

### Test-runner behaviors to re-verify on Deno upgrades

The CI test harness (log grouping, failure reporting — see
`dev-docs/ci-test-log-grouping-design.md` and
`dev-docs/ci-raw-deno-test-coverage-spec.md`) depends on `deno test`
behaviors that are version-verified facts, not documented guarantees.
When bumping Deno, re-check each (all verified empirically on v2.7.14,
2026-07-23):

- **Per-file module graph instantiation**: each test file's module
  graph is instantiated separately (module-level state is per FILE,
  `unload` fires once per file) — the SCOPE WARNING in `tests/test.ts`.
  All harness per-file state (grouping, summary headers, clustering)
  assumes this.
- **`--preload` semantics** (flag landed 2.4.0): evaluated once per
  test-file module graph, BEFORE the file's module eval, with
  `Deno.mainModule` already set to the test file. If Deno ever makes
  preload once-per-process, preload-based per-file grouping breaks.
- **`Deno.test` is a plain writable property** and the runner reads it
  at call time (never captures it) — required if we ever wrap it for
  instrumentation or runtime guards.
- **Lifecycle hooks** (`Deno.test.beforeAll` etc., landed 2.5.0) are
  per-file-realm and apply to raw `Deno.test` tests.
- **Custom reporters: absent and declined** — `--reporter` is `pretty |
  dot | junit | tap` only, and the tracking issue
  (denoland/deno#8550) was closed as not planned. Console-level
  grouping is therefore the sanctioned long-term approach, not a
  stopgap. Still skim release notes for machine-readable/streaming
  test output additions, which could matter for CI run analysis.
- **`--junit-path` works alongside the pretty stdout reporter**
  (per-file/per-step XML at run end) — candidate input for CI run
  analysis tooling.
- **Custom lint plugin API** (landed 2.2.0) is officially "evolving" —
  if we adopt a plugin for test conventions, expect possible churn.

### Upgrade Deno download link for RHEL build from conda-forge

- Go to <https://anaconda.org/conda-forge/deno/files> and find the version of Deno required.
  - BTW those versions are built at <https://github.com/conda-forge/deno-feedstock>
- Take the hash part of the download link for linux-64 (e.g. `hcab8b69_0` for `linux-64/deno-1.46.3-hcab8b69_0.conda`)
- Use it in the build release action: `.github\workflows\create-release.yml` at the step `- name: Move Custom Deno`. The hash appears in **three places** inside that step (echo line, curl line, tar line). All three must be updated.
  ```
  echo Placing custom Deno ${DENO:1}. See available versions at https://anaconda.org/conda-forge/deno/files hcab8b69_0
  curl -L https://anaconda.org/conda-forge/deno/${DENO:1}/download/linux-64/deno-${DENO:1}-hcab8b69_0.conda --output deno.conda
  unzip deno.conda
  tar --use-compress-program=unzstd -xvf pkg-deno-${DENO:1}-hcab8b69_0.tar.zst
  ```
- The `make-tarball-rhel` job that wraps these steps may carry `if: false` for unrelated reasons; the hash is updated for forward consistency even while the job is disabled.
- Commit the `create-release.yml`

## Upgrade mermaidjs

Apparently mermaidjs doesn't actually build mermaid in their releases :shrug:.
They also don't appear to offer any clear documentation on how to do it, and `npm install` from their `packages/mermaidjs` directory just fails.

So, we grab the published javascript bundles from unpkg.com.

For version 11.2.0, for example, these are:

- https://unpkg.com/mermaid@11.2.0/dist/mermaid.js
- https://unpkg.com/mermaid@11.2.0/dist/mermaid.min.js

Copy these files to `src/resources/formats/html/mermaid`.
