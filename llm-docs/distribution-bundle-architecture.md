---
main_commit: 676e0b012
analyzed_date: 2026-06-01
key_files:
  - configuration
  - package/src/common/prepare-dist.ts
  - package/src/common/dependencies/dependencies.ts
  - package/src/common/dependencies/pandoc.ts
  - package/src/common/dependencies/deno.ts
  - package/src/macos/installer.ts
  - package/src/windows/installer.ts
  - package/scripts/common/quarto
  - package/scripts/macos/pkg/postinstall
  - package/launcher/src/main.rs
  - src/core/resources.ts
---

# Distribution Bundle Architecture & Size

How a Quarto release is assembled on disk, why it is the size it is, how the
launcher selects binaries at runtime, and what levers exist to reduce size.
Written after investigating why RStudio's bundled Quarto roughly doubled in
*installed* size between RStudio 2023.09 and 2024.09.

For how these bundles get signed/notarized, see
[code-signing-installers.md](code-signing-installers.md). This doc is about
**layout and size**, not signing.

## TL;DR

- Quarto barely "builds" anything itself. The large payload is **downloaded
  third-party binaries** (Pandoc, Deno, Typst, Dart Sass, esbuild) shipped
  **once per architecture**. The only things Quarto compiles are `quarto.js`
  (the TS→JS bundle, ~5.5 MB) and, on Windows only, a ~300 KB Rust launcher.
- **macOS is a universal bundle: it ships BOTH `aarch64` and `x86_64`
  toolchains** under `bin/tools/<arch>/`, fully duplicated (no `lipo`, no
  dedup). The launcher selects one at runtime. This roughly doubles the binary
  half of the install on any given Mac.
- **Linux and Windows are single-architecture** (`x86_64` only; no ARM build is
  published). They do not have the macOS doubling.
- The macOS doubling was introduced by PR **#6182 `feature/no-rosetta`**
  (merged 2023-07-11, first shipped in Quarto **v1.4.216**), which added native
  Apple-Silicon binaries (esp. a native arm64 Pandoc) instead of running an
  Intel-only Pandoc under Rosetta 2.

## What gets bundled

Versions of every bundled dependency are pinned in the root `configuration`
file (`DENO`, `PANDOC`, `DARTSASS`, `ESBUILD`, `TYPST`, `TYPST_GATHER`,
`DENO_DOM`, `VERAPDF`, plus many JS/web libraries vendored into resources).

Per-platform binaries that dominate the installed footprint:

| Binary | Role | Source / who builds it |
| --- | --- | --- |
| Pandoc | document converter | upstream (statically-linked Haskell) — **largest binary** |
| Deno | Quarto's JS/TS runtime | upstream |
| Typst | PDF engine (added in 1.4) | upstream |
| Dart Sass | SCSS compiler | upstream |
| esbuild | JS bundler | upstream |
| deno_dom | DOM plugin (`.dylib`/`.so`/`.dll`) | upstream |
| `quarto.js` | compiled Quarto CLI | built here (esbuild/deno) |
| `quarto.exe` | Rust launcher (**Windows only**) | built here (`package/launcher`) |

Plus ~55–70 MB of **architecture-independent resources** under `share/`
(formats, reveal.js, PDF.js, Mermaid, fonts, Lua filters, schemas).

## Bundle layout per platform

```
macOS (universal):           Linux / Windows (single arch):
bin/                         bin/
  quarto         (bash)        quarto            (bash, *nix)
  quarto.js                    quarto.cmd + quarto.exe (Windows)
  tools/                       quarto.js
    aarch64/                   tools/
      pandoc deno typst ...      x86_64/
    x86_64/                        deno typst dart-sass esbuild deno_dom
      pandoc deno typst ...      pandoc(.exe)   <- kept flat (see note)
share/   (resources)         share/   (resources)
```

Notes:
- **macOS ships two complete `tools/<arch>` trees.** Verified: the two `pandoc`
  copies are distinct inodes (no hardlink dedup), and the bundle is **not** a
  `lipo` fat binary — `grep -r lipo package/` returns nothing.
- **Pandoc is kept at the flat `bin/tools/pandoc(.exe)` level** (not only inside
  the arch dir) for **RStudio compatibility**: RStudio's CMake resolves pandoc
  at `${QUARTO_DIR}/bin/tools`. On macOS the `.pkg` `postinstall` creates a
  symlink `tools/pandoc -> tools/<arch>/pandoc`; on Windows the real
  `pandoc.exe` lives flat.

## How both architectures are produced (macOS)

`package/src/common/prepare-dist.ts` — `configArchDependency()` configures each
dependency **twice** when `config.os === "darwin"` (once for `aarch64`, once for
`x86_64`); other OSes configure a single arch. Each dependency's download URLs
are defined per-arch in `package/src/common/dependencies/*.ts` (e.g.
`pandoc.ts` has both `pandoc-<v>-arm64-macOS.zip` and
`pandoc-<v>-x86_64-macOS.zip`). `package/src/macos/installer.ts` then
codesigns every tool in **both** arch trees
(`["aarch64","x86_64"].forEach(...)`).

## Runtime architecture selection

There are **two** launchers depending on platform — both end up running
`deno run quarto.js` after pointing Deno at the right `tools/<arch>` dir.

- **macOS / Linux: bash script** `package/scripts/common/quarto` (shipped as
  `bin/quarto`). On macOS it detects the CPU via
  `sysctl machdep.cpu.brand_string` (not `uname` — see issue #2420) and sets
  `ARCH_DIR=aarch64|x86_64`; on Linux it uses `uname -m`.
- **Windows: Rust binary** `quarto.exe` built from `package/launcher/src/main.rs`
  (+ a `quarto.cmd`). There is **no `quarto.exe` on macOS/Linux** — confirmed by
  inspecting the bundles. The Rust source is actually cross-platform
  (`#[cfg(target_os = ...)]`, a `deno_dir()` that handles `Darwin arm64/x86_64`)
  and *could* serve all platforms, but today it is only compiled/shipped on
  Windows.
- TS-side tool resolution uses `Deno.build.arch` via
  `architectureToolsPath()` in `src/core/resources.ts` (e.g.
  `pandocBinaryPath()`).

**Why a compiled exe only on Windows?** Code-signing/trust, not function.
Windows SmartScreen/Defender extend trust to a process tree only if the entry
`.exe` carries an Authenticode signature; a `.cmd`/`.bat` cannot be signed, so
Quarto ships a real signed PE as the entry point. On macOS the bash `quarto`
script is `codesign`'d for tamper-evidence, and Gatekeeper enforcement applies
to the Mach-O binaries it invokes (which are signed individually). See
[code-signing-installers.md](code-signing-installers.md).

## Measured installed sizes (v1.6.42)

Measured by downloading the release tarballs and `du`-ing the unpacked trees
(Linux `du`; macOS-disk figures may differ a few % due to APFS block sizing).

| | macOS (universal) | Linux (x86_64) | Windows (x86_64) |
| --- | --- | --- | --- |
| **Total installed** | **~670 MB** | ~419 MB | ~442 MB |
| Architectures shipped | **2** | 1 | 1 |
| Pandoc | 172 + 106 | 144 | **213** |
| Deno | 106 + 112 | 137 | 103 |
| Typst | 29 + 30 | 36 | 33 |
| dart-sass / esbuild / deno_dom | ~21 ×2 | ~25 | ~20 |
| `share/` (arch-independent) | 71 | 71 | 71 |
| Launcher | bash (signed) | bash | Rust `quarto.exe` (~0.3 MB) |

The macOS total is ~1.6× the single-arch platforms purely because the binary
half is duplicated. Windows ≈ Linux; Windows is slightly larger mainly because
**Windows `pandoc.exe` (~213 MB) is the single largest binary in the project**.
The ~300 KB Rust launcher is irrelevant to size.

## History: the 1.3 → 1.4 size jump (PR #6182)

Quarto 1.3 shipped an **Intel-only Pandoc** that ran under **Rosetta 2** on
Apple Silicon (Deno was already per-arch). PR **#6182 `feature/no-rosetta`**
(merge `a68aad0cd`, 2023-07-11, first in **v1.4.216**) reorganized macOS into
native per-arch `tools/aarch64` + `tools/x86_64` trees and added a **native
arm64 Pandoc**.

Measured effect, macOS install: **1.3.x ≈ 342 MB → 1.4.x ≈ 654 MB** (+312 MB).
Decomposition:

| Cause | macOS Δ | Also on Linux? |
| --- | --- | --- |
| **#6182 — native arm64 Pandoc** (94 MB Intel-only → ~280 MB dual native) | **+186** | no (macOS-only) |
| #6182 — second-arch copies of typst/esbuild/sass | ~+25 | no (macOS-only) |
| Typst added (new in 1.4, per arch) | ~+58 | yes (+32 on Linux) |
| Pandoc v2→v3 + Deno bump | ~+40 | yes |

Cross-check: Linux (single-arch) grew only **+87 MB** across the same step. The
**~+225 MB macOS-only difference is #6182** (the second native architecture).

### Case study: RStudio install size

RStudio bundles Quarto's macOS *universal* tarball verbatim. Mapping RStudio
releases to the Quarto they pin (`dependencies/common/install-quarto`):

| RStudio | Quarto | Layout |
| --- | --- | --- |
| 2023.09.0 / 2023.09.1 | 1.3.433 | pre-#6182 (Rosetta) |
| 2023.12.0 / 2023.12.1 | 1.3.450 | pre-#6182 |
| 2024.04.0 | 1.4.553 | **post-#6182** |
| 2024.09.0 / 2024.09.1 | 1.5.57 | post-#6182 |

So RStudio's macOS install roughly doubled at the **2023.12 → 2024.04**
transition (Quarto 1.3.450 → 1.4.553), i.e. exactly when it crossed the
#6182 boundary — not because RStudio "started bundling Quarto" (it always did).

## Levers to reduce installed size (roughly by impact)

1. **Eliminate the macOS dual-arch waste (biggest, most targeted).** Either:
   - **Strip the non-native arch at install.** The `.pkg` `postinstall` already
     detects arch and symlinks pandoc — it could additionally
     `rm -rf tools/<other-arch>`. For RStudio specifically, its bundling step
     could prune the unused arch. Keeps a single universal download.
     *(Caveat: unsafe if one install is shared across arch types, e.g. a network
     mount; fine for an app bundle.)*
   - **Publish per-arch macOS tarballs** (`macos-arm64`/`macos-x86_64`), like
     Linux already does. Cleanest for downstream bundlers; costs more release
     assets and a non-universal `.pkg`.
2. **Lazy/optional downloads** for non-core engines (Typst, and especially
   `verapdf` which is a JVM app, and `typst-gather`) — fetch on first use.
   Tradeoff: hurts offline/reproducible installs and the "everything present"
   expectation downstream tools rely on.
3. **Pandoc (~144–213 MB)** is upstream and statically linked — not shrinkable
   here. Windows is the worst case.
4. **Deno (~100–137 MB)** is intrinsic (it *is* the runtime). `deno compile`
   would not meaningfully shrink it and would complicate the shared-deno model.
5. **Rust launcher** is ~0.3 MB — no size lever. The only "alternative" is
   unifying on the Rust launcher across platforms for consistency, which is a
   maintainability question, not a size one.

Net: the change that directly attacks the RStudio-size problem is **stripping or
avoiding the unused macOS architecture** — everything else is upstream-bound or a
download-time-vs-offline tradeoff.

## When to update this doc

Re-analyze when any of these change:

- `package/src/common/prepare-dist.ts` arch-handling (`configArchDependency`).
- A new bundled binary is added to `configuration` /
  `package/src/common/dependencies/` (affects size and the sign lists).
- macOS gains per-arch tarballs, or the dual-arch packaging model changes.
- The launcher model changes (e.g. Rust launcher adopted on macOS/Linux), in
  `package/scripts/common/quarto` or `package/launcher/`.
- Windows gains an ARM64 build.
