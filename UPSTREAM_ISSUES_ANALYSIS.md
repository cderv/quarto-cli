# Quarto-CLI Upstream Issues Analysis

**Date**: 2026-03-08
**Source**: quarto-dev/quarto-cli (public GitHub)
**Total open issues (incl PRs)**: 1,714
**Issues analyzed**: 350+ (top by reactions, comments, regressions, recent)
**Issues created (last 30 days)**: 30
**Issues closed (last 30 days)**: 109
**Close rate**: ~3.6x creation rate (backlog is being reduced)
**Issues with no comments**: 357 (21%)
**Issues with no labels**: 2

---

## Executive Summary

The upstream quarto-cli repo has significant community demand across several key areas. Based on reaction counts (community votes), comment volume (community engagement), and label analysis, here are the **priority themes** for planning work.

---

## Priority 1: Regressions (15 open)

These are the most urgent -- things that used to work and broke. Fixing regressions prevents user churn.

| # | Issue | Area | Detail |
|---|-------|------|--------|
| #13583 | code-tools buttons don't work if code-copy enabled | code-blocks | +1:2 |
| #12890 | `monofont` no longer works since 1.6.27 | html/brand | +1:1 |
| #13450 | brand causes three stylesheets in output | themes/brand | +1:1 |
| #13448 | Code annotation line highlight overflow | code-blocks | +1:0 |
| #13390 | Code blocks in lists have two backgrounds | html/themes | +1:1 |
| #13446 | Code-copy button outside of code blocks | callouts/code | +1:0 |
| #13444 | Code-copy button overlaps annotation marker | code-blocks | +1:0 |
| #13669 | Copy button missing in Bibtex citation | citations | +1:0 |
| #12992 | preview crashes with embedded ipynb | embed | +1:0 |
| #10146 | Multi-part figures get stretched | crossref/jupyter | +1:0 |
| #13372 | Subtables/Subfigures fail in beamer | beamer | +1:0 |
| #11727 | Caption formatting ignored in lightbox | lightbox | +1:0 |
| #11117 | Computed Captions crash manuscript HTML | manuscript | +1:0 |
| #10520 | Embedded Jupyter plot not scaled in LaTeX | embed/latex | +1:0 |
| #10436 | Multiple files with jupyter frontmatter | jupyter | +1:0 |

### Regression Cluster: Code Blocks / Code-Copy / Annotations
Issues #13583, #13448, #13446, #13444, #13390 are all related to **code block rendering regressions** (code-copy, code-annotation, code-tools). These likely share a root cause and could be fixed together.

### Regression Cluster: Brand/Themes
Issues #12890 and #13450 are both **brand/theme regressions** introduced around v1.6.27+.

---

## Priority 2: Top 20 Most Wanted Features (by community reactions)

| Rank | # | Reactions | Title | Theme |
|------|---|-----------|-------|-------|
| 1 | #1697 | 49 | Glossary support | crossref |
| 2 | #341 | 37 | Result Folding | computations |
| 3 | #3795 | 32 | Page-navigation for blog listings | websites |
| 4 | #2022 | 31 | Google Slides output | formats |
| 5 | #275 | 25 | Multilingual websites/books | websites/books |
| 6 | #844 | 24 | Custom callout boxes | callouts |
| 7 | #6741 | 23 | Bootswatch paired light/dark themes | themes |
| 8 | #4677 | 20 | PDF export for revealjs (decktape) | revealjs |
| 9 | #6518 | 19 | Execution Planner (epic) | execution |
| 10 | #2275 | 19 | Equation refs in align environments | crossref/math |
| 11 | #5827 | 18 | Extension install from private GitLab | extensions |
| 12 | #1328 | 18 | Collapse callouts in revealjs | revealjs/callouts |
| 13 | #376 | 18 | Wrapfigure support | latex |
| 14 | #5961 | 17 | TOC visible only in wide desktop | html/responsive |
| 15 | #2556 | 17 | Books without index.qmd | books |
| 16 | #419 | 16 | Dropdown tabset panels | tabsets |
| 17 | #5072 | 15 | Confluence Data Center support | publishing |
| 18 | #3917 | 15 | `quarto create post` command | cli/dx |
| 19 | #1585 | 15 | Backlinks in website output | crossref/websites |
| 20 | #3416 | 14 | Tabset anchors in HTML | tabsets/html |

---

## Priority 3: Most Active/Discussed Issues

High comment count indicates ongoing pain or complexity.

| # | Comments | Title | Theme |
|---|----------|-------|-------|
| #4197 | 59 | Deno memory issues in limited environments | deno/infra |
| #7151 | 49 | gt table docx rendering incorrect | tables/docx |
| #275 | 44 | Multilingual websites/books | i18n |
| #7817 | 43 | Julia Makie resolution deprecation | julia |
| #2275 | 36 | Equation refs in align | crossref/math |
| #6011 | 36 | Improve .gitignore handling | dx |
| #13006 | 33 | Render on shared storage fails (locked db) | file-systems |
| #12558 | 28 | Confluence republish 500 error | confluence |
| #7967 | 27 | Bokeh plot wrongly split into subfigures | jupyter/crossref |

---

## Thematic Analysis

### Theme 1: Cross-References (36 issues, 189 total reactions)
**The single most impactful area.** Key asks:
- **Glossary** (#1697, 49 reactions) -- most wanted feature overall
- **Equation refs in align** (#2275, 36 comments) -- longstanding bug
- **Backlinks** (#1585, 15 reactions)
- **List of figures/tables** (#1600, #2138)
- **Flextable/docx crossref bugs** (#7321, #7151)

### Theme 2: HTML/Websites (56 issues, 266 total reactions)
Largest volume of issues. Key asks:
- **Blog listing page navigation** (#3795, 32 reactions)
- **Multilingual sites** (#275, 25 reactions)
- **Paired light/dark themes** (#6741, 23 reactions)
- **Responsive TOC** (#5961, 17 reactions)
- **Breadcrumbs for listings** (#8004)

### Theme 3: Code Execution & Engines (52 issues, 98 reactions)
- **Execution Planner epic** (#6518, 19 reactions)
- **Per-cell caching for Python** (#1092)
- **Jupyter fig-width** (#3155)
- **Julia engine issues** (#7817, #12298)
- **Deno memory** (#4197, 59 comments)

### Theme 4: Cross-format Output Quality (Tables, Figures, Docx)
- **gt table docx rendering** (#7151, 49 comments)
- **Table font size** (#1153, 9 reactions)
- **Flextable+crossref+docx** (#7321)
- **fig-alt removed in docx** (#5514)

### Theme 5: Callouts & Tabsets (18 issues, ~72 reactions)
- **Custom callout boxes** (#844, 24 reactions)
- **Dropdown panels** (#419, 16 reactions)
- **Collapse callouts in revealjs** (#1328, 18 reactions)
- **Tabset anchors** (#3416, 14 reactions)

### Theme 6: Revealjs/Presentations (16 issues, 67 reactions)
- **PDF export via decktape** (#4677, 20 reactions)
- **Callout collapse** (#1328)
- **Code-overflow across formats** (#11158)

### Theme 7: Typst (21 issues, emerging format)
Growing interest as alternative to LaTeX for PDF. Issues span crossrefs, tables, templates, and brand support.

### Theme 8: Books & Projects (36 issues, 138 reactions)
- **Multilingual** (#275)
- **No index.qmd** (#2556)
- **Project-level engine config** (#3157)

### Theme 9: Extensions (9 issues)
- **Private GitLab install** (#5827, 18 reactions)
- **Custom cell handlers** (#4839)
- **Extension update tracking** (#11468)

### Theme 10: Developer Experience
- **`quarto create post`** (#3917, 15 reactions)
- **Improve .gitignore** (#6011, 36 comments)
- **Language server** (#239, 25 comments)
- **Improve YAML error messages** (#12744)

---

## Milestones Overview

| Milestone | Open Issues | Description |
|-----------|------------|-------------|
| **v1.10** | 351 | Next release — active development target |
| **Future** | 1,354 | Deferred / longer-term backlog |
| **v1.9** | 3 | Previous release — nearly closed |
| **Hot-fix** | 1 | Urgent patch |

The **v1.10 milestone contains 351 issues** — this is the actionable planning scope. The remaining 1,354 are in "Future" (deferred).

---

## Assignee: cderv (278 open issues)

You have **278 open issues assigned**. High-reaction items in your queue:

| # | +1 | Title | Milestone | Quick Win? |
|---|-----|-------|-----------|------------|
| #4677 | 20 | PDF export for revealjs (decktape) | Future | No — significant feature |
| #5827 | 18 | Extension install from private GitLab | Future | No — auth/protocol work |
| #1328 | 15 | Collapse callouts in revealjs | Future | Moderate |
| #2795 | 7 | Preview not updated on included file changes | v1.10 | Moderate |
| #5036 | 6 | QED symbol at end of proof | Future | Yes — LaTeX/typst tweak |
| #11877 | 5 | Chrome Headless improvements | v1.10 | No — epic |
| #12104 | 4 | Add uv among venv examples | v1.10 | **Yes — docs only** |
| #11800 | 4 | Customize TinyTeX install directory | Future | Moderate |
| #1167 | 4 | Callout boxes for ipynb output | Future | Moderate |
| #11727 | 0 | Lightbox caption formatting (regression) | Future | Moderate |
| #13006 | 0 | Locked database on shared storage | v1.10 | No — complex |

### cderv v1.10 issues (assigned, in next release)
Key items from your v1.10 queue that need attention:
- **#2795** Preview not updated on included files (7 reactions)
- **#11877** Chrome Headless improvements (epic)
- **#12104** Add uv venv example (quick docs fix)
- **#12065** Contents shortcode with knitr
- **#13006** Locked database on shared filesystems (33 comments, complex)
- **#5766** Plotly image rendering in GH actions
- **#1460** format-resources not cleaned up

---

## Quick Wins vs Deep Problems

### Quick Wins (can likely be fixed in 1-2 hours)

**Unassigned regressions — CSS/JS fixes with clear root causes:**

| # | Issue | Why it's quick | Status |
|---|-------|----------------|--------|
| #13390 | Code blocks in lists: two backgrounds | Reporter provided fix: `background-color: inherit` | Unassigned |
| #13583 | code-tools broken with code-copy | Root cause identified: `code-copy-outer-scaffold` breaks querySelector | Unassigned |
| #13448 | Code annotation highlight overflow | CSS regression from PR #13429, clear screenshots | Unassigned |
| #13446 | Code-copy button outside code blocks | CSS positioning issue in callouts | Unassigned |
| #13444 | Code-copy overlaps annotation marker | CSS overlap, clear repro | Unassigned |
| #13669 | BibTeX copy button missing | Simple UI regression | Unassigned |

**Other quick wins:**

| # | Issue | Why it's quick | Assignee |
|---|-------|----------------|----------|
| #12104 | Add uv among venv command examples | Documentation-only change | cderv |
| #12476 | Remove console.log from codebase | Grep + delete, early-in-release | Unassigned |
| #5036 | QED symbol at end of proof | Small LaTeX/typst addition | cderv |

### Moderate Effort (half-day to a few days)

| # | Issue | Complexity | Assignee |
|---|-------|------------|----------|
| #12890 | monofont broken since 1.6.27 | Version narrowed, CSS rule issue | cscheid |
| #13450 | Brand causes 3 stylesheets | Brand/dark-mode heuristics | gordonwoodhull |
| #13372 | Subtables fail in beamer | LaTeX subcaption, from specific PR | cscheid |
| #11727 | Lightbox caption formatting | Lightbox strips markdown in captions | cderv |
| #2795 | Preview not updated on includes | File watcher scope | cderv |
| #1460 | format-resources not cleaned up | Cleanup logic in render pipeline | cderv |

### Deep Problems (days to weeks, architecture involved)

| # | Issue | Why it's deep | Assignee |
|---|-------|---------------|----------|
| #13006 | Locked database on shared storage | SQLite + NFS fundamentally incompatible, needs alternative locking strategy | cderv |
| #10146 | Multi-part figures stretched | Long-standing subfigure layout, 20 comments of debugging | cscheid |
| #10436 | Multiple jupyter frontmatter files | YAML delimiter detection, PR #13947 in progress | cscheid |
| #12992 | Preview crash with ipynb+qmd | Race condition in temp directory cleanup | Unassigned |
| #4197 | Deno memory (8GB mmap) | V8 engine limitation, needs upstream Deno changes | Unassigned |
| #7151 | gt table docx rendering | Cross-ref + gt + docx interaction, 49 comments | Unassigned |

---

## Top Unassigned Issues (Opportunities)

High-reaction issues with **no assignee** — available for anyone to pick up:

| # | +1 | Title | Milestone | Effort |
|---|-----|-------|-----------|--------|
| #3795 | 32 | Blog listing page navigation | v1.10 | Medium |
| #275 | 25 | Multilingual websites/books | Future | Large |
| #2022 | 24 | Google Slides output | Future | Large |
| #844 | 24 | Custom callout boxes | Future | Medium |
| #376 | 18 | Wrapfigure support | v1.10 | Medium |
| #5961 | 17 | TOC only visible in wide desktop | Future | Small-Medium |
| #2556 | 17 | Books without index.qmd | Future | Medium |
| #6741 | 17 | Bootswatch paired light/dark themes | Future | Medium |
| #419 | 16 | Dropdown tabset panels | Future | Medium |
| #5072 | 15 | Confluence Data Center | Future | Large (epic) |
| #3917 | 15 | `quarto create post` command | Future | Small-Medium |
| #1585 | 15 | Backlinks in website output | Future | Medium |
| #2065 | 13 | Pretty URLs for websites | Future | Medium |
| #474 | 11 | Version selector for websites/books | Future | Large |

---

## Early-in-Release Issues (41 open)

Issues labeled `early-in-release` are flagged to address early in the release cycle. Notable ones:

| # | +1 | Title | Assignee |
|---|-----|-------|----------|
| #6092 | 9 | `default-image-extension` docs | cscheid |
| #11877 | 5 | Chrome Headless improvements (epic) | cderv |
| #12476 | 2 | Remove console.log from codebase | Unassigned |
| #5766 | 2 | Plotly image rendering in GH actions | cderv |
| #7843 | 2 | Leaflet legends centered with fig- label | cscheid |
| #4955 | 2 | Don't bundle JS anymore | cscheid |
| #12065 | 1 | Contents shortcode with knitr | cderv |
| #11305 | 1 | Control engine resolution order | cscheid |
| #7064 | 1 | Custom crossrefs localization | cscheid |
| #1460 | 1 | format-resources not cleaned up | cderv |

---

## Recommended Work Priorities

### Immediate: Quick Wins (unassigned, clear fixes)
1. **Code block regression cluster** (#13583, #13448, #13446, #13444, #13390) — 5 CSS/JS fixes, unassigned, likely 1-2 PRs
2. **Citation copy button** (#13669) — unassigned, simple UI fix
3. **Remove console.log** (#12476) — unassigned, early-in-release, grep+delete
4. **Add uv venv docs** (#12104) — assigned to cderv, docs-only

### Short-term: High Impact (v1.10 milestone)
1. **Result folding** (#341) — 37 reactions, v1.10
2. **Blog listing pagination** (#3795) — 32 reactions, v1.10, unassigned
3. **Wrapfigure support** (#376) — 18 reactions, v1.10
4. **Code-fold for non-executable** (#4693) — 13 reactions, v1.10
5. **Preview on included files** (#2795) — 7 reactions, v1.10, cderv

### Medium-term: High Impact (Future milestone, high reactions)
1. **Glossary support** (#1697) — 49 reactions, Future
2. **Custom callout boxes** (#844) — 24 reactions, Future, unassigned
3. **Multilingual websites/books** (#275) — 25 reactions, Future, unassigned
4. **Paired light/dark themes** (#6741) — 23 reactions, Future, unassigned
5. **Collapse callouts in revealjs** (#1328) — 18 reactions, Future, cderv

### Strategic (Epics / Architecture)
1. **Execution Planner** (#6518) — 19 reactions, epic
2. **Chrome Headless improvements** (#11877) — cderv, early-in-release, v1.10
3. **PDF rethink** (#7039) — 10 reactions, epic
4. **Confluence Data Center** (#5072) — 15 reactions, unassigned

---

## Label Distribution (Full Repo Counts)

Open issue counts per label across the entire repository:

| Label | Count | | Label | Count |
|-------|------:|-|-------|------:|
| **bug** | 769 | | **enhancement** | 740 |
| **crossref** | 137 | | **latex** | 107 |
| **revealjs** | 106 | | **html** | 87 |
| **websites** | 78 | | **engines-jupyter** | 76 |
| **books** | 66 | | **tables** | 56 |
| **typst** | 43 | | **engines-knitr** | 42 |
| **docx** | 40 | | **early-in-release** | 36 |
| **preview** | 35 | | **dashboards** | 35 |
| **accessibility** | 32 | | **engines-julia** | 29 |
| **extensions** | 28 | | **figures** | 27 |
| **brand** | 24 | | **math** | 22 |
| **callouts** | 21 | | **regression** | 15 |
| **performance** | 3 | | | |

### Key Observations

- **bug vs enhancement split is nearly even** (769 vs 740) — the backlog has equal parts broken features and new requests
- **crossref (137)** is the single largest functional area — spanning equations, figures, tables, books
- **latex (107) + revealjs (106)** are the most issue-heavy output formats
- **typst (43)** is growing rapidly for a newer format — watch for acceleration
- **regression (15)** count is manageable if prioritized
- **performance (3)** is surprisingly low — most perf issues may be filed under other labels
- **357 issues (21%) have zero comments** — potential triage opportunity

---

## Regression Issue Details

All 15 regressions with root cause summaries:

**Code Block Cluster (5 issues, likely shared root cause):**
- **#13583**: code-tools Show/Hide All buttons broken when code-copy enabled — `code-copy-outer-scaffold` div interferes
- **#13448**: Code annotation line highlights overflow left + gutter misalignment
- **#13446**: Code-copy button renders above/outside code blocks (in callouts)
- **#13444**: Code-copy button overlaps code-annotation markers, breaking hover/click
- **#13390**: Code blocks nested in lists get double background colors from conflicting CSS rules

**Brand/Theme Cluster (2 issues):**
- **#13450**: Any use of `brand` causes 3 `<link rel="stylesheet">` tags (light, dark, light duplicate)
- **#12890**: `monofont` stopped working in v1.6.27 — CSS rule issue in brand integration

**Format-specific (4 issues):**
- **#13372**: Subtables/Subfigures fail in beamer — broke between v1.7.34 and v1.8.24
- **#10520**: Embedded Jupyter plots overflow page in LaTeX PDF — broke in v1.5
- **#10146**: Multi-part figures get stretched — subcaption-specific, 20 comments of debugging
- **#10436**: Multiple files with `jupyter: python3` frontmatter print frontmatter content — broke in v1.5.54

**Feature-specific (4 issues):**
- **#13669**: Copy button missing in BibTeX citation display
- **#12992**: Preview crashes editing both ipynb + embedding qmd — verify-fixed tag
- **#11727**: Caption formatting (bold/italic) ignored in lightbox images
- **#11117**: Computed Captions crash manuscript HTML rendering

---

## Top Issue Summaries

### Most Wanted: Glossary (#1697, 49 reactions)
Users want first-class glossary support: link terms to definitions (hover display), collect into glossary sections at chapter/book end. Model: psyTeachR glossary package.

### Most Wanted: Result Folding (#341, 37 reactions)
Ability to collapse output/results in HTML (not just code), like minidown R package. Requested since 2022.

### Most Wanted: Blog Pagination (#3795, 32 reactions)
`page-navigation` only works in sidebars, not blog listings. Users want prev/next navigation between blog posts and series.

### Most Painful Bug: Deno Memory (#4197, 59 comments)
V8 startup mmaps 8GB — breaks in containers, CI, and resource-limited environments. Needs configurable memory limits.

### Most Painful Bug: gt/docx Tables (#7151, 49 comments)
gt tables with cross-reference labels produce corrupted docx output. Word shows repair dialog.

### Most Painful Bug: Locked Database (#13006, 33 comments)
`quarto render` on NFS/shared filesystems fails with SQLite "database is locked" since v1.7.31.

---

## Velocity & Health Metrics

| Metric | Value |
|--------|-------|
| Open issues | 1,714 |
| Created last 30 days | 30 |
| Closed last 30 days | 109 |
| Net reduction / month | ~79 |
| Zero-comment issues | 357 (21%) |
| Unlabeled issues | 2 |
| Open regressions | 15 |
| Issues 3+ years old | ~200+ |

The project is **closing issues faster than they're created** (3.6:1 ratio), indicating active maintenance. Triage discipline is excellent (only 2 unlabeled issues). The 21% zero-comment rate suggests some issues could be closed as stale or need-repro.
