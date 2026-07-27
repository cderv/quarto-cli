# Quarto 1.10 — social posts

Source: `news/changelog-1.10.md` (74 entries) + <https://quarto.org/docs/prerelease/1.10/>.
Stable release: **1.10.18**, published 2026-07-24.

Links used:

- Download: <https://quarto.org/docs/download/>
- Changelog: <https://quarto.org/docs/download/changelog/1.10/>

---

## LinkedIn

Quarto 1.10 is out 🎉

Released last week, and this one is heavy on polish — about 75 fixes and improvements across the whole toolchain. A few things worth calling out:

♿ Accessibility got real attention
The built-in `axe` accessibility check matured a lot. Violations now carry their WCAG conformance level (e.g. `WCAG 2.0 AA (1.4.3)`) and are listed most-important-first, by impact. You can scope a scan to a conformance target with `axe: {standard: wcag21aa}` — including the rules axe-core keeps off by default — and toggle best-practice rules separately. axe-core is now bundled with Quarto instead of being pulled from a CDN, so checking works offline and your readers' browsers no longer phone home. The report overlay is itself keyboard-accessible, and it stopped clobbering the colors from your `_brand.yml`.

🔎 `quarto preview` is a lot less annoying
Stale pages after editing a `.qmd` in a website or book project, a `format:` change needing a second render to take effect, a `_brand.yml` added mid-session being ignored, `BadResource` crashes on re-render, `.quarto_ipynb` files piling up on disk — all fixed. Preview behind a Posit Workbench proxy and in code-server works properly too.

📄 PDF and Typst
Font fallbacks no longer crash LuaLaTeX on TeX Live 2026, and a missing fallback font auto-installs again instead of failing the render. Typst gets better CSS translation, brand fonts that actually reach `typst compile` in book projects, and no more `unknown font family` noise for fonts you don't have installed. `quarto install tinytex` now defaults to the CDN-backed TinyTeX mirror.

🧰 Under the hood
Pandoc 3.10, Typst 0.15.1, dart-sass 1.101, Deno 2.7.14. And a new `quarto.language.*` Pandoc template-variable namespace, so custom templates and partials can finally reach Quarto's resolved localized strings.

Full changelog: https://quarto.org/docs/download/changelog/1.10/
Download: https://quarto.org/docs/download/

#Quarto #DataScience #RStats #Python #OpenScience #Accessibility #a11y #TechnicalWriting

---

## Bluesky

Thread of 3 posts (each within the 300-character limit).

**1/3**

Quarto 1.10 is out 🎉

~75 fixes and improvements. My picks:

♿ built-in `axe` a11y checks matured a lot — WCAG levels, impact sorting, and fully offline now
🔎 `quarto preview` stops serving stale pages
📄 PDF font fallbacks fixed for TeX Live 2026

quarto.org/docs/download/

**2/3**

More in 1.10:

`axe: {standard: wcag21aa}` scopes a scan to a WCAG target. Typst gets better CSS translation + brand fonts in books. `quarto.language.*` exposes localized strings to custom Pandoc templates. `quarto install tinytex` uses the CDN-backed mirror.

**3/3**

Bundled tooling moves too: Pandoc 3.10, Typst 0.15.1, dart-sass 1.101, Deno 2.7.14.

Full changelog 👇
quarto.org/docs/download/changelog/1.10/

#Quarto #DataScience

---

## Notes / variants

- If you want a single Bluesky post instead of a thread, post 1/3 alone works standalone.
- LinkedIn: consider attaching a screenshot of the `axe` report overlay (WCAG level badges + impact
  sorting) — it is the most visual change in the release.
- The only feature listed on the 1.10 prerelease highlights page is the `quarto.language`
  template-variable namespace; everything else above comes from the changelog.
