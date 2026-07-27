# Quarto 1.10 — social posts

Source: `news/changelog-1.10.md` (74 entries) + <https://quarto.org/docs/prerelease/1.10/> (one
highlighted feature). Stable release **1.10.18**, published Friday 2026-07-24.

Links:

- Download: <https://quarto.org/docs/download/>
- Changelog: <https://quarto.org/docs/download/changelog/1.10/>

Bundled tool versions, 1.9 → 1.10 (from `configuration` on each branch):

| Tool      | 1.9    | 1.10    |
| --------- | ------ | ------- |
| Pandoc    | 3.8.3  | 3.10    |
| Typst     | 0.14.2 | 0.15.1  |
| dart-sass | 1.87.0 | 1.101.0 |
| Deno      | 2.4.5  | 2.7.14  |

---

## LinkedIn

1430 characters. Hook is 134 characters, so it lands above the "see more" fold on mobile too.
No emoji, no em dashes. Link goes in the first comment (see below), not the post body.

> Quarto 1.10 shipped Friday without a headline feature. The prerelease highlights page lists exactly one thing. The changelog lists 74.
>
> That's the kind of release I like.
>
> Most of the 74 are small annoyances people had learned to work around. Edit a page in a website preview, get served the old HTML. Add a `_brand.yml` while preview is running, watch it get ignored until you restart. Change `format:` in the frontmatter, need a second render before it takes.
>
> None of that is a feature. But preview is a button you press twenty times an hour in Positron, so that friction is most of your experience of Quarto. It should be boring, and now it mostly is. Preview behind a proxy, in Workbench or code-server, behaves too.
>
> There's more in the release than we wrote. Quarto ships its own Pandoc and its own Typst, and 1.10 moves both a long way: Pandoc 3.8.3 to 3.10, Typst 0.14.2 to 0.15.1. Every fix those two landed since 1.9 rides along with ours. Same for TeX, which we also install for you: `quarto install tinytex` now pulls from tlnet.yihui.org, refreshed daily, not whichever CTAN mirror was stale that week.
>
> The one piece of genuinely new work is accessibility. The built-in axe check now maps each violation to its WCAG level and sorts them by impact. And axe-core ships inside Quarto now, so scans run offline instead of pulling a script from a CDN.
>
> Changelog in the comments.
>
> \#Quarto #DataScience #Positron #Accessibility

**First comment** (post immediately after publishing):

> Full changelog: https://quarto.org/docs/download/changelog/1.10/
> Download: https://quarto.org/docs/download/

### Things to check before posting

- "That's the kind of release I like" is an opinion put in your mouth. Keep it only if it is
  actually yours. Same for "twenty times an hour" — pick a number you'd actually say.
- Swap one of the preview annoyances for one you personally hit or fixed, with the detail only you
  would know. That single change does more for authenticity than everything else here.
- **The Positron framing is editorial, not something the changelog says.** No 1.10 entry names
  Positron. What it names is Posit Workbench proxied preview ([#14298](https://github.com/quarto-dev/quarto-cli/issues/14298)),
  code-server reload ([#14595](https://github.com/quarto-dev/quarto-cli/issues/14595)), and
  RStudio's Render button shape ([#14683](https://github.com/quarto-dev/quarto-cli/issues/14683)).
  The stale-page, `_brand.yml` and `format:` fixes are engine-side, so they land in any editor that
  drives `quarto preview` — Positron included. The draft claims the experience, not the fix, which
  is accurate. Don't tighten it into "we fixed Positron".
- Dropped along the way: the `QUARTO_TINYTEX_REPOSITORY` override, "same default the R tinytex
  package took in March", and the LuaLaTeX font-fallback fix. The first two are good first-comment
  material if a LaTeX user asks why the default changed; the third is in Bluesky 5/5.
- The dependency line stays generic on purpose: it claims that bumping the bundled compilers carries
  upstream's fixes to you, and nothing more. That is safe. Naming individual Typst or Pandoc
  features is not — some of them (Typst multiple bibliographies, MathML HTML export) need
  Quarto-side plumbing before you can reach them from YAML. If you do want to name one, check it
  works through Quarto first; there is a list to pick from in the background section below.
- It does not explain that installing your own Pandoc changes nothing, though that is true
  (`QUARTO_PANDOC`, `QUARTO_TYPST` and `QUARTO_DART_SASS` override the binaries, but they are
  packaging and development escape hatches — conda builds use them — not a user-facing choice).
  That is a mechanism the reader did not ask about, and explaining it mid-post was what broke the
  flow in an earlier draft. Save it for a reply if someone asks.
- 1430 characters, with room to add a sentence before the 1500 ceiling.
- LinkedIn strips backticks — inline code renders as plain text. Check `axe: {standard: wcag21aa}`
  and the env var still read clearly.
- Consider attaching a screenshot of the axe report overlay (WCAG badges + impact sorting). Image
  posts want a shorter body, roughly 100–150 words, so trim to the hook plus the accessibility
  paragraph if you go that route.

---

## Bluesky

Five posts, all within the 300-character limit (278 / 123 / 237 / 246 / 275).

**1/5**

> Quarto 1.10 shipped Friday. The highlights page lists one feature. The changelog lists 74.
>
> Mostly preview friction: stale pages, `_brand.yml` ignored until restart, `format:` changes
> needing two renders. If you work in Positron, that's your whole day.
>
> A boring release. Best kind.

**2/5**

> Preview behind a proxy got fixed too — Posit Workbench and code-server both reload properly now.
>
> quarto.org/docs/download/

**3/5**

> The one piece of real new work is accessibility. The built-in axe check now maps each violation to
> its WCAG level and sorts by impact — and axe-core is bundled, so scans run offline and readers'
> browsers stop pulling a script from a CDN.

**4/5**

> We move the bundled tools every release, and 1.10 is a big one: Pandoc 3.8.3 → 3.10,
> Typst 0.14.2 → 0.15.1, plus dart-sass 1.101 and Deno 2.7.14.
>
> Quarto ships its own copies, so every fix those projects landed since 1.9 arrives with the
> upgrade.

**5/5**

> Same for TeX, which Quarto also installs for you: `quarto install tinytex` now pulls from
> tlnet.yihui.org, refreshed daily, not whichever CTAN mirror is stale that week.
>
> And font fallbacks stopped crashing LuaLaTeX on TeX Live 2026.
>
> quarto.org/docs/download/changelog/1.10/

Post 1/5 works standalone if you don't want a thread. The download link moved to 2/5 because 1/5 is
already at 278 characters.

---

## Background for the two added topics

**Dependencies.** 1.10 crosses two Pandoc releases (3.9 and 3.10) and a Typst minor. The posts keep
this generic, but if you ever want a concrete example: Pandoc 3.10 rewrote OpenDocument/ODT output
to use predefined styles instead of an automatic style per paragraph, so `.odt` output is finally
stylable, and grid tables can now be indented up to three spaces. Typst 0.15 brought variable fonts
(`text(variations: …)`), multiple bibliographies, MathML in HTML export, and clearer diagnostics;
0.15.1 is a bug-fix patch.

**CTAN default** (changelog [#14538](https://github.com/quarto-dev/quarto-cli/pull/14538)). CTAN
mirrors pick up a new TeX Live at different times, so `mirror.ctan.org` can redirect you to a mirror
still serving the old snapshot — worst around the annual TeX Live release in spring, when an install
and an update disagree about what exists. `tlnet.yihui.org` mirrors only `systems/texlive/tlnet/`,
refreshes daily, and is served from one Cloudflare origin, so everyone sees the same state. Quarto
falls back to `mirror.ctan.org` and then the US university mirrors when it is unreachable.
`QUARTO_TINYTEX_REPOSITORY` or `CTAN_REPO` override it.

---

## Why these drafts look like this

Guidance the drafts follow, from current LinkedIn writing advice:

- Hook under ~210 characters (~140 on mobile) — it is all anyone sees before "see more", so it
  carries a claim, not a greeting.
- 1,200–1,500 characters total; engagement drops off past 1,500.
- Line break every one or two sentences.
- One or two emoji at most — a row of emoji section headers reads as low-effort, and it was the
  main AI tell in the first draft.
- Three to five hashtags. They barely matter for reach now (the feed ranks on topic modelling and
  behaviour), so they are there for context, not discovery.
- External links in the first comment; links in the post body cut reach.
- Against the AI-written tells: no "thrilled to announce", concrete specifics over generic claims,
  contractions and varied sentence length, a stated opinion, and a list of 74 rather than a tidy
  list of five.

Sources:

- <https://connectsafely.ai/articles/linkedin-post-best-practices-guide-2026>
- <https://connectsafely.ai/articles/ideal-linkedin-post-length-engagement-guide-2026>
- <https://postformatter.com/linkedin-post-formatting-guide/>
- <https://windmillgrowth.com/blogseo/how-to-write-linkedin-posts-that-dont-sound-like-ai>
- <https://magicpost.in/blog/ai-linkedin-posts>
- <https://github.com/jgm/pandoc/releases/tag/3.10>
- <https://typst.app/blog/2026/typst-0.15/>
- <https://yihui.org/en/2026/03/tinytex-ctan-mirror/>
