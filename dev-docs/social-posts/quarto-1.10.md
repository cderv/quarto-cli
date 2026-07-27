# Quarto 1.10 — social posts

Source: `news/changelog-1.10.md` (74 entries) + <https://quarto.org/docs/prerelease/1.10/> (one
highlighted feature). Stable release **1.10.18**, published Friday 2026-07-24.

Links:

- Download: <https://quarto.org/docs/download/>
- Changelog: <https://quarto.org/docs/download/changelog/1.10/>

---

## LinkedIn

1288 characters. Hook is 139 characters, so it lands above the "see more" fold on mobile too.
No emoji. Link goes in the first comment (see below), not the post body.

> Quarto 1.10 shipped last Friday without a headline feature. The prerelease highlights page lists exactly one thing. The changelog lists 74.
>
> That's the kind of release I like.
>
> Most of those 74 entries are small annoyances people had quietly learned to work around. Edit a page in a website preview, get served the old HTML. Add a `_brand.yml` while preview is running, watch it get ignored until you restart. Change `format:` in the frontmatter, need a second render before it takes.
>
> None of that is a feature. All of it is friction you hit every single day.
>
> The one piece of real new work is accessibility. Quarto's built-in axe check now tells you which WCAG level each violation maps to, and sorts them by impact so you fix the critical ones first instead of scrolling. You can scope a scan with `axe: {standard: wcag21aa}`. And axe-core is bundled now, so checking runs offline and your readers' browsers stop fetching a script from a CDN nobody asked for.
>
> If you render PDFs, one to know about: font fallbacks stopped crashing LuaLaTeX on TeX Live 2026, and a missing fallback font installs itself again instead of failing the render.
>
> Pandoc 3.10, Typst 0.15.1 and Deno 2.7.14 come along for the ride.
>
> Changelog link in the comments.
>
> \#Quarto #DataScience #Accessibility #OpenSource

**First comment** (post immediately after publishing):

> Full changelog: https://quarto.org/docs/download/changelog/1.10/
> Download: https://quarto.org/docs/download/

### Things to check before posting

- "That's the kind of release I like" is an opinion put in your mouth. Keep it only if it is
  actually yours.
- Swap one of the preview annoyances for one you personally hit or fixed, with the detail only you
  would know. That single change does more for authenticity than everything else here.
- LinkedIn strips backticks — the inline code above will render as plain text. Fine, but check
  `axe: {standard: wcag21aa}` still reads clearly.
- Consider attaching a screenshot of the axe report overlay (WCAG badges + impact sorting). Image
  posts want a shorter body, roughly 100–150 words, so trim to the hook plus the accessibility
  paragraph if you go that route.

---

## Bluesky

Three posts, all within the 300-character limit (279 / 237 / 227).

**1/3**

> Quarto 1.10 shipped Friday. The highlights page lists one feature. The changelog lists 74.
>
> Mostly small friction: preview serving stale pages, `_brand.yml` ignored until you restart,
> `format:` changes needing a second render.
>
> A boring release. Best kind.
>
> quarto.org/docs/download/

**2/3**

> The one piece of real new work is accessibility. The built-in axe check now maps each violation to
> its WCAG level and sorts by impact — and axe-core is bundled, so scans run offline and readers'
> browsers stop pulling a script from a CDN.

**3/3**

> If you render PDFs: font fallbacks no longer crash LuaLaTeX on TeX Live 2026, and a missing
> fallback font installs itself again.
>
> Also Pandoc 3.10, Typst 0.15.1, Deno 2.7.14.
>
> Changelog: quarto.org/docs/download/changelog/1.10/

Post 1/3 works standalone if you don't want a thread.

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
