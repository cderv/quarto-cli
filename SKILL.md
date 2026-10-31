---
name: quarto-release-triage
description: Triage the upstream quarto-dev/quarto-cli issue backlog into a release work plan (per-release, per-developer, with milestone and Future-backlog triage). Use this whenever the user wants to plan the next Quarto release (v1.11, v1.12, "what should I work on for 1.x", "refaire le triage", "analyse les issues upstream", "organise les issues pour planifier"), re-run a previous issue analysis, find quick wins vs deep problems, see what is assigned to them vs orphaned (Windows issues), or decide which Future-milestone issues to pull back into the next release. Also use it when the user asks to refresh or diff UPSTREAM_ISSUES_ANALYSIS.md.
---

# Quarto release triage

Turn the open issue backlog of quarto-dev/quarto-cli into a plan document that one
developer (by default `cderv`) can schedule work from: phases of concrete issues with
effort classes, the deferred ("Future") backlog triaged into pull-back / maybe / leave,
and a delta against the previous plan. The method was built by hand once; this skill
makes it repeatable in about an hour instead of a day.

The plan lives at the repo root as `UPSTREAM_ISSUES_ANALYSIS.md`, same filename every
release, so `git log -p` on it is the history of the planning and the delta section is
computed from the previous version.

## Inputs to settle first

- **Target milestone**: the next release being planned (e.g. `v1.12` when `v1.11` ships
  Monday). List milestones and confirm with the user when it is not obvious — the biggest
  open `vX.Y` milestone is usually it, and `Future` is the deferred backlog.
- **User**: the GitHub login being planned for (default `cderv`).
- **Owner areas**: labels/keywords the user owns de facto because nobody else covers
  them. For cderv: `windows,file-systems` (sole Windows developer) — unassigned issues
  there are orphaned and must be surfaced. Ask if planning for someone else.
- **Previous plan**: `UPSTREAM_ISSUES_ANALYSIS.md` at the repo root if it exists; it
  feeds the delta and carries over the "why deferred" reasons so threads are not re-read.

## Step 1 — GitHub API access (check before anything else)

Run `gh api repos/quarto-dev/quarto-cli/issues?per_page=1`. Three failure modes seen in
practice, each with a different fix — say which one applies instead of retrying:

| Symptom | Cause | Fix |
|---|---|---|
| `gh auth status` says not logged in / token invalid | no token in this environment | user exports `GH_TOKEN` (classic PAT, `repo` scope) or runs `gh auth login --with-token` |
| `Resource protected by organization SAML enforcement` | PAT exists but is not SSO-authorized for `quarto-dev` | GitHub → Settings → Personal access tokens → Configure SSO → authorize the org |
| `GitHub access to this repository is not enabled for this session` (Claude cloud) | the proxy only lets API calls through for attached repos | attach `quarto-dev/quarto-cli` with `add_repo` (needs a session where the fork is not attached under the same name), or run the fetch locally |

Anonymous `curl` is a last resort (`--no-auth`): 60 requests/hour is enough for one full
fetch (~18 calls) but not for the comment threads. Do not burn it on retries.

## Step 2 — Fetch once, derive locally

```bash
S=<scratchpad>/triage-$(date +%Y%m%d)
.claude/skills/quarto-release-triage/scripts/fetch_issues.sh --out $S/data
python3 -I .claude/skills/quarto-release-triage/scripts/analyze_issues.py \
  --data $S/data --milestone v1.12 --user cderv --areas windows,file-systems \
  --previous UPSTREAM_ISSUES_ANALYSIS.md --out $S/report.md --json $S/derived.json
```

`fetch_issues.sh` pulls every open issue (PRs excluded) into `issues.jsonl` plus
milestones, labels and 30-day velocity. One fetch, then everything else is local — in
the first run of this method, issue queries were made one label/sort at a time and the
rate limit was hit three times. `analyze_issues.py` prints the mechanical views:
overview, delta vs previous plan, regressions, early-in-release, the user's queue in the
target and deferred milestones, owner-area orphans, top wanted / discussed / unassigned,
quick-win and deep-problem heuristics, label counts and theme clusters, and the list of
deferred issues whose story must be read. Read the report; it is the raw material, not
the plan.

## Step 3 — Read the stories that decide the plan

Reaction counts rank; comment threads decide. For the deferred-milestone candidates the
report lists (+1 ≥ 5 or ≥ 12 comments), the user's top assigned items, and anything you
intend to call Quick or Deep, dump the thread:

```bash
.claude/skills/quarto-release-triage/scripts/fetch_comments.sh 5961 11475 844 4677
```

Summarize each as **Status / Story / decision** using the vocabulary and the pull-back /
maybe / leave rules in `references/triage-criteria.md`. This is the part worth
delegating: give subagents the issue numbers in batches of ~8, the script path, and the
output format, and let them work in parallel. A quote from a maintainer with its date
("cscheid, Nov 2024: unlikely in the foreseeable future") is worth more than any score —
capture it, the next triage will reuse it.

Carry over stories from the previous plan for issues whose `updated_at` has not moved;
re-read only threads with new activity.

## Step 4 — Classify

Apply `references/triage-criteria.md`:
- effort class per item: Quick / Medium / Deep, with the signal that justifies it
- cluster regressions by shared root cause before counting them
- deferred items: pull back / maybe (name the decision needed) / leave (quote the reason)
- owner-area orphans: recommend self-assignment for recent ones, re-test for 2022–2023 ones

## Step 5 — Write the plan

Use `references/plan-template.md`. Order matters: the delta since last plan first, then
the phased plan (early-in-release → regressions → user's assigned → owner-area orphans →
high-impact unowned → epics), then the deferred-backlog triage, then the reference
material. A reader opening the file during a planning meeting needs the actionable part
in the first screen and the evidence below.

Keep every table row to one line and put the issue number first so rows grep cleanly;
that is what makes the next delta computation work.

Commit the plan on the user's planning branch (never push to upstream). Mention in the
commit message which milestone it targets.

## What to tell the user at the end

Lead with the counts that changed (closed since last plan, new regressions, orphaned
owner-area issues), then the three to five items you would start Monday and why, then
the deferred items that need a decision from them. Everything else is in the file.
