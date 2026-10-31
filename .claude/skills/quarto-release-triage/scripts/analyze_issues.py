#!/usr/bin/env python3
"""Derive every triage view from the data fetched by fetch_issues.sh.

Reads DATA_DIR/{repo,milestones,velocity,labels}.json and DATA_DIR/issues.jsonl and
prints a Markdown report to stdout (or --out FILE) with the raw material of the plan:
overview, milestone split, regressions, early-in-release, the user's queue (target
milestone and deferred), owner-area issues (e.g. Windows) left unassigned, top wanted /
most discussed, quick-win heuristics, label and theme clusters for the target milestone
and for the deferred one, and a delta against the previous plan document.

The report is deliberately mechanical: it ranks and groups, it does not decide. The
judgment calls (quick win vs deep problem, pull back vs leave deferred) are made in the
plan document after reading the comment threads of the candidates (see SKILL.md).

Usage:
  analyze_issues.py --data DIR --milestone v1.12 --user cderv
                    [--deferred Future] [--areas windows,file-systems]
                    [--previous UPSTREAM_ISSUES_ANALYSIS.md] [--top 25] [--out report.md]
                    [--json derived.json]
"""
import argparse
import json
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

# Labels that group naturally into a theme; order = display order. A label absent from
# the repo is simply skipped, so this list can be generous.
THEME_LABELS = [
    "crossref", "html", "websites", "books", "latex", "typst", "revealjs", "docx",
    "engines-jupyter", "engines-knitr", "engines-julia", "tables", "figures", "layout",
    "callouts", "tabsets", "preview", "extensions", "themes", "brand", "dark-mode",
    "dashboards", "manuscript", "publishing-confluence", "publishing", "lightbox",
    "code-blocks", "code-annotation", "diagrams-mermaid", "math", "citations",
    "accessibility", "windows", "file-systems", "lua", "pandoc", "yaml-validation",
    "documentation", "performance", "epic",
]
QUICK_WIN_HINTS = re.compile(
    r"\b(typo|docs?-only|documentation|wording|message|rename|css|scss|style|"
    r"missing (import|package|option)|console\.log|cleanup|chore)\b", re.I)


def load(data_dir: Path):
    issues = [json.loads(l) for l in (data_dir / "issues.jsonl").read_text().splitlines() if l.strip()]
    meta = {}
    for name in ("repo", "milestones", "velocity", "labels"):
        p = data_dir / f"{name}.json"
        meta[name] = json.loads(p.read_text()) if p.exists() else None
    return issues, meta


def md_table(rows, headers):
    out = ["| " + " | ".join(headers) + " |", "|" + "|".join("---" for _ in headers) + "|"]
    for r in rows:
        out.append("| " + " | ".join(str(c).replace("|", "\\|") for c in r) + " |")
    return "\n".join(out)


def row(i, extra=()):
    base = [f"#{i['number']}", i["plus1"], i["comments"], ", ".join(i["assignees"]) or "—",
            i["milestone"] or "—", i["title"]]
    return base + list(extra)


HEAD = ["#", "+1", "c", "assignee", "milestone", "title"]


def section(title, body):
    return f"\n## {title}\n\n{body}\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--data", required=True, type=Path)
    ap.add_argument("--milestone", required=True, help="target release milestone title, e.g. v1.12")
    ap.add_argument("--deferred", default="Future", help="deferred-backlog milestone title")
    ap.add_argument("--user", required=True, help="GitHub login whose queue is being planned")
    ap.add_argument("--areas", default="windows,file-systems",
                    help="comma-separated labels/keywords the user owns de facto (orphaned if unassigned)")
    ap.add_argument("--previous", type=Path, help="previous plan .md, to report what was closed/moved since")
    ap.add_argument("--top", type=int, default=25)
    ap.add_argument("--out", type=Path)
    ap.add_argument("--json", type=Path, help="also dump the derived groups as JSON")
    a = ap.parse_args()

    issues, meta = load(a.data)
    by_num = {i["number"]: i for i in issues}
    areas = [s.strip().lower() for s in a.areas.split(",") if s.strip()]
    tgt, dfr = a.milestone, a.deferred

    def has(i, label):
        return label in i["labels"]

    def in_area(i):
        labs = [l.lower() for l in i["labels"]]
        t = i["title"].lower() + " " + i.get("body", "").lower()[:300]
        return any(x in labs or x in t for x in areas)

    unassigned = [i for i in issues if not i["assignees"]]
    mine = [i for i in issues if a.user in i["assignees"]]
    in_tgt = [i for i in issues if i["milestone"] == tgt]
    in_dfr = [i for i in issues if i["milestone"] == dfr]
    regressions = [i for i in issues if has(i, "regression")]
    early = [i for i in issues if has(i, "early-in-release")]
    by_plus1 = sorted(issues, key=lambda i: (-i["plus1"], -i["comments"]))
    by_comments = sorted(issues, key=lambda i: (-i["comments"], -i["plus1"]))

    parts = []
    # ---------------- overview
    repo, vel = meta["repo"] or {}, meta["velocity"] or {}
    ms_rows = sorted(((m["title"], m["open_issues"]) for m in (meta["milestones"] or [])), key=lambda x: -x[1])
    ratio = (vel.get("closed", 0) / vel["created"]) if vel.get("created") else 0
    parts.append(section("Overview", "\n".join([
        f"- Repo: {repo.get('full_name', '?')} — open issues+PRs: {repo.get('open_issues_count', '?')}, "
        f"open issues (no PRs): {len(issues)}",
        f"- Milestones: " + ", ".join(f"**{t}**={n}" for t, n in ms_rows),
        f"- Velocity (last {vel.get('days', '?')}d since {vel.get('since', '?')}): created {vel.get('created', '?')}, "
        f"closed {vel.get('closed', '?')} (close/create ratio {ratio:.1f}x)",
        f"- Unassigned: {len(unassigned)} ({100 * len(unassigned) // max(1, len(issues))}%), "
        f"zero-comment: {sum(1 for i in issues if i['comments'] == 0)}, "
        f"unlabeled: {sum(1 for i in issues if not i['labels'])}",
        f"- Target milestone **{tgt}**: {len(in_tgt)} issues ({sum(1 for i in in_tgt if not i['assignees'])} unassigned, "
        f"{sum(1 for i in in_tgt if a.user in i['assignees'])} assigned to {a.user})",
        f"- Deferred milestone **{dfr}**: {len(in_dfr)} issues ({sum(1 for i in in_dfr if a.user in i['assignees'])} assigned to {a.user})",
    ])))
    if tgt not in {t for t, _ in ms_rows}:
        parts.append(f"\n> WARNING: milestone `{tgt}` not found. Known: {[t for t, _ in ms_rows]}\n")

    # ---------------- delta vs previous plan
    if a.previous and a.previous.exists():
        prev_nums = sorted({int(n) for n in re.findall(r"#(\d{3,6})\b", a.previous.read_text())})
        gone = [n for n in prev_nums if n not in by_num]
        moved = [(n, by_num[n]["milestone"]) for n in prev_nums if n in by_num]
        parts.append(section(f"Delta vs previous plan ({a.previous.name})", "\n".join([
            f"- Issues referenced in previous plan: {len(prev_nums)}",
            f"- No longer open (closed or converted): {len(gone)} → " + ", ".join(f"#{n}" for n in gone[:80]),
            f"- Still open, now in **{tgt}**: " + ", ".join(f"#{n}" for n, m in moved if m == tgt),
            f"- Still open, in **{dfr}**: " + ", ".join(f"#{n}" for n, m in moved if m == dfr),
            f"- Still open, other/no milestone: " + ", ".join(f"#{n} ({m})" for n, m in moved if m not in (tgt, dfr)),
        ])))

    # ---------------- regressions & early-in-release
    parts.append(section(f"Regressions ({len(regressions)})",
                         md_table([row(i, [", ".join(l for l in i["labels"] if l not in ("bug", "regression"))])
                                   for i in sorted(regressions, key=lambda i: (-i["plus1"], -i["comments"]))],
                                  HEAD + ["other labels"])))
    parts.append(section(f"Early-in-release ({len(early)})",
                         md_table([row(i) for i in sorted(early, key=lambda i: (-i["plus1"], -i["comments"]))], HEAD)))

    # ---------------- the user's queue
    mine_tgt = sorted([i for i in mine if i["milestone"] == tgt], key=lambda i: (-i["plus1"], -i["comments"]))
    mine_dfr = sorted([i for i in mine if i["milestone"] == dfr], key=lambda i: (-i["plus1"], -i["comments"]))
    mine_other = [i for i in mine if i["milestone"] not in (tgt, dfr)]
    parts.append(section(f"{a.user}: assigned in {tgt} ({len(mine_tgt)})",
                         md_table([row(i, [", ".join(i["labels"])]) for i in mine_tgt], HEAD + ["labels"])))
    theme_cnt = Counter(l for i in mine_dfr for l in i["labels"] if l in THEME_LABELS)
    parts.append(section(f"{a.user}: assigned in {dfr} ({len(mine_dfr)}) — top {a.top} by +1 and theme counts",
                         "Themes: " + ", ".join(f"{l}={n}" for l, n in theme_cnt.most_common(15)) + "\n\n" +
                         md_table([row(i, [", ".join(i["labels"])]) for i in mine_dfr[:a.top]], HEAD + ["labels"])))
    if mine_other:
        parts.append(section(f"{a.user}: assigned in other milestones ({len(mine_other)})",
                             md_table([row(i) for i in mine_other], HEAD)))

    # ---------------- owner areas left unassigned
    area_un = sorted([i for i in unassigned if in_area(i)], key=lambda i: (i["milestone"] != tgt, -i["plus1"], -i["comments"]))
    parts.append(section(f"Unassigned issues in owner areas [{', '.join(areas)}] ({len(area_un)})",
                         "These default to the owner of those areas; old ones should be re-tested before investing.\n\n" +
                         md_table([row(i, [i["created_at"], ", ".join(i["labels"])]) for i in area_un],
                                  HEAD + ["created", "labels"])))

    # ---------------- most wanted / most discussed / unassigned opportunities
    parts.append(section(f"Top {a.top} most wanted (+1)", md_table([row(i, [", ".join(i["labels"][:3])]) for i in by_plus1[:a.top]], HEAD + ["labels"])))
    parts.append(section(f"Top {a.top} most discussed", md_table([row(i, [", ".join(i["labels"][:3])]) for i in by_comments[:a.top]], HEAD + ["labels"])))
    un_top = sorted(unassigned, key=lambda i: (-i["plus1"], -i["comments"]))[:a.top]
    parts.append(section(f"Top {a.top} unassigned by +1 (opportunities)", md_table([row(i, [", ".join(i["labels"][:3])]) for i in un_top], HEAD + ["labels"])))
    tgt_top = sorted(in_tgt, key=lambda i: (-i["plus1"], -i["comments"]))[:a.top]
    parts.append(section(f"{tgt}: top {a.top} by +1", md_table([row(i, [", ".join(i["labels"][:3])]) for i in tgt_top], HEAD + ["labels"])))

    # ---------------- quick-win heuristics
    qw = []
    for i in issues:
        score, why = 0, []
        if has(i, "regression"): score += 2; why.append("regression")
        if has(i, "good-first-issue"): score += 2; why.append("good-first-issue")
        if has(i, "documentation"): score += 1; why.append("docs")
        if QUICK_WIN_HINTS.search(i["title"]): score += 1; why.append("title hints small fix")
        if i["comments"] <= 2 and i["plus1"] >= 3: score += 1; why.append("wanted, undiscussed")
        if has(i, "verify-fixed"): score += 1; why.append("verify-fixed")
        if score >= 2:
            qw.append((score, i, why))
    qw.sort(key=lambda t: (-t[0], -t[1]["plus1"]))
    parts.append(section(f"Quick-win candidates (heuristic score ≥2, top {a.top})",
                         "Heuristic only — confirm by reading the issue (clear root cause? reporter-provided fix? one-file change?).\n\n" +
                         md_table([[s] + row(i, ["; ".join(w)]) for s, i, w in qw[:a.top]], ["score"] + HEAD + ["why"])))
    deep = sorted([i for i in issues if i["comments"] >= 15 or has(i, "epic") or has(i, "needs-design") or has(i, "needs-discussion")],
                  key=lambda i: (-i["comments"], -i["plus1"]))[:a.top]
    parts.append(section(f"Deep-problem candidates (≥15 comments, epic, needs-design/discussion; top {a.top})",
                         md_table([row(i, [", ".join(l for l in i["labels"] if l in ("epic", "needs-design", "needs-discussion", "upstream"))]) for i in deep],
                                  HEAD + ["flags"])))

    # ---------------- label & theme clusters
    def label_counts(subset):
        c = Counter(l for i in subset for l in i["labels"])
        return c

    all_c, tgt_c, dfr_c = label_counts(issues), label_counts(in_tgt), label_counts(in_dfr)
    rows = [[l, all_c[l], tgt_c[l], dfr_c[l]] for l in THEME_LABELS if all_c[l]]
    rows += [[l, all_c[l], tgt_c[l], dfr_c[l]] for l in ("bug", "enhancement", "regression", "early-in-release")]
    parts.append(section("Label counts (all / target / deferred)", md_table(rows, ["label", "all", tgt, dfr])))

    def theme_block(subset, name):
        lines = []
        for l in THEME_LABELS:
            grp = sorted([i for i in subset if has(i, l)], key=lambda i: (-i["plus1"], -i["comments"]))
            if len(grp) < 3:
                continue
            top = "; ".join(f"#{i['number']} ({i['plus1']}+1{', ' + i['assignees'][0] if i['assignees'] else ''}) {i['title'][:60]}" for i in grp[:4])
            lines.append(f"- **{l}** ({len(grp)}): {top}")
        return section(f"Theme clusters in {name}", "\n".join(lines) or "_none_")

    parts.append(theme_block(in_tgt, tgt))
    parts.append(theme_block(in_dfr, dfr))

    # deferred: high-reaction unassigned = triage-back candidates to read
    dfr_cand = sorted([i for i in in_dfr if i["plus1"] >= 5 or i["comments"] >= 12],
                      key=lambda i: (-i["plus1"], -i["comments"]))
    parts.append(section(f"{dfr}: candidates whose story to read ({len(dfr_cand)}; +1≥5 or ≥12 comments)",
                         "Run fetch_comments.sh on these before deciding pull-back / maybe / leave.\n\n" +
                         md_table([row(i, [", ".join(i["labels"][:3])]) for i in dfr_cand], HEAD + ["labels"])))

    report = f"# Triage data report — target {tgt}, user {a.user}\n" + "".join(parts)
    if a.out:
        a.out.write_text(report)
        print(f"wrote {a.out}", file=sys.stderr)
    else:
        print(report)
    if a.json:
        a.json.write_text(json.dumps({
            "target": tgt, "deferred": dfr, "user": a.user,
            "regressions": [i["number"] for i in regressions], "early": [i["number"] for i in early],
            "mine_target": [i["number"] for i in mine_tgt], "mine_deferred": [i["number"] for i in mine_dfr],
            "area_unassigned": [i["number"] for i in area_un], "quick_win_candidates": [i["number"] for _, i, _ in qw],
            "deferred_story_candidates": [i["number"] for i in dfr_cand],
            "top_unassigned": [i["number"] for i in un_top],
        }, indent=1))


if __name__ == "__main__":
    main()
