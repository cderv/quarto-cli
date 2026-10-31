#!/usr/bin/env bash
# Dump an issue and its full comment thread in a compact, readable form, so the
# "story" of the issue (why it was deferred, workarounds, maintainer commitments,
# linked PRs) can be summarized. Use it on the handful of issues that need judgment:
# Future-milestone candidates, high-reaction items, long threads.
#
# Usage:
#   fetch_comments.sh [--repo owner/name] [--no-auth] [--max-chars N] NUMBER [NUMBER...]
#
# Output per issue: header (state, milestone, assignees, labels, reactions, dates),
# body (truncated), then each comment as "--- user (date) ---" + text (truncated).

set -euo pipefail
REPO="quarto-dev/quarto-cli"
NO_AUTH=0
MAX=1200
NUMS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo) REPO="$2"; shift 2 ;;
    --no-auth) NO_AUTH=1; shift ;;
    --max-chars) MAX="$2"; shift 2 ;;
    -h|--help) sed -n '2,12p' "$0"; exit 0 ;;
    *) NUMS+=("$1"); shift ;;
  esac
done
[[ ${#NUMS[@]} -gt 0 ]] || { echo "need at least one issue number" >&2; exit 2; }

get() {
  if [[ $NO_AUTH -eq 0 ]] && gh api "$1" 2>/dev/null; then return; fi
  curl -sS --max-time 60 -H "Accept: application/vnd.github+json" "https://api.github.com/$1"
}

# The Python below is inside a single-quoted bash string: use only double quotes in it.
for n in "${NUMS[@]}"; do
  { get "repos/$REPO/issues/$n"; echo; get "repos/$REPO/issues/$n/comments?per_page=100"; } \
  | MAX="$MAX" python3 -I -c '
import json, sys, os
mx = int(os.environ["MAX"])
dec = json.JSONDecoder(); s = sys.stdin.read(); i = 0; objs = []
while i < len(s):
    while i < len(s) and s[i].isspace(): i += 1
    if i >= len(s): break
    o, i = dec.raw_decode(s, i); objs.append(o)
issue, comments = objs[0], (objs[1] if len(objs) > 1 else [])
if "number" not in issue:
    print("ERROR:", json.dumps(issue)[:300]); sys.exit(3)
def cut(t):
    t = (t or "").replace("\r", "").strip()
    return t if len(t) <= mx else t[:mx] + " [...]"
ms = (issue.get("milestone") or {}).get("title")
print("### #%d — %s" % (issue["number"], issue["title"]))
print("state=%s milestone=%s assignees=%s labels=%s" % (
    issue["state"], ms, [a["login"] for a in issue["assignees"]], [l["name"] for l in issue["labels"]]))
print("author=%s created=%s updated=%s +1=%s comments=%s" % (
    issue["user"]["login"], issue["created_at"][:10], issue["updated_at"][:10],
    issue["reactions"]["+1"], issue["comments"]))
print(); print(cut(issue.get("body"))); print()
for c in comments:
    print("--- %s (%s) ---" % (c["user"]["login"], c["created_at"][:10]))
    print(cut(c.get("body"))); print()
print("=" * 72); print()
'
done
