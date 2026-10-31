#!/usr/bin/env bash
# Fetch everything needed for a release triage of a GitHub repo into a data directory.
#
# Produces in OUT_DIR:
#   repo.json        - repo metadata (open_issues_count includes PRs)
#   milestones.json  - open milestones with open_issues counts
#   issues.jsonl     - ALL open issues (PRs excluded), one compact JSON object per line
#   velocity.json    - issues created / closed in the last N days
#   labels.json      - all labels with descriptions
#
# Everything else (per-milestone, per-assignee, per-label views, top reactions...) is
# derived locally by analyze_issues.py from issues.jsonl, so this script is the only
# place that talks to GitHub for the bulk data. Comment threads are fetched separately
# by fetch_comments.sh for the few issues whose story matters.
#
# Usage:
#   fetch_issues.sh --out DIR [--repo owner/name] [--days 30] [--no-auth]
#
# Auth: uses `gh api` when `gh` is authenticated AND the token is SSO-authorized for the
# org. Falls back to anonymous curl (60 requests/hour) with --no-auth or when gh fails.
# ~1700 open issues = ~17 pages, so a full anonymous fetch fits in one hour's quota,
# but don't run it twice in a row anonymously.

set -euo pipefail

REPO="quarto-dev/quarto-cli"
OUT=""
DAYS=30
NO_AUTH=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out) OUT="$2"; shift 2 ;;
    --repo) REPO="$2"; shift 2 ;;
    --days) DAYS="$2"; shift 2 ;;
    --no-auth) NO_AUTH=1; shift ;;
    -h|--help) sed -n '2,24p' "$0"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

[[ -n "$OUT" ]] || { echo "--out DIR is required" >&2; exit 2; }
mkdir -p "$OUT"

API="https://api.github.com"
MODE="gh"

if [[ $NO_AUTH -eq 1 ]]; then
  MODE="curl"
elif ! gh api "repos/$REPO/issues?per_page=1" >/dev/null 2>"$OUT/.gh_err"; then
  MODE="curl"
  echo "gh api failed, falling back to anonymous curl. Reason:" >&2
  head -c 300 "$OUT/.gh_err" >&2; echo >&2
  if grep -q "SAML" "$OUT/.gh_err"; then
    echo "HINT: the token exists but is not SSO-authorized for this org." >&2
    echo "      GitHub > Settings > Personal access tokens > Configure SSO > authorize the org." >&2
  fi
fi
rm -f "$OUT/.gh_err"
echo "mode: $MODE" >&2

# get PATH -> prints JSON body. For curl mode, handles rate-limit errors loudly.
get() {
  local path="$1"
  if [[ $MODE == gh ]]; then
    gh api "$path"
  else
    local body
    body=$(curl -sS --max-time 60 -H "Accept: application/vnd.github+json" "$API/$path")
    # GitHub (or a proxy in front of it) answers errors with a {"message": ...} object.
    if printf '%s' "$body" | python3 -I -c 'import json,sys
try: d=json.load(sys.stdin)
except Exception: sys.exit(0)
sys.exit(1 if isinstance(d,dict) and "message" in d and "number" not in d else 0)'; then
      printf '%s' "$body"
    else
      echo "ERROR from GitHub API on $path:" >&2
      printf '%s\n' "$body" | head -c 400 >&2; echo >&2
      if printf '%s' "$body" | grep -q 'rate limit'; then
        curl -s "$API/rate_limit" | python3 -I -c 'import json,sys,datetime;r=json.load(sys.stdin)["rate"];print("anonymous quota resets at",datetime.datetime.fromtimestamp(r["reset"]),file=sys.stderr)'
      fi
      echo "Authenticate gh (and SSO-authorize the token for the org), or in a Claude cloud session attach the repo (add_repo) so the proxy allows API calls." >&2
      exit 3
    fi
  fi
}

echo "repo..." >&2
get "repos/$REPO" > "$OUT/repo.json"

echo "milestones..." >&2
get "repos/$REPO/milestones?state=open&per_page=100" > "$OUT/milestones.json"

echo "labels..." >&2
{ get "repos/$REPO/labels?per_page=100&page=1"; get "repos/$REPO/labels?per_page=100&page=2"; } \
  | python3 -I -c 'import json,sys; out=[]; dec=json.JSONDecoder(); s=sys.stdin.read(); i=0
while i < len(s):
    while i < len(s) and s[i].isspace(): i+=1
    if i>=len(s): break
    o,i=dec.raw_decode(s,i); out+=o
json.dump([{"name":l["name"],"description":l.get("description") or ""} for l in out], sys.stdout)' \
  > "$OUT/labels.json"

echo "all open issues (paginated)..." >&2
: > "$OUT/issues.jsonl"
page=1
while :; do
  body=$(get "repos/$REPO/issues?state=open&per_page=100&page=$page")
  n=$(printf '%s' "$body" | python3 -I -c '
import json,sys
data=json.load(sys.stdin)
n=0
for i in data:
    if "pull_request" in i: continue
    n+=1
    rec={
      "number": i["number"], "title": i["title"],
      "labels": [l["name"] for l in i["labels"]],
      "milestone": (i["milestone"] or {}).get("title"),
      "assignees": [a["login"] for a in i["assignees"]],
      "comments": i["comments"],
      "plus1": i["reactions"]["+1"], "reactions": i["reactions"]["total_count"],
      "created_at": i["created_at"][:10], "updated_at": i["updated_at"][:10],
      "author": i["user"]["login"],
      "body": (i.get("body") or "")[:400],
    }
    print(json.dumps(rec))
print(len(data), file=sys.stderr)
' 2>&1 >>"$OUT/issues.jsonl")
  echo "  page $page: $n items" >&2
  [[ "$n" -lt 100 ]] && break
  page=$((page+1))
  [[ $MODE == curl ]] && sleep 1
done

echo "velocity (last $DAYS days)..." >&2
SINCE=$(date -u -d "-$DAYS days" +%Y-%m-%d 2>/dev/null || date -u -v-${DAYS}d +%Y-%m-%d)
created=$(get "search/issues?q=repo:$REPO+is:issue+is:open+created:>$SINCE&per_page=1" | python3 -I -c 'import json,sys;print(json.load(sys.stdin).get("total_count",-1))')
closed=$(get "search/issues?q=repo:$REPO+is:issue+is:closed+closed:>$SINCE&per_page=1" | python3 -I -c 'import json,sys;print(json.load(sys.stdin).get("total_count",-1))')
printf '{"days": %s, "since": "%s", "created": %s, "closed": %s, "fetched_at": "%s"}\n' \
  "$DAYS" "$SINCE" "$created" "$closed" "$(date -u +%Y-%m-%d)" > "$OUT/velocity.json"

echo "done -> $OUT ($(wc -l < "$OUT/issues.jsonl") open issues)" >&2
