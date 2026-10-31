#!/usr/bin/env bash
# Publish this skill directory as its own branch (files at the branch root, own history),
# without `git subtree split`, which walks the whole quarto-cli history (~20k commits,
# ~20 minutes). Instead, for every commit on the current branch that touched the skill
# directory, create a commit whose tree is that directory's subtree. Result: the same
# linear history, in a second.
#
# The directory must be present and complete at HEAD: the rebuilt branch's tip is HEAD's
# subtree, so a partial directory would publish a partial skill.
#
# Usage (from the repo root, on the branch holding the skill):
#   scripts/publish_branch.sh [--prefix .claude/skills/quarto-release-triage]
#                             [--branch skill/quarto-release-triage] [--push]
set -euo pipefail
PREFIX=".claude/skills/quarto-release-triage"
BRANCH="skill/quarto-release-triage"
PUSH=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix) PREFIX="$2"; shift 2 ;;
    --branch) BRANCH="$2"; shift 2 ;;
    --push) PUSH=1; shift ;;
    -h|--help) sed -n '2,14p' "$0"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

cd "$(git rev-parse --show-toplevel)"
if ! git rev-parse -q --verify "HEAD:$PREFIX/SKILL.md" >/dev/null; then
  echo "refusing: $PREFIX/SKILL.md is not in HEAD's tree (directory missing or partial)." >&2
  echo "Check out a branch where the skill directory is complete, or edit a clone of $BRANCH directly." >&2
  exit 1
fi

parent=""
n=0
# oldest first; only commits where the directory exists
for c in $(git rev-list --reverse HEAD -- "$PREFIX"); do
  tree=$(git rev-parse -q --verify "$c:$PREFIX" 2>/dev/null) || continue
  msg=$(git log -1 --format=%B "$c")
  export GIT_AUTHOR_NAME="$(git log -1 --format=%an "$c")" GIT_AUTHOR_EMAIL="$(git log -1 --format=%ae "$c")" \
         GIT_AUTHOR_DATE="$(git log -1 --format=%aD "$c")"
  if [[ -n "$parent" ]]; then
    new=$(printf '%s' "$msg" | git commit-tree "$tree" -p "$parent")
  else
    new=$(printf '%s' "$msg" | git commit-tree "$tree")
  fi
  parent="$new"; n=$((n+1))
done
[[ -n "$parent" ]] || { echo "no commits touch $PREFIX" >&2; exit 1; }
git branch -f "$BRANCH" "$parent"
echo "branch $BRANCH -> $parent ($n commits)" >&2
git ls-tree -r --name-only "$BRANCH" >&2
if [[ $PUSH -eq 1 ]]; then
  git push --force-with-lease -u origin "$BRANCH"
fi
