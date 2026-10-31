# quarto-release-triage

Claude Code skill that turns the open issue backlog of quarto-dev/quarto-cli into a
per-release, per-developer work plan (`UPSTREAM_ISSUES_ANALYSIS.md`). See `SKILL.md`
for the method; `references/` for the triage criteria and the plan template;
`scripts/` for the bulk fetch, the local analysis and the comment-thread dump.

## Install

The skill lives on the `skill/quarto-release-triage` branch of `cderv/quarto-cli`
(files at the branch root, own history, no quarto-cli sources). Clone that branch alone
into a skills directory Claude Code scans:

```bash
# personal, all projects
git clone --depth 1 --single-branch --branch skill/quarto-release-triage \
  https://github.com/cderv/quarto-cli ~/.claude/skills/quarto-release-triage
# or project-local
git clone --depth 1 --single-branch --branch skill/quarto-release-triage \
  https://github.com/cderv/quarto-cli /path/to/quarto-cli/.claude/skills/quarto-release-triage
```

## Update / publish changes

Edit the skill in that clone and `git push` — it is a normal branch.

If instead the skill was edited inside a work branch of the fork (directory
`.claude/skills/quarto-release-triage`, complete), rebuild and push the skill branch with:

```bash
.claude/skills/quarto-release-triage/scripts/publish_branch.sh --push
```

The script replays, with `git commit-tree`, every commit of the work branch that touched
the directory; `git subtree split` would give the same result but walks the full
quarto-cli history (~20 min). It refuses to run if the directory is missing at HEAD,
because it would otherwise publish a branch with only the files present in the last
commits.

## Requirements

- `gh` authenticated with a token SSO-authorized for the `quarto-dev` org
  (anonymous `curl` fallback: `--no-auth`, 60 requests/hour)
- Python 3.9+, `curl`, bash

## Tests

`evals/` holds three cases in the `claude plugin eval` layout:

```bash
claude plugin eval . --allow-tools Write,Edit,Bash
```

They need live GitHub API access, so run them from a machine where `gh api
repos/quarto-dev/quarto-cli/issues?per_page=1` succeeds.
