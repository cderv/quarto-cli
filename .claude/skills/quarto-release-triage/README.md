# quarto-release-triage

Claude Code skill that turns the open issue backlog of quarto-dev/quarto-cli into a
per-release, per-developer work plan (`UPSTREAM_ISSUES_ANALYSIS.md`). See `SKILL.md`
for the method; `references/` for the triage criteria and the plan template;
`scripts/` for the bulk fetch, the local analysis and the comment-thread dump.

## Install

Clone (or symlink) this repository into a skills directory Claude Code scans:

```bash
# personal, all projects
git clone <this-repo> ~/.claude/skills/quarto-release-triage
# or project-local
git clone <this-repo> /path/to/quarto-cli/.claude/skills/quarto-release-triage
```

Then in a session: "refaire le triage des issues upstream pour v1.12".

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

## Update

```bash
git -C ~/.claude/skills/quarto-release-triage pull
```
