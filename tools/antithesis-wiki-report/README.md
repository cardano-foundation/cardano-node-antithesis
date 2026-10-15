# antithesis-wiki-report

Upload **weekly Antithesis platform status** pages to the
[cardano-node-antithesis wiki](https://github.com/cardano-foundation/cardano-node-antithesis/wiki/Antithesis-Reports)
(pragma engagement).

**Cadence:** every **Thursday before the Discord meeting**.

**Format:** owned by Antithesis. Generate the body with **their** skill;
this tool only publishes the file, updates the index/sidebar, and enforces
public-safety checks. CF does not ship a report template.

## Quick start

```bash
# After Antithesis tooling wrote REPORT.md in their format:
./scripts/upload-report.sh publish \
  --week 2026-W31 \
  --authors "Your Name (@github)" \
  REPORT.md
```

Prints a single wiki URL on success.

## Safety

The wiki is public, so it carries the **summary only**: project table, run
counts and Antithesis platform failures. Findings about a system under test
(real or potential bugs, their evidence, pointers to owners, suggested fixes)
may be undisclosed bugs: they go privately to each project's owner, and
anything touching consensus or the ledger goes through that project's
security disclosure process.

The script refuses bodies carrying a real or potential bug finding (🟥, 🟨),
an owner pointer or a suggested fix, and bodies that look like authenticated
Antithesis report URLs or session material. Use **run IDs** only. Structure is
otherwise left unchanged. The marker check is a tripwire for the agreed
format, not a classifier: a finding written without the markers passes it.

## Skill

See [SKILL.md](./SKILL.md) for agent instructions. Symlink into
`~/.claude/skills/antithesis-wiki-report` if you want slash-command discovery.
