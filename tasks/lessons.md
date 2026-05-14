# Lessons Learned

Patterns captured after user corrections. Review at session start when relevant.

| Date | Context | Mistake | Rule |
|----|----|----|----|
| 2026-05-14 | AGENTS.md edit | `edit` tool matched the same `old_string` text in a QMD file that had AGENTS.md content embedded in it, corrupting the QMD's YAML front matter | Before using `edit` on AGENTS.md (or any file), run a repo-wide grep for the `old_string` to confirm it appears in exactly one file. If it matches multiple files, use a more unique surrounding context as the match string. |
