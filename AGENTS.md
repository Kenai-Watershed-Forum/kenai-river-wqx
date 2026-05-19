# Kenai River Baseline Water Quality Monitoring — Project Context

## Companion Files (load on demand)

| File | Load when... |
|---|---|
| `other/agent_context/context_files/conventions.md` | WQX formatting, CDX export, data ingestion, or any data convention question (sample fraction, flagging, trip blanks, pipeline architecture) |
| `other/agent_context/context_files/data_architecture.md` | Navigating data storage, regulatory thresholds, or R package choices |
| `other/agent_context/context_files/report_structure.md` | Working on the Quarto book, parameter chapters, or adding new pages |
| `other/agent_context/context_files/project_overview.md` | Orienting to the project, HMW/ATTAINS, governance, or external links |
| `other/agent_context/context_files/known_issues.md` | Before any CDX/WQP work, troubleshooting data, or reviewing QA/QC decisions |
| `other/agent_context/session_log.md` | Full log of completed session work, resolved issues, and task context |
| `other/documents/sample_fraction_correction_handoff.md` | CDX fraction correction handoff for the qaqc repo |
| `tasks/lessons.md` | Accumulated correction patterns; review when relevant to current task |

## Memory System Note

This is the **report repo** (`kenai-river-wqx`). A separate qaqc repo (`kenai-river-wqx-qaqc`) handles annual data preparation and CDX submission. `other/agent_context/session_log.md` is auto-synced from this repo to the qaqc repo on every push to `main` that touches it (see `.github/workflows/sync-agent-context.yml`). Edit `session_log.md` only from the report repo.

This repo also contains `CLAUDE.md` (Claude Code instructions, separate tool) alongside `AGENTS.md`. They are independent. Do not embed `AGENTS.md` content inside `.qmd` or other source files.

------------------------------------------------------------------------

## Next Session Priorities

**EPA WQX 2021 re-upload READY — deletion confirmed complete; proceed with CDX re-upload. WQP query on 2026-05-18 returned 0 rows for KENAI_WQX 2021 data, confirming the warehouse refresh deleted the 835 orphaned records. The "Domain Value Invalid" CDX batch delete errors were because the records were already gone from WQX Web's internal DB. Re-upload file is ready: `other/output/wqx_formatted/results_activities.csv`. Also upload `project.csv` and `station.csv`. Verify in WQP after ETL processes (~days).**

Top priorities (load `session_log.md` for full context on any task):

1. **Task 18 (HIGH)** — Create `templates/pipeline_template.qmd` in the qaqc repo (canonical single-QMD pipeline)
2. **Task 1c (HIGH)** — CALM 5-year window sample count check (2017-2021); source: `other/output/wqx_formatted/intermediate/2021_export_data_flagged.csv`
3. **Task 1b (HIGH)** — Audit all distinct `CharacteristicName` values in WQP for org `KENAI_WQX`
4. **Task 5 (Medium)** — Verify 6 `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx`
5. **Task 5a (Medium)** — Add CALM methodology notes to FC, turbidity, and BTEX chapter narratives

------------------------------------------------------------------------

## Active Tasks

| # | Priority | Description | Status |
|---|---|---|---|
| 1a-reupload | HIGH | Re-upload 835 2021 records. Deletion confirmed complete (WQP 2026-05-18 query: 0 rows). Upload `results_activities.csv`, `project.csv`, `station.csv` to CDX. | **Ready to upload** |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP | Pending |
| 1c | HIGH | CALM 5-year window sample count check (2017-2021) | Pending |
| 2 | Medium | Fix HMW visibility for 15 legacy numeric-ID stations | Pending |
| 2a | Low | Contact ADEC liaison re: tributary ATTAINS assessment units | Pending |
| 4 | Low | Verify boxplot DOCX sizing fix (`Sys.getenv("QUARTO_PROFILE") == "docx"`) | Pending |
| 5 | Medium | Verify 6 `review_needed = Y` rows in `standard_types` sheet | Pending |
| 5a | Medium | Add CALM methodology notes to FC, turbidity, BTEX narratives | Pending |
| 6 | Low | Add narrative to `chapters/benzene.qmd` | Pending |
| 7 | Low | Resolve ALS lab duplicate (DUP) status issue — 4 results | Pending |
| 8 | Low | LOQ logic flow chart for ADEC | Pending |
| 9 | Low | Make `appendix_a.qmd` year-neutral | **Complete** |
| 10 | Low | Extract ingestion logic to `.R` scripts | **Complete** |
| 11 | Low | Restructure WQX data Activity to Results level (future years, qaqc repo) | Pending |
| 18 | HIGH | Create `templates/pipeline_template.qmd` in qaqc repo | Pending |
| 19 | Medium | Build `2023.qmd` in qaqc repo from template | Pending |
| 12 | Low | Address historical CDX corrections (e.g., spring 2013 specific conductance) | Pending |
| 13 | Low | Move `wqx_corrections.qmd` to qaqc repo | Pending |
| 14 | Low | Add inline tables alongside calculated-result download links in `appendix_a.qmd` | Pending |
| 15 | Low | Dynamically generate numerical values in parameter chapter prose | Pending |
| 16 | Low | Parameter chapter review workflow — post-2025 data integration | Pending |
| 17 | Low | Add multi-year duplicate RPD summary table to `data_qa_qc.qmd` | Pending |

------------------------------------------------------------------------

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash; use different punctuation instead.
- **Minimal impact.** Code changes should touch only what is necessary. Before implementing, confirm the approach with the user. Find root causes rather than applying temporary fixes.
- **Collaboration standard.** Before implementing any code, explain what it does and why. Wait for user confirmation. Prioritize clarity over cleverness.
