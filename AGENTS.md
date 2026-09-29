# Kenai River Baseline Water Quality Monitoring — Project Context

## Repo Relationship

This repo is one of two that together form the full data pipeline for Kenai River Baseline Water Quality Monitoring:

- **`kenai-river-wqx`** (this repo — report repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx Hosts the Quarto book that integrates and displays long-term monitoring data. Data is accessed directly from EPA's Water Quality Portal (WQP), the authoritative public source. The report does not hold raw data locally; it reads from WQP and applies regulatory thresholds, visualizations, and narrative interpretation.
- **`kenai-river-wqx-qaqc`** (qaqc repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc Prepares annual monitoring data for submission to EPA WQX via CDX. Each year's pipeline ingests raw lab EDDs, applies QA/QC, formats data to WQX schema, and produces CDX-ready upload files. Once submitted, that data becomes publicly available through WQP, where the report repo accesses it.

qaqc repo submits data to EPA WQX → EPA publishes to WQP → report repo reads from WQP and displays it. Changes in either repo are relevant to both.

**`other/agent_context/session_log.md`** is auto-synced to the qaqc repo via GitHub Actions on every push to main that touches it. Each repo maintains its own `AGENTS.md` (not synced); only `session_log.md` is shared. **Edit `session_log.md` only from this repo (`kenai-river-wqx`).**

------------------------------------------------------------------------

## Companion Files (load on demand — not every session)

| File | Load when... |
|----|----|
| `other/agent_context/context_files/conventions.md` | Working on WQX formatting, CDX export, data ingestion, sample fraction rules, or the qaqc-repo pipeline architecture |
| `other/agent_context/context_files/data_architecture.md` | Navigating data storage layout, regulatory threshold structure, or R package choices |
| `other/agent_context/context_files/report_structure.md` | Working on the Quarto book, parameter chapters, or adding new pages |
| `other/agent_context/context_files/parameters_and_links.md` | You need the full parameter list, governance document index, or external links |
| `other/agent_context/context_files/lessons.md` | Session start — small file, cheap to check for standing gotchas |
| `other/agent_context/session_log.md` | You need history on a completed task, a resolved issue, or a past session's detail (recent entries, from 2026-07-13) |
| `other/agent_context/session_log_archive.md` | You need history predating 2026-07-13 (original appendix_a.qmd audit, pipeline architecture rationale, CDX delete workflow reference, HMW architecture detail) |
| `other/documents/sample_fraction_correction_handoff.md` | CDX fraction correction handoff notes for the qaqc repo |
| `other/documents/md/` | Reading any governance/reference document (QAPP, CALM, 18 AAC 70, etc.) — see `parameters_and_links.md` for the full table |

`other/agent_context/archive/` holds superseded one-off notes (old interview transcripts, scratch to-do files) kept for reference only — do not treat as current.

------------------------------------------------------------------------

## Next Session Priorities

**EPA WQX sync issue — Project and Station CSVs live at CDX; Results & Activities import blocked by a mismatched WQX Web import configuration.** The 835 orphaned 2021 Activity deletions are confirmed complete (0 rows via live WQP query, last checked 2026-09-28). `project.csv`/`station.csv` re-uploaded successfully 2026-09-28. `results_activities.csv` re-upload is blocked by a mismatched WQX Web import configuration (legacy 44-column `KWF_Results_Baseline_Template`, Uid 8515, column-position mismatch against the current 53-column export). **Full root-cause narrative and the three unrelated bugs fixed in `generate_cdx_export.R` along the way: see `other/agent_context/session_log.md`, Session Entry (2026-09-28).**

**WQX Web config rebuild — IN PROGRESS, live in the UI.** Building a replacement configuration, Uid 9481 (`KWF_Results_Baseline_Template_v2`), via screenshot-driven collaboration (the WQX Web UI is login-gated and not directly accessible to the assistant). **Full play-by-play, the resolved element mappings, the two "Ignore Column" fields (`Monitoring Location Name`, `Laboratory Sample ID`), and the target 53-column order for the final reorder: see `other/agent_context/session_log.md`, Session Entry (2026-09-29).**

Remaining steps at a glance (detail in session log): (1) confirm the 11-element checkbox batch + both Ignore Column actions were applied; (2) add the last Generated Value `Activity End Time Zone` (constant `AKDT`); (3) full bottom-up reorder of all rows to match the 53-column export order; (4) test-import a 5-10 row subset before the full 830-row file; (5) update `chapters/cdx_upload.qmd` with the corrected walkthrough; (6) download and commit the finished config; (7) decide whether to fix the Unicode-hyphen longitude bug at its pipeline source vs. leaving it as an on-disk patch.

1.  **Complete Results & Activities WQX Web config rebuild (start here)** — see the two items above.
2.  **Task 18 (HIGH)** — Create `templates/pipeline_template.qmd` in the qaqc repo. See `context_files/conventions.md`, Pipeline Architecture section.
3.  **Task 1c (HIGH)** — CALM 5-year window sample count check. Count `result_status_identifier == "Accepted"` results per parameter + site for `activity_start_date >= 2017-01-01`. Source: `other/output/wqx_formatted/intermediate/2021_export_data_flagged.csv`. Flag combinations below 10 (or 5 for toxics) — these fall to ADEC Screening Level. Consider sharing with ADEC.
4.  **Task 1b (HIGH)** — Audit all distinct `CharacteristicName` values in WQP for org `KENAI_WQX`. Cross-reference against current WQX domain list. Map variants to canonical names. Feeds next CDX re-upload.
5.  **Task 5 (Medium)** — Verify `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx`. Six codes: `fw_acute`, `fw_chronic`, `harvest_aquatic_life`, `noncarc_aquatic_org`, `noncarc_water`, `secondary_water_recreation`. Confirm against 18 AAC 70 and USEPA criteria docs; set `review_needed = N`.
6.  **Task 5a (Medium)** — Add CALM methodology notes to FC, turbidity, and BTEX chapter narratives noting each is excluded from the standard CALM binomial methodology. Links at https://dec.alaska.gov/water/water-quality/integrated-report/

------------------------------------------------------------------------

## Active Tasks

See `other/agent_context/session_log.md` for full context on any task.

| \# | Priority | Description | Status |
|----|----|----|----|
| 1a-reupload | HIGH | Upload corrected 2021 files to CDX: `results_activities.csv`, `project.csv`, `station.csv`. `project.csv`/`station.csv` submitted successfully 2026-09-28. `results_activities.csv` blocked by a mismatched WQX Web import configuration; a new 53-column configuration (Uid 9481) is being built live in WQX Web. See Next Session Priorities. | **In progress** — project/station done; new config ~80% built, needs final reorder + test import |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP | Pending |
| 1c | HIGH | CALM 5-year window sample count check (2017–2021) | Pending |
| 2 | Medium | Fix HMW visibility for 15 legacy numeric-ID stations — move process to qaqc repo | Pending |
| 2a | Low | Contact ADEC liaison re: tributary ATTAINS assessment units | Pending |
| 4 | Low | Verify boxplot DOCX sizing fix (check `Sys.getenv("QUARTO_PROFILE") == "docx"`) | Pending |
| 5 | Medium | Verify 6 `review_needed = Y` rows in `standard_types` sheet | Pending |
| 5a | Medium | Add CALM methodology notes to FC, turbidity, BTEX narratives | Pending |
| 6 | Low | Add narrative to `chapters/benzene.qmd` (no standalone standard; refer to BTEX chapter) | Pending |
| 7 | Low | Resolve ALS lab duplicate (DUP) status issue — 4 results, does not block CDX | Pending |
| 8 | Low | LOQ logic flow chart for ADEC | Pending |
| 9 | Low | Make `appendix_a.qmd` year-neutral (single `year` variable at top) | **Complete** |
| 10 | Low | Extract ingestion logic to `.R` scripts (do with Task 9) | **Complete** |
| 11 | Low | Restructure WQX data Activity → Results level (future years, qaqc repo) | Pending |
| 18 | HIGH | Create `templates/pipeline_template.qmd` in qaqc repo — canonical single-QMD pipeline. See `context_files/conventions.md`. | Pending |
| 19 | Medium | Build `2023.qmd` in qaqc repo from template; adapt Part A for 2023 EDD quirks. See `session_log_archive.md` for 2023-specific notes. | Pending |
| 12 | Low | Address historical CDX corrections (e.g., spring 2013 specific conductance) in qaqc repo | Pending |
| 13 | Low | Move `wqx_corrections.qmd` to qaqc repo | Pending |
| 14 | Low | Add inline tables alongside calculated-result download links in `appendix_a.qmd` | Pending |
| 15 | Low | Dynamically generate numerical values in parameter chapter prose via inline R | Pending |
| 16 | Low | Parameter chapter review workflow — post-2025 data integration | Pending |
| 17 | Low | Add multi-year duplicate RPD summary table to `data_qa_qc.qmd` | Pending |

------------------------------------------------------------------------

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash; use different punctuation instead.

------------------------------------------------------------------------

## Project Overview

Long-term cooperative monitoring led by Kenai Watershed Forum (KWF), south-central Alaska. Biannual (spring + summer) sampling at 22 sites (13 mainstem + 9 tributaries) since 2000. Current deliverable: Quarto book covering 2000–2025, modeled on 2007 and 2016 comprehensive reports.

- **Project home:** https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
- **GitHub:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
- **QA/QC repo:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
- **Public data:** https://www.waterqualitydata.us/ (org: `KENAI_WQX`)

Primary downstream consumer: **ADEC**, which draws from EPA CDX every two years for the Integrated Report (impairment decisions). Also serves KWF scientists, general public, and funding partners. A primary goal is that all data is publicly visible in [How's My Waterway](https://mywaterway.epa.gov/).

**HMW:** Monitoring data (Past Water Conditions) is WQP-driven via client-side HUC12 spatial ops. Waterbody condition (Overview/Aquatic Life) is ATTAINS-driven — all 13 mainstem sites have ATTAINS units; no tributary sites do (requires ADEC action in a future Integrated Report cycle).

**Collaboration standard:** Before implementing any code, explain what it does and why. Wait for user confirmation. Prioritize clarity over cleverness.

------------------------------------------------------------------------

## Known Data Issues (Active / Unresolved)

- **WQX/STORET sync (RESOLVED, re-upload in progress):** the 835 orphaned 2021 records are confirmed deleted from WQP (0 rows via live query, last checked 2026-09-28). `project.csv`/`station.csv` re-uploaded successfully 2026-09-28. `results_activities.csv` re-upload is blocked by a mismatched WQX Web import configuration, not by data content — see Task 1a-reupload.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years. Full audit needed (Task 1b).
- **Sample fraction inconsistency (RESOLVED in local files, not yet uploaded):** dissolved metals correctly `"Dissolved"` locally (verified 2026-09-28); not yet reflected in CDX/WQP pending the results_activities.csv re-upload.
- Full list including Turbidity, hydrocarbon, ALS duplicate, TSS, and spring-2013 specific-conductance issues: `context_files/known_issues.md`.

------------------------------------------------------------------------

## Governance Documents

Original PDFs are in `other/agent_context/`; text-extracted `.md` versions (preferred for AI ingestion) are in `other/documents/md/`. Full document index: `context_files/parameters_and_links.md`.
