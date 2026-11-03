# Kenai River Baseline Water Quality Monitoring — Project Context

## Repo Relationship

This repo is one of two that together form the full data pipeline for Kenai River Baseline Water Quality Monitoring:

- **`kenai-river-wqx`** (this repo — report repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx Hosts the Quarto book that integrates and displays long-term monitoring data. Data is accessed directly from EPA's Water Quality Portal (WQP), the authoritative public source. The report does not hold raw data locally; it reads from WQP and applies regulatory thresholds, visualizations, and narrative interpretation.
- **`kenai-river-wqx-qaqc`** (qaqc repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc Prepares annual monitoring data for submission to EPA WQX via CDX. Each year's pipeline ingests raw lab EDDs, applies QA/QC, formats data to WQX schema, and produces CDX-ready upload files. Once submitted, that data becomes publicly available through WQP, where the report repo accesses it.

qaqc repo submits data to EPA WQX → EPA publishes to WQP → report repo reads from WQP and displays it. Changes in either repo are relevant to both.

**Roles (clarified 2026-10-28):** this repo stays a standalone book on long-term trends and observations; `chapters/appendix_a.qmd` is a worked example of taking one annual dataset through QA/QC. The qaqc repo is a single-year template that repeats that process in code for any project year and produces a draft report like the 2025 one. It keeps its own copies of the ingest and format code (this repo's are in `functions/appendix_a_scripts/`); the two currently diverge and which is the source of truth is undecided. The qaqc repo's design record is `other/agent_context/plans/2026-10-28-qaqc-repo-as-parameterized-template.md` there.

**Working split (2026-10-28):** day-to-day pipeline, QA/QC, CDX and preliminary-report work now happens in the qaqc repo, in its own Posit Assistant session (`D:\GitHub\kenai-river-wqx-qaqc`). Do not edit the qaqc repo from a session here unless the user asks. The transfer audit is complete: the qaqc repo received the plans, this repo's six pipeline scripts (`other/misc/report_repo_scripts/`), `master_reg_limits.xlsx` and `baseline_sites.csv`, the 2025 preliminary report source, governance `.md` files, `session_log_archive.md`, the pre-split `AGENTS.md`, the WQX templates, the CDX upload history, and `agent_context.qmd`. **The CDX upload chapter (`cdx_upload.qmd`) moved to the qaqc repo and was removed from this book.** Upload and delete records are in the qaqc repo's `other/documents/cdx_upload_history/`; copies remain here in `other/output/epa_wqp_uploads/` (delete them here only if a single source is wanted). Not moved: the book itself, governance PDFs (`other/agent_context/`), the 2021 export CSVs in `other/output/wqx_formatted/`.

**`other/agent_context/session_log.md`** is auto-synced to the qaqc repo via GitHub Actions on every push to main that touches it. Each repo maintains its own `AGENTS.md` (not synced); only `session_log.md` is shared. **Edit `session_log.md` only from this repo (`kenai-river-wqx`).**

**File ownership:** `AGENTS.md` (this file) is maintained by Posit Assistant and rewritten as the project evolves; the user does not edit it directly. `other/agent_context/agent_context.qmd` is the reverse: user-authored standing instructions for Posit Assistant, read only when the user points to it (not every session). Do not edit `agent_context.qmd` unless the user explicitly asks.

------------------------------------------------------------------------

## Companion Files (load on demand — not every session)

| File | Load when... |
|----|----|
| `other/agent_context/agent_context.qmd` | The user tells you to read it, or references new instructions they've added there. User-authored; do not edit unless explicitly asked |
| `other/agent_context/session_log.md` | You need history on a completed task, a resolved issue, or a past session's detail (recent entries, from 2026-07-13) |
| `other/agent_context/session_log_archive.md` | You need history predating 2026-07-13 (original appendix_a.qmd audit, pipeline architecture rationale, CDX delete workflow reference, HMW architecture detail) |
| `other/documents/sample_fraction_correction_handoff.md` | CDX fraction correction handoff notes for the qaqc repo (a copy is in that repo) |
| `other/documents/md/` | Reading any governance/reference document (QAPP, CALM, 18 AAC 70, etc.) (no index file exists; list the folder) |

**Companion `context_files/` do not exist (confirmed 2026-10-28).** The 2026-05-18 split of AGENTS.md announced `other/agent_context/context_files/{conventions,data_architecture,report_structure,project_overview,known_issues,lessons}.md`, but they were never committed and are not on either PC; the split commit deleted about 250 lines of content from `AGENTS.md`. The pre-split text is recoverable with `git show 0e4319c^:AGENTS.md` (286 lines; some of it is stale). The only surviving lesson (from `tasks/lessons.md`): before using `edit` on any file, grep the repo for the `old_string` to confirm it matches exactly one file.

`other/agent_context/archive/` holds superseded one-off notes (old interview transcripts, scratch to-do files) kept for reference only — do not treat as current.

------------------------------------------------------------------------

## Architecture Decision (revised 2026-10-28)

The per-year-repo plan of 2026-10-14 (template repo `kenai-river-baseline-wqx-template`, one repo per year, archiving the qaqc repo) is **superseded**. The approved design is `.posit/assistant/plans/2026-10-28-qaqc-repo-as-parameterized-template.md` (this repo, gitignored; copies of it and the 10-14 plan are tracked in the qaqc repo under `other/agent_context/plans/`). The qaqc repo is not archived: it is a single-year template that holds one working year at a time, archives finished years as GitHub Release bundles (`raw-data-{year}`), and is developed 2025 first, then backwards. Phases A-E and the decisions are in that plan; phase A (contract, layout, archive mechanics) is largely done and phase B (per-lab ingest functions, `dat` contract check) is next, both in the qaqc repo.

**Still in force from the 2026-10-14 plan:** Phase 0 (housekeeping, pushed) and Phase 1 (git history cleanup). This repo's `.git` shrank from 1.48 GB to 224 MB via `git filter-repo` (stripped `docs/**`, `*_cache/`, `*_files/` from all history); `docs/` is gitignored on `main` and published via `quarto publish gh-pages` to the `gh-pages` branch (Pages source confirmed live). The qaqc repo's ~460 MB history bloat was left as is by user decision.

------------------------------------------------------------------------

## Publishing the Book

The live site (https://kenai-watershed-forum.github.io/kenai-river-wqx/) is served from the `gh-pages` branch, **not** rebuilt automatically from pushes to `main`. Rendered HTML (`docs/`) is gitignored and never committed to `main`. To refresh the published site after merging changes to `main`:

``` bash
quarto publish gh-pages
```

This renders the full book and pushes the output to `gh-pages`, which GitHub Pages serves directly (source setting: "gh-pages /(root)", switched from the old "main /docs" during the Phase 1 git-history cleanup below). Run a plain `quarto render` first if you want to catch render errors without touching the live site. Full detail: `README.md`, "Rendering the Report" section.

------------------------------------------------------------------------

## Data Sourcing: Live WQP Cache (since 2026-10-16)

`chapters/data_sourcing.qmd` no longer reads static downloads. It reads only a local, gitignored cache in `other/input/WQX_downloads/wqp_live/` (`narrowresult.csv`, `station.csv`, `detection_limits.csv`, `refresh_metadata.csv`), built from live WQP queries (org `KENAI_WQX`, via `dataRetrieval`). **Refreshes are manual:** run `functions/refresh_wqp_data.R` (a script, not a function) whenever new data is expected on WQP. A guard in `data_sourcing.qmd` stops the render with instructions if the cache is missing; the narrative shows the refresh date inline. Rendering never calls the WQP API. **Refreshed 2026-11-02:** WQP now holds the corrected 2021 data (830 rows for 2021, 16,435 results total, 135 stations, 0 doubled-prefix stations, data span 2000-07-18 to 2021-07-27); the cache was refreshed and the full book rendered without errors (still no 2014-2020 data). Plan: `.posit/assistant/plans/2026-10-16-1120-live-wqp-data-sourcing-for-data-sourcingqmd.md`.

Regenerated downstream outputs (`baseline_export_format.csv`, `2000_2021_sitenames.csv`, `baseline_outliers.csv`, `baseline_analysis_format.csv`) now reflect whatever WQP currently holds, so they can show large diffs after a refresh; this is expected.

**Characteristic name matching table (2026-11-02):** `other/input/characteristic_name_matching_table.csv` maps WQP names to the names the book uses (applied in `data_sourcing.qmd`; original name kept in `characteristic_name_wqp`; `exclude_from_btex_sum` flag read by `reg_limits.qmd`). Add rows there for new name variants (feeds Task 1b). Only a local remap; WQP itself still holds the original names.

------------------------------------------------------------------------

## Trend Analyses in Parameter Chapters (opt-in, ZN pilot complete and committed as of 2026-10-21)

A chapter opts in by setting `trend_analysis <- TRUE` before knitting `templates/_parameter_chunk.Rmd`, which then knits `templates/_trend_chunk.Rmd`. Helpers are in `functions/trend_plots.R` (`load_trend_data()`, `trend_stats()`). Only `parameters/zinc.qmd` is opted in so far.

**Template improvements (2026-10-21, committed):** 
- **Single-site example figure:** Replaces the 4-panel quadrant with one interactive plot (log10 scale, single site x season). The example is selected automatically as the combination with the most detected values (n > 8), or can be manually overridden.
- **Figure title:** Two rows via `plotly::layout()`—bold site and season on row 1, statistics (n, Kendall tau, Sen slope, p-values) on row 2 (smaller font).
- **Results table:** Now a `DT::datatable` with sortable columns, global search, per-column filters, 10 rows/page by default. Headers wrap; font size reduced (0.85em) to fit. Falls back to static `kable` for DOCX output.
- **Reference:** Updated to Helsel et al. (2020) TM 4-A3 (supersedes 2002). Clarified that Mann-Kendall tolerates censored data robustly; alternative methods (ATS, etc.) are for slope estimation when non-detect rates are high.

**Methodological position (finalized 2026-10-21):**
- Keep Benjamini-Hochberg FDR adjustment across all tests within a parameter. Report both unadjusted p (in figure) and adjusted p (in table). 
- Exclude non-detects from Mann-Kendall and Sen slope fits, accepting the trade-off of reduced power when ND rates are high. Mann-Kendall itself is robust to censoring; exclusion is a simplification.
- No interpretive text in the trend section; table and figure speak for themselves.
- Example site/season automatic selection ensures reproducibility across parameters.

------------------------------------------------------------------------

## Analysis Notes Chapter (new as of 2026-10-21)

New appendix `chapters/analysis_notes.qmd` documents and justifies statistical choices (trend analysis methodology, non-detect handling, multiple comparisons, example site selection). Also includes a **"Data Access" section** with a download link to `wqp_live.zip`—an automatically-regenerated zip of all four CSV files in `other/input/WQX_downloads/wqp_live/`. Zip is created by `functions/create_wqp_live_zip.R` via a pre-render hook in `_quarto.yml`, ensuring the downloadable data is always current with the latest WQP cache. The zip is stored in `docs/assets/wqp_live.zip` (gitignored), linked from the rendered HTML as `../assets/wqp_live.zip`.

------------------------------------------------------------------------

## Next Session Priorities

**EPA WQX sync — `results_activities.csv` upload to CDX succeeded 2026-10-14.** The WQX Web config rebuild (Uid 9481, `KWF_Results_Baseline_Template_v2`) is complete and working, and all 830 rows of `results_activities.csv` imported cleanly after fixing both a config-level issue and a full set of data-level validation errors. `project.csv`/`station.csv` were already uploaded successfully 2026-09-28. **Full root-cause narrative, the config fix, and the complete list of data-level fixes (characteristic name mapping, Method Speciation, container color, detection limits, the Unicode-hyphen longitude bug fixed at its pipeline source, etc.): see `other/agent_context/session_log.md`, Session Entry (2026-10-14).**

**2021 data is now visible on WQP, but under doubled station IDs (found 2026-10-20).** A live query on 2026-10-20 returned 830 rows across 22 stations for 2021 under `KENAI_WQX` (2013 sanity check: 382 rows), so the indexing lag has cleared. A row-level comparison against `results_activities.csv` (matched on site, date, characteristic, fraction, activity type) paired all 830 rows one-to-one with 0 differences in values, detection limits, and lab name, and identical sample fraction, longitude, method speciation, and container color counts; unit/qualifier differences were cosmetic only (case, NA vs empty string). WQX applied synonym mapping to three characteristic names: `Total Phosphorus` to `Total Phosphorus, mixed forms`, `Xylenes (total)` to `Total xylenes`, `Iron, Total` to `Iron` (relevant to Task 1b). **Problem:** the uploaded `Monitoring Location ID` values carried the `KENAI_WQX-` prefix, and WQX Web prepends it again, so WQP now holds 22 original single-prefix stations (no 2021 data) plus 22 new `KENAI_WQX-KENAI_WQX-100000xx` stations that carry all 830 results. See Known Data Issues and Task 1a-reupload.

Remaining steps at a glance: (1) **finish the doubled-station cleanup on WQP** (see the 2026-10-20 progress below); (2) once 2021 is correct on WQP, assess whether `chapters/data_sourcing.qmd` or the book render need adjustment for 2021 (per `other/agent_context/agent_context.qmd`, "10/14/2026 follow up" note); (3) ~~commit the finished WQX Web config (Uid 9481)~~ done 2026-10-28, committed in the qaqc repo at `other/documents/EPA_WQX_templates/cdx_import_config_results_activities_20261028.cfg` (the walkthrough, including the config fix, the "Ignore Column" pattern, and the no-org-prefix rule, is now in `cdx_upload.qmd` in the qaqc repo (moved there 2026-10-28), section "Results Import Configuration in WQX Web").

1.  **2021 data on WQP: integrated and checked (2026-11-02); BTEX alignment implemented through step 3 (2026-11-02); full-render check is next.** Live query returned 830 rows for 2021 on the original single-prefix stations, 16,435 results total, 0 doubled-prefix rows; cache refreshed, book rendered. The chapter check (2026-11-02) found: sites `KBL_m_50.0`/`KBL_m_70.0` are fine (22 sites, one name each, 2000-2021). All 2021 fecal coliform (48) and spring 2021 nitrate+nitrite (24) are `Rejected` (documented in Appendix A); plots drop Rejected rows, downloads keep them (verified). Total Ca/Mg are missing for 9 spring 2021 site-events (KBL_m_40.0, 43.0, 50.0, 70.0, 82.0, KBL_t_36.0, 44.0, 74.0, 79.5), so hardness-dependent thresholds are unavailable there; user confirmed it was a lab miscommunication, not sampling design (both this and the Dissolved/`Filtered, field` equivalence are now bullets in `appendix_a.qmd` 'Notes for 2021 CDX upload'). `Filtered, field` in chapter `sample_fraction` filters is now unused (WQP holds all 2021 metals as `Dissolved`) but harmless; field filtering ended and lab filtering began summer 2022 (confirm date against QAPP). **BTEX alignment (plan approved 2026-11-02; steps 1-4 done, uncommitted):** the old bug was that `reg_limits.qmd` kept only volatile rows named 'Benzene, toluene, ethyl benzene, xylenes mix', so summer 2012 and 2021 never reached the chapter and plotted data stopped at 2010. Now `functions/btex_aggregate.R` (`btex_aggregate(dat, trip_blanks = FALSE)`, tests in `tests/test_btex_aggregate.R`, run with `testthat::test_file()`, 34 expectations pass) builds one BTEX row per event (site + date + sample type; duplicates averaged, ND as 0): the reported total if it exists, else benzene+toluene+ethylbenzene+xylenes with total xylene preferred over isomers; all-ND events get an NA value and 'Not Detected'; units set to ug/L; trip blanks (Activity ID ends `-Blank`) excluded. `reg_limits.qmd` calls it (the `exclude_from_btex_sum` flag is no longer used there, kept in the matching table). `baseline_analysis_format.csv` (regenerated by rendering `chapters/reg_limits.qmd`; a single-chapter render of `btex.qmd` does not regenerate it) has 358 BTEX events: 2000-2010 counts unchanged, 2012 (20) and 2021 (4) added, 0 blanks. Appendix A Q42 has a table of the 2021 trip-blank results (all U, detection limits 0.4-3.0 ug/L, from the CDX export); other years' blanks live in the separate repos. `static_boxplot_function.R` draws ND events as hollow circles at 0 with an 'ND' legend entry, BTEX only (`nd_enabled`); box statistics still treat ND as 0 (open choice: detected-only boxes would raise medians). `clean_plotly_legend()` now normalizes constant-colour trace names. The supplement at the end of `parameters/btex.qmd` shows the reported total equals the individual-analyte sum in 70 of 75 detect events (72 using total xylene). ND are stated without reporting limits for now. Plan file: `.posit/assistant/plans/2026-11-02-btex-alignment-across-years.md`. **Step 5 pending:** full book render and before/after checks across chapters. **Still to do:** (a) log the re-upload outcome in `other/output/epa_wqp_uploads/2026-10-20_doubled_station_cleanup/` README; (b) BTEX step 5 (full render check); (c) `detection_limits.csv` does not match `narrowresult.csv` on `ResultIdentifier` (0 of 7,033 non-detects, cause unconfirmed, possibly ID type); (d) static chapter narrative is deferred until 2000-2025 data are all on WQP. The qaqc repo's export script copy has the same prefix guard (see Known Data Issues).
2.  **Trend analysis decision and rollout** — settle the multiplicity/statistical approach (see "Trend Analyses" section), update the trend table and methods text accordingly, remove the redundant `zn_year_max` sentence in `zinc.qmd`, then commit the trend files and roll out to other chapters (lead next, per prior discussion). **Deferred by user (2026-11-02) until all 2000-2025 data are on WQX.**
3.  **Tasks 18/19 (moved to the qaqc repo)** — the qaqc repo (`D:\GitHub\kenai-river-wqx-qaqc`) is now the single-year template; its work is tracked by the 2026-10-28 plan there, not by this repo's task list.
4.  **Task 1c (HIGH)** — CALM 5-year window sample count check. Count `result_status_identifier == "Accepted"` results per parameter + site for `activity_start_date >= 2017-01-01`. Source: `other/output/wqx_formatted/intermediate/2021_export_data_flagged.csv`. Flag combinations below 10 (or 5 for toxics) — these fall to ADEC Screening Level. Consider sharing with ADEC. **Deferred by user (2026-11-02) until all 2000-2025 data are on WQX.**
5.  **Task 1b (HIGH)** — (partly started 2026-11-02: matching table exists with the nitrogen, phosphorus and xylene rows) Audit all distinct `CharacteristicName` values in WQP for org `KENAI_WQX`. Cross-reference against current WQX domain list. Map variants to canonical names. Feeds next CDX re-upload. **User (2026-11-02): this audit will be part of a separate qaqc-repo effort, not done here.**
6.  **Task 5 (Medium)** — Verify `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx`. Six codes: `fw_acute`, `fw_chronic`, `harvest_aquatic_life`, `noncarc_aquatic_org`, `noncarc_water`, `secondary_water_recreation`. Confirm against 18 AAC 70 and USEPA criteria docs; set `review_needed = N`.
7.  **Task 5a (Medium)** — Add CALM methodology notes to FC, turbidity, and BTEX chapter narratives noting each is excluded from the standard CALM binomial methodology. Links at https://dec.alaska.gov/water/water-quality/integrated-report/

------------------------------------------------------------------------

## Active Tasks

See `other/agent_context/session_log.md` for full context on any task.

| \# | Priority | Description | Status |
|----|----|----|----|
| 1a-reupload | HIGH | Upload corrected 2021 files to CDX. `project.csv`/`station.csv` submitted 2026-09-28; `results_activities.csv` imported and submitted 2026-10-14 (Uid 9481). 2026-10-20: data visible on WQP (830 rows, values verified) but under 22 doubled-prefix stations because the uploaded Monitoring Location IDs included the org prefix. Fix applied in `functions/appendix_a_scripts/generate_cdx_export.R` (prefix stripped in both files; same guard ported to the qaqc repo copy). Also corrected on 2026-10-20: sites 10000030/10000031 are mainstem (`KBL_m_50.0`, `KBL_m_70.0`), not `t`, in the `adec_site_names` sheet (both repos), which changed 34 Activity IDs. Deletes of the 830 results (dataset 117563) and the 22 stations (dataset 117564) completed at CDX 2026-10-20. Still needed: wait for WQP to clear, re-upload `station.csv` then `results_activities.csv`, re-verify. | **Complete 2026-11-02:** published on WQP (830 rows for 2021, original stations), cache refreshed, book rendered; follow-up checks in Next Session Priorities item 1 |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP. Includes correcting WQP result 56514557 (detection limit stored as "O.18" with a letter O). | Pending (moves to a separate qaqc-repo effort, per user 2026-11-02) |
| T1 | Medium | Opt-in trend analyses in parameter chapters (zinc pilot). Decide BH vs unadjusted approach, then roll out. | **In progress**, uncommitted; deferred until all 2000-2025 data are on WQX (user, 2026-11-02) |
| 1c | HIGH | CALM 5-year window sample count check (2017–2021) | Deferred until all 2000-2025 data are on WQX (user, 2026-11-02) |
| 2 | Medium | Fix HMW visibility for 15 legacy numeric-ID stations (process lives in the qaqc repo) | Pending |
| 2a | Low | Contact ADEC liaison re: tributary ATTAINS assessment units | Pending |
| 2b | Medium | Check whether results display correctly in How's My Waterway despite characteristic name variants (retired Nitrate+Nitrite name, `Phosphorus` vs `Total Phosphorus, mixed forms`, `Xylene` vs `Total xylenes`). To be done in a separate qaqc-repo variant, not here. Not yet verified how HMW groups names. | Pending (qaqc repo variant) |
| BTEX | HIGH | Align BTEX across years (2012 and 2021 missing from the chapter, ND handling, trip blanks). Steps 1-4 done 2026-11-02 (`btex_aggregate.R` + tests, `reg_limits.qmd` wired, ND marker, Appendix A Q42 table). Plan: `.posit/assistant/plans/2026-11-02-btex-alignment-across-years.md`. | **In progress**, uncommitted; step 5 (full render and checks) pending |
| 4 | Low | Verify boxplot DOCX sizing fix (check `Sys.getenv("QUARTO_PROFILE") == "docx"`) | Pending |
| 5 | Medium | Verify 6 `review_needed = Y` rows in `standard_types` sheet | Pending |
| 5a | Medium | Add CALM methodology notes to FC, turbidity, BTEX narratives | Pending |
| 6 | Low | Add narrative to `chapters/benzene.qmd` (no standalone standard; refer to BTEX chapter) | Pending |
| 7 | Low | Resolve ALS lab duplicate (DUP) status issue — 4 results, does not block CDX | Pending |
| 8 | Low | LOQ logic flow chart for ADEC | Pending |
| 9 | Low | Make `appendix_a.qmd` year-neutral (single `year` variable at top) | **Complete** |
| 10 | Low | Extract ingestion logic to `.R` scripts (do with Task 9) | **Complete** |
| 11 | Low | Restructure WQX data Activity → Results level (future years, qaqc repo) | Pending (qaqc repo) |
| 18 | HIGH | Create `templates/pipeline_template.qmd` in qaqc repo — canonical single-QMD pipeline. | **Superseded** — qaqc repo already has `project_year/{2014-2025}.qmd`; `2025.qmd` is largely complete. See Architecture Decision note above; now tracked in the qaqc repo. |
| 19 | Medium | Build `2023.qmd` in qaqc repo from template; adapt Part A for 2023 EDD quirks. See `session_log_archive.md` for 2023-specific notes. | **Moved to the qaqc repo**; years are developed 2025 first, then backwards, per the 2026-10-28 plan |
| 12 | Low | Address historical CDX corrections (e.g., spring 2013 specific conductance) in qaqc repo | Pending (qaqc repo) |
| 13 | Low | Move `wqx_corrections.qmd` to qaqc repo | Pending (qaqc repo) |
| 14 | Low | Add inline tables alongside calculated-result download links in `appendix_a.qmd` | Pending |
| 15 | Low | Dynamically generate numerical values in parameter chapter prose via inline R | Pending |
| 16 | Low | Parameter chapter review workflow — post-2025 data integration | Pending |
| 17 | Low | Add multi-year duplicate RPD summary table to `data_qa_qc.qmd` | Pending |

------------------------------------------------------------------------

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash; use different punctuation instead.
- **Line endings:** `.qmd`, `.Rmd`, and `AGENTS.md` use CRLF, which breaks the `edit` tool's exact-string matching on multi-line strings. Use a Python read/replace/write that normalizes and restores CRLF for those files. An R `readChar`/`sub(fixed = TRUE)`/`writeChar(useBytes = TRUE)` round trip with `\r\n` in the patterns also works and preserves CRLF.
- **Rendering `chapters/appendix_a.qmd` (gotchas):** on the R 4.6.1 install it needs `openxlsx`, `anytime`, `plotrix`, and `patchwork` (installed 2026-10-20). `generate_cdx_export.R` reads `export_dat` from the global environment, so it cannot be sourced standalone; render the chapter instead (about 1 minute: `quarto render chapters/appendix_a.qmd --to html`). A render rewrites three unrelated `.xlsx` files in `other/input/wqx_templates/` (`sample_type_abbreviations`, `sgs_site_names_matching_table`, `taurianen_site_names_matching_table`) byte-wise with identical content; restore them with `git checkout --` before committing. `wqx_template_matching_table.xlsx` is edited with `openxlsx2` (`wb_load`/`add_data`/`wb_save`; verify other sheets and the cell comment survive); the qaqc repo copy cannot be written from R in this setup, so edit a local copy via `bash cp` and copy it back.
- **Chapters removed from the book:** `cdx_upload.qmd` (2026-10-28, now in the qaqc repo); the book's appendix list in `_quarto.yml` is `analysis_notes`, `appendix_a`, `appendix_b`, `finance_history`.
- **Unit case:** all ingest scripts must emit `mg/L` (WQX capitalization). `ingest_tss.R` was fixed on 2026-10-20 (it emitted `mg/l`); `appendix_a.qmd` now has a `stopifnot()` guard instead of an in-memory recode. Regenerating from an older intermediate CSV will still show `mg/l` until the chapter is re-rendered.

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

- **WQX/STORET sync (RESOLVED):** the 835 orphaned 2021 records are confirmed deleted from WQP (0 rows via live query, last checked 2026-09-28). `project.csv`/`station.csv` re-uploaded successfully 2026-09-28. `results_activities.csv` imported successfully to WQX Web 2026-10-14 (all 830 rows, zero validation errors) and Step 3 CDX submission confirmed the same day — see Task 1a-reupload. Corrected 2021 data is not yet publicly visible in WQP as of 2026-10-14 (0 rows via live query); this is an expected post-submission indexing lag, recheck in a future session. **Update 2026-10-20:** the 2021 data is now visible on WQP but under doubled station IDs; see the next bullet.
- **Doubled station ID prefix (RESOLVED 2026-11-02, found 2026-10-20):** `Monitoring Location ID` in `results_activities.csv` and `MonitoringLocationIdentifier` in `station.csv` were copied unchanged from WQP downloads, which include the `KENAI_WQX-` prefix. WQX Web prepends the org prefix itself on import, producing 22 duplicate stations `KENAI_WQX-KENAI_WQX-100000xx` that hold all 830 2021 results, while the 22 original stations have no 2021 data. **Code fixes (both uncommitted unless noted in git):** the report repo's `generate_cdx_export.R` strips the prefix; the qaqc repo copy (`other/misc/qaqc_repo_transition/functions/generate_cdx_export.R`) now also strips it, builds Activity IDs from site codes, and stops on any prefixed or unknown location ID (station-side guards tested; results-side guards not independently exercised). **WQX cleanup:** both delete imports completed at CDX 2026-10-20 (see Task 1a-reupload); WQX accepted the station delete, so it allows station deletion once a station has no results. **Re-upload 2026-10-25:** bare-ID files submitted and completed at CDX; confirmed 2026-11-02 that WQX matched bare IDs to the original stations (no doubled-prefix stations on WQP). Issue resolved. **Related fixes 2026-10-20:** (1) `adec_site_names` listed sites 10000030/10000031 as `KBL_t_50.0`/`KBL_t_70.0`; both are mainstem, corrected to `m` in both repos' `wqx_template_matching_table.xlsx` (the earlier 2021 `t` Activity IDs for those two sites, 34 rows, were among those deleted); (2) total suspended solids was ingested with lowercase `mg/l` (`ingest_tss.R`), fixed at the source to `mg/L`, and the in-memory fix in `appendix_a.qmd` is now a `stopifnot()` guard; a full render confirmed `results_activities.csv` and `station.csv` unchanged apart from the intended ID changes.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years. Full audit needed (Task 1b).
- **Sample fraction inconsistency (RESOLVED):** dissolved metals correctly `"Dissolved"` locally (verified 2026-09-28) and reflected in the `results_activities.csv` imported to WQX Web 2026-10-14.
- **Non-numeric detection limit in WQP:** result 56514557 has a limit stored as "O.18" (letter O). `functions/trend_plots.R` coerces it to NA, so that non-detect is dropped from trend analyses. Correct at the source (see Task 1b).
- Full list including Turbidity, hydrocarbon, ALS duplicate, TSS, and spring-2013 specific-conductance issues: not written down in one place; see `session_log.md` and `session_log_archive.md`.

------------------------------------------------------------------------

## Governance Documents

Text-extracted `.md` versions are in `other/documents/md/` (QAPP, CALM, 18 AAC 70, toxics manual, 2016 report, 2021 field report, 2025 MOU, SGS DL/LOD/LOQ note, 2024 WaterSMART proposal). The original PDFs/docx were removed from `other/agent_context/` on 2026-11-02 (recoverable from git history, commit before that date); the QAPP and SGS PDFs also remain in `other/documents/QAPP/`. The 2007 report PDF (`Kenai Watershed Forum Baseline Water Quality Assessment 2007.pdf`) is corrupt (all zero bytes, also in git), so it has no `.md` version; re-obtain it if needed. The pre-split index lives in git history (see the note under Companion Files).
