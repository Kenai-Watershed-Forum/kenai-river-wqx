# Kenai River Baseline Water Quality Monitoring — Project Context

## Repo Relationship

This repo is one of two that together form the full data pipeline for Kenai River Baseline Water Quality Monitoring:

- **`kenai-river-wqx`** (this repo — report repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx Hosts the Quarto book that integrates and displays long-term monitoring data. Data is accessed directly from EPA's Water Quality Portal (WQP), the authoritative public source. The report does not hold raw data locally; it reads from WQP and applies regulatory thresholds, visualizations, and narrative interpretation.
- **`kenai-river-wqx-qaqc`** (qaqc repo): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc Prepares annual monitoring data for submission to EPA WQX via CDX. Each year's pipeline ingests raw lab EDDs, applies QA/QC, formats data to WQX schema, and produces CDX-ready upload files. Once submitted, that data becomes publicly available through WQP, where the report repo accesses it.

qaqc repo submits data to EPA WQX → EPA publishes to WQP → report repo reads from WQP and displays it. Changes in either repo are relevant to both.

**`other/agent_context/session_log.md`** is auto-synced to the qaqc repo via GitHub Actions on every push to main that touches it. Each repo maintains its own `AGENTS.md` (not synced); only `session_log.md` is shared. **Edit `session_log.md` only from this repo (`kenai-river-wqx`).**

**File ownership:** `AGENTS.md` (this file) is maintained by Posit Assistant and rewritten as the project evolves; the user does not edit it directly. `other/agent_context/agent_context.qmd` is the reverse: user-authored standing instructions for Posit Assistant, read only when the user points to it (not every session). Do not edit `agent_context.qmd` unless the user explicitly asks.

------------------------------------------------------------------------

## Companion Files (load on demand — not every session)

| File | Load when... |
|----|----|
| `other/agent_context/context_files/conventions.md` | Working on WQX formatting, CDX export, data ingestion, sample fraction rules, or the qaqc-repo pipeline architecture |
| `other/agent_context/context_files/data_architecture.md` | Navigating data storage layout, regulatory threshold structure, or R package choices |
| `other/agent_context/context_files/report_structure.md` | Working on the Quarto book, parameter chapters, or adding new pages |
| `other/agent_context/context_files/parameters_and_links.md` | You need the full parameter list, governance document index, or external links |
| `other/agent_context/context_files/lessons.md` | Session start — small file, cheap to check for standing gotchas |
| `other/agent_context/agent_context.qmd` | The user tells you to read it, or references new instructions they've added there. User-authored; do not edit unless explicitly asked |
| `other/agent_context/session_log.md` | You need history on a completed task, a resolved issue, or a past session's detail (recent entries, from 2026-07-13) |
| `other/agent_context/session_log_archive.md` | You need history predating 2026-07-13 (original appendix_a.qmd audit, pipeline architecture rationale, CDX delete workflow reference, HMW architecture detail) |
| `other/documents/sample_fraction_correction_handoff.md` | CDX fraction correction handoff notes for the qaqc repo |
| `other/documents/md/` | Reading any governance/reference document (QAPP, CALM, 18 AAC 70, etc.) — see `parameters_and_links.md` for the full table |

`other/agent_context/archive/` holds superseded one-off notes (old interview transcripts, scratch to-do files) kept for reference only — do not treat as current.

------------------------------------------------------------------------

## Architecture Change In Progress (started 2026-10-14)

A migration to a per-year-repo architecture is underway; see `.posit/assistant/plans/2026-10-14-1300-per-year-repo-architecture-migration.md` for the full plan and resolved decisions. Summary: `kenai-river-wqx-qaqc` will eventually be archived. A new template repo, `kenai-river-baseline-wqx-template`, will replace it; each data year will get its own standalone repo (`kenai-river-wqx-{year}`) created from that template on demand, handling ingestion, QA/QC, CDX export, and a preliminary annual report together. **Phase 0 and Phase 1 are complete.** Phase 0 (housekeeping) is pushed. Phase 1 (git history cleanup): this repo's `.git` shrank from 1.48 GB to 224 MB via `git filter-repo` (stripped `docs/**`, `*_cache/`, `*_files/` render artifacts from all history); `docs/` is now gitignored on `main` and published via `quarto publish gh-pages` to a dedicated `gh-pages` branch instead (GitHub Pages source updated accordingly, confirmed live). The qaqc repo's equivalent bloat (`docs/2023.html` etc., ~460 MB total) was found but a history rewrite there was explicitly skipped by user decision — too small and too close to eventual archival to be worth the risk; a separate unrelated `.gitignore` bug (committed merge-conflict markers) was fixed there in passing. **Phase 2 (build `kenai-river-baseline-wqx-template` and validate with 2025) is next.** Check the plan file for full detail before starting new work on this.

------------------------------------------------------------------------

## Publishing the Book

The live site (https://kenai-watershed-forum.github.io/kenai-river-wqx/) is served from the `gh-pages` branch, **not** rebuilt automatically from pushes to `main`. Rendered HTML (`docs/`) is gitignored and never committed to `main`. To refresh the published site after merging changes to `main`:

``` bash
quarto publish gh-pages
```

This renders the full book and pushes the output to `gh-pages`, which GitHub Pages serves directly (source setting: "gh-pages /(root)", switched from the old "main /docs" during the Phase 1 git-history cleanup below). Run a plain `quarto render` first if you want to catch render errors without touching the live site. Full detail: `README.md`, "Rendering the Report" section.

------------------------------------------------------------------------

## Data Sourcing: Live WQP Cache (since 2026-10-16)

`chapters/data_sourcing.qmd` no longer reads static downloads. It reads only a local, gitignored cache in `other/input/WQX_downloads/wqp_live/` (`narrowresult.csv`, `station.csv`, `detection_limits.csv`, `refresh_metadata.csv`), built from live WQP queries (org `KENAI_WQX`, via `dataRetrieval`). **Refreshes are manual:** run `functions/refresh_wqp_data.R` (a script, not a function) whenever new data is expected on WQP. A guard in `data_sourcing.qmd` stops the render with instructions if the cache is missing; the narrative shows the refresh date inline. Rendering never calls the WQP API. As of the 2026-10-16 refresh the cache had 15,605 narrowresult rows, 157 stations, and no 2014-2021 data. Plan: `.posit/assistant/plans/2026-10-16-1120-live-wqp-data-sourcing-for-data-sourcingqmd.md`.

Regenerated downstream outputs (`baseline_export_format.csv`, `2000_2021_sitenames.csv`, `baseline_outliers.csv`, `baseline_analysis_format.csv`) now reflect whatever WQP currently holds, so they can show large diffs after a refresh; this is expected.

------------------------------------------------------------------------

## Trend Analyses in Parameter Chapters (opt-in, pilot in progress, uncommitted as of 2026-10-16)

A chapter opts in by setting `trend_analysis <- TRUE` before knitting `templates/_parameter_chunk.Rmd`, which then knits `templates/_trend_chunk.Rmd`. Helpers are in `functions/trend_plots.R` (`load_trend_data()`, `trend_stats()`; `trend_plot_time()`/`trend_plot_space()` are retained but no longer used by the template). Only `parameters/zinc.qmd` is opted in so far.

**Current output** ("Trends over time and space" section): a dynamic one-paragraph intro with the data year range (computed from the data, not hardcoded), the "Trend tests by site" paragraph and table (Mann-Kendall per site x season, Sen slope, BH-adjusted p), and one interactive example figure (`ggplotly`, RM 1.5 and Russian River, facet_grid season x site, LOESS on log10 scale, per-panel fit). The user rejected combined multi-site time-series plots (watershed and sampling design too heterogeneous) and asked that **no interpretive text be written yet**. Non-detects are excluded from fits and tests. `zinc.qmd` still has a redundant `zn_year_max` sentence that duplicates the template's year-range text (not yet removed).

**Open methodological decision (user is settling one approach for all chapters):** whether to keep Benjamini-Hochberg across a parameter's tests. Findings from 2026-10-16: for zinc, 1 of 42 tests is nominally significant (Beaver Creek spring, p = 0.023) versus about 2.1 expected by chance; 0 after BH or Holm. Power is very low (median within-site scatter 0.51 log10 units; Mann-Kendall power of about 8-15% even for 5-10%/yr trends at n = 8-12), so "0 of N significant" is not evidence of no trend. Proposed standard (awaiting user decision): report unadjusted p plus Sen slope with CI (units and %/yr on log scale), BH q as a secondary column, and nominal hits versus expected by chance. Other unresolved issues before rolling out: censored methods for non-detects (`NADA2::cenken`/ATS) for high-ND parameters, detection-limit and method changes, flow and hardness confounding, and gaps once 2014-2025 data reach WQP.

------------------------------------------------------------------------

## Next Session Priorities

**EPA WQX sync — `results_activities.csv` upload to CDX succeeded 2026-10-14.** The WQX Web config rebuild (Uid 9481, `KWF_Results_Baseline_Template_v2`) is complete and working, and all 830 rows of `results_activities.csv` imported cleanly after fixing both a config-level issue and a full set of data-level validation errors. `project.csv`/`station.csv` were already uploaded successfully 2026-09-28. **Full root-cause narrative, the config fix, and the complete list of data-level fixes (characteristic name mapping, Method Speciation, container color, detection limits, the Unicode-hyphen longitude bug fixed at its pipeline source, etc.): see `other/agent_context/session_log.md`, Session Entry (2026-10-14).**

**CDX submission confirmed; WQP not yet showing the data.** The user confirmed Step 3 ("submit to CDX") was completed 2026-10-14. A live `dataRetrieval::readWQPdata()` check the same day found 0 rows for 2021 under `KENAI_WQX` (sanity-checked against 2013, which correctly returned 382 rows), so the dataset has not yet propagated to the public Water Quality Portal. This is an expected indexing lag, not a known problem. **See `other/agent_context/session_log.md`, Session Entry (2026-10-14, later) for detail.**

Remaining steps at a glance: (1) recheck WQP visibility for 2021 data (run `functions/refresh_wqp_data.R`, then check the cache's `narrowresult.csv` for 2021 rows; the 2026-10-16 refresh still had none); once rows appear, verify record counts/corrected fields against what was uploaded, then assess whether `chapters/data_sourcing.qmd` or the book render need adjustment for 2021 (per `other/agent_context/agent_context.qmd`, "10/14/2026 follow up" note); (2) update `chapters/cdx_upload.qmd` with the corrected walkthrough (config fix + the "Ignore Column" pattern) now that the process is proven end-to-end; (3) download and commit the finished config (Uid 9481) to the repo for version control.

1.  **Recheck WQP visibility for 2021 data (start here)** — see the item above.
2.  **Trend analysis decision and rollout** — settle the multiplicity/statistical approach (see "Trend Analyses" section), update the trend table and methods text accordingly, remove the redundant `zn_year_max` sentence in `zinc.qmd`, then commit the trend files and roll out to other chapters (lead next, per prior discussion).
3.  **Task 18 (CORRECTED — largely done, not "Pending")** — The qaqc repo (`kenai-river-wqx-qaqc`, local clone `D:\GitHub\kenai-river-wqx-qaqc`) already has `project_year/{2014-2025}.qmd` scaffolded, with `2025.qmd` a largely complete working pipeline (Parts A-D) and `2023.qmd` a partially-ported skeleton (chunks `eval: false`). This repo's prior "Pending" status for Task 18/19 was stale. Going forward this work feeds the per-year-repo migration above, not further edits to the qaqc repo's existing per-year-QMD structure.
4.  **Task 1c (HIGH)** — CALM 5-year window sample count check. Count `result_status_identifier == "Accepted"` results per parameter + site for `activity_start_date >= 2017-01-01`. Source: `other/output/wqx_formatted/intermediate/2021_export_data_flagged.csv`. Flag combinations below 10 (or 5 for toxics) — these fall to ADEC Screening Level. Consider sharing with ADEC.
5.  **Task 1b (HIGH)** — Audit all distinct `CharacteristicName` values in WQP for org `KENAI_WQX`. Cross-reference against current WQX domain list. Map variants to canonical names. Feeds next CDX re-upload.
6.  **Task 5 (Medium)** — Verify `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx`. Six codes: `fw_acute`, `fw_chronic`, `harvest_aquatic_life`, `noncarc_aquatic_org`, `noncarc_water`, `secondary_water_recreation`. Confirm against 18 AAC 70 and USEPA criteria docs; set `review_needed = N`.
7.  **Task 5a (Medium)** — Add CALM methodology notes to FC, turbidity, and BTEX chapter narratives noting each is excluded from the standard CALM binomial methodology. Links at https://dec.alaska.gov/water/water-quality/integrated-report/

------------------------------------------------------------------------

## Active Tasks

See `other/agent_context/session_log.md` for full context on any task.

| \# | Priority | Description | Status |
|----|----|----|----|
| 1a-reupload | HIGH | Upload corrected 2021 files to CDX: `results_activities.csv`, `project.csv`, `station.csv`. `project.csv`/`station.csv` submitted successfully 2026-09-28. `results_activities.csv` imported successfully 2026-10-14 via the rebuilt config (Uid 9481) after fixing all data-level validation errors. Step 3 CDX submission confirmed by user 2026-10-14; WQP public visibility not yet confirmed (0 rows via live query same day; expected indexing lag). | **CDX submitted** — recheck WQP visibility next session |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP. Includes correcting WQP result 56514557 (detection limit stored as "O.18" with a letter O). | Pending |
| T1 | Medium | Opt-in trend analyses in parameter chapters (zinc pilot). Decide BH vs unadjusted approach, then roll out. | **In progress**, uncommitted |
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
| 18 | HIGH | Create `templates/pipeline_template.qmd` in qaqc repo — canonical single-QMD pipeline. See `context_files/conventions.md`. | **Superseded** — qaqc repo already has `project_year/{2014-2025}.qmd`; `2025.qmd` is largely complete. See Architecture Change In Progress note above. |
| 19 | Medium | Build `2023.qmd` in qaqc repo from template; adapt Part A for 2023 EDD quirks. See `session_log_archive.md` for 2023-specific notes. | **In progress in qaqc repo** — `2023.qmd` skeleton rebuilt to 2025 template, chunks `eval: false`, not yet ported |
| 12 | Low | Address historical CDX corrections (e.g., spring 2013 specific conductance) in qaqc repo | Pending |
| 13 | Low | Move `wqx_corrections.qmd` to qaqc repo | Pending |
| 14 | Low | Add inline tables alongside calculated-result download links in `appendix_a.qmd` | Pending |
| 15 | Low | Dynamically generate numerical values in parameter chapter prose via inline R | Pending |
| 16 | Low | Parameter chapter review workflow — post-2025 data integration | Pending |
| 17 | Low | Add multi-year duplicate RPD summary table to `data_qa_qc.qmd` | Pending |

------------------------------------------------------------------------

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash; use different punctuation instead.
- **Line endings:** `.qmd`, `.Rmd`, and `AGENTS.md` use CRLF, which breaks the `edit` tool's exact-string matching on multi-line strings. Use a Python read/replace/write that normalizes and restores CRLF for those files.

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

- **WQX/STORET sync (RESOLVED):** the 835 orphaned 2021 records are confirmed deleted from WQP (0 rows via live query, last checked 2026-09-28). `project.csv`/`station.csv` re-uploaded successfully 2026-09-28. `results_activities.csv` imported successfully to WQX Web 2026-10-14 (all 830 rows, zero validation errors) and Step 3 CDX submission confirmed the same day — see Task 1a-reupload. Corrected 2021 data is not yet publicly visible in WQP as of 2026-10-14 (0 rows via live query); this is an expected post-submission indexing lag, recheck in a future session.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years. Full audit needed (Task 1b).
- **Sample fraction inconsistency (RESOLVED):** dissolved metals correctly `"Dissolved"` locally (verified 2026-09-28) and reflected in the `results_activities.csv` imported to WQX Web 2026-10-14.
- **Non-numeric detection limit in WQP:** result 56514557 has a limit stored as "O.18" (letter O). `functions/trend_plots.R` coerces it to NA, so that non-detect is dropped from trend analyses. Correct at the source (see Task 1b).
- Full list including Turbidity, hydrocarbon, ALS duplicate, TSS, and spring-2013 specific-conductance issues: `context_files/known_issues.md`.

------------------------------------------------------------------------

## Governance Documents

Original PDFs are in `other/agent_context/`; text-extracted `.md` versions (preferred for AI ingestion) are in `other/documents/md/`. Full document index: `context_files/parameters_and_links.md`.
