# Kenai River Baseline Water Quality Monitoring — Project Context for Posit Assistant

## Next Session — Start Here

**Goal:** Revise, complete, and solidify `appendix_a.qmd` as a clean, well-documented pipeline for: (1) intake raw lab data → (2) apply QA/QC → (3) flag data → (4) format for EPA CDX upload. Once stable, extract the logic into sourced `.R` scripts so it can be reused for each year in the separate qaqc project.

### Completed this session (March 25, 2026)

-   ~~Inspect and correct the Ca/Mg/Fe unit errors in the 2021 data.~~ **DONE.** Fix applied in the SGS ingestion block of `appendix_a.qmd`. See Known Data Issues for full details.

### Completed this session (March 26, 2026)

-   ~~**Re-render `appendix_a.qmd`**~~ **DONE.** Render completes successfully. Two bugs fixed in the process:

    1.  The main WQX column-formatting/write chunk (lines \~1132–1249) had `eval = F` — changed to `eval = T`.
    2.  Enabling that chunk caused downstream QA/QC chunks to break (they expected raw column names like `analyte`, `collect_date`). Fixed by saving `dat_raw <- dat` before the WQX mutate and restoring `dat <- dat_raw` afterward, so the formatted copy is written to CSV while raw `dat` is preserved for QA/QC.

    -   Spot-check confirmed: Ca (11.8–39.1 mg/L), Fe (0.257–1.020 mg/L), Mg (0.919–68.6 mg/L) — all correct, units mg/L.

-   ~~**Replace AQWMS/AWQWMS terminology**~~ **DONE (March 2026).** All references replaced with `wqx_formatted`/`wqx_templates`. See Terminology section for full list of renamed files and paths.

### Completed this session (March 27, 2026)

-   ~~**Fix 11 results with missing `monitoring_location_id`**~~ **DONE.** Root cause: a typo in `sgs_site_names_matching_table_manual_edit.xlsx`. The Part C transformations in `appendix_a.qmd` (lines \~327–346) convert raw sample names from hyphen+space format to underscore format — `"RM1.5-Kenai City Dock"` → `"RM1.5_Kenai_City_Dock"`. All other sites had a matching entry in the table without a leading underscore (e.g., `RM74_Russian_River`), but the RM 1.5 entry was typed as `RM_1.5_Kenai_City_Dock` (extra underscore after `RM`), causing the left join to return NA. Fix: added one row to `sgs_site_names_matching_table_manual_edit.xlsx` with `sgs_sitenames = RM1.5_Kenai_City_Dock`, `Monitoring Location ID = KR RM 1.5`. All spring and summer SGS site names now resolve with zero NAs. Re-render completed successfully. See Known Data Issues section (updated below).

### Completed this session (March 28, 2026)

-   ~~**Integrate project.csv and station.csv generation into `appendix_a.qmd`**~~ **DONE.** The previously commented-out block (lines \~2691–2708) now runs automatically and applies all CDX-required transformations in code (no manual editing needed). Deleted `other/output/wqx_formatted/final_preparation/` folder — its contents are now fully auto-generated.

-   ~~**Clean up redundant output files in `wqx_formatted/`**~~ **DONE.** Deleted stale `2021_kwf_baseline_results_aqwms.csv` (old AQWMS-era name, pre-fix errors, not referenced in pipeline). Moved two pipeline intermediates to `other/output/wqx_formatted/intermediate/`. Updated all 18 path references in `appendix_a.qmd`. See Output File Structure below.

-   ~~**Fix `_quarto.yml` corruption**~~ **DONE.** R code was accidentally injected into the chapters list, and `copper.qmd`, `lead.qmd`, and `zinc.qmd` were missing from the book. Restored all three chapter entries correctly.

-   ~~**Add per-chapter regulatory threshold tables**~~ **DONE.** Created `functions/threshold_table.R` with a `show_threshold_table(characteristic)` function. For static thresholds it reads `other/output/regulatory_values/all_reg_vals.csv`; for hardness-dependent metals (Cd, Cr, Cu, Pb, Zn) it computes the min/max range of calculated values from `other/input/regulatory_limits/formatted_reg_vals/calculated_metals_reg_vals.csv`. Table columns: Standard Type, Value, Unit, Regulatory Authority. Returns silently for parameters with no defined thresholds. Added `source("functions/threshold_table.R")` + `show_threshold_table(characteristic)` to all 19 active parameter chapters, placed after the CSV download link. Regulatory authority mapping is hard-coded in `threshold_table.R` lines \~32–41 — **needs user verification before final render.**

-   ~~**Restore out-of-range threshold legend entries**~~ **DONE.** Removed the `hline_visible` filter from `static_boxplot_function.R`. All threshold lines now appear in the legend regardless of whether they fall within the visible y range. Companion threshold table provides the numerical values.

-   ~~**Code quality: replace `!!!` / `dplyr::recode()` in `threshold_table.R`**~~ **DONE.** Replaced with plain named-vector lookups using `dplyr::coalesce()`. No metaprogramming, no deprecated functions.

-   ~~**Tooltip: prefix "RM" to mainstem river mile labels**~~ **DONE.** Tooltip bold header now shows e.g. "RM 1.5" instead of bare "1.5" for mainstem sites. One-line change in `static_boxplot_function.R`.

-   ~~**Enable DOCX render**~~ **DONE.** Uncommented `docx:` format block in `_quarto.yml` and fixed a YAML indentation error (stray `output-dir: docs` line). Fixed all DOCX-incompatible HTML-only features: (1) created `functions/embed_if_html.R` helper and replaced all 32 `xfun::embed_file()` calls in `appendix_a.qmd` plus calls in `table_download.R` and `reg_limits.qmd`; (2) wrapped two `leaflet` maps in `appendix_a.qmd` and one `ggplotly` in `appendix_b.qmd` with `if (knitr::is_html_output())` conditionals. Root bug: `rm(list=ls())` at line \~76 of `appendix_a.qmd` cleared the function from the environment; fixed by adding `source("functions/embed_if_html.R")` immediately after the `rm()`. Also added `cache: false` to `appendix_a.qmd` YAML execute block to prevent stale HTML cache from masking the issue in future renders. Run with `quarto render --to docx`.

-   **Boxplot whisker display (decision logged):** Long whiskers on Chromium and Copper are intentional — they reflect genuine right-skewed distributions at RM 1.5 and RM 6.5 (most developed lower-river sites, n=21 and n=22 observations respectively). Display left as-is; wide spread at lower-river sites should be noted in chapter narratives.

-   ~~**Fix `appendix_b.qmd` DOCX render failure**~~ **DONE.** Root cause: `kable(format = "html")` + `kable_styling()` in the summary table chunk (lines \~192–207) produced HTML output unconditionally — not guarded by an HTML check. Fixed by wrapping in `if (knitr::is_html_output())` with a plain `knitr::kable()` fallback for DOCX.

-   ~~**Fix site count discrepancy (21 vs 22)**~~ **DONE.** `README.md` line 57 comment said "21 sites"; `AGENTS.md` said "21 sites: 13 mainstem + 8 tributaries." Both corrected to 22 sites: 13 mainstem + 9 tributaries (verified from `baseline_sites.csv`).

-   ~~**Set up Quarto render profile dropdown**~~ **DONE.** Created `_quarto-html.yml` and `_quarto-docx.yml` profile files. Moved `format:` blocks out of `_quarto.yml` into the respective profile files. Added `profile: default: html / group: [[html, docx]]` to `_quarto.yml`. RStudio now shows "html" and "docx" as selectable profiles in the Render button dropdown. Default profile is HTML.

-   ~~**Fix code print leakage in `data_sourcing.qmd`**~~ **DONE.** Two root causes: (1) YAML front matter (including `execute: echo: false` and `date: "\`r Sys.Date()\`"`) was placed *after* the`\# Data Sourcing`heading on line 1, so Quarto did not parse it as document front matter — the date expression appeared as raw text in DOCX. Fixed by moving YAML to the top of the file. (2) The`knitr::knit_exit()`chunk had no`echo = FALSE`, so the source code could appear in DOCX. Fixed by adding`echo = FALSE\` to that chunk.

-   ~~**Integrate 2016 report site descriptions into `study_area.qmd`**~~ **DONE.** Converted the 2016 Baseline Water Quality Assessment DOCX to markdown using pandoc and extracted 22 embedded site photos. Copied all photos to `other/documents/site_photos/` with descriptive filenames. Rewrote the "Sampling Site Descriptions" section of `study_area.qmd` with text, coordinates, photo, and caption for all 13 mainstem sites (RM 82 through RM 1.5) and all 9 tributary sites, following the 2016 report order (mainstem first). Added a note that descriptions and photos are from the 2016 report and may be updated.

-   ~~**DOCX formatting: suppress all code output globally**~~ **DONE.** Added `execute: echo: false / warning: false / message: false` to `_quarto-docx.yml`. Applies across all chapters during DOCX rendering regardless of per-chapter YAML settings.

-   ~~**DOCX formatting: chapter page breaks**~~ **DONE.** Created `filters/pagebreak-h1.lua` — a Lua filter that inserts an OpenXML `<w:br w:type="page"/>` element before every H1 heading. Guarded by `if FORMAT == "docx"` so it has no effect on HTML rendering. Referenced from `_quarto-docx.yml` under `filters:`.

-   ~~**Separate HTML and DOCX output directories**~~ **DONE.** Added `project: output-dir: docs-docx` to `_quarto-docx.yml`. HTML profile continues to render to `docs/` (for GitHub Pages); DOCX profile now renders to `docs-docx/`. The two formats no longer interfere on successive renders. Removed `downloads: [docx]` from `_quarto.yml` book section — that sidebar link pointed to a DOCX inside `docs/`, which is no longer the case.

-   **DOCX formatting: boxplot point/text sizing — PARTIALLY DONE, needs verification.** Added conditional size variables to `create_facet_plots` in `static_boxplot_function.R` (point size 1.5 → 3.0, facet strip text 16 → 22, axis labels 14–16 → 18–20, legend text default → 14). The DOCX detection originally used `isTRUE(knitr::pandoc_to("docx"))`, which may not fire in Quarto's rendering pipeline. Updated to `identical(Sys.getenv("QUARTO_PROFILE"), "docx") || isTRUE(knitr::pandoc_to("docx"))`. **Verify that sizes are now correct in DOCX at start of next session.**

### Tasks for next session (in order)

1.  **Verify boxplot DOCX sizing fix.** Render DOCX and confirm that jitter points, facet strip text, and legend text are visibly larger than in HTML output. The fix is in `functions/static_boxplot_function.R` — the `is_docx` detection now uses `Sys.getenv("QUARTO_PROFILE") == "docx"` as the primary check. If still not working, add `cat(Sys.getenv("QUARTO_PROFILE"), "\n")` in a temporary test chunk to confirm the environment variable is set during rendering.

2.  **Re-upload 2021 data to EPA CDX** (IN PROGRESS — files are ready, manual upload on CDX website required). Upload all three files from `other/output/wqx_formatted/`: `results_activities.csv`, `project.csv`, `station.csv`. Also upload the QAPP PDF attachment referenced in `project.csv`: `KenaiWatershedForum_QAPP_v3_2023_with_Addendum_April_2024.pdf`. After uploading, verify data visibility in [How's My Waterway](https://mywaterway.epa.gov/) as an end-to-end check.

3.  **Verify regulatory authority column in threshold tables.** The USEPA/ADEC mapping in `functions/threshold_table.R` lines \~32–41 is hard-coded — confirm assignments are correct before final render.

4.  **Resolve ALS lab duplicate (DUP) status issue.** Four 2021 results have DUP status unexpectedly assigned in the ALS data — it appears ALS ran these as lab duplicates for Total Ca/Fe/Mg (method 200.7), which may not have been requested or covered by the QAPP. Sites: KR RM 22 SOC 2021-05-11; KR RM 1.5 2021-07-27; KR RM 23 2021-05-11; KR RM 70 2021-07-27. **Investigated March 28, 2026:** The DUP1 records are silently dropped by the pipeline — only primary sample values (Ca, Fe, Mg) appear in the export as `Field Msr/Obs` with correct single values. The exported data is not incorrect. What is missing is the QA/QC value of those lab duplicates: RPDs were never computed for them. **This does not block the CDX upload.** Outstanding questions for a future QA/QC session: (a) were lab duplicates requested/covered by QAPP? (b) should lab duplicate RPDs be computed and reported? Note: in SGS, field duplicates are labeled "PS" in `sample_type`; in ALS, lab duplicates are labeled "DUP1" in `sample_type`. Flagged originally March 2022.

5.  **LOQ logic flow chart for ADEC.** Draw a logic flow chart for the 2-step winnowing process for LOQ and 2× LOQ criteria and send to ADEC staff to confirm interpretation. Flagged June 2023 — unclear if completed.

6.  **Make `appendix_a.qmd` year-neutral** (lower priority). The document has hardcoded references to "2021" throughout — in file paths, variable names, narrative text, and chunk labels (e.g., `{r, 2021 WQX formatting for SGS, ...}`). Goal: replace with a `year` variable set once at the top so the script can be reused for any year without find-and-replace edits. Closely related to task 6 (extract ingestion logic to `.R` scripts); both tasks should be done together.

7.  **Extract ingestion logic to `.R` scripts** (lower priority — see File Structure section below).

8.  **Address historical CDX data corrections** (e.g., spring 2013 specific conductance) in a dedicated chapter of the `kenai-river-wqx-qaqc` repo — not in `appendix_a.qmd`.

9.  **Move `wqx_corrections.qmd` to `kenai-river-wqx-qaqc` repo.** This file contains corrections to previously uploaded CDX data (e.g., historical unit errors) and belongs in the QA/QC repo, not this report repo. It is currently a loose file in the project root and is not included in the book chapter list. Move the file and its associated data to `kenai-river-wqx-qaqc`, then delete it from this repo.

10. **Add inline tables alongside all calculated-result download links** (lower priority — polish, does not affect data quality or CDX submission). Currently, \~10 places in `appendix_a.qmd` offer downloadable CSVs with no inline table, which will render as broken download-only links in PDF output. Use the pattern below for dual HTML/PDF output. Raw source files (PDFs, XLSs, JPGs from labs) do not need tables — download-only is appropriate for those. Calculated CSVs that need tables:

    -   `planned_actual_analyses_2021.csv` (planned vs. actual analyses)
    -   `sample_holding_times.csv` (max holding times by sample type)
    -   `holding_time_calcs.csv` (spring/summer 2021 holding time calcs)
    -   Ca, Fe, Mg total vs. dissolved CSVs (3 files)
    -   `rpd_check_dat.csv` (duplicate RPD values)
    -   `matrix_spike_recovery_fails_2021.csv` (matrix spike failures)
    -   CMA outputs: `cma_parameter.csv`, `cma_site.csv`, `cma_project.csv`
    -   CMB outputs: `cmb_parameter.csv`, `cmb_site.csv`, `cmb_project.csv`
    -   Observation count by characteristic CSV (Q39 area)
    -   Skip inline table for the large WQX-formatted results file (`2021_kwf_baseline_results_wqx.csv`) — too many rows to display usefully.

    **Pattern for dual HTML/PDF tables:**

    ``` r
    if (knitr::is_html_output()) {
      DT::datatable(df)
    } else {
      knitr::kable(df)
    }
    ```

------------------------------------------------------------------------

## Decisions Made — March 2026

### EPA WQX Flagging Convention (settled)

The existing flagging approach in `appendix_a.qmd` is already correct and aligns with WQX conventions. Do not change the flagging design. Specifically:

-   **`Result Qualifier`** column: Lab qualifiers from the laboratory (`U` = non-detect, `J` = below LOQ, `=` = detected). These come from the lab EDD files and are mapped in the data ingestion step.
-   **`Result Status ID`** column: KWF's QA/QC decision. Values are `Accepted` or `Rejected`. This is the standard WQX field for flagging data that does not meet project QA/QC standards (e.g., RPD failures, matrix spike failures). The binary internal `flag` (Y/N) in `2021_data_flag_decisions.csv` maps to this field at the CDX export step.
-   The `FQC` qualifier code idea from the To Do notes is **not needed** and should not be implemented. The `Result Status ID = Rejected` convention is sufficient and standard.

### File Structure (decided but not yet implemented)

Extract processing logic from `appendix_a.qmd` into sourced `.R` scripts. This makes each step independently testable and allows the qaqc repo to reuse the same scripts with year-specific inputs. `appendix_a.qmd` should become narrative prose + `source()` calls + interpretation.

Planned script breakdown:

| Script | Content |
|----|----|
| `R/ingest_sgs_als.R` | SGS EDD + ALS CSV read-in, column normalization, site name mapping, method code mapping (Parts A–E of current appendix) |
| `R/ingest_fc.R` | SWWTP + Taurianen fecal coliform read-in |
| `R/ingest_tss.R` | SWWTP TSS read-in |
| `R/format_wqx.R` | Lookup joins (lat/long, sample fraction, detection condition, container type, preservative), WQX column renaming, Activity ID construction |
| `R/apply_qaqc_flags.R` | Holding time calcs, RPD calcs, completeness measures, flag application, CDX export |

Each script should accept year-specific inputs (file paths, sample dates) as arguments or via a config object so they work for multiple years.

------------------------------------------------------------------------

## appendix_a.qmd Audit Results (March 2026)

Full audit was completed. Section-by-section status. **Note: line numbers are approximate and have shifted with each editing session. As of March 26, 2026 edits, add \~15 lines to the original estimates.**

| Approx. Lines | Section | Status |
|----|----|----|
| 1–518 | SGS/ALS ingestion (Parts A–E) | Working. Includes Ca/Mg/Fe unit correction (March 2026). Dense — priority candidate for extraction to `R/ingest_sgs_als.R`. |
| 520–983 | FC and TSS ingestion | Mostly working. TSS has a documented QA gap: SWWTP did not report lab QA results (blanks, duplicates, check standards) as required by QAPP in 2021/2022. Lab QA export block is commented out. |
| 985–1260 | Lookup joins + first WQX export | Working (March 26, 2026). The write chunk was `eval = F` and is now `eval = T`. A `dat_raw`/`dat` save-restore pattern was added so the WQX CSV is written without corrupting the raw-column `dat` used by downstream QA/QC. The full column-rename block is redundantly repeated later for the CDX export — consolidate in the script extraction step. |
| 1268–2410 | QA/QC Checklist (Questions 1–42) | Mostly complete. Two gaps: (1) Completeness Measure B block is `eval = F` and produces nonsensical results — unfinished (\~line 2141). (2) Q39 (range check / outliers) defers to visual review in report chapters rather than doing anything here. |
| 2411–2533 | Flagging and flagged export | Working. Ca/Mg/Fe unit error is now corrected upstream at ingestion (March 2026); this section no longer needs a workaround. |
| 2535–2680 | CDX export (Results and Activities) | Working but nearly duplicates the format block at lines 985–1243. Consolidate. |
| 2705–2786 | Post-hoc corrections + `knitr::knit_exit()` | Incomplete placeholder. The chunk is `eval = F` and never executes. The `knitr::knit_exit()` near the end of the file stops rendering prematurely. |

------------------------------------------------------------------------

## Project Overview

The **Kenai River Baseline Water Quality Monitoring Project** is a long-term cooperative monitoring initiative led by the Kenai Watershed Forum (KWF) in south-central Alaska. Biannual (spring and summer) sampling has occurred at 21 sites since 2000. The current deliverable is a comprehensive Quarto-based report covering 2000–2021 data.

-   **Project home page:** https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/ *(inspect annually for updated context)*
-   **GitHub:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
-   **QA/QC repo (separate):** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
-   **Public data:** https://www.waterqualitydata.us/ (organization: "Kenai Watershed Forum")

### Intended Audience

This project serves multiple audiences simultaneously:

-   **Regulatory agencies** — primarily ADEC, which draws data from EPA CDX every two years for its [Integrated Report](https://dec.alaska.gov/water/water-quality/integrated-report/). That report makes determination decisions about which water bodies may need management action. This is the most immediate real-world use of KWF's submitted data.
-   **KWF scientists** tracking long-term trends
-   **General public** and local stakeholders
-   **Funding partners** — a cohort of local agencies funds the laboratory work. They want assurance that funds are being spent wisely. Communicate with them regularly; this is a project priority that is not obvious from the code itself.

The report format is modeled on previously completed comprehensive reports from **2007 and 2016** (PDFs in `other/agent_context/`).

### QA/QC Decision Authority

KWF staff scientists have final say on all QA/QC flag decisions (Accepted vs. Rejected). All decisions must be thoroughly documented and transparent. Labs, ADEC, and EPA Region 10 do not make flagging decisions, but ADEC is the primary downstream consumer of flagged data. If ADEC disagrees with KWF's QA/QC decisions, they may choose to exclude the data from analyses in the Integrated Report — making thorough documentation of all flag decisions especially important.

### Long-Term Vision and Intended Use

This codebase is intended for **ongoing annual use**, not a one-time archive. The workflow is:

1.  New annual data is processed each year in the **separate qaqc repo** (`kenai-river-wqx-qaqc`), using `appendix_a.qmd` as the template/example.
2.  Processed, QA/QC'd data is submitted to **EPA CDX**.
3.  This main report repo downloads data directly from EPA CDX on render, automatically integrating new years.

`appendix_a.qmd` is the **example pipeline for 2021** — once it is stable and correct, its logic is extracted to `.R` scripts reusable across years in the qaqc repo.

A "complete" version of this report can: (a) access data from EPA CDX, and (b) automatically integrate it into the report on render. The Quarto book renders to both **HTML** (hosted on GitHub Pages) and **PDF**, with the PDF accessible/downloadable by readers.

Each parameter chapter should contain **inline R code** for summary statistics so values update automatically on re-render.

**A primary goal of this project is that all KWF water quality data is publicly visible in EPA's [How's My Waterway](https://mywaterway.epa.gov/) web app.** This requires that data be correctly formatted and successfully submitted to EPA CDX/WQX. Verifying visibility in How's My Waterway is a meaningful end-to-end check that the submission pipeline worked correctly.

### Known Scientific Context

-   The **lower Kenai River area** is experiencing faster-paced development and use. Parameters of concern are more likely to show trends there.
-   **Climate change** may be driving trends in turbidity, temperature, and related field parameters — worth watching.

### Future Flexibility

-   Future years may involve different or additional partner laboratories, or external partners for QA/QC review.
-   Code must be written flexibly enough to accommodate new labs and remain clearly documented for future scientists.

## Collaboration Standards (Important)

This is a 25+ year monitoring project. Code must remain readable and maintainable by future scientists.

-   **Before implementing any code, explain what the proposed code does and why.** Wait for user confirmation before applying changes.
-   Prioritize code clarity and readability over cleverness or brevity.
-   All analytical decisions should be transparent and documented.

## Report Structure

A Quarto book/website publishing to HTML and DOCX simultaneously. Key source files:

| File | Purpose |
|----|----|
| `_quarto.yml` | Project configuration |
| `index.qmd` | Front matter / introduction |
| `data_sourcing.qmd` | Data download and preparation pipeline |
| `data_qa_qc.qmd` | QA/QC overview |
| `reg_limits.qmd` | Regulatory limits framework |
| `appendix_a.qmd` | Detailed 2021 QA/QC example |
| `functions/static_boxplot_function.R` | Reusable boxplot function |

Parameter result chapters (one per water quality parameter) follow a consistent structure: boxplot, regulatory comparison, downloadable data table.

## Annual CDX Submission Steps

Each annual CDX submission requires three components uploaded together. This logic is currently stubbed out (commented) at the end of `appendix_a.qmd` and must be built into the annual pipeline in `kenai-river-wqx-qaqc`:

1.  **Results & Activities CSV** — the main QA/QC'd output (e.g., `2021_export_data_flagged.csv`)
2.  **Project CSV** — read from existing CDX download, update QAPP status to `Y` and add website URL before uploading
3.  **Station CSV** — read from existing CDX download, filter to KWF sites only (`grepl("KBL", MonitoringLocationName)`)

## Annual CDX Historical Corrections

Corrections to data from years other than the current submission year (e.g., errors discovered in previously uploaded data) should be addressed in a **dedicated chapter of the `kenai-river-wqx-qaqc` repo**, not in `appendix_a.qmd`. Known issues to address there:

-   **Spring 2013 specific conductance units:** Values are stored in CDX as `mS/cm` but should be `uS/cm`. Original field records confirm `uS/cm` is correct. This correction has never been applied. Reference Google Doc with full issue log: https://docs.google.com/document/d/1p1fsh2dVfvIWNmRrpjteOTyLPSEeMYoQDRk5XhKTcNk/edit?tab=t.0
-   **2021 MonitoringLocationName format mismatch:** String formats from 2021 do not match those from 2000–2013. Low priority.

## Data Pipeline Summary

1.  **Download** from EPA Water Quality Portal via `dataRetrieval` package (transitioning to TADA)
2.  **Filter** to KWF organization records
3.  **Harmonize** units (e.g., µg/L → mg/L for metals)
4.  **Handle non-detects** using half the MDL/MRL value
5.  **Classify season** by Julian day: ≤ 155 = Spring, \> 155 = Summer (boundary ≈ June 4)
6.  **Remove** air temperature observations and manually-identified outliers
7.  **QA/QC flagging** against QAPP Data Quality Objectives (DQOs)
8.  **Submit** cleaned data to EPA WQX via CDX

## Data Storage Structure

```         
other/
├── agent_context/         # Project governance docs (QAPP, MOU, funding proposal)
├── input/
│   ├── WQX_downloads/     # Downloaded EPA data (large files, excluded from GitHub)
│   ├── wqx_templates/     # WQX reference files, matching tables, lookup CSVs (formerly AQWMS/)
│   │   └── wqx_qaqc/      # QA/QC info spreadsheets (formerly aqwms_qaqc/)
│   ├── 2021_wqx_data/     # Raw lab results (SGS North America, Soldotna WWTP, Taurianen Engineering)
│   ├── outliers/          # Manually identified outliers spreadsheet
│   ├── regulatory_limits/ # Hardness-dependent thresholds
│   └── baseline_sites.csv # Site metadata (22 sites: 13 mainstem + 9 tributaries)
└── output/
    ├── wqx_formatted/             # CDX submission-ready files: results_activities.csv, project.csv, station.csv
    │   └── intermediate/          # Pipeline intermediates (not for upload): 2021_kwf_baseline_results_wqx.csv, 2021_export_data_flagged.csv
    ├── analysis_format/           # Processed data ready for analysis
    └── regulatory_values/         # Combined regulatory threshold files
```

**Raw data note:** Files in `other/input/` are untouched as received from the laboratory. Do not modify raw inputs; all transformations happen in code.

## Parameters Monitored

| Category | Parameters |
|----|----|
| Dissolved Metals | Arsenic, Cadmium, Chromium, Copper, Lead, Zinc |
| Total Metals | Calcium, Iron, Magnesium |
| Nutrients | Nitrate + Nitrite, Phosphorus |
| Hydrocarbons | BTEX (Benzene, Toluene, Ethylbenzene, m/p-Xylene, o-Xylene) |
| Biological | Fecal Coliform |
| Field Parameters | pH, Specific Conductance, TSS, Turbidity, Water Temperature, Dissolved Oxygen |

## Regulatory Limits

-   Standards are Alaska and Federal freshwater aquatic life criteria
-   Some metal thresholds (Cadmium, Chromium, Copper, Lead, Zinc) are **hardness-dependent** — threshold values are calculated per-sample based on measured hardness
-   Regulatory values are stored in `output/regulatory_values/`
-   Session variables `static_metals_thresholds`, `static_metals_reg_vals`, `diss_metals_hard_param` hold processed threshold data

## Terminology: AQWMS / AWQWMS

`AQWMS` (also written `AWQWMS` or `aqwms`) stands for "Ambient Water Quality Monitoring System," software sold by Gold Systems. KWF considered using it in 2021 but ultimately did not. **Completed March 2026:** All references have been replaced with `wqx_formatted` / `wqx_templates` as appropriate throughout `appendix_a.qmd` and on disk.

Renamed paths: - `other/output/aqwms_formatted_results/` → `other/output/wqx_formatted/` - `other/input/AQWMS/` → `other/input/wqx_templates/` - `other/input/AQWMS/aqwms_qaqc/` → `other/input/wqx_templates/wqx_qaqc/` - `other/documents/AQWMS_documents/` → `other/documents/wqx_documents/` - `AQWMS_template_matching_table.xlsx` → `wqx_template_matching_table.xlsx` - `aqwms_qaqc_info.xlsx` → `wqx_qaqc_info.xlsx` - Output file: `2021_kwf_baseline_results_aqwms.csv` → `2021_kwf_baseline_results_wqx.csv` - R variables: `aqwms_colnames` → `wqx_colnames`, `aqwms21_sitenames` → `wqx_sitenames`, `aqwms_analytical_methods` → `wqx_analytical_methods`

Note: The original vendor template file `AWQMS_KWF_Baseline_2021.xlsx` retains its name — it is a received file from Gold Systems and is kept as-is per the raw data policy.

## QA/QC Notes

-   **Flagging:** The correct WQX convention is already implemented. `Result Qualifier` = lab qualifiers (U, J, =). `Result Status ID` = KWF QA/QC decision (Accepted/Rejected). Do not add FQC or other custom codes — the existing design is complete and correct.
-   **Outliers:** Visually-identified outliers (especially pre-2014 legacy data lacking field blanks) are excluded from visualizations but retained in archived data.
-   **Lab qualifiers** (from labs) are distinct from **KWF QA/QC flags** — treat them separately.

## Known Data Issues (as of March 2026)

-   **11 results missing `monitoring_location_id`: RESOLVED (March 27, 2026).** Root cause was a typo in `sgs_site_names_matching_table_manual_edit.xlsx`: the RM 1.5 entry was `RM_1.5_Kenai_City_Dock` (extra underscore after `RM`) rather than `RM1.5_Kenai_City_Dock` (the form produced by the Part C transformations). Fix applied by adding one correctly-formatted row to the Excel file. All spring and summer SGS site names now resolve without NAs. Re-render completed successfully March 27, 2026; output CSVs are current and ready for CDX upload.

-   **Ca/Mg/Fe unit errors: RESOLVED (March 2026).** Root cause identified: the summer 2021 SGS EDD reported dissolved Calcium, Iron, and Magnesium (method EP200.8) with unit label `ug/L`, but the numeric values were on the mg/L scale (e.g., Ca = 16,200 "ug/L" at RM 0 NNC = 16.2 mg/L, which is physically plausible; 16.2 ug/L would be unrealistically low for freshwater). Spring 2021 SGS data contained no dissolved Ca/Fe/Mg runs (confirmed). ALS total metals were correctly labeled mg/L throughout. Fix applied in `appendix_a.qmd` at the SGS ingestion step (Part A): rows where `analyte %in% c("Calcium", "Iron", "Magnesium")` and `units == "ug/L"` are divided by 1000 and relabeled `mg/L`. Corrected hardness values for summer 2021 are 37–380 mg/L as CaCO3 (plausible); erroneous values were 37,000–380,000 mg/L.

-   **Output CSVs regenerated (March 27, 2026, clean).** Both `other/output/wqx_formatted/2021_kwf_baseline_results_wqx.csv` and `2021_export_data_flagged.csv` have been regenerated with corrected Ca/Mg/Fe values and correct RM 1.5 site IDs. **The 2021 EPA WQX upload was previously deleted and is ready to be re-submitted to CDX** — this is Task 1 for the next session.

-   **Hydrocarbon data** missing from 2025 WQP download (uploaded Jan 2024 but not appearing in download).

-   **Turbidity: one spurious `uS/cm` record** exists in the analysis dataset (`result_measure_measure_unit_code = "uS/cm"` for a Turbidity row — almost certainly a data entry error; uS/cm is a conductance unit). Needs to be identified and corrected at the source. Found during interactive plot testing, March 2026.

-   **Turbidity: anomalously high value at RM 1.5 spring** (\~3,200 NTU) warrants review during historical data outlier check. Flagged as a candidate outlier, March 2026.

-   **Completeness Measure B** calculation is complete and enabled. Project-wide CMB is 52.2% (below the 60% QAPP goal), primarily driven by dissolved metals being fully below LOQ and fecal coliform RPD failures.

-   **TSS lab QA gap:** SWWTP did not report required lab QA results for TSS in 2021/2022. Note in code, address for 2023+.

## Visualization Wish List

These are desired but not yet implemented features for the report figures:

-   **A) Interactive boxplots:** DONE (March 2026). Hover tooltips show site, date, value+units, fraction, lab, method, and QA/QC status. Implemented via `plotly::ggplotly()` in `static_boxplot_function.R`, with HTML/PDF conditional rendering in each parameter chapter.
-   **B) QA/QC toggle:** Figures should support two views — (1) data that passed QA/QC only, and (2) all data including failed QA/QC — switchable via a toggle. Symbology should differ between the two views.

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `magrittr`, `dataRetrieval`, `TADA`, `xfun`

Federal agency packages — `dataRetrieval`, `TADA`, and others — should be integrated as appropriate and as recommended by EPA/ADEC.

Note: `magrittr` pipe (`%>%`) appears in existing legacy code. For all new code, use the base pipe (`|>`). Do not mass-convert existing `%>%` usage — leave legacy files as-is to avoid unnecessary git churn and risk.

## Governance Documents (in `other/agent_context/`)

-   **QAPP** — Quality Assurance Project Plan, approved by ADEC and EPA Region 10 (2023 + April 2024 addendum)
-   **MOU** — Baseline Water Quality MOU 2025 Final
-   **Funding Proposal** — KWF 2024 BOR WaterSMART CWMP Proposal
-   **ADEC Water Quality Standards** — Alaska Dept of Environmental Conservation Water Quality Standards (18 AAC 70)
-   **DL/LOD/LOQ Interpretation** — SGS Laboratories document on detection limit terminology
