# Kenai River Baseline Water Quality Monitoring - Project Context for Posit Assistant

## Next Session - Start Here

**Goal:** Revise, complete, and solidify `appendix_a.qmd` as a clean, well-documented pipeline for: (1) intake raw lab data → (2) apply QA/QC → (3) flag data → (4) format for EPA CDX upload. Once stable, extract the logic into sourced `.R` scripts so it can be reused for each year in the separate qaqc project.

### Completed this session (March 25, 2026)

-   ~~Inspect and correct the Ca/Mg/Fe unit errors in the 2021 data.~~ **DONE.** Fix applied in the SGS ingestion block of `appendix_a.qmd`. See Known Data Issues for full details.

### Completed this session (March 26, 2026)

-   ~~**Re-render `appendix_a.qmd`**~~ **DONE.** Render completes successfully. Two bugs fixed in the process:

    1.  The main WQX column-formatting/write chunk (lines \~1132–1249) had `eval = F` - changed to `eval = T`.
    2.  Enabling that chunk caused downstream QA/QC chunks to break (they expected raw column names like `analyte`, `collect_date`). Fixed by saving `dat_raw <- dat` before the WQX mutate and restoring `dat <- dat_raw` afterward, so the formatted copy is written to CSV while raw `dat` is preserved for QA/QC.

    -   Spot-check confirmed: Ca (11.8–39.1 mg/L), Fe (0.257–1.020 mg/L), Mg (0.919–68.6 mg/L) - all correct, units mg/L.

-   ~~**Replace AQWMS/AWQWMS terminology**~~ **DONE (March 2026).** All references replaced with `wqx_formatted`/`wqx_templates`. See Terminology section for full list of renamed files and paths.

### Completed this session (March 27, 2026)

-   ~~**Fix 11 results with missing `monitoring_location_id`**~~ **DONE.** Root cause: a typo in `sgs_site_names_matching_table_manual_edit.xlsx`. The Part C transformations in `appendix_a.qmd` (lines \~327–346) convert raw sample names from hyphen+space format to underscore format - `"RM1.5-Kenai City Dock"` → `"RM1.5_Kenai_City_Dock"`. All other sites had a matching entry in the table without a leading underscore (e.g., `RM74_Russian_River`), but the RM 1.5 entry was typed as `RM_1.5_Kenai_City_Dock` (extra underscore after `RM`), causing the left join to return NA. Fix: added one row to `sgs_site_names_matching_table_manual_edit.xlsx` with `sgs_sitenames = RM1.5_Kenai_City_Dock`, `Monitoring Location ID = KR RM 1.5`. All spring and summer SGS site names now resolve with zero NAs. Re-render completed successfully. See Known Data Issues section (updated below).

### Completed this session (March 28, 2026)

-   ~~**Integrate project.csv and station.csv generation into `appendix_a.qmd`**~~ **DONE.** The previously commented-out block (lines \~2691–2708) now runs automatically and applies all CDX-required transformations in code (no manual editing needed). Deleted `other/output/wqx_formatted/final_preparation/` folder - its contents are now fully auto-generated.

-   ~~**Clean up redundant output files in `wqx_formatted/`**~~ **DONE.** Deleted stale `2021_kwf_baseline_results_aqwms.csv` (old AQWMS-era name, pre-fix errors, not referenced in pipeline). Moved two pipeline intermediates to `other/output/wqx_formatted/intermediate/`. Updated all 18 path references in `appendix_a.qmd`. See Output File Structure below.

-   ~~**Fix `_quarto.yml` corruption**~~ **DONE.** R code was accidentally injected into the chapters list, and `copper.qmd`, `lead.qmd`, and `zinc.qmd` were missing from the book. Restored all three chapter entries correctly.

-   ~~**Add per-chapter regulatory threshold tables**~~ **DONE.** Created `functions/threshold_table.R` with a `show_threshold_table(characteristic)` function. For static thresholds it reads `other/output/regulatory_values/all_reg_vals.csv`; for hardness-dependent metals (Cd, Cr, Cu, Pb, Zn) it computes the min/max range of calculated values from `other/input/regulatory_limits/formatted_reg_vals/calculated_metals_reg_vals.csv`. Table columns: Standard Type, Value, Unit, Regulatory Authority. Returns silently for parameters with no defined thresholds. Added `source("functions/threshold_table.R")` + `show_threshold_table(characteristic)` to all 19 active parameter chapters, placed after the CSV download link. Regulatory authority mapping is hard-coded in `threshold_table.R` lines \~32–41 - **needs user verification before final render.**

-   ~~**Restore out-of-range threshold legend entries**~~ **DONE.** Removed the `hline_visible` filter from `static_boxplot_function.R`. All threshold lines now appear in the legend regardless of whether they fall within the visible y range. Companion threshold table provides the numerical values.

-   ~~**Code quality: replace `!!!` / `dplyr::recode()` in `threshold_table.R`**~~ **DONE.** Replaced with plain named-vector lookups using `dplyr::coalesce()`. No metaprogramming, no deprecated functions.

-   ~~**Tooltip: prefix "RM" to mainstem river mile labels**~~ **DONE.** Tooltip bold header now shows e.g. "RM 1.5" instead of bare "1.5" for mainstem sites. One-line change in `static_boxplot_function.R`.

-   ~~**Enable DOCX render**~~ **DONE.** Uncommented `docx:` format block in `_quarto.yml` and fixed a YAML indentation error (stray `output-dir: docs` line). Fixed all DOCX-incompatible HTML-only features: (1) created `functions/embed_if_html.R` helper and replaced all 32 `xfun::embed_file()` calls in `appendix_a.qmd` plus calls in `table_download.R` and `reg_limits.qmd`; (2) wrapped two `leaflet` maps in `appendix_a.qmd` and one `ggplotly` in `appendix_b.qmd` with `if (knitr::is_html_output())` conditionals. Root bug: `rm(list=ls())` at line \~76 of `appendix_a.qmd` cleared the function from the environment; fixed by adding `source("functions/embed_if_html.R")` immediately after the `rm()`. Also added `cache: false` to `appendix_a.qmd` YAML execute block to prevent stale HTML cache from masking the issue in future renders. Run with `quarto render --to docx`.

-   **Boxplot whisker display (decision logged):** Long whiskers on Chromium and Copper are intentional - they reflect genuine right-skewed distributions at RM 1.5 and RM 6.5 (most developed lower-river sites, n=21 and n=22 observations respectively). Display left as-is; wide spread at lower-river sites should be noted in chapter narratives.

-   ~~**Fix `appendix_b.qmd` DOCX render failure**~~ **DONE.** Root cause: `kable(format = "html")` + `kable_styling()` in the summary table chunk (lines \~192–207) produced HTML output unconditionally - not guarded by an HTML check. Fixed by wrapping in `if (knitr::is_html_output())` with a plain `knitr::kable()` fallback for DOCX.

-   ~~**Fix site count discrepancy (21 vs 22)**~~ **DONE.** `README.md` line 57 comment said "21 sites"; `AGENTS.md` said "21 sites: 13 mainstem + 8 tributaries." Both corrected to 22 sites: 13 mainstem + 9 tributaries (verified from `baseline_sites.csv`).

-   ~~**Set up Quarto render profile dropdown**~~ **DONE.** Created `_quarto-html.yml` and `_quarto-docx.yml` profile files. Moved `format:` blocks out of `_quarto.yml` into the respective profile files. Added `profile: default: html / group: [[html, docx]]` to `_quarto.yml`. RStudio now shows "html" and "docx" as selectable profiles in the Render button dropdown. Default profile is HTML.

-   ~~**Fix code print leakage in `data_sourcing.qmd`**~~ **DONE.** Two root causes: (1) YAML front matter (including `execute: echo: false` and `date: "\`r Sys.Date()\`"`) was placed *after* the`\# Data Sourcing`heading on line 1, so Quarto did not parse it as document front matter - the date expression appeared as raw text in DOCX. Fixed by moving YAML to the top of the file. (2) The`knitr::knit_exit()`chunk had no`echo = FALSE`, so the source code could appear in DOCX. Fixed by adding`echo = FALSE\` to that chunk.

-   ~~**Integrate 2016 report site descriptions into `study_area.qmd`**~~ **DONE.** Converted the 2016 Baseline Water Quality Assessment DOCX to markdown using pandoc and extracted 22 embedded site photos. Copied all photos to `other/documents/site_photos/` with descriptive filenames. Rewrote the "Sampling Site Descriptions" section of `study_area.qmd` with text, coordinates, photo, and caption for all 13 mainstem sites (RM 82 through RM 1.5) and all 9 tributary sites, following the 2016 report order (mainstem first). Added a note that descriptions and photos are from the 2016 report and may be updated.

-   ~~**DOCX formatting: suppress all code output globally**~~ **DONE.** Added `execute: echo: false / warning: false / message: false` to `_quarto-docx.yml`. Applies across all chapters during DOCX rendering regardless of per-chapter YAML settings.

-   ~~**DOCX formatting: chapter page breaks**~~ **DONE.** Created `filters/pagebreak-h1.lua` - a Lua filter that inserts an OpenXML `<w:br w:type="page"/>` element before every H1 heading. Guarded by `if FORMAT == "docx"` so it has no effect on HTML rendering. Referenced from `_quarto-docx.yml` under `filters:`.

-   ~~**Separate HTML and DOCX output directories**~~ **DONE.** Added `project: output-dir: docs-docx` to `_quarto-docx.yml`. HTML profile continues to render to `docs/` (for GitHub Pages); DOCX profile now renders to `docs-docx/`. The two formats no longer interfere on successive renders. Removed `downloads: [docx]` from `_quarto.yml` book section - that sidebar link pointed to a DOCX inside `docs/`, which is no longer the case.

-   **DOCX formatting: boxplot point/text sizing - PARTIALLY DONE, needs verification.** Added conditional size variables to `create_facet_plots` in `static_boxplot_function.R` (point size 1.5 → 3.0, facet strip text 16 → 22, axis labels 14–16 → 18–20, legend text default → 14). The DOCX detection originally used `isTRUE(knitr::pandoc_to("docx"))`, which may not fire in Quarto's rendering pipeline. Updated to `identical(Sys.getenv("QUARTO_PROFILE"), "docx") || isTRUE(knitr::pandoc_to("docx"))`. **Verify that sizes are now correct in DOCX at start of next session.**

### Completed this session (March 31, 2026)

-   ~~**CDX WQX delete file debugging**~~ **DONE.** Ben attempted to delete all 835 existing 2021 WQX records using a batch Activity ID delete file (`resultphyschem_DELETE.csv`). The import failed for all records with error "Domain Value Invalid."

    **Root cause:** The DELETE file contained Activity IDs with the `KENAI_WQX-` org prefix (e.g., `KENAI_WQX-KBL_t_30.0-2021-07-27-P`). WQX's batch delete process expects IDs *without* the org prefix - the org context is provided by the logged-in account. The org prefix is added by WQX when storing and should not be included when submitting delete references.

    **Fix:** A corrected DELETE file was generated with the prefix stripped: `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/resultphyschem_DELETE_v2.csv`. **This file was successfully used to delete all 835 existing 2021 records on 3/31/2026.**

    **Why full delete was required (not just re-upload):** 66 Activity IDs in WQX used old naming conventions (`Nitrate_Nitrite-N`, `Total xylenes`) that differ from the corrected file (`Total Nitrate/Nitrite-N`, individual xylene names). A re-upload without deletion would have left those stale records permanently.

    **WQX delete UI path (confirmed working 3/31/2026):** Import & Submit → Import a batch of IDs for records to delete from WQX → Import a file of Activity IDs to be deleted. Type of File: CSV (Comma delimited). **Leave "Ignore First Row of Import File?" checked** (file has a header row). Organization ID is auto-populated from login. Note: the UI labels this as "Activity Group IDs to Delete" but it correctly deletes individual Activities.

-   **EPA WQX support call - structural recommendation logged (3/31/2026).** Spoke with Kevin Christian (wqx\@epa.gov). Key notes:

    -   Current KWF data in WQX is organized at the **"Activity" level** (each Activity = one result). Kevin recommends organizing at the **"Results" level** instead (each Activity = one sampling event, with multiple results nested under it). This is not necessarily wrong as-is, but performance degrades as the number of records increases.
    -   Addressing this involves using the **"Review" function** in WQX Web to access existing records and re-define them as Results rather than Activities.
    -   This restructuring is best handled in the **`kenai-river-wqx-qaqc` repo**, not here, and should be considered for future annual submissions. See tasks below.

-   ~~**2021 CDX re-upload**~~ **DONE.** All three files (`results_activities.csv`, `project.csv`, `station.csv`) were uploaded to EPA CDX. WQP download on 3/31/2026 confirms 835 2021 records are present in WQP (`other/input/WQX_downloads/wqp_download_20260331/narrowresult.csv`).

-   ~~**Add missing threshold values to `all_reg_vals.csv`**~~ **DONE.** Water Temperature and Fecal Coliform added directly to `all_reg_vals.csv` and `threshold_table.R`. **Iron required a separate pipeline fix** - see iron pipeline entry below.

    -   **Water Temperature** (rows 56–58): 20°C (`temp_all_freshwaters`), 15°C (`temp_rearing_migration`), 13°C (`temp_egg_fry_spawning`) - all ADEC 18 AAC 70, unit `deg_c`.
    -   **Fecal Coliform** (rows 59–60): 400 CFU/100mL (`recreation_single_sample`), 40 CFU/100mL (`drinking_water_single_sample`) - both ADEC, unit `cfu/100ml`.

-   ~~**Update `threshold_table.R`**~~ **DONE.** Added `standard_labels`, `standard_authority`, and `unit_labels` entries for all new standard type codes, including `aquatic_life_chronic` (which was missing and would have shown the raw code string for iron). Unit labels added: `deg_c` → `°C`, `cfu/100ml` → `CFU/100 mL`. All temperature standards map to ADEC authority; FC single-sample limits map to ADEC.

-   ~~**Update `water_temp.qmd` narrative**~~ **DONE.** Removed the "Writing in progress here 1/15/2025; old text below" placeholder. Rewrote the opening to: (1) retain the AKTEMP and logger context; (2) add the three ADEC temperature standards (20/15/13°C) with proper attribution to 18 AAC 70; (3) include a note that single-point grab samples should be evaluated alongside continuous logger records in AKTEMP. Old 2016 data narrative text preserved below.

-   ~~**Update `fecal_coliform.qmd` narrative**~~ **DONE.** Rewrote the opening paragraph to: lead with the reason geometric mean cannot be evaluated (two events per year, not a 30-day window); name the single-sample limits (400 and 40 CFU/100 mL) as the applicable comparisons; note that a single exceedance should prompt additional monitoring rather than be treated as a definitive standard violation. Old data narrative text preserved below.

-   ~~**Update `iron.qmd` citation**~~ **DONE.** `(ADEC, 2008; USEPA, 2014)` → `(ADEC, 2008; USEPA, 1976)`. The 2014 date referenced the NRWQC summary page; the iron criterion actually originates from USEPA 1976 Red Book and is not in the current priority pollutant table.

-   ~~**Fix iron threshold not displaying in boxplot**~~ **DONE (3/31/2026).** Root cause: `all_reg_vals.csv` is overwritten on every render by `static_boxplot_function.R` from three formatted CSVs derived from `master_reg_limits.xlsx`. Directly editing `all_reg_vals.csv` is therefore ineffective. Fix applied in three places:

    1.  **`other/input/regulatory_limits/master_reg_limits.xlsx`** - added iron row to `static_regulatory_values` sheet: `static_category="total_metals_aquatic_life"`, `parameter_baseline_name="Iron"`, `standard_type="aquatic_life_chronic"`, `reg_value=1`, `reg_unit="mg/l"`, `source="ADEC 2008; USEPA 1976"`. Note: stored in **mg/L** (not ug/L) to match the iron analysis data units.
    2.  **`reg_limits.qmd`** - added a new export block after the `ph_reg_vals` block that filters `static_category == "total_metals_aquatic_life"` and writes `other/input/regulatory_limits/formatted_reg_vals/total_metals_aq_reg_vals.csv`.
    3.  **`functions/static_boxplot_function.R`** - added `total_metals_aq_reg_vals` to the `bind_rows()` call so it flows into `all_reg_vals.csv`.

    **Architecture note:** `master_reg_limits.xlsx` is the **single source of truth** for all regulatory threshold values. Any future threshold additions should go there first, then be routed through the appropriate export block in `reg_limits.qmd`.

-   ~~**How's My Waterway (HMW) visibility - root cause identified and fix applied (3/31/2026).**~~ Downloaded a sample dataset from HMW for zip code 99669 (Soldotna area), which showed ADEC data from "Kenai River – River Mile 20.75" (org `AKDECWQ`). Compared the ADEC station's WQP metadata field-by-field against KWF's station records.

    **Root cause confirmed:** `HorizontalCollectionMethodName = "Interpolation-Satellite"` is **not a valid WQX domain value**. WQX substitutes `"Unknown"` for the invalid value and, critically, **drops the `HUCEightDigitCode` field entirely**. All 16,440 KWF records in WQP - both historical (2000–2013) and 2021 - are linked to stations with `HUCEightDigitCode = NA`. HMW uses HUC codes to link monitoring data to watersheds and waterbodies; without them, KWF stations have no geographic context in HMW and do not appear. The ADEC station that appears in HMW uses `"GPS-Unspecified"` and has a populated HUC code.

    **Fix applied:** Added a `mutate()` in the station generation block of `appendix_a.qmd` (lines \~2753–2769) to replace `"Interpolation-Satellite"` → `"GPS-Unspecified"` for all stations. `station.csv` regenerated - all 22 stations now have `GPS-Unspecified` and `HUCEightDigitCode = 19020302` with no NAs.

    **Next step: re-upload `station.csv` to CDX**, then verify in WQP that HUC codes are now stored for all 22 stations. Once confirmed, check HMW to see if KWF data appears. Note: there may be a 24–72 hour propagation delay before HMW reflects the corrected station metadata. If HUC codes still appear as NA in WQP after re-upload, the fallback is to manually set them through the WQX Web interface (which has a geocoding tool that auto-populates HUC from coordinates).

-   **Station ID coverage analysis completed (3/31/2026).** Compared distinct `MonitoringLocationIdentifier` values in the WQP download (`wqp_download_20260331/narrowresult.csv`) against the 22 stations in the current `station.csv`. Found 37 total station IDs in WQP: 22 named IDs (e.g., `KENAI_WQX-KBL_m_01.5`) covering 12,556 results across 2000–2021, and 15 legacy numeric IDs (e.g., `KENAI_WQX-10000066`) covering 3,884 results from 2002–2009 only. The 15 numeric-ID stations are named `KBX_m_*` and predate the current naming convention. Re-uploading `station.csv` will fix HUC codes for the 22 named stations only; the 15 numeric-ID stations require a separate corrective upload (see Task 2). Corrective file generated: `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/station_historical_correction.csv`.

-   **HMW partial visibility observed (3/31/2026).** Before any corrective steps were taken, some KWF sites with historical data were already appearing in HMW. This was unexpected given that all 37 station records in WQP show `HUCEightDigitCode = NA`. The reason is not yet fully understood - HMW may have alternate geographic lookup paths, or some station records may have been partially corrected previously. This finding does not eliminate the need for the station corrections (HUC codes should be populated for all stations regardless), but it does mean HMW visibility is not entirely broken at present. Investigate further when picking up Task 2.

### Completed this session (April 1, 2026)

-   ~~**Correct CMA and CMB narrative errors in `appendix_a.qmd` (Q25–Q26)**~~ **DONE.** Root cause: an older version of the flagging logic had treated dissolved metals (EPA Method 200.8) as Rejected when RPD could not be calculated. That decision was later reversed (see Q19 narrative, line ~1921): below-LOQ results are retained as Accepted, not flagged. The flag decisions CSV (`2021_data_flag_decisions.csv`) correctly reflects this - only Fecal Coliform (both seasons) and spring Total Nitrate/Nitrite-N are flagged - but the Q25/Q26 prose had never been updated to match.

    Corrected figures:
    - **CMA:** 50.1% → **87.4%** (72 of 570 in-QAPP results flagged, i.e., 12.6%)
    - **CMB:** 52.2% → **92.2%** (498 usable results out of 540 planned) - project now meets the 60% QAPP goal
    - **CMA by site range:** 44.4%-71.4% → **80.6%-92.1%**
    - **Dissolved metals CMA:** 0% → **100%** (all Accepted)

    Narrative changes applied: (1) Q25 CMA discussion now correctly identifies the two flagged methods and notes dissolved metals are Accepted; (2) Q26 CMB discussion removes dissolved metals from the driver list, updates the project % and count, and adds dissolved metals to the "100% CMB" list. Added `cma-narrative-values` and `cmb-narrative-values` chunks so all CMA/CMB figures are now driven by inline R and will update automatically on re-render.

-   ~~**Convert hardcoded numerical prose values to inline R in `appendix_a.qmd`**~~ **DONE.** Three sections converted:
    - **Q18 Metals comparison:** site counts (Ca/Fe/Mg), Ca % difference range and absolute range, Mg by-site exception percentages and RM 1.5 concentration. Variables computed inside the existing metals chunk before `rm(ca, fe, mg)`.
    - **Q19 RPD analysis:** total paired observations, eligible pairs, exceedance count and parameter breakdown, FC >60% count. New summary chunk added after `write.csv(rpd_check_dat)`. Note: converting these values revealed that the hardcoded prose was inaccurate (e.g., said "3 of 4 FC RPDs >60%" but computed data shows 1 of 3; said "4 exceedances with 1 phosphorus" but data shows 5 exceedances, 2 FC + 3 TSS). The inline R now produces correct values on render.
    - **Q20 Matrix spike:** recovery range min/max and estimated bias range. New chunk added after `write.csv(matrix_spike_recovery_fails_tbl)`.

-   ~~**Remove all em-dashes from `appendix_a.qmd`**~~ **DONE.** Replaced 7 instances: prose em-dashes replaced with semicolons or hyphens; code comment em-dashes replaced with hyphens.

-   **HMW visibility investigation (April 1, 2026) - CONCLUDED.** Investigated why mainstem KWF sites appear in HMW but tributary sites do not. Key findings:

    **HMW has two distinct display mechanisms with different fix paths:**
    1.  **"Past Water Conditions" / Monitoring tab** is WQP-driven. HMW was recently updated to use client-side geospatial (HUC12 spatial) operations to find monitoring locations - it no longer relies solely on the stored `HUCEightDigitCode` field. This means once `station.csv` is uploaded with correct coordinates and `GPS-Unspecified`, tributary sites should appear in HMW's monitoring tab when a user searches near those tributaries (within their HUC12). **This is fixable by KWF** via the corrected station.csv upload.
    2.  **Waterbody condition status** (Overview/Aquatic Life/Fishing/Swimming tabs) is ATTAINS-driven and shows only assessed waterbodies. HUC 19020302 has ATTAINS assessment units only for the Kenai River mainstem (3 segments) and Kenai Lake. **No tributaries** (Moose River, Funny River, Russian River, Beaver Creek, etc.) have ATTAINS assessment units. This is why tributary sites cannot show waterbody condition in HMW - it is **outside KWF's control** and requires ADEC to create assessment units for those streams in a future Integrated Report cycle.

    **Action items from this investigation:**
    - Upload corrected `station.csv` (Task 1 below) - fixes the WQP metadata issue for monitoring data visibility.
    - Populate `WaterbodyName` for all tributary stations (e.g., "Moose River", "Funny River") before upload - this may improve NHD reach linking in HMW.
    - Contact ADEC liaison to ask whether KWF tributaries can be added to a future Integrated Report assessment cycle. KWF's WQP data is the primary data source for these streams and strengthens the case for assessment.

### Tasks for next session (in order)

1.  **Re-upload corrected `station.csv` to CDX.** The corrected `station.csv` is ready at `other/output/wqx_formatted/station.csv` with `HorizontalCollectionMethodName = "GPS-Unspecified"` and `HUCEightDigitCode = 19020302` for all 22 stations. Upload it to CDX, then verify in WQP that all 22 stations now show populated HUC codes. Allow 24-72 hours for HMW to reflect the change. **Important caveat (April 1, 2026):** investigation revealed that KBL_m_23.0 (mainstem) appears in HMW but KBL_t_36.0 (Moose River tributary) does not, despite both having identical WQP metadata issues. Fixing the station.csv is still necessary but may not be sufficient for tributary sites - see Task 2a.

2.  **\[HIGH PRIORITY\] Fix HMW visibility for 15 legacy numeric-ID stations - move process to `kenai-river-wqx-qaqc` repo and carry out the upload.** During session 3/31/2026, a corrective station upload file was generated at `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/station_historical_correction.csv` for 15 historical stations (IDs `KENAI_WQX-10000063` through `KENAI_WQX-10000131`, named `KBX_m_*`, covering 2002-2009, 3,884 results). These stations have `HorizontalCollectionMethodName = "Unknown"` and blank `HUCEightDigitCode` in WQP. **This process belongs in `kenai-river-wqx-qaqc`, not here.** Action items: (a) recreate the corrective upload script in `kenai-river-wqx-qaqc`; (b) upload to CDX; (c) verify HUC codes in WQP; (d) delete `station_historical_correction.csv` from this repo.

2a. **HMW tributary visibility - confirmed root cause, next steps defined (April 1, 2026).** ATTAINS query confirmed HUC 19020302 contains assessment units only for Kenai River mainstem segments and Kenai Lake - all 9 KWF tributary sites (Moose River, Funny River, Russian River, Beaver Creek, etc.) have no ATTAINS assessment units. This means tributaries **cannot** show waterbody condition status in HMW regardless of WQP metadata corrections - this is ADEC's responsibility. **Next steps:** (a) Before uploading corrected `station.csv` (Task 1), update `WaterbodyName` for all 9 tributary stations to their correct stream name (currently blank or "Kenai River"). (b) After upload, verify in HMW that monitoring data now appears for tributary sites when searching from within their sub-watershed. (c) Contact ADEC liaison to request that KWF tributaries be added to a future Integrated Report assessment cycle; point to KWF's WQP dataset as the data source.

3.  **Verify boxplot DOCX sizing fix.** Render DOCX and confirm that jitter points, facet strip text, and legend text are visibly larger than in HTML output. The fix is in `functions/static_boxplot_function.R` - the `is_docx` detection now uses `Sys.getenv("QUARTO_PROFILE") == "docx"` as the primary check. If still not working, add `cat(Sys.getenv("QUARTO_PROFILE"), "\n")` in a temporary test chunk to confirm the environment variable is set during rendering.

4.  **Verify regulatory authority column in threshold tables.** The USEPA/ADEC mapping in `functions/threshold_table.R` is hard-coded - confirm assignments are correct before final render. Also verify that the three new parameters (Iron, Water Temperature, Fecal Coliform) render the threshold table as expected.

5.  **Benzene chapter (`parameters/benzene.qmd`) has no narrative text.** Needs prose added noting that: (a) no separate standard exists for individual benzene in freshwater; (b) the applicable ADEC standard is 10 µg/L for total aromatic hydrocarbons (BTEX combined) - see BTEX chapter.

6.  **Resolve ALS lab duplicate (DUP) status issue.** Four 2021 results have DUP status unexpectedly assigned in the ALS data - it appears ALS ran these as lab duplicates for Total Ca/Fe/Mg (method 200.7), which may not have been requested or covered by the QAPP. Sites: KR RM 22 SOC 2021-05-11; KR RM 1.5 2021-07-27; KR RM 23 2021-05-11; KR RM 70 2021-07-27. **Investigated March 28, 2026:** The DUP1 records are silently dropped by the pipeline - only primary sample values (Ca, Fe, Mg) appear in the export as `Field Msr/Obs` with correct single values. The exported data is not incorrect. What is missing is the QA/QC value of those lab duplicates: RPDs were never computed for them. **This does not block the CDX upload.** Outstanding questions for a future QA/QC session: (a) were lab duplicates requested/covered by QAPP? (b) should lab duplicate RPDs be computed and reported? Note: in SGS, field duplicates are labeled "PS" in `sample_type`; in ALS, lab duplicates are labeled "DUP1" in `sample_type`. Flagged originally March 2022.

7.  **LOQ logic flow chart for ADEC.** Draw a logic flow chart for the 2-step winnowing process for LOQ and 2× LOQ criteria and send to ADEC staff to confirm interpretation. Flagged June 2023 - unclear if completed.

8.  **Make `appendix_a.qmd` year-neutral** (lower priority). The document has hardcoded references to "2021" throughout - in file paths, variable names, narrative text, and chunk labels (e.g., `{r, 2021 WQX formatting for SGS, ...}`). Goal: replace with a `year` variable set once at the top so the script can be reused for any year without find-and-replace edits. Closely related to task 6 (extract ingestion logic to `.R` scripts); both tasks should be done together.

9.  **Extract ingestion logic to `.R` scripts** (lower priority - see File Structure section below).

10. **Restructure WQX data from Activity-level to Results-level** (lower priority - future annual submissions). Per EPA WQX support (Kevin Christian, 3/31/2026): current data is organized one-result-per-Activity. The correct WQX structure is one-Activity-per-sampling-event with multiple Results nested under it. Performance degrades at scale with the current structure. Use the "Review" function in WQX Web to access and re-define existing records. Address in `kenai-river-wqx-qaqc` repo; incorporate correct structure into the annual pipeline for future years.

11. **Address historical CDX data corrections** (e.g., spring 2013 specific conductance) in a dedicated chapter of the `kenai-river-wqx-qaqc` repo - not in `appendix_a.qmd`.

12. **Move `wqx_corrections.qmd` to `kenai-river-wqx-qaqc` repo.** This file contains corrections to previously uploaded CDX data (e.g., historical unit errors) and belongs in the QA/QC repo, not this report repo. It is currently a loose file in the project root and is not included in the book chapter list. Move the file and its associated data to `kenai-river-wqx-qaqc`, then delete it from this repo.

13. **Add inline tables alongside all calculated-result download links** (lower priority - polish, does not affect data quality or CDX submission). Currently, \~10 places in `appendix_a.qmd` offer downloadable CSVs with no inline table, which will render as broken download-only links in PDF output. Use the pattern below for dual HTML/PDF output. Raw source files (PDFs, XLSs, JPGs from labs) do not need tables - download-only is appropriate for those. Calculated CSVs that need tables:

    -   `planned_actual_analyses_2021.csv` (planned vs. actual analyses)
    -   `sample_holding_times.csv` (max holding times by sample type)
    -   `holding_time_calcs.csv` (spring/summer 2021 holding time calcs)
    -   Ca, Fe, Mg total vs. dissolved CSVs (3 files)
    -   `rpd_check_dat.csv` (duplicate RPD values)
    -   `matrix_spike_recovery_fails_2021.csv` (matrix spike failures)
    -   CMA outputs: `cma_parameter.csv`, `cma_site.csv`, `cma_project.csv`
    -   CMB outputs: `cmb_parameter.csv`, `cmb_site.csv`, `cmb_project.csv`
    -   Observation count by characteristic CSV (Q39 area)
    -   Skip inline table for the large WQX-formatted results file (`2021_kwf_baseline_results_wqx.csv`) - too many rows to display usefully.

    **Pattern for dual HTML/PDF tables:**

    ``` r
    if (knitr::is_html_output()) {
      DT::datatable(df)
    } else {
      knitr::kable(df)
    }
    ```

14. **Dynamically generate numerical values in parameter chapter narratives.** Hardcoded statistics in prose (e.g. "the highest concentration of magnesium was 582 mg/L and occurred at Mile 1.5 during spring 2011") should be replaced with inline R expressions so they update automatically when new data years are added. This applies to all parameter chapters. The approach: create a helper function in `functions/` (e.g. `summarise_parameter.R`) that accepts a filtered data frame and returns a named list of common summary values (e.g. `max_val`, `max_site`, `max_season`, `max_year`, `min_val`, `min_site`, etc.). Each chapter sources this function and calls it in a single, minimal setup chunk - keeping chapter `.qmd` files uncluttered. Inline values are then referenced in prose as `` `r stats$max_val` `` etc., formatted consistently (e.g. `round()` or `format()` as appropriate for the parameter's units and precision). Scope: all parameter chapters that contain hardcoded numerical summaries - scan each `.qmd` in `parameters/` before implementing.

------------------------------------------------------------------------

## Decisions Made - March 2026

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
| 1–518 | SGS/ALS ingestion (Parts A–E) | Working. Includes Ca/Mg/Fe unit correction (March 2026). Dense - priority candidate for extraction to `R/ingest_sgs_als.R`. |
| 520–983 | FC and TSS ingestion | Mostly working. TSS has a documented QA gap: SWWTP did not report lab QA results (blanks, duplicates, check standards) as required by QAPP in 2021/2022. Lab QA export block is commented out. |
| 985–1260 | Lookup joins + first WQX export | Working (March 26, 2026). The write chunk was `eval = F` and is now `eval = T`. A `dat_raw`/`dat` save-restore pattern was added so the WQX CSV is written without corrupting the raw-column `dat` used by downstream QA/QC. The full column-rename block is redundantly repeated later for the CDX export - consolidate in the script extraction step. |
| 1268–2410 | QA/QC Checklist (Questions 1–42) | Mostly complete. Two gaps: (1) Completeness Measure B block is `eval = F` and produces nonsensical results - unfinished (\~line 2141). (2) Q39 (range check / outliers) defers to visual review in report chapters rather than doing anything here. |
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

-   **Regulatory agencies** - primarily ADEC, which draws data from EPA CDX every two years for its [Integrated Report](https://dec.alaska.gov/water/water-quality/integrated-report/). That report makes determination decisions about which water bodies may need management action. This is the most immediate real-world use of KWF's submitted data.
-   **KWF scientists** tracking long-term trends
-   **General public** and local stakeholders
-   **Funding partners** - a cohort of local agencies funds the laboratory work. They want assurance that funds are being spent wisely. Communicate with them regularly; this is a project priority that is not obvious from the code itself.

The report format is modeled on previously completed comprehensive reports from **2007 and 2016** (PDFs in `other/agent_context/`).

### QA/QC Decision Authority

KWF staff scientists have final say on all QA/QC flag decisions (Accepted vs. Rejected). All decisions must be thoroughly documented and transparent. Labs, ADEC, and EPA Region 10 do not make flagging decisions, but ADEC is the primary downstream consumer of flagged data. If ADEC disagrees with KWF's QA/QC decisions, they may choose to exclude the data from analyses in the Integrated Report - making thorough documentation of all flag decisions especially important.

### Long-Term Vision and Intended Use

This codebase is intended for **ongoing annual use**, not a one-time archive. The workflow is:

1.  New annual data is processed each year in the **separate qaqc repo** (`kenai-river-wqx-qaqc`), using `appendix_a.qmd` as the template/example.
2.  Processed, QA/QC'd data is submitted to **EPA CDX**.
3.  This main report repo downloads data directly from EPA CDX on render, automatically integrating new years.

`appendix_a.qmd` is the **example pipeline for 2021** - once it is stable and correct, its logic is extracted to `.R` scripts reusable across years in the qaqc repo.

A "complete" version of this report can: (a) access data from EPA CDX, and (b) automatically integrate it into the report on render. The Quarto book renders to both **HTML** (hosted on GitHub Pages) and **PDF**, with the PDF accessible/downloadable by readers.

Each parameter chapter should contain **inline R code** for summary statistics so values update automatically on re-render.

**A primary goal of this project is that all KWF water quality data is publicly visible in EPA's [How's My Waterway](https://mywaterway.epa.gov/) web app.** This requires that data be correctly formatted and successfully submitted to EPA CDX/WQX. Verifying visibility in How's My Waterway is a meaningful end-to-end check that the submission pipeline worked correctly.

**HMW visibility architecture (confirmed April 1, 2026):** HMW has two distinct display mechanisms. (1) **Monitoring data** (Past Water Conditions tab) is WQP-driven - HMW uses client-side HUC12 spatial operations to find monitoring locations by coordinates, so correct station metadata (GPS-Unspecified, populated HUC, correct coordinates) is sufficient for monitoring data to appear. (2) **Waterbody condition status** (Overview/Aquatic Life tabs) is ATTAINS-driven - only assessed waterbodies with ATTAINS assessment units appear. All 13 KWF mainstem sites map to Kenai River assessment units in ATTAINS. **None of the 9 KWF tributary sites have ATTAINS assessment units** - they cannot show waterbody condition status in HMW until ADEC creates assessment units for those streams in a future Integrated Report cycle.

### Known Scientific Context

-   The **lower Kenai River area** is experiencing faster-paced development and use. Parameters of concern are more likely to show trends there.
-   **Climate change** may be driving trends in turbidity, temperature, and related field parameters - worth watching.

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

1.  **Results & Activities CSV** - the main QA/QC'd output (e.g., `2021_export_data_flagged.csv`)
2.  **Project CSV** - read from existing CDX download, update QAPP status to `Y` and add website URL before uploading
3.  **Station CSV** - read from existing CDX download, filter to KWF sites only (`grepl("KBL", MonitoringLocationName)`)

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
-   Some metal thresholds (Cadmium, Chromium, Copper, Lead, Zinc) are **hardness-dependent** - threshold values are calculated per-sample based on measured hardness
-   Regulatory values are stored in `output/regulatory_values/`
-   Session variables `static_metals_thresholds`, `static_metals_reg_vals`, `diss_metals_hard_param` hold processed threshold data

## Terminology: AQWMS / AWQWMS

`AQWMS` (also written `AWQWMS` or `aqwms`) stands for "Ambient Water Quality Monitoring System," software sold by Gold Systems. KWF considered using it in 2021 but ultimately did not. **Completed March 2026:** All references have been replaced with `wqx_formatted` / `wqx_templates` as appropriate throughout `appendix_a.qmd` and on disk.

Renamed paths: - `other/output/aqwms_formatted_results/` → `other/output/wqx_formatted/` - `other/input/AQWMS/` → `other/input/wqx_templates/` - `other/input/AQWMS/aqwms_qaqc/` → `other/input/wqx_templates/wqx_qaqc/` - `other/documents/AQWMS_documents/` → `other/documents/wqx_documents/` - `AQWMS_template_matching_table.xlsx` → `wqx_template_matching_table.xlsx` - `aqwms_qaqc_info.xlsx` → `wqx_qaqc_info.xlsx` - Output file: `2021_kwf_baseline_results_aqwms.csv` → `2021_kwf_baseline_results_wqx.csv` - R variables: `aqwms_colnames` → `wqx_colnames`, `aqwms21_sitenames` → `wqx_sitenames`, `aqwms_analytical_methods` → `wqx_analytical_methods`

Note: The original vendor template file `AWQMS_KWF_Baseline_2021.xlsx` retains its name - it is a received file from Gold Systems and is kept as-is per the raw data policy.

## QA/QC Notes

-   **Flagging:** The correct WQX convention is already implemented. `Result Qualifier` = lab qualifiers (U, J, =). `Result Status ID` = KWF QA/QC decision (Accepted/Rejected). Do not add FQC or other custom codes - the existing design is complete and correct.
-   **Outliers:** Visually-identified outliers (especially pre-2014 legacy data lacking field blanks) are excluded from visualizations but retained in archived data.
-   **Lab qualifiers** (from labs) are distinct from **KWF QA/QC flags** - treat them separately.

## Known Data Issues (as of March 2026)

-   **11 results missing `monitoring_location_id`: RESOLVED (March 27, 2026).** Root cause was a typo in `sgs_site_names_matching_table_manual_edit.xlsx`: the RM 1.5 entry was `RM_1.5_Kenai_City_Dock` (extra underscore after `RM`) rather than `RM1.5_Kenai_City_Dock` (the form produced by the Part C transformations). Fix applied by adding one correctly-formatted row to the Excel file. All spring and summer SGS site names now resolve without NAs. Re-render completed successfully March 27, 2026; output CSVs are current and ready for CDX upload.

-   **Ca/Mg/Fe unit errors: RESOLVED (March 2026).** Root cause identified: the summer 2021 SGS EDD reported dissolved Calcium, Iron, and Magnesium (method EP200.8) with unit label `ug/L`, but the numeric values were on the mg/L scale (e.g., Ca = 16,200 "ug/L" at RM 0 NNC = 16.2 mg/L, which is physically plausible; 16.2 ug/L would be unrealistically low for freshwater). Spring 2021 SGS data contained no dissolved Ca/Fe/Mg runs (confirmed). ALS total metals were correctly labeled mg/L throughout. Fix applied in `appendix_a.qmd` at the SGS ingestion step (Part A): rows where `analyte %in% c("Calcium", "Iron", "Magnesium")` and `units == "ug/L"` are divided by 1000 and relabeled `mg/L`. Corrected hardness values for summer 2021 are 37–380 mg/L as CaCO3 (plausible); erroneous values were 37,000–380,000 mg/L.

-   **Output CSVs regenerated (March 27, 2026, clean).** Both `other/output/wqx_formatted/2021_kwf_baseline_results_wqx.csv` and `2021_export_data_flagged.csv` have been regenerated with corrected Ca/Mg/Fe values and correct RM 1.5 site IDs. **2021 CDX re-upload completed 3/31/2026** (all three files: `results_activities.csv`, `project.csv`, `station.csv`). WQP download on 3/31/2026 confirms 835 records present. Note: a corrected `station.csv` with `HorizontalCollectionMethodName = "GPS-Unspecified"` and populated `HUCEightDigitCode = 19020302` was generated after the initial upload - **re-upload of the corrected `station.csv` is Task 1 for the next session** (required for HMW visibility).

-   **Hydrocarbon data** missing from 2025 WQP download (uploaded Jan 2024 but not appearing in download).

-   **Turbidity: one spurious `uS/cm` record** exists in the analysis dataset (`result_measure_measure_unit_code = "uS/cm"` for a Turbidity row - almost certainly a data entry error; uS/cm is a conductance unit). Needs to be identified and corrected at the source. Found during interactive plot testing, March 2026.

-   **Turbidity: anomalously high value at RM 1.5 spring** (\~3,200 NTU) warrants review during historical data outlier check. Flagged as a candidate outlier, March 2026.

-   **Completeness Measure A/B** calculations are complete and enabled. Project-wide **CMA = 87.4%** and **CMB = 92.2%** (498 usable / 540 planned) - both above the 60% QAPP goal. The only flagged parameters are Fecal Coliform (both seasons, 9222D) and spring Total Nitrate/Nitrite-N (4500-NO3(F)). Dissolved metals (200.8) are all Accepted at 100% CMA/CMB; below-LOQ results are retained per KWF's adopted approach (see Q19 in `appendix_a.qmd`). All CMA/CMB figures in `appendix_a.qmd` Q25-Q26 are now driven by inline R and update automatically on re-render. **Note:** earlier versions of AGENTS.md incorrectly stated CMB = 52.2% - this was based on a superseded flagging decision that had dissolved metals as Rejected. Corrected April 1, 2026.

-   **TSS lab QA gap:** SWWTP did not report required lab QA results for TSS in 2021/2022. Note in code, address for 2023+.

## Visualization Wish List

These are desired but not yet implemented features for the report figures:

-   **A) Interactive boxplots:** DONE (March 2026). Hover tooltips show site, date, value+units, fraction, lab, method, and QA/QC status. Implemented via `plotly::ggplotly()` in `static_boxplot_function.R`, with HTML/PDF conditional rendering in each parameter chapter.
-   **B) QA/QC toggle:** Figures should support two views - (1) data that passed QA/QC only, and (2) all data including failed QA/QC - switchable via a toggle. Symbology should differ between the two views.

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `magrittr`, `dataRetrieval`, `TADA`, `xfun`

Federal agency packages - `dataRetrieval`, `TADA`, and others - should be integrated as appropriate and as recommended by EPA/ADEC.

Note: `magrittr` pipe (`%>%`) appears in existing legacy code. For all new code, use the base pipe (`|>`). Do not mass-convert existing `%>%` usage - leave legacy files as-is to avoid unnecessary git churn and risk.

## Governance Documents (in `other/agent_context/`)

-   **QAPP** - Quality Assurance Project Plan, approved by ADEC and EPA Region 10 (2023 + April 2024 addendum)
-   **MOU** - Baseline Water Quality MOU 2025 Final
-   **Funding Proposal** - KWF 2024 BOR WaterSMART CWMP Proposal
-   **ADEC Water Quality Standards** - Alaska Dept of Environmental Conservation Water Quality Standards (18 AAC 70)
-   **DL/LOD/LOQ Interpretation** - SGS Laboratories document on detection limit terminology
