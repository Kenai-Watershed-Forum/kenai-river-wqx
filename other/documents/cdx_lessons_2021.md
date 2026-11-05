# Lessons from the 2021 CDX upload and re-integration into the book

Written 2026-11-05 in the report repo (`kenai-river-wqx`) for transfer to the qaqc template repo (`kenai-river-wqx-qaqc`). Each lesson gives what went wrong or was learned, the fix, and where to find detail. Detail lives in `other/agent_context/session_log.md` (entries 2026-10-14 through 2026-11-02), `AGENTS.md` (Known Data Issues, Data Sourcing), and `chapters/appendix_a.qmd`. Items marked "unconfirmed" were not verified.

## A. Upload mechanics (WQX Web and CDX)

1. **Monitoring Location ID must have no org prefix.** WQP downloads carry `KENAI_WQX-`; WQX Web prepends it on import. Copying IDs unchanged created 22 doubled stations (`KENAI_WQX-KENAI_WQX-100000xx`) holding all 830 results. Fix: strip the prefix in both `station.csv` and `results_activities.csv`; stop with an error on any prefixed or unknown ID (guard in `functions/appendix_a_scripts/generate_cdx_export.R`).
2. **Delete order and mechanics.** Delete results first, then stations; WQX allows station deletion once a station has no results. Both deletes were done as CDX imports (results dataset 117563, stations dataset 117564, 2026-10-20). Records are in `other/documents/cdx_upload_history/` (qaqc repo).
3. **Re-upload order:** `project.csv`/`station.csv` first, then `results_activities.csv`. After the clean re-upload, WQX matched bare IDs to the original stations (confirmed 2026-11-02).
4. **WQX Web import configuration** (Uid 9481, `KWF_Results_Baseline_Template_v2`): the config itself caused errors, separate from the data errors. Key points are the "Ignore Column" pattern for non-WQX columns and the no-org-prefix rule. The `.cfg` is committed in the qaqc repo; the walkthrough is in `cdx_upload.qmd` there.
5. **Data-level validation errors fixed before the import passed:** characteristic name mapping, Method Speciation, container color, detection limits, and a Unicode-hyphen character in longitude (fixed at its pipeline source). Full list: session log entry 2026-10-14.
6. **WQX applies synonym mapping on import.** `Total Phosphorus` became `Total Phosphorus, mixed forms`, `Xylenes (total)` became `Total xylenes`, `Iron, Total` became `Iron`. Check names after upload, not only before.

## B. Data hygiene to enforce in the template

1. **Site-name mapping errors propagate to Activity IDs.** Sites 10000030/10000031 were listed as tributary (`KBL_t_50.0`/`KBL_t_70.0`) but are mainstem (`KBL_m_50.0`/`KBL_m_70.0`) in the `adec_site_names` sheet; fixing it changed 34 Activity IDs. Validate every site code against the station list and its mainstem/tributary class.
2. **Unit case:** emit `mg/L` (WQX capitalization). `ingest_tss.R` emitted `mg/l`; a `stopifnot()` guard now checks it. The 2025 qaqc data still has 48 SWWTP TSS `mg/l` rows.
3. **Sample fraction:** all 2021 dissolved metals are `Dissolved` on WQP. `Filtered, field` and `Dissolved` both mean the dissolved fraction. Field filtering is believed to have ended and lab filtering begun summer 2022 (unconfirmed; check the QAPP revisions). Decide which name the template emits for each year and keep it consistent.
4. **Non-numeric values:** WQP result 56514557 has a detection limit stored as `O.18` (letter O). Validate that detection limits and values are numeric before export.
5. **Result status.** All 2021 fecal coliform (48) and spring 2021 nitrate+nitrite (24) were `Rejected`. They are uploaded, and plots in the book drop `Rejected` rows while downloads keep them. The template should carry `Rejected` through to the export deliberately and document why.
6. **Missing hardness inputs.** Total Ca/Mg were missing for 9 spring 2021 site-events (lab miscommunication), so hardness-dependent thresholds are unavailable there. Add a check that flags site-events without Ca/Mg.
7. **Characteristic name variants** (Nitrate+Nitrite appears under 3+ names across years) are mapped locally in the book via `other/input/characteristic_name_matching_table.csv`. WQP still holds the original names. A full audit (Task 1b) is planned in the qaqc effort.

## C. Hydrocarbons (BTEX)

1. Some years report a BTEX total ("Benzene, toluene, ethyl benzene, xylenes mix"); others report only components. Old book code kept only the reported total, so summer 2012 and 2021 were silently dropped and plots stopped at 2010.
2. The book now uses `functions/btex_aggregate.R` (tests in `tests/test_btex_aggregate.R`): one row per event; the reported total if present, else benzene + toluene + ethylbenzene + xylenes, preferring total xylene over isomers; ND counted as 0 within sums; all-ND events are `Not Detected`; trip blanks (Activity ID ending `-Blank`) excluded. Consider whether the template should use or mirror this logic.
3. 2021 trip-blank results are all non-detects (qualifier U, limits 0.4 to 3.0 ug/L); the table is in Appendix A, Q42.
4. 2025 contract problems seen in the template: 24 BTEX surrogate rows in the export, 44 `Field Blank` rows, 56 Activity IDs ending `-NA`, a dead `Blank` branch in `format_wqx.R`.

## D. Verifying after upload (WQP side)

1. **Indexing lag is real.** The 2021 upload was submitted 2026-10-14. Data appeared on WQP by 2026-10-20 (under the wrong IDs). After deletes (10-20) and re-upload (10-25), the corrected data was confirmed on 2026-11-02. Allow days, not hours, before concluding anything.
2. **Verification recipe:** run `functions/refresh_wqp_data.R` (live query, org `KENAI_WQX`), then compare against the upload file row by row, matching on site, date, characteristic, fraction and activity type. For 2021 this paired all 830 rows with 0 differences in values, detection limits and lab. Also check: station count, zero doubled-prefix stations, characteristic names after synonym mapping.
3. **Known unresolved:** `detection_limits.csv` does not match `narrowresult.csv` on `ResultIdentifier` (0 of 7,033 non-detects; cause unconfirmed). Reporting limits for non-detects are therefore not available in the book.

## E. Book-side integration (what the report expects from WQP)

1. The book reads only the local WQP cache and never calls the API during rendering. Refresh is manual.
2. Plots drop `Rejected` rows; BTEX non-detects are drawn as hollow circles at 0.
3. Appendix A in this repo is the worked 2021 example and the canonical text for the 42 QA/QC questions. The template's draft reports should stay consistent with it.
4. Rendering Appendix A needs `openxlsx`, `anytime`, `plotrix`, `patchwork`, rewrites three unrelated `.xlsx` files in `other/input/wqx_templates/` (restore with `git checkout --`), and `generate_cdx_export.R` reads `export_dat` from the global environment, so it cannot be sourced standalone.
