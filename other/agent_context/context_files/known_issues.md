# Kenai River WQX — Known Data Issues

Load this file before any CDX/WQP work, when troubleshooting data, or when reviewing QA/QC decisions.

------------------------------------------------------------------------

## Known Data Issues (Active / Unresolved)

- **WQX/STORET sync (BLOCKED):** 835 2021 records in WQP but absent from WQX Web internal DB. CDX batch delete fails. Wait for EPA ETL fix. Ready files: `resultphyschem_DELETE_v4.csv` (column `ActivityIdentifier`, no org prefix), `results_activities.csv`.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years. Full audit needed (Task 1b).
- **Sample fraction inconsistency:** 2021 dissolved metals submitted as `"Filtered, field"` in CDX — needs re-upload with `"Dissolved"` (blocked by Task 1a-reupload).
- **Turbidity:** one spurious `uS/cm` unit record; anomalously high value at RM 1.5 spring (\~3,200 NTU).
- **Hydrocarbon data** missing from 2025 WQP download (uploaded Jan 2024 but not appearing).
- **ALS lab duplicates:** 4 results with unexpected DUP status (Task 7) — does not block CDX upload.
- **TSS lab QA gap:** SWWTP did not report required lab QA results in 2021/2022.
- **Spring 2013 specific conductance:** values stored as `mS/cm` but should be `uS/cm`. Correction not yet applied — address in qaqc repo (Task 12).

------------------------------------------------------------------------

## QA/QC Notes

- Flagging design is complete and correct — do not change it.
- Outliers: visually identified (especially pre-2014) excluded from visualizations, retained in archive.
- Lab qualifiers (U, J, =) are distinct from KWF QA/QC flags (Accepted/Rejected).
- **QA/QC Decision Authority:** KWF staff have final say. All decisions must be thoroughly documented.
- **CMA = 87.4%, CMB = 92.2%** (498/540) — both above 60% QAPP goal. Only flagged methods: FC (both seasons) and spring Total Nitrate/Nitrite-N. All dissolved metals Accepted at 100%.
