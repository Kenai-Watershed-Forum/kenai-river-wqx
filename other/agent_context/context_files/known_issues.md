# Kenai River WQX — Known Data Issues

Load this file before any CDX/WQP work, when troubleshooting data, or when reviewing QA/QC decisions.

------------------------------------------------------------------------

## Known Data Issues (Active / Unresolved)

- **WQX/STORET sync (RESOLVED, re-upload in progress):** the 835 orphaned 2021 records are confirmed deleted from WQP (0 rows returned by live query as of 2026-09-28). No further EPA action needed. `project.csv`/`station.csv` re-uploaded successfully 2026-09-28. `results_activities.csv` re-upload is blocked by a mismatched WQX Web import configuration, not by data content — see Task 1a-reupload.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years. Full audit needed (Task 1b).
- **Sample fraction inconsistency (RESOLVED in local files, not yet uploaded):** `other/output/wqx_formatted/results_activities.csv` now has dissolved metals correctly as `"Dissolved"` (verified 2026-09-28); this fix has not yet reached CDX/WQP because the results_activities.csv re-upload (Task 1a-reupload) is still blocked on the WQX Web import configuration.
- **WQX Web import configuration mismatch (2026-09-28, rebuild in progress):** the only saved Results & Activities import configuration (`KWF_Results_Baseline_Template`, Uid 8515) is a legacy 44-column, position-based template that does not match the current 53-column export column order, causing WQX Web to crash on import (a date string lands in the numeric Activity Latitude field). A new configuration (Uid 9481, `KWF_Results_Baseline_Template_v2`) is being built live in WQX Web; most elements are mapped, remaining work is one more Generated Value, a full column reorder, and a test import. Two export columns (`Monitoring Location Name`, `Laboratory Sample ID`) have no matching WQX schema element and will be set to "Ignore Column." See `AGENTS.md` Task 1a-reupload for full detail.
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
