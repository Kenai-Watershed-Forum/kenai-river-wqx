# Kenai River Baseline Water Quality Monitoring — Project Context for Posit Assistant

## Next Session — Start Here

**Goal:** Revise, complete, and solidify `appendix_a.qmd` as a clean, well-documented pipeline for: (1) intake raw lab data → (2) apply QA/QC → (3) flag data → (4) format for EPA CDX upload. Once stable, extract the logic into sourced `.R` scripts so it can be reused for each year in the separate qaqc project.

**Immediate first task:** Inspect and correct the Ca/Mg/Fe unit errors in the 2021 data. These values were reported in the wrong units (ug/L vs. mg/L) and propagate into the hardness calculation, which breaks the regulatory analysis for all hardness-dependent metals (Cd, Cr, Cu, Pb, Zn). The correction should be applied in the data ingestion pipeline — not as a post-hoc patch — so future years don't repeat the problem.

The relevant raw data files are in `other/input/2021_wqx_data/`. The export to check is `other/output/aqwms_formatted_results/2021_export_data_flagged.csv`. Look at Ca, Mg, Fe `result_value` and `result_unit` columns to confirm the nature of the error before writing any correction.

---

## Decisions Made — March 2026

### EPA WQX Flagging Convention (settled)

The existing flagging approach in `appendix_a.qmd` is already correct and aligns with WQX conventions. Do not change the flagging design. Specifically:

- **`Result Qualifier`** column: Lab qualifiers from the laboratory (`U` = non-detect, `J` = below LOQ, `=` = detected). These come from the lab EDD files and are mapped in the data ingestion step.
- **`Result Status ID`** column: KWF's QA/QC decision. Values are `Accepted` or `Rejected`. This is the standard WQX field for flagging data that does not meet project QA/QC standards (e.g., RPD failures, matrix spike failures). The binary internal `flag` (Y/N) in `2021_data_flag_decisions.csv` maps to this field at the CDX export step.
- The `FQC` qualifier code idea from the To Do notes is **not needed** and should not be implemented. The `Result Status ID = Rejected` convention is sufficient and standard.

### File Structure (decided but not yet implemented)

Extract processing logic from `appendix_a.qmd` into sourced `.R` scripts. This makes each step independently testable and allows the qaqc repo to reuse the same scripts with year-specific inputs. `appendix_a.qmd` should become narrative prose + `source()` calls + interpretation.

Planned script breakdown:

| Script | Content |
|---|---|
| `R/ingest_sgs_als.R` | SGS EDD + ALS CSV read-in, column normalization, site name mapping, method code mapping (Parts A–E of current appendix) |
| `R/ingest_fc.R` | SWWTP + Taurianen fecal coliform read-in |
| `R/ingest_tss.R` | SWWTP TSS read-in |
| `R/format_wqx.R` | Lookup joins (lat/long, sample fraction, detection condition, container type, preservative), WQX column renaming, Activity ID construction |
| `R/apply_qaqc_flags.R` | Holding time calcs, RPD calcs, completeness measures, flag application, CDX export |

Each script should accept year-specific inputs (file paths, sample dates) as arguments or via a config object so they work for multiple years.

---

## appendix_a.qmd Audit Results (March 2026)

Full audit was completed. Section-by-section status:

| Lines | Section | Status |
|---|---|---|
| 1–488 | SGS/ALS ingestion (Parts A–E) | Working. Dense — priority candidate for extraction to `R/ingest_sgs_als.R` |
| 490–953 | FC and TSS ingestion | Mostly working. TSS has a documented QA gap: SWWTP did not report lab QA results (blanks, duplicates, check standards) as required by QAPP in 2021/2022. Lab QA export block is commented out. |
| 955–1213 | Lookup joins + first AQWMS export | Working, but the full column-rename block is redundantly repeated later for the CDX export. Consolidate in the script extraction step. |
| 1238–2380 | QA/QC Checklist (Questions 1–42) | Mostly complete. Two gaps: (1) Completeness Measure B block is `eval = F` and produces nonsensical results — unfinished. (2) Q39 (range check / outliers) defers to visual review in report chapters rather than doing anything here. |
| 2381–2503 | Flagging and flagged export | Working in principle. Ca/Mg/Fe unit errors are baked into the data at this point and pass through uncorrected. |
| 2505–2650 | CDX export (Results and Activities) | Working but nearly duplicates the format block at lines 955–1213. Consolidate. |
| 2675–2756 | Post-hoc corrections + `knitr::knit_exit()` | Incomplete placeholder. The chunk is `eval = F` and never executes. The `knitr::knit_exit()` at line 2752 stops rendering. |

---

## Project Overview

The **Kenai River Baseline Water Quality Monitoring Project** is a long-term cooperative monitoring initiative led by the Kenai Watershed Forum (KWF) in south-central Alaska. Biannual (spring and summer) sampling has occurred at 21 sites since 2000. The current deliverable is a comprehensive Quarto-based report covering 2000–2021 data.

-   **GitHub:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
-   **QA/QC repo (separate):** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
-   **Public data:** https://www.waterqualitydata.us/ (organization: "Kenai Watershed Forum")

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
│   ├── AQWMS/             # 2021 lab data & WQX templates
│   ├── 2021_wqx_data/     # Raw lab results (SGS North America, Soldotna WWTP, Taurianen Engineering)
│   ├── outliers/          # Manually identified outliers spreadsheet
│   ├── regulatory_limits/ # Hardness-dependent thresholds
│   └── baseline_sites.csv # Site metadata (21 sites: 13 mainstem + 8 tributaries)
└── output/
    ├── analysis_format/   # Processed data ready for analysis
    └── regulatory_values/ # Combined regulatory threshold files
```

## Parameters Monitored

| Category | Parameters |
|------------------------------------|------------------------------------|
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

## QA/QC Notes

-   **Flagging:** The correct WQX convention is already implemented. `Result Qualifier` = lab qualifiers (U, J, =). `Result Status ID` = KWF QA/QC decision (Accepted/Rejected). Do not add FQC or other custom codes — the existing design is complete and correct.
-   **Outliers:** Visually-identified outliers (especially pre-2014 legacy data lacking field blanks) are excluded from visualizations but retained in archived data.
-   **Lab qualifiers** (from labs) are distinct from **KWF QA/QC flags** — treat them separately.

## Known Data Issues (as of March 2026)

-   **Ca/Mg/Fe unit errors:** 2021 values for Calcium, Magnesium, and Iron have unit mismatches (ug/L vs mg/L) in the ALS subcontracted data. This corrupts hardness calculations and breaks hardness-dependent regulatory thresholds. Fix in ingestion, not as a post-hoc patch.
-   **2021 EPA WQX upload** was deleted and needs to be re-uploaded after corrections.
-   **Hydrocarbon data** missing from 2025 WQP download (uploaded Jan 2024 but not appearing in download).
-   **Completeness Measure B** calculation is incomplete (`eval = F` in appendix_a.qmd, line ~2111).
-   **TSS lab QA gap:** SWWTP did not report required lab QA results for TSS in 2021/2022. Note in code, address for 2023+.

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `magrittr`, `dataRetrieval`, `xfun`

Note: `magrittr` pipe (`%>%`) appears in existing legacy code. For all new code, use the base pipe (`|>`). Do not mass-convert existing `%>%` usage — leave legacy files as-is to avoid unnecessary git churn and risk.

## Governance Documents (in `other/agent_context/`)

-   **QAPP** — Quality Assurance Project Plan, approved by ADEC and EPA Region 10 (2023 + April 2024 addendum)
-   **MOU** — Baseline Water Quality MOU 2025 Final
-   **Funding Proposal** — KWF 2024 BOR WaterSMART CWMP Proposal
