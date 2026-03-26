# Kenai River Baseline Water Quality Monitoring — Project Context for Posit Assistant

## Next Session — Start Here

**Goal:** Revise, complete, and solidify `appendix_a.qmd` as a clean, well-documented pipeline for: (1) intake raw lab data → (2) apply QA/QC → (3) flag data → (4) format for EPA CDX upload. Once stable, extract the logic into sourced `.R` scripts so it can be reused for each year in the separate qaqc project.

### Completed this session (March 25, 2026)

- ~~Inspect and correct the Ca/Mg/Fe unit errors in the 2021 data.~~ **DONE.** Fix applied in the SGS ingestion block of `appendix_a.qmd`. See Known Data Issues for full details.

### Tasks for next session (in order)

1. **Re-render `appendix_a.qmd`** to regenerate the corrected output CSVs (`2021_kwf_baseline_results_aqwms.csv` and `2021_export_data_flagged.csv`). Spot-check Ca/Mg/Fe values and hardness in the rendered output before proceeding.
2. **Re-upload 2021 data to EPA CDX** using the regenerated export files. The previous upload was deleted pending this correction.
3. **Fix the `Completeness Measure B` block** (~line 2141 after recent edits, currently `eval = F`). The block produces nonsensical results and is unfinished. Understand what it is supposed to calculate (per the QAPP), diagnose why results are wrong, and either fix or document as a known gap.
4. **Address the post-hoc corrections placeholder** (~line 2705, `eval = F`). This chunk never executes and `knitr::knit_exit()` at the end of the file halts rendering prematurely. Decide whether to complete, remove, or document this section.
5. **Extract ingestion logic to `.R` scripts** (lower priority — see File Structure section below). Do not start this until steps 1–3 are complete.

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

Full audit was completed. Section-by-section status. **Note: line numbers are approximate and shifted ~30 lines after March 2026 edits.**

| Approx. Lines | Section | Status |
|---|---|---|
| 1–518 | SGS/ALS ingestion (Parts A–E) | Working. Includes Ca/Mg/Fe unit correction (March 2026). Dense — priority candidate for extraction to `R/ingest_sgs_als.R`. |
| 520–983 | FC and TSS ingestion | Mostly working. TSS has a documented QA gap: SWWTP did not report lab QA results (blanks, duplicates, check standards) as required by QAPP in 2021/2022. Lab QA export block is commented out. |
| 985–1243 | Lookup joins + first AQWMS export | Working, but the full column-rename block is redundantly repeated later for the CDX export. Consolidate in the script extraction step. |
| 1268–2410 | QA/QC Checklist (Questions 1–42) | Mostly complete. Two gaps: (1) Completeness Measure B block is `eval = F` and produces nonsensical results — unfinished (~line 2141). (2) Q39 (range check / outliers) defers to visual review in report chapters rather than doing anything here. |
| 2411–2533 | Flagging and flagged export | Working. Ca/Mg/Fe unit error is now corrected upstream at ingestion (March 2026); this section no longer needs a workaround. |
| 2535–2680 | CDX export (Results and Activities) | Working but nearly duplicates the format block at lines 985–1243. Consolidate. |
| 2705–2786 | Post-hoc corrections + `knitr::knit_exit()` | Incomplete placeholder. The chunk is `eval = F` and never executes. The `knitr::knit_exit()` near the end of the file stops rendering prematurely. |

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

-   **Ca/Mg/Fe unit errors: RESOLVED (March 2026).** Root cause identified: the summer 2021 SGS EDD reported dissolved Calcium, Iron, and Magnesium (method EP200.8) with unit label `ug/L`, but the numeric values were on the mg/L scale (e.g., Ca = 16,200 "ug/L" at RM 0 NNC = 16.2 mg/L, which is physically plausible; 16.2 ug/L would be unrealistically low for freshwater). Spring 2021 SGS data contained no dissolved Ca/Fe/Mg runs (confirmed). ALS total metals were correctly labeled mg/L throughout. Fix applied in `appendix_a.qmd` at the SGS ingestion step (Part A): rows where `analyte %in% c("Calcium", "Iron", "Magnesium")` and `units == "ug/L"` are divided by 1000 and relabeled `mg/L`. Corrected hardness values for summer 2021 are 37–380 mg/L as CaCO3 (plausible); erroneous values were 37,000–380,000 mg/L.
-   **⚠️ Action required:** Both `other/output/aqwms_formatted_results/2021_kwf_baseline_results_aqwms.csv` and `other/output/aqwms_formatted_results/2021_export_data_flagged.csv` were generated before this fix and contain the erroneous values. These files must be regenerated by re-rendering `appendix_a.qmd` before the EPA CDX re-upload.
-   **2021 EPA WQX upload** was deleted and needs to be re-uploaded after corrections and after the output CSVs above are regenerated.
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
