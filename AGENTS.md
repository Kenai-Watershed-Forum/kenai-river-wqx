# Kenai River Baseline Water Quality Monitoring — Project Context for Posit Assistant

## Next Session — Start Here

Run a diagnostic pass on the current WQP download to assess what 2021 data is actually present. This will determine whether to work upstream (fixing `appendix_a.qmd` export → CDX re-upload) or downstream (re-integrating a fresh WQP download into the report). Use `dataRetrieval` to query KWF organization records and check for 2021 coverage across parameters, especially hydrocarbons (known missing) and metals with unit issues (Ca, Mg, Fe).

---

## Project Overview

The **Kenai River Baseline Water Quality Monitoring Project** is a long-term cooperative monitoring initiative led by the Kenai Watershed Forum (KWF) in south-central Alaska. Biannual (spring and summer) sampling has occurred at 21 sites since 2000. The current deliverable is a comprehensive Quarto-based report covering 2000–2021 data.

- **GitHub:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
- **QA/QC repo (separate):** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
- **Public data:** https://www.waterqualitydata.us/ (organization: "Kenai Watershed Forum")

## Collaboration Standards (Important)

This is a 25+ year monitoring project. Code must remain readable and maintainable by future scientists.

- **Before implementing any code, explain what the proposed code does and why.** Wait for user confirmation before applying changes.
- Prioritize code clarity and readability over cleverness or brevity.
- All analytical decisions should be transparent and documented.

## Report Structure

A Quarto book/website publishing to HTML and DOCX simultaneously. Key source files:

| File | Purpose |
|---|---|
| `_quarto.yml` | Project configuration |
| `index.qmd` | Front matter / introduction |
| `data_sourcing.qmd` | Data download and preparation pipeline |
| `data_qa_qc.qmd` | QA/QC overview |
| `reg_limits.qmd` | Regulatory limits framework |
| `appendix_a.qmd` | Detailed 2021 QA/QC example |
| `functions/static_boxplot_function.R` | Reusable boxplot function |

Parameter result chapters (one per water quality parameter) follow a consistent structure: boxplot, regulatory comparison, downloadable data table.

## Data Pipeline Summary

1. **Download** from EPA Water Quality Portal via `dataRetrieval` package (transitioning to TADA)
2. **Filter** to KWF organization records
3. **Harmonize** units (e.g., µg/L → mg/L for metals)
4. **Handle non-detects** using half the MDL/MRL value
5. **Classify season** by Julian day: ≤ 155 = Spring, > 155 = Summer (boundary ≈ June 4)
6. **Remove** air temperature observations and manually-identified outliers
7. **QA/QC flagging** against QAPP Data Quality Objectives (DQOs)
8. **Submit** cleaned data to EPA WQX via CDX

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
|---|---|
| Dissolved Metals | Arsenic, Cadmium, Chromium, Copper, Lead, Zinc |
| Total Metals | Calcium, Iron, Magnesium |
| Nutrients | Nitrate + Nitrite, Phosphorus |
| Hydrocarbons | BTEX (Benzene, Toluene, Ethylbenzene, m/p-Xylene, o-Xylene) |
| Biological | Fecal Coliform |
| Field Parameters | pH, Specific Conductance, TSS, Turbidity, Water Temperature, Dissolved Oxygen |

## Regulatory Limits

- Standards are Alaska and Federal freshwater aquatic life criteria
- Some metal thresholds (Cadmium, Chromium, Copper, Lead, Zinc) are **hardness-dependent** — threshold values are calculated per-sample based on measured hardness
- Regulatory values are stored in `output/regulatory_values/`
- Session variables `static_metals_thresholds`, `static_metals_reg_vals`, `diss_metals_hard_param` hold processed threshold data

## QA/QC Notes

- **Flagging:** Transitioning from binary Y/N flags to standardized codes (e.g., `FQC` for QC failures). Flags should be retained in downloadable tables but flagged data excluded from visualizations.
- **Outliers:** Visually-identified outliers (especially pre-2014 legacy data lacking field blanks) are excluded from visualizations but retained in archived data.
- **Lab qualifiers** (from labs) are distinct from **KWF QA/QC flags** — treat them separately.

## Known Data Issues (as of early 2026)

- **2021 EPA WQX upload** had inconsistencies; a full delete and re-upload is planned
- Hydrocarbon data missing from 2025 WQP download (uploaded Jan 2024 but not appearing)
- Unit conversion errors for some 2021 measurements (Calcium, Magnesium, Iron)
- Hardness calculation issues with 2021 metal data affecting hardness-dependent thresholds

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `magrittr`, `dataRetrieval`, `xfun`

Note: `magrittr` pipe (`%>%`) appears in existing legacy code. For all new code, use the base pipe (`|>`). Do not mass-convert existing `%>%` usage — leave legacy files as-is to avoid unnecessary git churn and risk.

## Governance Documents (in `other/agent_context/`)

- **QAPP** — Quality Assurance Project Plan, approved by ADEC and EPA Region 10 (2023 + April 2024 addendum)
- **MOU** — Baseline Water Quality MOU 2025 Final
- **Funding Proposal** — KWF 2024 BOR WaterSMART CWMP Proposal
