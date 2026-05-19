# Kenai River WQX — Data Architecture

Load this file when navigating data storage, regulatory thresholds, or R package choices.

------------------------------------------------------------------------

## Data Storage Structure

```         
other/
├── agent_context/         # Governance docs, QAPP, session_log.md
├── input/
│   ├── WQX_downloads/     # Downloaded EPA data (excluded from GitHub)
│   ├── wqx_templates/     # WQX reference files, matching tables, lookup CSVs
│   │   ├── wqx_qaqc/      # QA/QC info spreadsheets
│   │   └── trip_blank_crews_{year}.csv  # Year-specific trip blank crew assignments
│   ├── 2021_wqx_data/     # Raw lab results (SGS, SWWTP, Taurianen)
│   ├── outliers/          # Manually identified outliers
│   ├── regulatory_limits/ # master_reg_limits.xlsx + hardness-dependent CSVs
│   └── baseline_sites.csv # 22 sites: 13 mainstem + 9 tributaries
└── output/
    ├── wqx_formatted/             # CDX submission-ready: results_activities.csv, project.csv, station.csv
    │   └── intermediate/          # Pipeline intermediates (not for upload)
    ├── analysis_format/           # Processed data for analysis
    └── regulatory_values/         # Combined regulatory threshold files
```

**Raw data rule:** Never modify files in `other/input/`. All transformations happen in code.

------------------------------------------------------------------------

## Regulatory Threshold Architecture

`master_reg_limits.xlsx` is the **single source of truth** for all regulatory threshold data.

| Sheet | Contents |
|----|----|
| `static_regulatory_values` | All static thresholds. Categories: `static_metals`, `hydrocarbons`, `nutrients`, `other`, `total_metals_aquatic_life` (Iron), `field_bio_standards` (Water Temp, FC) |
| `calculated_regulatory_values` | Hardness-dependent formulas (Cd, Cr, Cu, Pb, Zn) |
| `diss_metals_hard_parameters` | Parameters for hardness calculations |
| `standard_types` | Display labels and regulatory authority for all standard type codes. Six rows flagged `review_needed = Y` (Task 5). |
| `pick_list` | Legacy — superseded by `standard_types` |

**Adding a new threshold:** add row to `static_regulatory_values`, confirm `standard_types` entry, add export block in `reg_limits.qmd` if new category needed, add resulting CSV to `bind_rows()` in `static_boxplot_function.R`. See Iron and `field_bio_standards` entries as examples.

------------------------------------------------------------------------

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `dataRetrieval`, `TADA`, `xfun`

Use base pipe `|>` for all new code. Do not mass-convert legacy `%>%` usage.
