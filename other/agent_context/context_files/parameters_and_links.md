# Kenai River WQX — Parameters, Governance Docs, External Links

Load this file when you need the full parameter list, the governance document index, or external reference links (not needed for most coding/data tasks).

------------------------------------------------------------------------

## Parameters Monitored

| Category | Parameters |
|----|----|
| Dissolved Metals | Arsenic, Cadmium, Chromium, Copper, Lead, Zinc |
| Total Metals | Calcium, Iron, Magnesium |
| Nutrients | Nitrate + Nitrite, Phosphorus |
| Hydrocarbons | BTEX (Benzene, Toluene, Ethylbenzene, m/p-Xylene, o-Xylene) |
| Biological | Fecal Coliform |
| Field Parameters | pH, Specific Conductance, TSS, Turbidity, Water Temperature, Dissolved Oxygen |

------------------------------------------------------------------------

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `dataRetrieval`, `TADA`, `xfun`

Use base pipe `|>` for all new code. Do not mass-convert legacy `%>%` usage.

------------------------------------------------------------------------

## Governance Documents

Original PDFs are in `other/agent_context/`. Text-extracted `.md` versions (preferred for AI ingestion — lower token cost) are in `other/documents/md/`. Always load from `other/documents/md/` when available.

| Document | Markdown path | Notes |
|----|----|----|
| CALM (Alaska Consolidated Assessment and Listing Methodology, rev. March 2021) | `other/documents/md/calm-rev-2021.md` |  |
| ADEC Water Quality Standards — 18 AAC 70 | `other/documents/md/ADEC-18-aac-70.md` |  |
| QAPP (approved ADEC + EPA Region 10, 2023 + April 2024 addendum) | `other/documents/md/QAPP-v3-2023-with-Addendum-April-2024.md` |  |
| MOU — Baseline Water Quality MOU 2025 Final | `other/documents/md/Kenai-River-Baseline-WQ-MOU-2025.md` |  |
| DL/LOD/LOQ Interpretation — SGS Laboratories | `other/documents/md/DL-LOD-LOQ-Interpretation-SGS.md` |  |
| Kenai Baseline WQ Assessment 2016 | `other/documents/md/Kenai-Baseline-WQ-Assessment-2016.md` |  |
| Kenai River 2021 Monitoring Field Report | `other/documents/md/kenai-river-2021-field-report.md` |  |
| Alaska WQ Criteria Manual for Toxic Substances 2022 | `other/documents/md/alaska-water-quality-criteria-manual-2022.md` | Converted from ADEC web version (text layer present); local copy in agent_context/ is scanned |
| Kenai Baseline WQ Assessment 2007 | PDF only — scanned, no text layer | `other/agent_context/Kenai Watershed Forum Baseline Water Quality Assessment 2007.pdf` |
| Funding Proposal — KWF 2024 BOR WaterSMART CWMP | PDF only | `other/agent_context/` |

**Convention:** When new PDFs are added as reference/governance documents, convert to `.md` using `pdftools::pdf_text()` and place in `other/documents/md/`. Raw data PDFs (field forms, lab reports, COC documents) do not need conversion.

------------------------------------------------------------------------

## Useful External Links

- ADEC Kenai River "exceptional river" press release (Nov 2023): https://dec.alaska.gov/commish/newsroom/23-11-kenai-river-an-exceptional-river-with-clean-water/
- ADEC Ambient Water Quality Data: https://dec.alaska.gov/water/water-quality/ambient-water-quality-data
- ADEC Integrated Report / CALM methodologies: https://dec.alaska.gov/water/water-quality/integrated-report/
