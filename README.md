# Water Quality Assessment of the Kenai River Watershed, 2000–2025

**Kenai Watershed Forum (KWF) — Baseline Water Quality Monitoring Project**

[![Online Report](https://img.shields.io/badge/Online%20Report-kenai--watershed--forum.github.io-blue)](https://kenai-watershed-forum.github.io/kenai-river-wqx-report-v2/)

------------------------------------------------------------------------

## Overview

This repository contains source code and data for a comprehensive water quality assessment of the Kenai River watershed in south-central Alaska, covering biannual (spring and summer) sampling from 2000 through 2025. The report is produced as a [Quarto book](https://quarto.org/docs/books/) that renders to both an interactive HTML website and a downloadable Word document.

The project is a long-term cooperative monitoring initiative led by KWF, with funding from a coalition of local agencies. It is designed to be updated annually as new data become available.

**Read the report online:** <https://kenai-watershed-forum.github.io/kenai-river-wqx/>

Monitoring Program

-   **22 sampling sites** on the Kenai River mainstem and major tributaries
-   **Biannual sampling** (spring and summer) since 2000
-   **Partner laboratories:** SGS North America, Analytica, ALS Environmental, Soldotna Wastewater Treatment Plant, Taurianen Engineering
-   **Data submitted** to the [EPA Water Quality Exchange (WQX)](https://www.epa.gov/waterdata/water-quality-data-wqx) and publicly available through the [EPA Water Quality Portal](https://www.waterqualitydata.us/) (organization: "Kenai Watershed Forum")
-   **Public-facing data viewer:** [How's My Waterway](https://mywaterway.epa.gov/) — search for Kenai River (Data Sync in Progress as of March 2026)

### Parameters Monitored

| Category | Parameters |
|------------------------------------|------------------------------------|
| Dissolved Metals | Arsenic, Cadmium, Chromium, Copper, Lead, Zinc |
| Total Metals | Calcium, Iron, Magnesium |
| Nutrients | Nitrate + Nitrite, Phosphorus |
| Hydrocarbons | BTEX (Benzene, Toluene, Ethylbenzene, Xylenes) |
| Biological | Fecal Coliform |
| Field Parameters | pH, Specific Conductance, TSS, Turbidity, Water Temperature, Dissolved Oxygen |

------------------------------------------------------------------------

## Repository Structure

```         
kenai-river-wqx/
├── _quarto.yml                  # Quarto book configuration
├── index.qmd                    # Preface / introduction
├── intro.qmd, methods.qmd, ...  # Report narrative chapters
├── parameters/                  # One .qmd chapter per water quality parameter
├── appendix_a.qmd               # 2021 QA/QC pipeline (SGS + ALS lab data → EPA CDX)
├── appendix_b.qmd               # Supplementary appendix
├── functions/
│   ├── static_boxplot_function.R  # Reusable boxplot function (interactive + static)
│   └── threshold_table.R          # Regulatory threshold display tables
└── other/
    ├── agent_context/             # Governance docs (QAPP, MOU, funding proposal)
    ├── input/
    │   ├── 2021_wqx_data/         # Raw lab data (SGS, ALS, SWWTP, Taurianen)
    │   ├── wqx_templates/         # WQX reference files and lookup tables
    │   ├── regulatory_limits/     # Hardness-dependent metal thresholds
    │   └── baseline_sites.csv     # Site metadata (22 sites)
    └── output/
        ├── wqx_formatted/         # CDX-ready upload files (results, project, station CSVs)
        └── analysis_format/       # Processed data for report figures and tables
```

> **Note:** Raw data files in `other/input/` are kept exactly as received from laboratories. All transformations are applied in code.

------------------------------------------------------------------------

## Data Pipeline

```         
Raw lab EDDs (SGS, ALS)
        │
        ▼
  appendix_a.qmd
  ├── Ingest & normalize (units, site names, method codes)
  ├── Join WQX lookup tables (lat/long, fractions, detection conditions)
  ├── QA/QC checks (holding times, RPDs, matrix spikes, completeness)
  ├── Apply flags (Result Status ID: Accepted / Rejected)
  └── Export CDX submission files
        │
        ▼
  EPA WQX / CDX Upload
        │
        ▼
  EPA Water Quality Portal → downloaded by report on render
        │
        ▼
  Parameter chapters (visualizations, regulatory comparisons, tables)
```

Annual EPA CDX submissions require three files uploaded together: 1. `results_activities.csv` — QA/QC'd lab results 2. `project.csv` — project metadata 3. `station.csv` — monitoring location metadata

------------------------------------------------------------------------

## Regulatory Context

Water quality results are compared against Alaska and Federal freshwater aquatic life criteria. Standards for five metals (Cadmium, Chromium, Copper, Lead, Zinc) are **hardness-dependent** and are calculated per sample using measured hardness values.

The project Quality Assurance Project Plan (QAPP) has been reviewed and approved by the Alaska Department of Environmental Conservation (ADEC) and EPA Region 10. ADEC uses data submitted through this pipeline in the [Alaska Integrated Report](https://dec.alaska.gov/water/water-quality/integrated-report/), which informs water body management decisions statewide.

------------------------------------------------------------------------

## Rendering the Report

This project uses [Quarto](https://quarto.org/). To render locally:

``` bash
quarto render
```

Rendered output goes to `docs/` (HTML) and is hosted on GitHub Pages.

**Requirements:** - R (≥ 4.x) with packages: `tidyverse`, `readxl`, `writexl`, `openxlsx`, `DT`, `plotly`, `janitor`, `lubridate`, `dataRetrieval`, `TADA`, `xfun` - Quarto (≥ 1.4)

Large raw data files in `other/input/WQX_downloads/` are excluded from version control (`.gitignore`). These are downloaded via the `dataRetrieval` package on first render using `data_sourcing.qmd`.

------------------------------------------------------------------------

## Related Repositories

| Repository | Purpose |
|------------------------------------|------------------------------------|
| [kenai-river-wqx](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx) | This repo — report source and 2021 QA/QC pipeline |
| [kenai-river-wqx-qaqc](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc) | Annual QA/QC processing for data years other than 2021 |

------------------------------------------------------------------------

## Project History

Comprehensive reports have previously been completed for: [2007 report](https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/) and McCard (2007) - [2016 report](https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/) — Guerron-Orejuela (2016)

This repository covers the update through 2025 and establishes a reproducible, annually-extensible pipeline for future reports.

------------------------------------------------------------------------

## Contributing and Reporting Issues

This is an active, long-running monitoring project. Contributions and issue reports from collaborators, partner agencies, and the broader scientific community are welcome.

### Reporting a data or code issue

Please open a [GitHub Issue](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx/issues) to report:

-   Errors or inconsistencies in reported water quality values
-   Broken code or failed renders
-   Incorrect regulatory threshold values
-   Suggestions for improving the QA/QC pipeline or report

When opening an issue, please include: the affected file or report section, a description of the problem, and (if applicable) the relevant parameter, site, or year.

### Contributing code or documentation

1.  Fork this repository and create a descriptive branch (e.g., `fix/arsenic-units`, `feature/2022-data`).
2.  Make your changes, keeping raw input files in `other/input/` untouched. All data transformations should happen in code.
3.  Test by rendering the report locally (`quarto render`) and confirming no new warnings or errors.
4.  Open a pull request with a clear description of what changed and why.

### Important notes for contributors

-   **QA/QC flag decisions** (accepting or rejecting individual results) are the responsibility of KWF staff scientists. If you believe a flagging decision is incorrect, open an issue rather than changing `other/input/wqx_templates/wqx_qaqc/` files directly.
-   **Raw input data** in `other/input/` must not be modified. Corrections are applied in code (see `appendix_a.qmd`).
-   For questions about the project's scientific methods or regulatory context, refer to the QAPP in `other/agent_context/`.

------------------------------------------------------------------------

## Contact

**Kenai Watershed Forum** <https://www.kenaiwatershed.org>

Project home page: https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
