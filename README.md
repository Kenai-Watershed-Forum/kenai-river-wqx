# Water Quality Assessment of the Kenai River Watershed, 2000–2025

**Kenai Watershed Forum (KWF): Baseline Water Quality Monitoring Project**

[![Online Report](https://img.shields.io/badge/Online%20Report-kenai--watershed--forum.github.io-blue)](https://kenai-watershed-forum.github.io/kenai-river-wqx/)

------------------------------------------------------------------------

## Overview

This repository contains source code and data for a water quality assessment of the Kenai River watershed in south-central Alaska, covering biannual (spring and summer) sampling from 2000 through 2025. The report is produced as a [Quarto book](https://quarto.org/docs/books/) that renders to both an interactive HTML website and a downloadable Word document.

The project is a long-term cooperative monitoring initiative led by KWF, with funding from a coalition of local agencies. It is designed to be updated annually as new data become available.

**Read the report online:** <https://kenai-watershed-forum.github.io/kenai-river-wqx/>

Monitoring Program

- **22 sampling sites** on the Kenai River mainstem and major tributaries
- **Biannual sampling** (spring and summer) since 2000
- **Partner laboratories:** SGS North America, Analytica, ALS Environmental, Soldotna Wastewater Treatment Plant, Taurianen Engineering
- **Data submitted** to the [EPA Water Quality Exchange (WQX)](https://www.epa.gov/waterdata/water-quality-data-wqx) and publicly available through the [EPA Water Quality Portal](https://www.waterqualitydata.us/) (organization: "Kenai Watershed Forum")
- **Public-facing data viewer:** [How's My Waterway](https://mywaterway.epa.gov/) (search for Kenai River). Corrected 2021 data was submitted to CDX in October 2026 and is awaiting WQP indexing.

### Parameters Monitored

| Category | Parameters |
|----|----|
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
├── _quarto.yml                  # Quarto book configuration (HTML and DOCX profiles)
├── index.qmd                    # Preface
├── chapters/                    # Narrative and methods chapters
│   ├── data_sourcing.qmd        # Reads the local WQP cache, builds analysis datasets
│   ├── data_qa_qc.qmd, reg_limits.qmd, methods.qmd, ...
│   └── appendix_a.qmd, appendix_b.qmd
├── parameters/                  # One .qmd chapter per water quality parameter
├── templates/
│   ├── _parameter_chunk.Rmd     # Shared template knitted by every parameter chapter
│   └── _trend_chunk.Rmd         # Optional trend analysis section (opt-in)
├── functions/
│   ├── refresh_wqp_data.R       # Manual script: refresh the local WQP cache
│   ├── static_boxplot_function.R, threshold_table.R, trend_plots.R, ...
└── other/
    ├── agent_context/           # Governance docs, AI assistant context and session log
    ├── documents/               # Reference documents (text-extracted copies in md/)
    ├── input/
    │   ├── WQX_downloads/wqp_live/  # Gitignored WQP cache (see "Data Sourcing")
    │   ├── 2021_wqx_data/, 2025_data/  # Raw lab data as received
    │   ├── wqx_templates/       # WQX reference files and lookup tables
    │   ├── regulatory_limits/   # Regulatory thresholds (master_reg_limits.xlsx)
    │   └── baseline_sites.csv   # Site metadata
    └── output/
        ├── wqx_formatted/       # CDX-ready upload files
        └── analysis_format/     # Processed data for report figures and tables
```

> **Note:** Raw data files in `other/input/` are kept exactly as received from laboratories. All transformations are applied in code.

------------------------------------------------------------------------

## Data Pipeline

The pipeline spans two repositories. Annual QA/QC and CDX preparation happen in the QA/QC repo (this repo holds the 2021 pipeline in `chapters/appendix_a.qmd`); the report reads only from the public Water Quality Portal.

```
Raw lab EDDs --> QA/QC, WQX formatting --> EPA CDX / WQX Web upload
                                                   |
                                                   v
                                    EPA Water Quality Portal (WQP)
                                                   |
                          functions/refresh_wqp_data.R  (manual)
                                                   |
                                                   v
                       other/input/WQX_downloads/wqp_live/  (local cache)
                                                   |
                                                   v
                 chapters/data_sourcing.qmd --> parameter chapters
```

Annual CDX submissions require three files: `results_activities.csv` (QA/QC'd lab results), `project.csv`, and `station.csv`.

### Data Sourcing

Rendering never calls the WQP API. `chapters/data_sourcing.qmd` reads a local, gitignored cache (`narrowresult.csv`, `station.csv`, `detection_limits.csv`, `refresh_metadata.csv`) in `other/input/WQX_downloads/wqp_live/`. To refresh it, run:

``` r
source("functions/refresh_wqp_data.R")
```

Refresh whenever new data is expected on WQP (organization `KENAI_WQX`, retrieved with `dataRetrieval`). If the cache is missing, the render stops with instructions. After a refresh, downstream outputs (`baseline_export_format.csv`, `baseline_analysis_format.csv`, etc.) regenerate and may show large diffs; this is expected.

### Trend Analyses (opt-in, pilot)

A parameter chapter can include a "Trends over time and space" section by setting `trend_analysis <- TRUE` before knitting `templates/_parameter_chunk.Rmd`. It reports per-site, per-season Mann-Kendall tests with Sen slopes and an interactive example figure; non-detects are excluded. Helpers are in `functions/trend_plots.R`. Currently only `parameters/zinc.qmd` is opted in, and the statistical approach is still under review.

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

Rendered HTML output goes to `docs/` locally, but `docs/` is gitignored and never committed to `main`. To publish the site, run:

``` bash
quarto publish gh-pages
```

This pushes the rendered site to a dedicated `gh-pages` branch, which is what GitHub Pages serves from. This keeps `main`'s history free of large rendered-HTML commits.

**Requirements:** Quarto (>= 1.4) and R (>= 4.x) with the packages used by the chapters, including `tidyverse`, `readxl`, `writexl`, `openxlsx`, `DT`, `plotly`, `janitor`, `lubridate`, `dataRetrieval`, `TADA`, `xfun`, and `leaflet`. Trend analyses additionally use the packages loaded in `functions/trend_plots.R`.

Before the first render, create the WQP cache (see "Data Sourcing" above). Large files in `other/input/WQX_downloads/` are excluded from version control.

------------------------------------------------------------------------

## Related Repositories

| Repository | Purpose |
|----|----|
| [kenai-river-wqx](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx) | This repo: report source and 2021 QA/QC pipeline |
| [kenai-river-wqx-qaqc](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc) | Annual QA/QC processing and CDX preparation for other data years (a per-year-repo template is planned to replace it) |

------------------------------------------------------------------------

## Project History

Earlier reports on this monitoring program:

- [2007 report](https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/) (McCard, 2007)
- [2016 report](https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/) (Guerron-Orejuela, 2016)

This repository covers the update through 2025 and is set up to be extended each year.

------------------------------------------------------------------------

## Contributing and Reporting Issues

This is an active, long-running monitoring project. Contributions and issue reports from collaborators, partner agencies, and the broader scientific community are welcome.

### Reporting a data or code issue

Please open a [GitHub Issue](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx/issues) to report:

- Errors or inconsistencies in reported water quality values
- Broken code or failed renders
- Incorrect regulatory threshold values
- Suggestions for improving the QA/QC pipeline or report

When opening an issue, please include: the affected file or report section, a description of the problem, and (if applicable) the relevant parameter, site, or year.

### Contributing code or documentation

1.  Fork this repository and create a descriptive branch (e.g., `fix/arsenic-units`, `feature/2022-data`).
2.  Make your changes, keeping raw input files in `other/input/` untouched. All data transformations should happen in code.
3.  Test by rendering the report locally (`quarto render`) and confirming no new warnings or errors.
4.  Open a pull request with a clear description of what changed and why.

### Important notes for contributors

- **QA/QC flag decisions** (accepting or rejecting individual results) are the responsibility of KWF staff scientists. If you believe a flagging decision is incorrect, open an issue rather than changing `other/input/wqx_templates/wqx_qaqc/` files directly.
- **Raw input data** in `other/input/` must not be modified. Corrections are applied in code (see `chapters/appendix_a.qmd`).
- For questions about the project's scientific methods or regulatory context, refer to the QAPP in `other/agent_context/`.

------------------------------------------------------------------------

## Contact

**Kenai Watershed Forum** <https://www.kenaiwatershed.org>

Project home page: https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
