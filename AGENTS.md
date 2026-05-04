# Kenai River Baseline Water Quality Monitoring — Project Context

## Repo Relationship

This repo is one of two that together form the full data pipeline for Kenai River Baseline Water Quality Monitoring:

- **`kenai-river-wqx`** (this repo — report repo):
  https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
  Hosts the Quarto book that integrates and displays long-term monitoring data.
  Data is accessed directly from EPA's Water Quality Portal (WQP), which serves
  as the authoritative public source. The report does not hold raw data locally;
  it reads from WQP and applies regulatory thresholds, visualizations, and
  narrative interpretation.

- **`kenai-river-wqx-qaqc`** (qaqc repo):
  https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
  Prepares annual monitoring data for submission to EPA WQX via CDX. Each year's
  pipeline ingests raw lab EDDs, applies QA/QC, formats data to WQX schema, and
  produces CDX-ready upload files. Once submitted, that data becomes publicly
  available through WQP — where the report repo accesses it.

The two repos form a complete pipeline: qaqc repo submits data to EPA WQX →
EPA publishes to WQP → report repo reads from WQP and displays it. Changes
in either repo — new annual pipelines, CDX corrections, WQX submission status,
regulatory threshold updates — are relevant to both. This shared context file
keeps the AI assistant aware of work across both repos regardless of which one
is active.

**`other/agent_context/session_log.md`** is auto-synced to the qaqc repo via
GitHub Actions on every push to main that touches it. Each repo maintains its
own `AGENTS.md` (not synced); only `session_log.md` is shared.
**Edit `session_log.md` only from this repo (`kenai-river-wqx`).**

------------------------------------------------------------------------

## Companion Files (load on demand, not every session)

- `other/agent_context/session_log.md` — full log of completed session work, resolved data issues, appendix_a.qmd audit, CDX delete workflow, HMW architecture detail, planned script refactor structure
- `other/documents/sample_fraction_correction_handoff.md` — CDX fraction correction handoff for the qaqc repo

------------------------------------------------------------------------

## Next Session Priorities

**EPA WQX sync issue — PARTIALLY RESOLVED but batch delete still blocked.** WQP warehouse refresh completed 2026-05-01 (Kevin Christian email). ETL pipeline restored for new submissions. However, batch delete of 835 orphaned 2021 records still fails: "Domain Value Invalid" for all 835 Activity IDs (attempted 2026-05-04, datasetUid=113161). Root cause: records exist in WQP/STORET but are absent from WQX Web's internal DB. ETL fix restored WQX → WQP sync but did not retroactively restore these records into WQX Web. Follow-up email sent to wqx@epa.gov 2026-05-04 requesting EPA either (1) delete orphaned records from WQP directly, or (2) restore them into WQX Web so batch delete can proceed.

**GitHub Actions session_log.md sync — ALMOST COMPLETE (2026-05-04).** PAT created (`kwf-report-to-qaqc-sync`, scoped to qaqc repo, Contents R/W). Secret `QAQC_SYNC_PAT` added to `kenai-river-wqx`. Workflow file created at `.github/workflows/sync-agent-context.yml`. qaqc repo `AGENTS.md` updated with Repo Relationship section; `other/agent_context/.gitkeep` added. **Remaining: commit and push both repos** (qaqc first, then report). See push commands in session log.

1.  **Complete GitHub Actions sync setup (start here)** — push qaqc repo, then report repo. Verify Action runs successfully in the Actions tab on GitHub.
2.  **Task 18 (HIGH)** — Create `templates/pipeline_template.qmd` in the qaqc repo. This is the canonical single-QMD-per-year pipeline. See Pipeline Architecture section below for full design.
3.  **Task 1c (HIGH)** — CALM 5-year window sample count check. Count `result_status_identifier == "Accepted"` results per parameter + site for `activity_start_date >= 2017-01-01`. Source: `other/output/wqx_formatted/intermediate/2021_export_data_flagged.csv`. Flag combinations below 10 (or 5 for toxics) — these fall to ADEC Screening Level. Consider sharing with ADEC.
4.  **Task 1b (HIGH)** — Audit all distinct `CharacteristicName` values in WQP for org `KENAI_WQX`. Cross-reference against current WQX domain list. Map variants to canonical names. Feeds next CDX re-upload.
5.  **Task 5 (Medium)** — Verify `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx`. Six codes: `fw_acute`, `fw_chronic`, `harvest_aquatic_life`, `noncarc_aquatic_org`, `noncarc_water`, `secondary_water_recreation`. Confirm against 18 AAC 70 and USEPA criteria docs; set `review_needed = N`.
6.  **Task 5a (Medium)** — Add CALM methodology notes to FC, turbidity, and BTEX chapter narratives noting each is excluded from the standard CALM binomial methodology. Links at https://dec.alaska.gov/water/water-quality/integrated-report/

------------------------------------------------------------------------

## Active Tasks

See `other/agent_context/session_log.md` for full context on any task.

| \# | Priority | Description | Status |
|------------------|------------------|------------------|------------------|
| 1a-reupload | HIGH | Re-upload 835 2021 records (delete + re-upload). Files ready: `results_activities.csv`, `resultphyschem_DELETE_v4.csv`. ETL restored 2026-05-01 but batch delete still fails (records absent from WQX Web internal DB). Follow-up sent to wqx@epa.gov 2026-05-04. | **Blocked** — awaiting EPA action on orphaned records |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP | Pending |
| 1c | HIGH | CALM 5-year window sample count check (2017–2021) | Pending |
| 2 | Medium | Fix HMW visibility for 15 legacy numeric-ID stations — move process to qaqc repo | Pending |
| 2a | Low | Contact ADEC liaison re: tributary ATTAINS assessment units | Pending |
| 4 | Low | Verify boxplot DOCX sizing fix (check `Sys.getenv("QUARTO_PROFILE") == "docx"`) | Pending |
| 5 | Medium | Verify 6 `review_needed = Y` rows in `standard_types` sheet | Pending |
| 5a | Medium | Add CALM methodology notes to FC, turbidity, BTEX narratives | Pending |
| 6 | Low | Add narrative to `chapters/benzene.qmd` (no standalone standard; refer to BTEX chapter) | Pending |
| 7 | Low | Resolve ALS lab duplicate (DUP) status issue — 4 results, does not block CDX | Pending |
| 8 | Low | LOQ logic flow chart for ADEC | Pending |
| 9 | Low | Make `appendix_a.qmd` year-neutral (single `year` variable at top) | **Complete** |
| 10 | Low | Extract ingestion logic to `.R` scripts (do with Task 9) | **Complete** |
| 11 | Low | Restructure WQX data Activity → Results level (future years, qaqc repo) | Pending |
| 18 | HIGH | Create `templates/pipeline_template.qmd` in qaqc repo — canonical single-QMD pipeline with Part A (inline ingest) and Parts B-D (sourced stable scripts). See Pipeline Architecture section. | Pending |
| 19 | Medium | Build `2023.qmd` in qaqc repo from template; adapt Part A for 2023 EDD quirks. See 2023-specific notes in session_log.md. | Pending |
| 12 | Low | Address historical CDX corrections (e.g., spring 2013 specific conductance) in qaqc repo | Pending |
| 13 | Low | Move `wqx_corrections.qmd` to qaqc repo | Pending |
| 14 | Low | Add inline tables alongside calculated-result download links in `appendix_a.qmd` | Pending |
| 15 | Low | Dynamically generate numerical values in parameter chapter prose via inline R | Pending |
| 16 | Low | Parameter chapter review workflow — post-2025 data integration | Pending |
| 17 | Low | Add multi-year duplicate RPD summary table to `data_qa_qc.qmd` | Pending |

------------------------------------------------------------------------

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash; use different punctuation instead.

------------------------------------------------------------------------

## Key Decisions & Conventions

### Sample Fraction Canonical Scheme

| Parameter type | Canonical fraction | Notes |
|------------------------|------------------------|------------------------|
| Dissolved metals (any method, any filtration) | `Dissolved` | Consistent all years including 2023+ lab-filtered |
| Total metals (unfiltered) | `Unfiltered` | For 2023+: method alone no longer distinguishes dissolved from total |
| Nutrients | `Total` |  |
| TSS | `Suspended` |  |
| BTEX / volatiles | `Volatile` |  |
| Fecal Coliform | `None` |  |

### EPA WQX Flagging Convention (complete — do not change)

- `Result Qualifier`: lab qualifiers from EDD (`U` = non-detect, `J` = below LOQ, `=` = detected)
- `Result Status ID`: KWF QA/QC decision — `Accepted` or `Rejected`
- The binary `flag` (Y/N) in `2021_data_flag_decisions.csv` maps to `Result Status ID` at CDX export
- Do not add FQC or other custom codes

### Trip Blank Crew Assignments

Trip blank-to-crew associations are year-specific and stored in per-year CSVs:

- Location: `other/input/wqx_templates/trip_blank_crews_{year}.csv`
- Columns: `blank_id` (e.g., `Trip_Blank_1`), `note` (crew + site string)
- Number of rows varies by year (2, 3, or 4 blanks). Non-blank rows get `NA` for `note` via `left_join`.
- To add a new year: create `trip_blank_crews_{year}.csv` — no script changes needed.
- Used in `functions/appendix_a_scripts/ingest_sgs_als.R` via `str_extract` + `left_join`.

### Pipeline Architecture (qaqc repo — canonical home)

The annual QA/QC pipeline follows a two-part template structure, with each year's work contained in a single QMD (`{year}.qmd`). The canonical template lives at `templates/pipeline_template.qmd` in the qaqc repo. `appendix_a.qmd` in the report repo is the 2021 worked example of this template — not the source of truth.

**Template structure (single QMD per year):**

```         
## Year Configuration        — sampling dates, file paths; only block that changes every year
## Part A: Data Ingestion    — inlined code, adapted per year for EDD format quirks
   ### SGS/ALS Lab Results
   ### Fecal Coliform (SWWTP)
   ### Total Suspended Solids (SWWTP)
   ### Bind and Standardize  — produces standardized `dat` (the contract between A and B)
## Part B: WQX Formatting    — sourced: functions/format_wqx.R (stable)
## Part C: QA/QC Checklist   — Q1–Q42; some formulaic, some manual entries
## Part D: Flag + CDX Export — sourced: functions/apply_qaqc_flags.R, generate_cdx_export.R
```

**Key design decisions:** - Part A ingest code is **inlined** in the QMD (not a sourced script) so it is visibly marked for adaptation each year. - Parts B and D are **sourced scripts** in `functions/` because they are stable and should not change year to year. - The stable scripts (`format_wqx.R`, `apply_qaqc_flags.R`, `generate_cdx_export.R`) use a `cfg` config list for all paths, making them portable. The report repo's copies in `functions/appendix_a_scripts/` are secondary; the qaqc repo's `functions/` copies are canonical. - A shared R package was considered and rejected: ingest logic varies too much per year to package reliably. The template-per-year approach is more honest about this variation.

**qaqc repo structure:**

```         
templates/
  pipeline_template.qmd   # canonical template — copy and adapt for each new year
functions/
  format_wqx.R            # stable WQX column formatting (canonical)
  apply_qaqc_flags.R      # stable flag join + write (canonical)
  generate_cdx_export.R   # stable CDX export (canonical)
{year}.qmd                # per-year pipeline (copy of template, adapted)
```

**Path parameterization:** All stable scripts read paths from a `cfg` list set in the Year Configuration block. Required `cfg` fields: `year`, `templates_dir`, `wqx_template_file`, `spring_data_dir`, `summer_data_dir`, `output_qaqc_dir`, `wqx_intermediate_path`, `flagged_export_path`, `flag_decisions_path`, `spring_sample_date`, `summer_sample_date`

### Lab Ingestion Scripts (report repo — 2021 worked example only)

The sourced scripts in `functions/appendix_a_scripts/` (`ingest_sgs_als.R`, `ingest_fc.R`, `ingest_tss.R`) remain in the report repo for `appendix_a.qmd`. They are NOT the canonical approach for future years. In the qaqc repo pipeline template, ingest code is inlined directly in Part A of the QMD. Lab-specific script names (e.g., `ingest_sgs_als.R`) reflect that EDD formats vary by lab — do not rename.

### appendix_a.qmd Year-Config Variables (2021)

The year-config block at the top of the first chunk sets all year-specific values. For 2021:

- `spring_sample_date <- "5/11/2021"`, `summer_sample_date <- "7/27/2021"`
- `spring_rec_date <- "2021-05-11"` (same day as collection), `summer_rec_date <- "2021-07-27"`
- `spring_fc_analysis_date <- mdy("5/12/2021")` (from cell G2 of SWWTP FC lab sheet), `summer_fc_analysis_date <- mdy("7/28/2021")`

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

## Report Structure

| File | Purpose |
|------------------------------------|------------------------------------|
| `_quarto.yml` | Project configuration. `margin-header` logo path is correct for `index.qmd` only. |
| `chapters/_metadata.yml` | Overrides `margin-header` with `../other/...` path for all chapter pages. |
| `parameters/_metadata.yml` | Overrides `margin-header` with `../other/...` path for all parameter pages. |
| `index.qmd` | Front matter / introduction (stays at project root) |
| `chapters/data_sourcing.qmd` | Data download and preparation |
| `chapters/data_qa_qc.qmd` | QA/QC overview |
| `chapters/reg_limits.qmd` | Regulatory limits framework |
| `chapters/appendix_a.qmd` | Detailed 2021 QA/QC pipeline example |
| `functions/static_boxplot_function.R` | Builds `plots` list; defines `clean_plotly_legend()`. Reads `std_labels` from `master_reg_limits.xlsx → standard_types` at top level (shared). |
| `functions/render_plots.R` | `render_parameter_plots(plots)`: tagList for HTML, `print()` for DOCX |
| `functions/threshold_table.R` | `show_threshold_table(characteristic)`. Reads labels/authority from `master_reg_limits.xlsx → standard_types`. |
| `functions/table_download.R` | `download_tbl(char)` |
| `templates/_parameter_chunk.Rmd` | Shared knitr child template for all parameter chapters |

**Adding a new parameter chapter:** create `.qmd`, set `characteristic` (+ optionally `sample_fraction`, `no_threshold_note`), call `knitr::knit_child("templates/_parameter_chunk.Rmd", envir = environment(), quiet = TRUE)` with `results='asis'`, add to `_quarto.yml`. No changes to function or template files needed.

**Logo path note:** The KWF logo is in `other/documents/images/KWF_logo_resized.png`. Because chapters and parameter pages are in subdirectories, `_quarto.yml` alone cannot serve the correct relative path for all pages. `_metadata.yml` files in `chapters/` and `parameters/` override `margin-header` with the corrected `../` prefix. If a new subdirectory level is added (e.g., `appendices/`), a matching `_metadata.yml` will be needed.

**Render commands:** `quarto render` (HTML default) \| `quarto render --profile docx` \| `quarto::quarto_render(profile = "docx")`

------------------------------------------------------------------------

## Regulatory Threshold Architecture

`master_reg_limits.xlsx` is the **single source of truth** for all regulatory threshold data.

| Sheet | Contents |
|------------------------------------|------------------------------------|
| `static_regulatory_values` | All static thresholds. Categories: `static_metals`, `hydrocarbons`, `nutrients`, `other`, `total_metals_aquatic_life` (Iron), `field_bio_standards` (Water Temp, FC) |
| `calculated_regulatory_values` | Hardness-dependent formulas (Cd, Cr, Cu, Pb, Zn) |
| `diss_metals_hard_parameters` | Parameters for hardness calculations |
| `standard_types` | Display labels and regulatory authority for all standard type codes. Six rows flagged `review_needed = Y` (Task 5). |
| `pick_list` | Legacy — superseded by `standard_types` |

**Adding a new threshold:** add row to `static_regulatory_values`, confirm `standard_types` entry, add export block in `reg_limits.qmd` if new category needed, add resulting CSV to `bind_rows()` in `static_boxplot_function.R`. See Iron and `field_bio_standards` entries as examples.

------------------------------------------------------------------------

## Project Overview

Long-term cooperative monitoring led by Kenai Watershed Forum (KWF), south-central Alaska. Biannual (spring + summer) sampling at 22 sites (13 mainstem + 9 tributaries) since 2000. Current deliverable: Quarto book covering 2000–2025, modeled on 2007 and 2016 comprehensive reports (PDFs in `other/agent_context/`).

- **Project home:** https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
- **GitHub:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
- **QA/QC repo:** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
- **Public data:** https://www.waterqualitydata.us/ (org: `KENAI_WQX`)

Primary downstream consumer: **ADEC**, which draws from EPA CDX every two years for the Integrated Report (impairment decisions). Also serves KWF scientists, general public, and funding partners. A primary goal is that all data is publicly visible in [How's My Waterway](https://mywaterway.epa.gov/).

**HMW:** Monitoring data (Past Water Conditions) is WQP-driven via client-side HUC12 spatial ops. Waterbody condition (Overview/Aquatic Life) is ATTAINS-driven — all 13 mainstem sites have ATTAINS units; no tributary sites do (requires ADEC action in a future Integrated Report cycle).

**Collaboration standard:** Before implementing any code, explain what it does and why. Wait for user confirmation. Prioritize clarity over cleverness.

------------------------------------------------------------------------

## Parameters Monitored

| Category | Parameters |
|------------------------------------|------------------------------------|
| Dissolved Metals | Arsenic, Cadmium, Chromium, Copper, Lead, Zinc |
| Total Metals | Calcium, Iron, Magnesium |
| Nutrients | Nitrate + Nitrite, Phosphorus |
| Hydrocarbons | BTEX (Benzene, Toluene, Ethylbenzene, m/p-Xylene, o-Xylene) |
| Biological | Fecal Coliform |
| Field Parameters | pH, Specific Conductance, TSS, Turbidity, Water Temperature, Dissolved Oxygen |

------------------------------------------------------------------------

## Known Data Issues (Active / Unresolved)

- **WQX/STORET sync (BLOCKED):** 835 2021 records in WQP but absent from WQX Web internal DB. CDX batch delete fails. Wait for EPA ETL fix (\~April 23). Ready files: `resultphyschem_DELETE_v4.csv` (column `ActivityIdentifier`, no org prefix), `results_activities.csv`.
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

------------------------------------------------------------------------

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`, `plotly`, `janitor`, `dataRetrieval`, `TADA`, `xfun`

Use base pipe `|>` for all new code. Do not mass-convert legacy `%>%` usage.

------------------------------------------------------------------------

## Governance Documents (`other/agent_context/`)

- QAPP (approved ADEC + EPA Region 10, 2023 + April 2024 addendum)
- MOU — Baseline Water Quality MOU 2025 Final
- Funding Proposal — KWF 2024 BOR WaterSMART CWMP
- ADEC Water Quality Standards — 18 AAC 70
- DL/LOD/LOQ Interpretation — SGS Laboratories
- CALM — `calm-rev-2021-acc.pdf` (Alaska Consolidated Assessment and Listing Methodology, revised March 2021)

------------------------------------------------------------------------

## Useful External Links

- ADEC Kenai River "exceptional river" press release (Nov 2023): https://dec.alaska.gov/commish/newsroom/23-11-kenai-river-an-exceptional-river-with-clean-water/
- ADEC Ambient Water Quality Data: https://dec.alaska.gov/water/water-quality/ambient-water-quality-data
- ADEC Integrated Report / CALM methodologies: https://dec.alaska.gov/water/water-quality/integrated-report/
