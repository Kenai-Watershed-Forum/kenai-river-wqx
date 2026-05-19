# Kenai River WQX — Data Conventions

Load this file when working on WQX formatting, CDX export, data ingestion, or any data convention question.

------------------------------------------------------------------------

## Sample Fraction Canonical Scheme

| Parameter type | Canonical fraction | Notes |
|---|---|---|
| Dissolved metals (any method, any filtration) | `Dissolved` | Consistent all years including 2023+ lab-filtered |
| Total metals (unfiltered) | `Unfiltered` | For 2023+: method alone no longer distinguishes dissolved from total |
| Nutrients | `Total` | |
| TSS | `Suspended` | |
| BTEX / volatiles | `Volatile` | |
| Fecal Coliform | `None` | |

------------------------------------------------------------------------

## EPA WQX Flagging Convention (complete — do not change)

- `Result Qualifier`: lab qualifiers from EDD (`U` = non-detect, `J` = below LOQ, `=` = detected)
- `Result Status ID`: KWF QA/QC decision — `Accepted` or `Rejected`
- The binary `flag` (Y/N) in `2021_data_flag_decisions.csv` maps to `Result Status ID` at CDX export
- Do not add FQC or other custom codes

------------------------------------------------------------------------

## Trip Blank Crew Assignments

Trip blank-to-crew associations are year-specific and stored in per-year CSVs:

- Location: `other/input/wqx_templates/trip_blank_crews_{year}.csv`
- Columns: `blank_id` (e.g., `Trip_Blank_1`), `note` (crew + site string)
- Number of rows varies by year (2, 3, or 4 blanks). Non-blank rows get `NA` for `note` via `left_join`.
- To add a new year: create `trip_blank_crews_{year}.csv` — no script changes needed.
- Used in `functions/appendix_a_scripts/ingest_sgs_als.R` via `str_extract` + `left_join`.

------------------------------------------------------------------------

## Pipeline Architecture (qaqc repo — canonical home)

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
## Part C: QA/QC Checklist   — Q1-Q42; some formulaic, some manual entries
## Part D: Flag + CDX Export — sourced: functions/apply_qaqc_flags.R, generate_cdx_export.R
```

**Key design decisions:**

- Part A ingest code is **inlined** in the QMD (not a sourced script) so it is visibly marked for adaptation each year.
- Parts B and D are **sourced scripts** in `functions/` because they are stable and should not change year to year.
- The stable scripts (`format_wqx.R`, `apply_qaqc_flags.R`, `generate_cdx_export.R`) use a `cfg` config list for all paths, making them portable. The report repo's copies in `functions/appendix_a_scripts/` are secondary; the qaqc repo's `functions/` copies are canonical.
- A shared R package was considered and rejected: ingest logic varies too much per year to package reliably. The template-per-year approach is more honest about this variation.

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

------------------------------------------------------------------------

## Lab Ingestion Scripts (report repo — 2021 worked example only)

The sourced scripts in `functions/appendix_a_scripts/` (`ingest_sgs_als.R`, `ingest_fc.R`, `ingest_tss.R`) remain in the report repo for `appendix_a.qmd`. They are NOT the canonical approach for future years. In the qaqc repo pipeline template, ingest code is inlined directly in Part A of the QMD. Lab-specific script names (e.g., `ingest_sgs_als.R`) reflect that EDD formats vary by lab — do not rename.

------------------------------------------------------------------------

## appendix_a.qmd Year-Config Variables (2021)

The year-config block at the top of the first chunk sets all year-specific values. For 2021:

- `spring_sample_date <- "5/11/2021"`, `summer_sample_date <- "7/27/2021"`
- `spring_rec_date <- "2021-05-11"` (same day as collection), `summer_rec_date <- "2021-07-27"`
- `spring_fc_analysis_date <- mdy("5/12/2021")` (from cell G2 of SWWTP FC lab sheet), `summer_fc_analysis_date <- mdy("7/28/2021")`
