# Kenai River WQX — Report Structure

Load this file when working on the Quarto book, parameter chapters, or adding new pages.

------------------------------------------------------------------------

## Report Files

| File | Purpose |
|----|----|
| `_quarto.yml` | Project configuration. `margin-header` logo path is correct for `index.qmd` only. |
| `chapters/_metadata.yml` | Overrides `margin-header` with `../other/...` path for all chapter pages. |
| `parameters/_metadata.yml` | Overrides `margin-header` with `../other/...` path for all parameter pages. |
| `index.qmd` | Front matter / introduction (stays at project root) |
| `chapters/data_sourcing.qmd` | Data download and preparation |
| `chapters/data_qa_qc.qmd` | QA/QC overview |
| `chapters/reg_limits.qmd` | Regulatory limits framework |
| `chapters/appendix_a.qmd` | Detailed 2021 QA/QC pipeline example |
| `functions/static_boxplot_function.R` | Builds `plots` list; defines `clean_plotly_legend()`. Reads `std_labels` from `master_reg_limits.xlsx -> standard_types` at top level (shared). |
| `functions/render_plots.R` | `render_parameter_plots(plots)`: tagList for HTML, `print()` for DOCX |
| `functions/threshold_table.R` | `show_threshold_table(characteristic)`. Reads labels/authority from `master_reg_limits.xlsx -> standard_types`. |
| `functions/table_download.R` | `download_tbl(char)` |
| `templates/_parameter_chunk.Rmd` | Shared knitr child template for all parameter chapters |

**Adding a new parameter chapter:** create `.qmd`, set `characteristic` (+ optionally `sample_fraction`, `no_threshold_note`), call `knitr::knit_child("templates/_parameter_chunk.Rmd", envir = environment(), quiet = TRUE)` with `results='asis'`, add to `_quarto.yml`. No changes to function or template files needed.

**Logo path note:** The KWF logo is in `other/documents/images/KWF_logo_resized.png`. Because chapters and parameter pages are in subdirectories, `_quarto.yml` alone cannot serve the correct relative path for all pages. `_metadata.yml` files in `chapters/` and `parameters/` override `margin-header` with the corrected `../` prefix. If a new subdirectory level is added (e.g., `appendices/`), a matching `_metadata.yml` will be needed.

------------------------------------------------------------------------

## Render Commands

`quarto render` (HTML default) \| `quarto render --profile docx` \| `quarto::quarto_render(profile = "docx")`
