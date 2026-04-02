# threshold_table.R
# Renders a summary table of regulatory threshold values for a given characteristic.
# Called from each parameter chapter after the CSV download link.
#
# For static thresholds, numeric values come from all_reg_vals.csv.
# For hardness-dependent metals (Cd, Cr, Cu, Pb, Zn), the table shows the range
# of calculated threshold values across all samples in the full dataset
# (calculated_metals_reg_vals.csv), reflecting the range of observed hardness.
# This gives readers actionable numerical context without requiring formula notation.
#
# Usage (in each parameter chapter, after source("functions/table_download.R")):
#   source("functions/threshold_table.R")
#   show_threshold_table(characteristic)

library(dplyr)
library(knitr)
library(readxl)

show_threshold_table <- function(characteristic, no_threshold_note = NULL) {

  # --- Lookup tables: Standard code -> display label and regulatory authority ---
  # Source of truth: "standard_types" sheet in master_reg_limits.xlsx.
  # To add or edit a standard type, update that sheet directly.
  # Rows with review_needed = "Y" have inferred labels/authorities that should
  # be verified against 18 AAC 70 and USEPA criteria documents before final render.
  std_types <- readxl::read_excel(
    "other/input/regulatory_limits/master_reg_limits.xlsx",
    sheet = "standard_types"
  )

  standard_labels    <- setNames(std_types$display_label,        std_types$standard_type)
  standard_authority <- setNames(std_types$regulatory_authority, std_types$standard_type)

  unit_labels <- c(
    "ug/l"      = "\u00b5g/L",
    "mg/l"      = "mg/L",
    "none"      = "\u2014",    # em dash for dimensionless (e.g. pH)
    "deg_c"     = "\u00b0C",
    "cfu/100ml" = "CFU/100 mL"
  )

  # --- Static thresholds ---
  reg_vals <- read.csv("other/output/regulatory_values/all_reg_vals.csv",
                       stringsAsFactors = FALSE)

  static_rows <- reg_vals |>
    filter(characteristic_name == characteristic, !is.na(value)) |>
    mutate(
      standard_type = dplyr::coalesce(standard_labels[Standard], Standard),
      authority     = dplyr::coalesce(standard_authority[Standard], ""),
      unit_display  = dplyr::coalesce(unit_labels[tolower(reg_unit)], reg_unit),
      value_fmt     = format(value, scientific = FALSE, drop0trailing = TRUE) |>
                        trimws()
    ) |>
    select(
      `Standard Type`        = standard_type,
      `Value`                = value_fmt,
      `Unit`                 = unit_display,
      `Regulatory Authority` = authority
    )

  # --- Hardness-dependent thresholds ---
  calc_path <- "other/input/regulatory_limits/formatted_reg_vals/calculated_metals_reg_vals.csv"
  hd_rows <- data.frame(
    `Standard Type`        = character(),
    `Value`                = character(),
    `Unit`                 = character(),
    `Regulatory Authority` = character(),
    check.names = FALSE
  )

  if (file.exists(calc_path)) {
    calc <- read.csv(calc_path, stringsAsFactors = FALSE)
    hd_data <- calc |>
      filter(characteristic_name == characteristic)

    # Acute criterion
    acute_vals <- hd_data$fw_acute_std[!is.na(hd_data$fw_acute_std) &
                                         is.finite(hd_data$fw_acute_std)]
    if (length(acute_vals) > 0) {
      hd_rows <- rbind(hd_rows, data.frame(
        `Standard Type`        = "Aquatic life \u2013 acute (hardness-dependent)",
        `Value`                = paste0(
          signif(min(acute_vals), 3), " \u2013 ", signif(max(acute_vals), 3)
        ),
        `Unit`                 = "\u00b5g/L",
        `Regulatory Authority` = "USEPA",
        check.names = FALSE
      ))
    }

    # Chronic criterion
    chronic_vals <- hd_data$fw_chronic_std[!is.na(hd_data$fw_chronic_std) &
                                              is.finite(hd_data$fw_chronic_std)]
    if (length(chronic_vals) > 0) {
      hd_rows <- rbind(hd_rows, data.frame(
        `Standard Type`        = "Aquatic life \u2013 chronic (hardness-dependent)",
        `Value`                = paste0(
          signif(min(chronic_vals), 3), " \u2013 ", signif(max(chronic_vals), 3)
        ),
        `Unit`                 = "\u00b5g/L",
        `Regulatory Authority` = "USEPA",
        check.names = FALSE
      ))
    }
  }

  # --- Combine and render ---
  all_rows <- rbind(static_rows, hd_rows)

  if (nrow(all_rows) == 0) {
    # Use custom note if provided, otherwise use the default message
    msg <- if (!is.null(no_threshold_note)) {
      no_threshold_note
    } else {
      paste0("*No regulatory threshold for ", characteristic,
             " has been established for freshwater aquatic life by ADEC or USEPA.*")
    }
    return(knitr::asis_output(msg))
  }

  knitr::kable(
    all_rows,
    caption = paste("Regulatory thresholds for", characteristic,
                    "(hardness-dependent ranges reflect observed hardness across",
                    "all dataset years)"),
    col.names = c("Standard Type", "Value", "Unit", "Regulatory Authority"),
    align = c("l", "r", "l", "l")
  )
}
