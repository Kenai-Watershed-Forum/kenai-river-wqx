# TO DO LIST
# - reg values for hydrocarbons, N, P, others

# - address outliers

# packages
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(writexl)
library(hms)
library(DT)
library(openxlsx)
library(DT)
library(ggpubr)
library(plotrix)
library(remotes)
library(magrittr)
library(janitor)
library(xfun)
library(cowplot)
library(forcats)
library(magrittr)
library(patchwork)

#### READ IN WQX DATA #####
# read in prepared data from local directory.
# for most parameter types, read in data directly downloaded from EPA WQX.



#### READ IN analysis format data ####
# note that we are reading in "analysis_format" rather than "export_format", which is in the same directory
# the "export_format" dataframe has been made less granular and is thus not uploaded to EPA WQX. The following was performed on the
# "analysis_format" version: 1) Aggregate all organic volatiles to BTEX in cases where individual volatiles data was provided
data_path <- "other/output/analysis_format/baseline_analysis_format.csv"


#### READ IN Regulatory Threshold Values ####
# read in and combine various static reg values

# static metals threshold values
static_metals_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/static_metals_reg_vals.csv")

# hydrocarbons reg vals
# define directory for where hydrocarbon reg values are saved
hydrocarbon_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/hydrocarbon_reg_vals.csv")

# field parameters threshold values
ph_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/ph_reg_vals.csv")

# total metals with freshwater aquatic life standards (e.g. Iron 1 mg/L chronic)
total_metals_aq_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/total_metals_aq_reg_vals.csv")

# field and biological parameters (Water Temperature, Fecal Coliform)
field_bio_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/field_bio_reg_vals.csv")

# join all static regulatory value dataframes
reg_vals <- bind_rows(static_metals_reg_vals, hydrocarbon_reg_vals, ph_reg_vals,
                      total_metals_aq_reg_vals, field_bio_reg_vals)

# write combined csv of all static regulatory values
reg_vals_path <- "other/output/regulatory_values/all_reg_vals.csv"
write.csv(reg_vals, reg_vals_path)

# Read display labels for regulatory standard type codes once, at the top level,
# so both create_facet_plots() and clean_plotly_legend() can use them.
# Source of truth: "standard_types" sheet in master_reg_limits.xlsx.
std_types  <- readxl::read_excel(
  "other/input/regulatory_limits/master_reg_limits.xlsx",
  sheet = "standard_types"
)
std_labels <- setNames(
  stringr::str_wrap(std_types$display_label, width = 20),
  std_types$standard_type
)



#### PREPARE PLOT #####

# Function to dynamically generate two ggplots with optional sample fraction filtering
create_facet_plots <- function(data_path, reg_vals_path, characteristic, sample_fraction = character(0)) {
  
  # Scale point and text sizes up for DOCX, where figures are larger and
  # code-folding is unavailable, making readability more important.
  # Two-check detection: QUARTO_PROFILE is set by Quarto's profile system
  # (most reliable); knitr::pandoc_to() is a fallback for non-profile renders.
  is_docx     <- identical(Sys.getenv("QUARTO_PROFILE"), "docx") ||
                 isTRUE(knitr::pandoc_to("docx"))
  pt_size     <- if (is_docx) 3.0 else 1.5
  strip_size  <- if (is_docx) 22  else 16
  axis_x_size <- if (is_docx) 18  else 14
  axis_y_size <- if (is_docx) 20  else 16
  title_size  <- if (is_docx) 20  else 16
  legend_size <- if (is_docx) 14  else 11
  
  # Read the datasets
  data <- read.csv(data_path, stringsAsFactors = FALSE) %>%
    filter(result_status_identifier != "Rejected")
  
  reg_vals <- read.csv(reg_vals_path, stringsAsFactors = FALSE)
  
  # Sample Fractions
  if (is.null(sample_fraction) && exists("sample_fraction", envir = .GlobalEnv)) {
    sample_fraction <- get("sample_fraction", envir = .GlobalEnv)
  }
  # When a fraction whitelist is provided, include matching rows plus rows where
  # fraction is NA (historical records where fraction was not recorded but the
  # same analytical method was used throughout the project).
  if (!is.null(sample_fraction) && length(sample_fraction) > 0) {
    data <- data %>% filter(result_sample_fraction_text %in% sample_fraction | is.na(result_sample_fraction_text))
  }
  
  # Regulatory Values with lines for geom_segment
  reg_values <- reg_vals %>% filter(characteristic_name == characteristic)
  hline_data <- reg_values %>%
    filter(!is.na(value)) %>%
    mutate(yintercept = as.numeric(value)) %>%
    select(yintercept, linetype = Standard)
  
  unique_river_miles <- unique(data$river_mile)
  
  preprocess_data <- function(x_var_filter) {
    data %>%
      mutate(
        activity_start_date = as.Date(activity_start_date, format = "%Y-%m-%d"),
        year = as.numeric(format(activity_start_date, "%Y")),
        season = as.factor(season)
      ) %>%
      filter((x_var_filter == "tributary_name" & trib_mainstem != "m") |
               (x_var_filter == "river_mile" & trib_mainstem != "t")) %>%
      mutate(
        tributary_name = factor(tributary_name,
                                levels = c("No Name Creek", 
                                           "Beaver Creek", 
                                           "Slikok Creek", 
                                           "Soldotna Creek", 
                                           "Funny River", 
                                           "Moose River", 
                                           "Killey River", 
                                           "Russian River", 
                                           "Juneau Creek")),
        river_mile = factor(river_mile, levels = sort(unique_river_miles))
      ) %>%
      filter(characteristic_name == characteristic) %>%
      mutate(
        result_measure_value = suppressWarnings(as.numeric(result_measure_value)),
        fw_acute_exceed = ifelse(is.na(fw_acute_exceed), NA, fw_acute_exceed),
        fw_chronic_exceed = ifelse(is.na(fw_chronic_exceed), NA, fw_chronic_exceed)
      ) %>%
      filter(!is.na(result_measure_value)) %>%
      mutate(
        tooltip_text = paste0(
          "<b>", ifelse(trib_mainstem == "m", paste0("RM ", site_name), site_name), "</b><br>",
          "Date: ", activity_start_date, "<br>",
          "Value: ", result_measure_value, " ", result_measure_measure_unit_code, "<br>",
          "Fraction: ", ifelse(is.na(result_sample_fraction_text), "Not specified", result_sample_fraction_text), "<br>",
          "Lab: ", ifelse(is.na(laboratory_name), "Not specified", laboratory_name), "<br>",
          "Method: ", result_analytical_method_method_identifier, "<br>",
          "Status: ", result_status_identifier
        )
      )
  }
  
  # Linetype assignments for all known standard type codes.
  # Only the codes that are present for the current parameter will appear
  # in the legend; the rest are silently ignored by ggplot2.
  linetype_map <- c(
    drinking_water               = "solid",
    irrigation_water             = "longdash",
    stock_water                  = "dotted",
    wildlife                     = "twodash",
    recreation                   = "dotdash",
    aquaculture_maximum_water    = "dashed",
    aquaculture_minimum_water    = "dotted",
    aquaculture_water            = "solid",
    aquatic_life_chronic         = "longdash",
    temp_all_freshwaters         = "solid",
    temp_rearing_migration       = "dashed",
    temp_egg_fry_spawning        = "dotted",
    recreation_single_sample     = "longdash",
    drinking_water_single_sample = "dotdash",
    harvest_aquatic_life         = "twodash",
    secondary_water_recreation   = "dotdash",
    noncarc_aquatic_org          = "dotdash",
    noncarc_water                = "solid",
    fw_acute                     = "solid",
    fw_chronic                   = "dashed"
  )

  create_plot <- function(subset_data, x_var) {
    if (nrow(subset_data) == 0) {
      stop("No data available for plotting after preprocessing.")
    }
    
    min_year <- as.character(min(subset_data$year, na.rm = TRUE))
    max_year <- as.character(max(subset_data$year, na.rm = TRUE))
    y_min <- min(subset_data$result_measure_value, na.rm = TRUE) * 0.9
    y_max <- max(subset_data$result_measure_value, na.rm = TRUE) * 1.1
    
    exceedance_present <- any(!is.na(subset_data$fw_acute_exceed) | !is.na(subset_data$fw_chronic_exceed))

    plot <- ggplot(subset_data, aes(x = .data[[x_var]], y = result_measure_value)) +
      geom_boxplot(aes(group = .data[[x_var]]), outlier.shape = NA) +
      geom_jitter(
        aes(
          color = if (exceedance_present) factor(case_when(
            is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "None",
            is.na(fw_acute_exceed) & fw_chronic_exceed == "Y" ~ "Chronic",
            fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "Acute"
          )) else NULL,
          shape = if (exceedance_present) factor(case_when(
            is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "None",
            is.na(fw_acute_exceed) & fw_chronic_exceed == "Y" ~ "Chronic",
            fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "Acute"
          )) else NULL,
          text = tooltip_text
        ),
        width = 0.2, size = pt_size, show.legend = exceedance_present
      ) +
      # All threshold lines are passed to geom_hline regardless of whether they
      # fall within the visible y range. Out-of-range lines will not appear in
      # the plot body but their legend entries are retained intentionally, so
      # readers can see that a standard exists even when no data is near it.
      # A companion threshold table in each chapter shows the numeric values.
      geom_hline(data = hline_data, aes(yintercept = yintercept, linetype = linetype),
                 color = "#D55E00", linewidth = 1.2, show.legend = TRUE) +
      facet_wrap(~season) +
      scale_y_continuous(limits = c(y_min, y_max))
    
    if (exceedance_present) {
      plot <- plot + scale_color_manual(name = "Hardness-dependent\nexceedance type",
                                        values = c("None" = "#7F7F7F", "Chronic" = "#E69F00", "Acute" = "#0072B2"))
      plot <- plot + scale_shape_manual(name = "Hardness-dependent\nexceedance type",
                                        values = c("None" = 16, "Chronic" = 16, "Acute" = 8))
    }
    
    # Only show legend entries for standards actually present in this parameter's
    # hline_data; labels come from std_labels (read from master_reg_limits.xlsx).
    present_standards <- unique(hline_data$linetype)
    plot <- plot + scale_linetype_manual(
      name   = "Static regulatory\nthresholds",
      values = linetype_map,
      labels = std_labels[present_standards],
      breaks = present_standards
    )
    
    
    plot <- plot + guides(
      linetype = guide_legend(order = 1, override.aes = list(color = "red", size = 1.2)),
      color    = if (exceedance_present) guide_legend(order = 2, override.aes = list(size = 4, linetype = 0)) else "none",
      shape    = if (exceedance_present) guide_legend(order = 2, override.aes = list(size = 4, linetype = 0)) else "none"
    )
    
    plot <- plot + theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = axis_x_size),
      axis.text.y = element_text(size = axis_y_size),
      strip.text = element_text(size = strip_size),
      axis.title.y = element_text(size = title_size),
      legend.text           = element_text(size = legend_size),
      legend.position       = "right",
      legend.box            = "vertical",
      legend.box.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.5),
      legend.box.margin     = margin(6, 6, 6, 6),
      legend.background     = element_blank()
    ) +
      labs(y = paste0(characteristic, " (", names(which.max(table(subset_data$result_measure_measure_unit_code))), ")"), x = "")
    
    return(plot)
  }
  
  tributary_data <- preprocess_data("tributary_name")
  river_mile_data <- preprocess_data("river_mile")
  
  return(list(
    tributary_plot = create_plot(tributary_data, "tributary_name"),
    river_mile_plot = create_plot(river_mile_data, "river_mile")
  ))
}





# Helper: deduplicate and style the plotly legend produced by ggplotly().
#
# ggplotly() generates one trace per layer per facet panel, e.g.
# "(recreation_single_sample,1)" / "(recreation_single_sample,2)".
# This function:
#   1. Strips the facet suffix to get the base trace name.
#   2. Assigns traces to one of two legend groups:
#        "threshold"  - static regulatory threshold lines (geom_hline)
#        "exceedance" - hardness-dependent exceedance points (geom_jitter color/shape)
#   3. Replaces raw standard-type code names with human-readable display labels
#      (read from std_labels, which is built at the top of this script from
#       the standard_types sheet in master_reg_limits.xlsx).
#   4. Suppresses duplicate legend entries within each group.
#   5. Applies a border box and group-gap to the overall legend layout.
clean_plotly_legend <- function(p) {
  exceedance_names <- c("None", "Chronic", "Acute")
  seen         <- character(0)
  group_titled <- list(threshold = FALSE, exceedance = FALSE)

  for (i in seq_along(p$x$data)) {
    nm <- p$x$data[[i]]$name

    # Strip facet suffix if present: "(NAME,N)" or "(NAME,N,NA)" -> "NAME"
    base_nm <- sub("^\\((.+),\\d+(?:,NA)?\\)$", "\\1", nm, perl = TRUE)

    # When the regex doesn't match (base_nm == nm), ggplotly has already stripped
    # the facet suffix (happens when no color/shape aesthetic is mapped). In that
    # case, only continue if the name is a known standard-type code or exceedance
    # label; skip all other traces (boxplot outlines, plain jitter points, etc.).
    if (base_nm == nm) {
      if (!(nm %in% c(names(std_labels), exceedance_names))) next
    } else if (nchar(base_nm) == 0) {
      next
    }

    is_exceedance <- base_nm %in% exceedance_names
    group <- if (is_exceedance) "exceedance" else "threshold"

    p$x$data[[i]]$legendgroup <- group

    if (base_nm %in% seen) {
      p$x$data[[i]]$showlegend <- FALSE
    } else {
      seen <- c(seen, base_nm)

      # Set the group subtitle on the first visible trace of each group only.
      # Subsequent traces in the same group get an empty string to prevent
      # plotly from rendering the subtitle multiple times.
      if (!group_titled[[group]]) {
        subtitle <- if (group == "threshold") {
          "<b>Static regulatory thresholds</b>"
        } else {
          "<b>Hardness-dependent<br>criteria</b>"
        }
        p$x$data[[i]]$legendgrouptitle <- list(text = subtitle, font = list(size = 11))
        group_titled[[group]] <- TRUE
      } else {
        p$x$data[[i]]$legendgrouptitle <- list(text = "")
      }

      # Replace raw standard-type code with human-readable label for display.
      # plotly uses <br> for line breaks; str_wrap uses \n.
      display_nm <- if (!is_exceedance && base_nm %in% names(std_labels)) {
        gsub("\n", "<br>", std_labels[[base_nm]])
      } else {
        base_nm
      }
      p$x$data[[i]]$name <- display_nm
    }
  }

  # Direct assignment is used throughout because plotly::layout() does not
  # reliably override properties already set by ggplotly().
  p$x$layout$legend$title$text    <- "<b>Legend</b>"
  p$x$layout$legend$bgcolor       <- "white"
  p$x$layout$legend$bordercolor   <- "rgba(119, 119, 119, 0.7)"
  p$x$layout$legend$borderwidth   <- 1
  p$x$layout$legend$tracegroupgap <- 20

  p
}

# Build plots — rendering is handled directly in each parameter chapter's chunk
# (htmlwidgets must be output from a direct chunk expression in Quarto, not inside source())
plots <- create_facet_plots(data_path, reg_vals_path, characteristic)

