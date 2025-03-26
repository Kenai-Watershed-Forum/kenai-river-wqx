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


# join all static regulatory value dataframes
# old: reg_vals <- static_metals_reg_vals
reg_vals <- bind_rows(static_metals_reg_vals,hydrocarbon_reg_vals,ph_reg_vals)

# write combined csv of all static regulatory values
reg_vals_path <- "other/output/regulatory_values/all_reg_vals.csv"
write.csv(reg_vals, reg_vals_path)



#### PREPARE PLOT #####

# Function to dynamically generate two ggplots with optional sample fraction filtering
create_facet_plots <- function(data_path, reg_vals_path, characteristic, sample_fraction = character(0)) {
  # Read the datasets
  data <- read.csv(data_path, stringsAsFactors = FALSE) %>%
    filter(result_status_identifier != "Rejected")
  
  reg_vals <- read.csv(reg_vals_path, stringsAsFactors = FALSE)
  
  # Sample Fractions
  if (is.null(sample_fraction) && exists("sample_fraction", envir = .GlobalEnv)) {
    sample_fraction <- get("sample_fraction", envir = .GlobalEnv)
  }
  if (!is.null(sample_fraction) && length(sample_fraction) > 0) {
    data <- data %>% filter(result_sample_fraction_text %in% sample_fraction)
  }
  
  # Regulatory Values with lines for geom_segment
  reg_values <- reg_vals %>% filter(characteristic_name == characteristic)
  hline_data <- reg_values %>%
    filter(!is.na(value)) %>%
    mutate(yintercept = as.numeric(value), x = -Inf, xend = Inf) %>%
    select(yintercept, linetype = Standard, x, xend)
  
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
      filter(!is.na(result_measure_value))
  }
  
  create_plot <- function(subset_data, x_var) {
    if (nrow(subset_data) == 0) {
      stop("No data available for plotting after preprocessing.")
    }
    
    min_year <- as.character(min(subset_data$year, na.rm = TRUE))
    max_year <- as.character(max(subset_data$year, na.rm = TRUE))
    y_min <- min(subset_data$result_measure_value, na.rm = TRUE) * 0.9
    y_max <- max(subset_data$result_measure_value, na.rm = TRUE) * 1.1
    
    exceedance_present <- any(!is.na(subset_data$fw_acute_exceed) | !is.na(subset_data$fw_chronic_exceed))
    
    plot <- ggplot(subset_data, aes(x = get(x_var), y = result_measure_value)) +
      geom_boxplot(aes(group = get(x_var)), outlier.shape = NA) +
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
          )) else NULL
        ),
        width = 0.2, size = 3, show.legend = exceedance_present
      ) +
      geom_segment(data = hline_data, aes(x = x, xend = xend, y = yintercept, yend = yintercept, linetype = linetype), 
                   color = "#D55E00", size = 1.2, show.legend = TRUE) +
      facet_wrap(~season) +
      scale_y_continuous(limits = c(y_min, y_max))
    
    if (exceedance_present) {
      plot <- plot + scale_color_manual(name = "Hardness Dependent\nExceedance Type",
                                        values = c("None" = "#7F7F7F", "Chronic" = "#E69F00", "Acute" = "#0072B2"))
      plot <- plot + scale_shape_manual(name = "Hardness Dependent\nExceedance Type",
                                        values = c("None" = 16, "Chronic" = 16, "Acute" = 8))
    }
    
    plot <- plot + scale_linetype_manual(name = "Regulatory Standard",
                                         values = c("drinking_water" = "solid", 
                                                    "irrigation_water" = "dotdash", 
                                                    "stock_water" = "dotted", 
                                                    "wildlife" = "twodash", 
                                                    "recreation" = "dotdash", 
                                                    "aquaculture_maximum_water" = "dashed", 
                                                    "aquaculture_minimum_water" = "dotted", 
                                                    "aquaculture_water" = "solid"))
    
    # Add dummy invisible lines to show legends even if out of plot range
    if (nrow(hline_data) > 0) {
      dummy_lines <- hline_data %>% mutate(yintercept = max(subset_data$result_measure_value, na.rm = TRUE) + 1000)
      plot <- plot + geom_segment(data = dummy_lines, aes(x = x, xend = xend, y = yintercept, yend = yintercept, linetype = linetype), 
                                  color = NA, size = 1.2, show.legend = TRUE)
    }
    
    plot <- plot + guides(
      linetype = guide_legend(override.aes = list(color = "red", size = 1.2)),
      color = if (exceedance_present) guide_legend(override.aes = list(size = 4, linetype = 0)) else "none",
      shape = if (exceedance_present) guide_legend(override.aes = list(size = 4, linetype = 0)) else "none"
    )
    
    plot <- plot + theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
      axis.text.y = element_text(size = 16),
      strip.text = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.position = "right"
    ) +
      labs(y = paste0(characteristic, " (", unique(subset_data$result_measure_measure_unit_code), ")"), x = "")
    
    return(plot)
  }
  
  tributary_data <- preprocess_data("tributary_name")
  river_mile_data <- preprocess_data("river_mile")
  
  return(list(
    tributary_plot = create_plot(tributary_data, "tributary_name"),
    river_mile_plot = create_plot(river_mile_data, "river_mile")
  ))
}





# Print plots
plots <- create_facet_plots(data_path, reg_vals_path, characteristic)
print(plots$tributary_plot)
print(plots$river_mile_plot)

