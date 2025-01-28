# TO DO LIST
# - symbology for exceedences of acute vs chronic hardness-dependent values
# - reg values for hydrocarbons, N, P, others
# - adjust text for referencing plotly objects in all chapters
# - fix legends
# - alter pop up windows for plotly
# - address outliers
# - make plotly appear on html render, jpg on docx render


# specify parameter within parameter-specific chapter


# load packages
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
library(plotly)
library(patchwork)

# Set longitudinal order for tributaries
trib_order <- c("No Name Creek",
                "Beaver Creek",
                "Slikok Creek",
                "Soldotna Creek",
                "Funny River",
                "Moose River",
                "Killey River",
                "Russian River",
                "Juneau Creek")


#### READ IN WQX DATA #####
# read in prepared data from local directory.
# for most parameter types, read in data directly downloaded from EPA WQX.

# note that we are reading in "analysis_format" rather than "export_format", which is in the same directory.
# the "export_format" dataframe has been made less granular and is thus not uploaded to EPA WQX. The following was performed on the
# "analysis_format" version: 

#### READ IN analysis format data ####
data <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")


#### READ IN Regulatory Threshold Values ####
# read in and combine various static reg values

# static metals threshold values
static_metals_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/static_metals_reg_vals.csv")

# hydrocarbons reg vals
# HERE

# field parameters threshold values
ph_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/ph_reg_vals.csv")



# join all static regulatory value dataframes
# old: reg_vals <- static_metals_reg_vals
reg_vals <- bind_rows(static_metals_reg_vals,ph_reg_vals)



#### PREPARE PLOT #####

# Function to generate and arrange plots
create_facet_plots <- function(data, reg_vals, characteristic) {
  
  # Subset data for the characteristic
  subset_data <- data %>% filter(characteristic_name == characteristic)
  
  # Ensure result_measure_value is numeric and remove non-numeric rows
  subset_data <- subset_data %>% mutate(result_measure_value = as.numeric(result_measure_value))
  subset_data <- subset_data %>% filter(!is.na(result_measure_value))
  
  # Add new columns for custom color and shape logic
  subset_data <- subset_data %>% mutate(
    acute_color = ifelse(is.na(fw_acute_exceed), "DarkGray", "Highlighted"),
    chronic_shape = case_when(
      is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "GrayCircle",
      fw_acute_exceed == "Y" & is.na(fw_chronic_exceed) ~ "ColoredCircle",
      fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "ColoredAsterisk",
      TRUE ~ "Other"
    )
  )
  
  # Subset regulatory values for the characteristic
  reg_value <- reg_vals %>% filter(characteristic_name == characteristic)
  hline_value <- ifelse(nrow(reg_value) > 0, reg_value$value, NA)
  hline_label <- ifelse(nrow(reg_value) > 0, reg_value$Standard, "")
  
  # Safeguard dynamic range calculations
  if (nrow(subset_data) == 0) {
    y_min <- 0
    y_max <- 1
  } else {
    y_min <- min(subset_data$result_measure_value, na.rm = TRUE)
    y_max <- max(subset_data$result_measure_value, na.rm = TRUE)
  }
  
  # Common theme for both plots
  common_theme <- theme_minimal() +
    theme(axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          strip.text = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"))
  
  # Plot 1: tributary_name vs. result_measure_value
  plot1 <- ggplot(subset_data, aes(x = tributary_name, y = result_measure_value)) +
    geom_boxplot() +
    geom_jitter(aes(color = acute_color, shape = chronic_shape), width = 0.2) +
    geom_hline(aes(yintercept = hline_value, linetype = hline_label), color = "red", show.legend = TRUE) +
    scale_color_manual(name = "Acute Exceedance", values = c("N" = "darkgray", "Y" = "orange")) +
    scale_shape_manual(name = "Chronic Exceedance", values = c("N" = 16, "Y" = 8)) +
    scale_linetype_manual(name = "Regulatory Standard", values = c("Regulatory Limit" = "dashed")) +
    guides(
      linetype = guide_legend(override.aes = list(color = "red")),
      color = guide_legend(override.aes = list(size = 4)),
      shape = guide_legend(override.aes = list(size = 4))
    ) +
    theme_minimal() +
    labs(
      title = "Facet Example: Plot 1",
      x = "Tributary Name",
      y = paste("Result Measure Value (", unique(subset_data$result_measure_unit_code), ")", sep = "")
    ) +
    facet_wrap(~season) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))
  
  # Plot 2: river_mile vs. result_measure_value
  plot2 <- ggplot(subset_data, aes(x = river_mile, y = result_measure_value)) +
    geom_boxplot() +
    geom_jitter(aes(color = acute_color, shape = chronic_shape), width = 0.2) +
    if (!is.na(hline_value)) {
      geom_hline(aes(yintercept = hline_value, linetype = hline_label), color = "red", show.legend = TRUE)
    } else {
      NULL
    } +
    facet_wrap(~season) +
    labs(title = paste("Characteristic:", characteristic), x = "River Mile", y = paste("Result Measure Value (", unique(subset_data$result_measure_unit_code), ")", sep = "")) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    scale_color_manual(name = "Acute Exceedance", values = c("DarkGray" = "darkgray", "Highlighted" = "orange")) +
    scale_shape_manual(name = "Chronic Exceedance", values = c("GrayCircle" = 16, "ColoredCircle" = 17, "ColoredAsterisk" = 8)) +
    scale_linetype_manual(name = "Regulatory Standard", values = c("dashed")) +
    guides(linetype = guide_legend(override.aes = list(color = "red"))) +
    common_theme +
    theme(axis.text.x = element_text(size = 14))
  
  # Handle cases with no valid data
  if (nrow(subset_data) == 0) {
    plot1 <- ggplot() +
      annotate("text", x = 1, y = 1, label = "No valid data available", size = 6) +
      theme_void()
    plot2 <- plot1
  }
  
  # Return the two separate plots
  return(list(plot1 = plot1, plot2 = plot2))
}

# Example usage
# plots <- create_facet_plots(baseline_data, reg_vals, "Arsenic")
# print(plots$plot1)
# print(plots$plot2)



# Example usage
# combined_plot <- create_facet_plots(baseline_data, reg_vals, "Arsenic")
# print(combined_plot)



# Example usage
# combined_plot <- create_facet_plots(baseline_data, reg_vals, "Arsenic")
# print(combined_plot)






# make geom_point symbology different for exceedences

## metals:
### acute exceedence: color A
### chronic exceedence: color B
### do we need different plot functions for this type of display vs where thresholds are just static?


