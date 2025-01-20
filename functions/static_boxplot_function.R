# TO DO LIST
# - symbology for exceedences of acute vs chronic hardness-dependent values
# - reg values for hydrocarbons, N, P, others
# - adjust text for referencing plotly objects in all chapters
# - fix legends
# - alter pop up windows for plotly
# - address outliers
# - make plotly appear on html render, jpg on docx render



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

# note that we are reading in "analysis_format" rather than "export_format", which is in the same directory
# the "export_format" dataframe has been made less granular and is thus not uploaded to EPA WQX. The following was performed on the
# "analysis_format" version: 1) Aggregate all organic volatiles to BTEX in cases where individual volatiles data was provided
dat <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")




#### READ IN Regulatory Threshold Values ####
# read in and combine various static reg values

# static metals threshold values
static_metals_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/static_metals_reg_vals.csv")

# hydrocarbons reg vals

# field parameters threshold values
ph_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/ph_reg_vals.csv")



# calculated metals threshold values (?)



# join all static regulatory value dataframes
# old: reg_vals <- static_metals_reg_vals
reg_vals <- bind_rows(static_metals_reg_vals,ph_reg_vals)



#### PREPARE PLOT #####

## define horizontal line positions for regulatory limits
hline_data <- reg_vals %>%
  filter(characteristic_name == parameter) 

# how to incorporate other reg vals here (N & P, bacteria, hydrocarbons, etc)

# plan
# 1.) develop lists of all STATIC reg limits. gather together into same DF here, w/ column for name type of limit
# 2.) re-knit doc each time new category is added
# 3.) develop separate calculated box plot for all non-static values

# WORKING HERE 11/27/23



# will need a step here to convert units to make reg value match parameter vals?

# Describe Overall Boxplot function

make_boxplot <- function(param) {
  
  # Create data table for a single parameter
  parameter_dat <- dat %>%
    filter(characteristic_name == param) %>%
    mutate(tributary_name = factor(tributary_name, levels = trib_order)) %>%
    left_join(reg_vals, by = "characteristic_name")
  
  # Get unit for parameter
  unit <- unique(parameter_dat$result_measure_measure_unit_code)
  
  # Set min and max time extent
  min_year <- as.character(min(year(parameter_dat$activity_start_date)))
  max_year <- as.character(max(year(parameter_dat$activity_start_date)))
  
  # Calculate y-axis ranges for tributaries and mainstem separately
  trib_data <- subset(parameter_dat, trib_mainstem == "t")
  mainstem_data <- subset(parameter_dat, trib_mainstem == "m")
  
  y_min_trib <- min(trib_data$result_measure_value, na.rm = TRUE)
  y_max_trib <- max(trib_data$result_measure_value, na.rm = TRUE)
  
  y_min_mainstem <- min(mainstem_data$result_measure_value, na.rm = TRUE)
  y_max_mainstem <- max(mainstem_data$result_measure_value, na.rm = TRUE)
  
  # Define a theme with 20% larger text
  larger_text_theme <- theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    strip.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    plot.title = element_text(size = rel(1.2), hjust = 0.5)
  )
  
  # Tribs plot
  p_trib <- ggplot(trib_data, aes(x = factor(tributary_name), y = result_measure_value)) +
    facet_grid(.~season) +
    geom_boxplot() +
    geom_jitter(aes(color = fw_acute_exceed), width = 0.1) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("Location") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard)) +
    coord_cartesian(ylim = c(y_min_trib, y_max_trib)) +
    larger_text_theme
  
  # Mainstem plot
  p_ms <- ggplot(mainstem_data, aes(x = factor(as.numeric(river_mile)), y = result_measure_value)) +
    facet_grid(.~season) +
    geom_boxplot() +
    geom_jitter(aes(color = fw_acute_exceed), width = 0.1) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("River Mile") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard)) +
    coord_cartesian(ylim = c(y_min_mainstem, y_max_mainstem)) +
    larger_text_theme
  
  # Define language for plot titles
  tribs <- p_trib + ggtitle(paste(param, "in Kenai River Tributaries", min_year, "to", max_year))
  ms <- p_ms + ggtitle(paste(param, "in Kenai River Mainstem", min_year, "to", max_year))
  
  # Create a blank spacer plot
  spacer <- ggplot() + theme_void() + theme(plot.margin = margin(50, 0, 50, 0)) # Adjust margins for spacing
  
  # Combine plots with the blank spacer in between
  plot_grid(
    ms, 
    spacer, 
    tribs, 
    ncol = 1, 
    rel_heights = c(1, 0.2, 1) # Adjust the spacer height without affecting figure sizes
  )
}







# make geom_point symbology different for exceedences

## metals:
### acute exceedence: color A
### chronic exceedence: color B
### do we need different plot functions for this type of display vs where thresholds are just static?

# make plotly appear on html render, jpg on docx render



