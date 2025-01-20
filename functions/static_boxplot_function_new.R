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
dat <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")


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

## define horizontal line positions for regulatory limits
hline_data <- reg_vals %>%
  filter(characteristic_name == parameter) 

# how to incorporate other reg vals here (N & P, bacteria, hydrocarbons, etc)

# plan
# 1.) develop lists of all STATIC reg limits. gather together into same DF here, w/ column for name type of limit
# 2.) re-knit doc each time new category is added




# will need a step here to convert units to make reg value match parameter vals?

# Describe Overall Boxplot function
make_boxplot <- function(param) {
  
  # Create data table for a single parameter
  parameter_dat <- dat %>%
    filter(characteristic_name == param) %>%
    mutate(tributary_name = factor(tributary_name, levels = trib_order)) %>%
    # Join regulatory values
    left_join(reg_vals, by = "characteristic_name")
  
  # Get unit for parameter
  unit <- unique(parameter_dat$result_measure_measure_unit_code)
  
  # Set min and max time extent
  min_year <- as.character(min(year(parameter_dat$activity_start_date)))
  max_year <- as.character(max(year(parameter_dat$activity_start_date)))
  
  # Tribs plot with explicit tooltips
  p_trib <- parameter_dat %>%
    ggplot(aes(
      x = factor(tributary_name),
      y = result_measure_value,
      text = paste(
        "Location:", factor(tributary_name),
        "<br>Parameter value:", result_measure_value,
        "<br>Unit:", result_measure_measure_unit_code
      )
    )) +
    facet_grid(.~season) +
    geom_boxplot() +
    geom_jitter(aes(color = fw_acute_exceed), width = 0.1) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("Location") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard))
  
  # Mainstem plot with explicit tooltips
  p_ms <- parameter_dat %>%
    ggplot(aes(
      x = factor(as.numeric(river_mile)),
      y = result_measure_value,
      text = paste(
        "Location:", factor(as.numeric(river_mile)),
        "<br>Parameter value:", result_measure_value,
        "<br>Unit:", result_measure_measure_unit_code
      )
    )) +
    facet_grid(.~season) +
    geom_boxplot() +
    geom_jitter(aes(color = fw_acute_exceed), width = 0.1) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("River Mile") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard))
  
  # Add ggplotly for interactive plots
  tribs_plot <- ggplotly(p_trib, tooltip = "text")
  mainstem_plot <- ggplotly(p_ms, tooltip = "text")
  
  # Return both plots as a list
  list(tribs = tribs_plot, mainstem = mainstem_plot)
}






# make geom_point symbology different for exceedences

## metals:
### acute exceedence: color A
### chronic exceedence: color B
### do we need different plot functions for this type of display vs where thresholds are just static?


