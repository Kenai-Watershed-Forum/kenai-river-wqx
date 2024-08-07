
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
# read in prepared data from local directory
dat <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")


#### READ IN Regulatory Threshold Values ####
# read in and combine various static reg values

# static metals threshold values
static_metals_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/static_metals_reg_vals.csv")

# join all regulatory value dataframes
reg_vals <- static_metals_reg_vals


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



# will need a step here to convert units to make reg value match parameter vals

# Describe Overall Boxplot function

make_boxplot <- function(param) {
  
  # create data table for a single parameter
  paramter_dat <- paste0(parameter,"_dat")
  parameter_dat <- dat %>%
    filter(characteristic_name == parameter) %>%
    mutate(tributary_name = factor(tributary_name, levels = trib_order)) %>%
    
    # join regulatory values
    left_join(reg_vals, by = "characteristic_name" )
  
  # get unit for parameter
  unit <- unique(parameter_dat$result_measure_measure_unit_code)
  
  # set  min time extent
  min_year <- parameter_dat %>%
    mutate(year = year(activity_start_date)) %>%
    summarise(min_year = min(year))
  min_year <- as.character(min_year$min_year)
  
  # set  max time extent
  max_year <- parameter_dat %>%
    mutate(year = year(activity_start_date)) %>%
    summarise(max_year = max(year))
  max_year <- as.character(max_year$max_year)
  
  # trib general plot
  p_trib <- parameter_dat %>%
    ggplot() +
    facet_grid(.~season) +
    geom_boxplot(aes(factor(tributary_name),result_measure_value)) +
    geom_jitter(aes(factor(tributary_name),result_measure_value), width = 0.1) +
    ylab(paste0(parameter," (",unit,")")) +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("Site") +
    # create horizontal threshold lines for STATIC regulatory values
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard)) +
    
    # hide legend in white color
    theme(
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.key = element_rect(fill = "white")) + 
    scale_color_discrete(
      guide = guide_legend(override.aes = list(color = "white")))
  
  
  # mainstem general plot
  p_ms <- parameter_dat %>%
    ggplot() +
    facet_grid(.~season) +
    geom_boxplot(aes(factor(as.numeric(river_mile)),result_measure_value)) +
    geom_jitter(aes(factor(as.numeric(river_mile)),result_measure_value), width = 0.1) +
    ylab(paste0(parameter," (",unit,")")) +
    xlab("River Mile") +
    
    # create horizontal threshold lines for STATIC regulatory values
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard, color = Standard)) 
  
  
  # define language for plot titles
  static_plot_type <- "\nStatic Regulatory Values"

  # tribs
  tribs <- p_trib %+% subset(parameter_dat, trib_mainstem %in% "t") +
    ggtitle(paste(parameter,"in Kenai River Tributaries ",min_year,"to",max_year,static_plot_type))
  
  # mainstem
  ms <- p_ms %+% subset(parameter_dat, trib_mainstem %in% "m") +
    ggtitle(paste(parameter,"in Kenai River Mainstem ",min_year,"to",max_year,static_plot_type))
  
  # place trib and mainstem plot images together
  plot_grid(ms,tribs, align = 'v', ncol = 1)

  
## plots completed ##
  
}




# make geom_point geometry different for exceedences
# make plotly appear on html render, jpg on docx render



