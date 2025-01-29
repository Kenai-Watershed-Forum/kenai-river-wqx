# TO DO LIST
# - symbology for exceedences of acute vs chronic hardness-dependent values (example: cadmium; has both)
# - reg values for hydrocarbons, N, P, others
# - adjust text for referencing plotly objects in all chapters
# - fix legends
# - alter pop up windows for plotly
# - address outliers
# - make plotly appear on html render, jpg on docx render
# - some pllots appear to have multiple boxplots per site (zinc?)


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
library (patchwork)

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
  filter(characteristic_name == parameter) %>%
  select(-static_category)

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
    mutate(
      tributary_name = factor(tributary_name, levels = trib_order),
      acute_color = ifelse(is.na(fw_acute_exceed), "DarkGray", "Highlighted"),
      chronic_shape = case_when(
        is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "GrayCircle",
        fw_acute_exceed == "Y" & is.na(fw_chronic_exceed) ~ "ColoredCircle",
        fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "ColoredAsterisk"
      )
    ) %>%
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
  size <- 1.2
  larger_text_theme <- theme(
    axis.title = element_text(size = rel(size)),
    axis.text = element_text(size = rel(size)),
    strip.text = element_text(size = rel(size)),
    legend.text = element_text(size = rel(size)),
    legend.title = element_text(size = rel(size)),
    plot.title = element_text(size = rel(size), hjust = 0.5)
  )
  
  # Define color and shape palettes
  color_palette <- c("DarkGray" = "#555555", "Highlighted" = "#E69F00")
  shape_palette <- c(
    "GrayCircle" = 16,
    "ColoredCircle" = 16,
    "ColoredAsterisk" = 8
  )
  
  # Tribs plot
  p_trib <- ggplot(trib_data, aes(
    x = factor(tributary_name), 
    y = result_measure_value,
    color = acute_color,
    shape = chronic_shape
  )) +
    facet_grid(.~season) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2) +
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = shape_palette) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard),
               color = "darkred",
               size = 1.2) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("Location") +
    ggtitle(paste(param, "in Kenai River Tributaries", min_year, "to", max_year)) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(y_min_trib, y_max_trib)) +
    larger_text_theme
  
  # Mainstem plot
  p_ms <- ggplot(mainstem_data, aes(
    x = factor(as.numeric(river_mile)), 
    y = result_measure_value,
    color = acute_color,
    shape = chronic_shape
  )) +
    facet_grid(.~season) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.2) +
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = shape_palette) +
    geom_hline(data = hline_data,
               aes(yintercept = value, linetype = Standard),
               color = "darkred",
               size = 1.2) +
    ylab(paste0(param, " (", unit, ")")) +
    xlab("River Mile") +
    ggtitle(paste(param, "in Kenai River Mainstem", min_year, "to", max_year)) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(y_min_mainstem, y_max_mainstem)) +
    larger_text_theme
  
  # Combined legend
  legend_data <- trib_data %>%
    bind_rows(mainstem_data) %>%
    distinct(acute_color, chronic_shape) %>%
    mutate(label = case_when(
      chronic_shape == "GrayCircle" ~ "NA for Acute and Chronic",
      chronic_shape == "ColoredCircle" ~ "Acute Y, Chronic NA",
      chronic_shape == "ColoredAsterisk" ~ "Acute Y, Chronic Y"
    ))
  
  # Add hline legend representation
  hline_legend <- hline_data %>%
    distinct(Standard) %>%
    mutate(linetype = Standard)
  
  legend_plot <- ggplot() +
    geom_point(data = legend_data, aes(x = 1, y = 1, color = acute_color, shape = chronic_shape), size = 4) +
    scale_color_manual(values = color_palette) +
    scale_shape_manual(values = shape_palette) +
    geom_hline(data = hline_legend, aes(yintercept = -Inf, linetype = linetype), color = "darkred", size = 1.2) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = rel(size)),
      legend.text = element_text(size = rel(size))
    ) +
    guides(
      color = guide_legend("Legend", override.aes = list(size = 4)),
      shape = guide_legend("Legend", override.aes = list(size = 4)),
      linetype = guide_legend("Static Regulatory Standards", override.aes = list(color = "darkred", size = 1.2))
    )
  
  # Combine plots without extra white space
  combined_plot <- plot_grid(
    p_ms, 
    p_trib, 
    ncol = 1, 
    rel_heights = c(1, 1)
  )
  
  # Add a precise white rectangle over extraneous symbols
  white_overlay <- ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white", alpha = 1) +
    theme_void()
  
  final_plot <- ggdraw() +
    draw_plot(combined_plot, x = 0, y = 0, width = 0.75, height = 1) +
    draw_plot(legend_plot, x = 0.75, y = 0, width = 0.25, height = 1) +
    draw_plot(white_overlay, x = 0.72, y = 0, width = 0.03, height = 1) # Precise masking of symbols
  
  final_plot
}
























































# make geom_point symbology different for exceedences

## metals:
### acute exceedence: color A
### chronic exceedence: color B
### do we need different plot functions for this type of display vs where thresholds are just static?

# make plotly appear on html render, jpg on docx render



