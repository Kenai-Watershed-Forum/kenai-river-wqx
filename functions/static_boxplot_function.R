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
library (patchwork)

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
# HERE

# field parameters threshold values
ph_reg_vals <- read.csv("other/input/regulatory_limits/formatted_reg_vals/ph_reg_vals.csv")


# join all static regulatory value dataframes
# old: reg_vals <- static_metals_reg_vals
reg_vals <- bind_rows(static_metals_reg_vals,ph_reg_vals)

# write combined csv of all static regulatory values
reg_vals_path <- "other/output/regulatory_values/all_reg_vals.csv"
write.csv(reg_vals, reg_vals_path)



#### PREPARE PLOT #####

# Function to dynamically generate two ggplots with optional sample fraction filtering
# Function to dynamically generate two ggplots
create_facet_plots <- function(data_path, reg_vals_path, characteristic) {
  
  # Read the datasets
  data <- read.csv(data_path, stringsAsFactors = FALSE) %>%
    # for plotting, exclude "Rejected" values (did not meet QAPP standards)
    filter(result_status_identifier != "Rejected")
  
  # read in regulatory values
  reg_vals <- read.csv(reg_vals_path, stringsAsFactors = FALSE)
  
  # Apply sample fraction filter if provided
  if (!is.null(sample_fraction)) {
    data <- data %>% filter(result_sample_fraction_text %in% sample_fraction)
  }
  
  
  # Dynamically filter regulatory values for the given characteristic_name
  reg_values <- reg_vals %>% filter(characteristic_name == characteristic)
  
  # Handle case where characteristic_name is not found
  if (nrow(reg_values) == 0) {
    hline_data <- data.frame(yintercept = numeric(0), linetype = character(0))
  } else {
    hline_data <- reg_values %>%
      filter(!is.na(value)) %>%  # Ensure regulatory values are not NA
      mutate(yintercept = as.numeric(value)) %>%  # Ensure numeric conversion
      select(yintercept, linetype = Standard)
  }
  
  # Prepare hline data for multiple standards
  hline_data <- reg_values %>%
    mutate(value = ifelse(is.na(value), NA, as.numeric(value))) %>%  # Ensure numeric values, even if NA
    select(yintercept = value, linetype = Standard)
  
  # Get unique river_mile values
  unique_river_miles <- unique(data$river_mile)
  
  # Filter and preprocess the main data
  preprocess_data <- function(x_var_filter) {
    data <- data %>%
      mutate(activity_start_date = as.Date(activity_start_date, format = "%Y-%m-%d"),
             year = as.numeric(format(activity_start_date, "%Y")))
    data %>%
      filter((x_var_filter == "tributary_name" & trib_mainstem != "m") | (x_var_filter == "river_mile" & trib_mainstem != "t")) %>%
      mutate(
        tributary_name = factor(
          tributary_name,
          levels = c(
            "No Name Creek",
            "Beaver Creek",
            "Slikok Creek",
            "Soldotna Creek",
            "Funny River",
            "Moose River",
            "Killey River",
            "Russian River",
            "Juneau Creek"
          )
        ),
        river_mile = factor(river_mile, levels = sort(unique_river_miles))
      ) %>%
      filter(characteristic_name == characteristic) %>%
      mutate(
        result_measure_value = as.numeric(result_measure_value),  # Ensure numeric
        fw_acute_exceed = ifelse(is.na(fw_acute_exceed), NA, fw_acute_exceed),
        fw_chronic_exceed = ifelse(is.na(fw_chronic_exceed), NA, fw_chronic_exceed),
        season = as.factor(season)  # Ensure season is categorical
      ) %>%
      filter(!is.na(result_measure_value))  # Remove rows with NA values
  }
  
  # Create plots for tributary_name and river_mile
  create_plot <- function(subset_data, x_var) {
    min_year <- as.character(min(subset_data$year, na.rm = TRUE))
    max_year <- as.character(max(subset_data$year, na.rm = TRUE))
    y_min <- min(subset_data$result_measure_value, na.rm = TRUE) * 0.9
    y_max <- max(subset_data$result_measure_value, na.rm = TRUE) * 1.1
    
    ggplot(subset_data, aes(x = get(x_var), y = result_measure_value)) +
      geom_boxplot(aes(group = get(x_var)), outlier.shape = NA) +
      geom_jitter(aes(
        color = factor(case_when(
          is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "gray",
          
          is.na(fw_acute_exceed) & fw_chronic_exceed == "Y" ~ "orange",
          
          fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "blue"
          
        )),
        shape = factor(case_when(
          
          is.na(fw_acute_exceed) & is.na(fw_chronic_exceed) ~ "gray",
          is.na(fw_acute_exceed) & fw_chronic_exceed == "Y" ~ "orange",
          fw_acute_exceed == "Y" & fw_chronic_exceed == "Y" ~ "blue"
          
        ))
      ), width = 0.2, size = 3, show.legend = TRUE) +
      geom_hline(data = hline_data, aes(yintercept = yintercept, linetype = linetype), color = "#D55E00", size = 1.2, show.legend = TRUE) +
      facet_wrap(~season) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      scale_color_manual(
        name = "Hardness Dependent\nExceedance Type",
        breaks = c("gray", "orange", "blue"),
        labels = c("gray" = "None", "orange" = "Chronic", "blue" = "Acute"),
        values = c(
          "gray" = "#7F7F7F",    # Gray for None (colorblind-friendly neutral gray)
          "orange" = "#E69F00",  # Orange for Acute (colorblind-friendly orange)
          "blue" = "#0072B2"      # Blue for Chronic (colorblind-friendly blue)
        )
      ) +
      scale_shape_manual(
        name = "Hardness Dependent\nExceedance Type",
        breaks = c("gray", "orange", "blue"),
        labels = c("gray" = "None", "orange" = "Chronic", "blue" = "Acute"),
        values = c(
          "gray" = 16,    # Acute: NA, Chronic: NA
          "orange" = 16,    # Acute: NA, Chronic: Y
          "blue" = 8      # Acute: Y, Chronic: Y
        )
      ) +
      scale_linetype_manual(
        name = "Regulatory Standard",
        values = c(
          "drinking_water" = "solid",
          "irrigation_water" = "longdash",
          "stock_water" = "dotted",
          "wildlife" = "twodash",
          "recreation" = "dotdash"
        )
      ) +
      guides(
        linetype = guide_legend(override.aes = list(color = "red", size = 1.2)),
        color = guide_legend(override.aes = list(size = 4, linetype = 0)),
        shape = guide_legend(override.aes = list(size = 4, linetype = 0))
      ) +
      
      labs(
        title = paste(
          "Kenai River",
          ifelse(x_var == "tributary_name", "Tributaries", "Mainstem"), ",",
          characteristic, ",",
          sample_fraction, ",",
          min_year, "-", max_year
        )
      ) +
      xlab("") +
      ylab(paste0(characteristic, " (",subset_data$result_measure_measure_unit_code,")")) +
      
      #theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16),
        legend.position = "right"
      )
  }
  
  # Generate plots
  tributary_data <- preprocess_data("tributary_name")
  river_mile_data <- preprocess_data("river_mile")
  
  plot_tributary <- create_plot(tributary_data, "tributary_name")
  plot_river_mile <- create_plot(river_mile_data, "river_mile")
  
  return(list(
    tributary_plot = plot_tributary,
    river_mile_plot = plot_river_mile
  ))
}


# print plots
plots <- create_facet_plots(data_path, reg_vals_path, characteristic)
print(plots$tributary_plot)
print(plots$river_mile_plot)







