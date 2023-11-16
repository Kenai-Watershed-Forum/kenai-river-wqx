
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

# Select mainstem Kenai River sites that are part of baseline monitoring

# read in regulatory values
reg_vals <- read_xlsx("other/input/regulatory_limits/master_reg_limits.xlsx", sheet = "static_regulatory_values") %>%
  filter(!is.na(agency),
         standard_type %in% c("drinking_water","irrigation_water","stock_water")) %>%
  remove_empty() %>%
  select(parameter_baseline_name,standard_type,reg_value,reg_unit) %>%
  pivot_wider(names_from = standard_type, values_from = reg_value) %>%
  rename(characteristic_name = parameter_baseline_name)

# --> to do: select whichever static limit is the lowest and show that one, distinguish what type of
# limit by legend

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
    geom_hline(yintercept = parameter_dat$drinking_water, color = "salmon", size = 1.1)

  # mainstem general plot
  p_ms <- parameter_dat %>%
    ggplot() +
    facet_grid(.~season) +
    geom_boxplot(aes(factor(as.numeric(river_mile)),result_measure_value)) +
    geom_jitter(aes(factor(as.numeric(river_mile)),result_measure_value), width = 0.1) +
    ylab(paste0(parameter," (",unit,")")) +
    xlab("River Mile") +
    geom_hline(yintercept = parameter_dat$drinking_water, color = "salmon", size = 1.1)


  # tribs
  tribs <- p_trib %+% subset(parameter_dat, trib_mainstem %in% "t") +
    ggtitle(paste(parameter,"in Kenai River Tributaries\n",min_year,"to",max_year))

  # mainstem
  ms <- p_ms %+% subset(parameter_dat, trib_mainstem %in% "m") +
    ggtitle(paste(parameter,"in Kenai River Mainstem\n",min_year,"to",max_year))

  # plot
  plot_grid(ms,tribs, align = 'v', ncol = 1)
}



# make geom_point geometry different for exceedences
# make plotly appear on html render, jpg on docx render







