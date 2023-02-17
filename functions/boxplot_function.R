
# load packages
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(writexl)
library(hms)
library(DT)
library(readxl)
library(openxlsx)
library(DT)
library(ggpubr)
library(plotrix)
library(remotes)
library(magrittr)
library(janitor)
library(xfun)
library(cowplot)


# Boxplot function

make_boxplot <- function(param) {

  # create data table for parameter
  paramter_dat <- paste0(parameter,"_dat")
  parameter_dat <- dat %>%
    filter(characteristic_name == parameter)

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


  # general plot
  p <- parameter_dat %>%
    ggplot(aes(factor(river_mile),result_measure_value)) +
    facet_grid(.~season) +
    geom_boxplot() +
    xlab("River Mile") +
    ylab(paste0(parameter," (",unit,")"))

  # tribs
  tribs <- p %+% subset(parameter_dat, trib_mainstem %in% "t") +
    ggtitle(paste(parameter,"in Kenai River Tributaries\n",min_year,"to",max_year))

  # mainstem
  ms <- p %+% subset(parameter_dat, trib_mainstem %in% "m") +
    ggtitle(paste(parameter,"in Kenai River Mainstem\n",min_year,"to",max_year))

  # plot
  plot_grid(ms,tribs, align = 'v', ncol = 1)
}

# "to do" on boxplot function
#  include adec reg limits as hlines
# make plot taller
# label tributaries with names on x axis





