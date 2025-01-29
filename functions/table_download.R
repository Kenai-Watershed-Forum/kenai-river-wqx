# point to source of parameter data for download tables
dat <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")

# Define table download function
download_tbl <- function(char){

  # create data table for parameter
  parameter_dat <- paste0(characteristic,"_dat")
  parameter_dat <- dat %>%
    filter(characteristic_name == characteristic)

  # write csv
  dir <- paste0("other/output/parameter_downloads/",characteristic," Kenai River Baseline Data",".csv")
  write.csv(parameter_dat, dir )
  xfun::embed_file(dir)
}

