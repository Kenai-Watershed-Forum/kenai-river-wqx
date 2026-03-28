# point to source of parameter data for download tables
dat <- read.csv("other/output/analysis_format/baseline_analysis_format.csv")

# Define table download function
download_tbl <- function(char){

  # create data table for parameter
  parameter_dat <- dat %>%
    filter(characteristic_name == characteristic)

  # Apply the same sample fraction filter used by the boxplot function, if set.
  # NA-fraction rows (historical records with no fraction recorded) are retained
  # alongside explicitly named fractions, consistent with the plot behavior.
  if (exists("sample_fraction", envir = .GlobalEnv) && length(sample_fraction) > 0) {
    parameter_dat <- parameter_dat %>%
      filter(result_sample_fraction_text %in% sample_fraction | is.na(result_sample_fraction_text))
  }

  # write csv
  dir <- paste0("other/output/parameter_downloads/",characteristic," Kenai River Baseline Data",".csv")
  write.csv(parameter_dat, dir)
  xfun::embed_file(dir)
}

