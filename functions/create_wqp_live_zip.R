# Create a zip file of the wqp_live cache directory for download
# This script is meant to be sourced during book rendering

source_dir <- "other/input/WQX_downloads/wqp_live"
output_zip <- "docs/assets/wqp_live.zip"

if (!dir.exists("docs/assets")) {
  dir.create("docs/assets", showWarnings = FALSE, recursive = TRUE)
}

# Create zip: include only the CSV files (narrowresult, station, detection_limits, refresh_metadata)
zip_files <- file.path(source_dir, c("narrowresult.csv", "station.csv", "detection_limits.csv", "refresh_metadata.csv"))
zip_files <- zip_files[file.exists(zip_files)]

if (length(zip_files) > 0) {
  zip(
    zipfile = output_zip,
    files = zip_files,
    flags = "-j"  # -j: store without directory structure
  )
  message("Created WQP data zip: ", output_zip)
} else {
  warning("No CSV files found in ", source_dir)
}
