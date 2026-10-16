# ==============================================================================
# REFRESH LOCAL CACHE OF EPA WATER QUALITY PORTAL (WQP) DATA
#
# Run this script MANUALLY (not sourced during book render) whenever new data
# is expected on WQP (KWF's baseline monitoring data is typically synced to
# WQP roughly once per year). It queries WQP live via the `dataRetrieval`
# package for organization "KENAI_WQX" and writes a local, gitignored cache
# that `chapters/data_sourcing.qmd` reads at render time.
#
# `chapters/data_sourcing.qmd` never calls the WQP API itself; it only reads
# the cache files produced here. This keeps renders fast/offline-capable and
# avoids hitting the WQP API on every render.
# ==============================================================================

library(dataRetrieval)
library(tidyverse)

cache_dir <- "other/input/WQX_downloads/wqp_live"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cat("Querying WQP for organization = KENAI_WQX...\n\n")

# ── Sample results ───────────────────────────────────────────────────────────
cat("Fetching sample results (readWQPdata, dataProfile = 'narrowResult')...\n")
narrowresult <- dataRetrieval::readWQPdata(
  organization = "KENAI_WQX",
  dataProfile  = "narrowResult"
)

# ── Station metadata ─────────────────────────────────────────────────────────
cat("Fetching station metadata (whatWQPsites)...\n")
station <- dataRetrieval::whatWQPsites(
  organization = "KENAI_WQX"
)

# ── Write cache files ────────────────────────────────────────────────────────
narrowresult_path <- file.path(cache_dir, "narrowresult.csv")
station_path      <- file.path(cache_dir, "station.csv")

write_csv(narrowresult, narrowresult_path)
write_csv(station, station_path)

# ── Write refresh metadata (surfaced in data_sourcing.qmd narrative) ────────
refresh_metadata <- tibble(
  refreshed_at         = Sys.time(),
  narrowresult_n_rows  = nrow(narrowresult),
  station_n_rows       = nrow(station),
  narrowresult_min_date = suppressWarnings(min(narrowresult$ActivityStartDate, na.rm = TRUE)),
  narrowresult_max_date = suppressWarnings(max(narrowresult$ActivityStartDate, na.rm = TRUE))
)

write_csv(refresh_metadata, file.path(cache_dir, "refresh_metadata.csv"))

cat("\nDone. Cache written to:", cache_dir, "\n")
cat("  -", narrowresult_path, "(", nrow(narrowresult), "rows )\n")
cat("  -", station_path, "(", nrow(station), "rows )\n")
cat("\nRefresh summary:\n")
print(refresh_metadata)
