# ==============================================================================
# GENERATE REFERENCE FILES FOR KENAI RIVER WATER QUALITY QA/QC
# Run this script ONCE to create the two remaining reference files
# ==============================================================================

library(readxl)
library(tidyverse)

# Set path
aqwms_path <- "other/input/AQWMS"

cat("Generating reference files...\n\n")

# ==============================================================================
# 1. CREATE qapp_dqo_standards.csv
# ==============================================================================

cat("Creating qapp_dqo_standards.csv...\n")

# Data Quality Objectives from 2023 QAPP Tables 2-6
qapp_dqo <- data.frame(
  parameter = c(
    "Fecal Coliform",
    "Total suspended solids",
    "Nitrate+Nitrite",
    "Phosphorus, total",
    "Calcium, Total",
    "Iron, Total",
    "Magnesium, Total",
    "Arsenic",
    "Cadmium",
    "Chromium",
    "Copper",
    "Lead",
    "Zinc",
    "Gasoline Range Organics",
    "Diesel Range Organics",
    "Residual Range Organics",
    "Benzene",
    "Ethylbenzene",
    "Toluene",
    "Xylene (m,p)",
    "Xylene (o)",
    "Xylenes (total)"
  ),
  method = c(
    "9222D",
    "2540-D",
    "4500-NO3(F)",
    "4500-P-E",
    "200.7",
    "200.7",
    "200.7",
    "200.8",
    "200.8",
    "200.8",
    "200.8",
    "200.8",
    "200.8",
    "AK101",
    "AK102",
    "AK103",
    "8260D",
    "8260D",
    "8260D",
    "8260D",
    "8260D",
    "8260D"
  ),
  units = c(
    "cfu/100ml",
    "mg/L",
    "mg/L",
    "mg/L",
    "mg/L",
    "mg/L",
    "mg/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "mg/L",
    "mg/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L",
    "ug/L"
  ),
  loq = c(
    1.0, 1.0, 0.20, 0.02, 4.0, 8.0, 2.0,
    5.0, 0.50, 2.0, 1.0, 0.2, 10.0,
    5.0, 110, 540,
    0.40, 1.0, 1.0, 2.0, 1.0, 3.0
  ),
  mdl = c(
    NA, 0.31, 0.05, 0.01, 0.9, 3.0, 0.3,
    1.5, 0.15, 0.78, 0.031, 0.06, 3.1,
    3.0, 6.5, 220,
    0.12, 0.31, 0.31, 0.62, 0.31, 0.82
  ),
  max_holding_time_hours = c(
    6, 168, 672, 672, 4320, 4320, 4320,
    4320, 4320, 4320, 4320, 4320, 4320,
    336, 336, 336,
    336, 336, 336, 336, 336, 336
  ),
  precision_rpd_target = c(
    NA, 5, 25, 25, 20, 20, 20,
    20, 20, 20, 20, 20, 20,
    20, 20, 20,
    20, 20, 20, 20, 20, 20
  ),
  accuracy_low = c(
    NA, NA, 90, 75, 85, 85, 85,
    NA, NA, NA, NA, NA, NA,
    60, NA, 60,
    NA, NA, NA, NA, NA, NA
  ),
  accuracy_high = c(
    NA, NA, 110, 125, 115, 115, 115,
    NA, NA, NA, NA, NA, NA,
    120, NA, 120,
    NA, NA, NA, NA, NA, NA
  ),
  notes = c(
    "Control checks: sterility, temperature",
    "7 days holding time",
    "28 days holding time",
    "28 days holding time",
    "6 months holding time",
    "6 months holding time",
    "6 months holding time",
    "Filter within 14 days then 6 months",
    "Filter within 14 days then 6 months",
    "Filter within 14 days then 6 months",
    "Filter within 14 days then 6 months",
    "Filter within 14 days then 6 months",
    "Filter within 14 days then 6 months",
    "14 days holding time",
    "14 days holding time",
    "14 days holding time",
    "14 days holding time - BTEX",
    "14 days holding time - BTEX",
    "14 days holding time - BTEX",
    "14 days holding time - BTEX",
    "14 days holding time - BTEX",
    "14 days holding time - BTEX"
  )
)

# Save
write.csv(qapp_dqo, file.path(aqwms_path, "qapp_dqo_standards.csv"), row.names = FALSE)
cat("  ✓ Created qapp_dqo_standards.csv\n")

# ==============================================================================
# 2. CREATE site_coordinates.csv (extract from AWQMS template)
# ==============================================================================

cat("\nCreating site_coordinates.csv...\n")

# Read from AWQMS template
sites <- read_excel(file.path(aqwms_path, "AWQMS_KWF_Baseline_2021.xlsx"), 
                    sheet = "Monitoring Locations") %>%
  select(`Monitoring Location ID`, 
         `Monitoring Location Name`,
         Latitude, 
         Longitude) %>%
  rename(
    monitoring_location_id = `Monitoring Location ID`,
    monitoring_location_name = `Monitoring Location Name`,
    latitude = Latitude,
    longitude = Longitude
  )

# Save
write.csv(sites, file.path(aqwms_path, "site_coordinates.csv"), row.names = FALSE)
cat("  ✓ Created site_coordinates.csv with", nrow(sites), "sites\n")

# ==============================================================================
# 3. VERIFY ALL REFERENCE FILES EXIST
# ==============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("VERIFICATION: Checking all required reference files\n")
cat(rep("=", 70), "\n", sep = "")

required_files <- c(
  "qapp_dqo_standards.csv",
  "site_coordinates.csv",
  "analysis_code_matching_table.xlsx",
  "AQWMS_template_matching_table.xlsx",
  "analytes_list_manual_edit.csv",
  "AWQMS_KWF_Baseline_2021.xlsx"
)

all_present <- TRUE
for(file in required_files) {
  file_path <- file.path(aqwms_path, file)
  if(file.exists(file_path)) {
    cat("✓", file, "\n")
  } else {
    cat("❌", file, "- MISSING\n")
    all_present <- FALSE
  }
}

cat(rep("=", 70), "\n", sep = "")

if(all_present) {
  cat("\n✅ SUCCESS: All reference files are present!\n")
  cat("You can now run the main QA/QC script.\n")
} else {
  cat("\n⚠️ WARNING: Some reference files are missing.\n")
  cat("Please ensure all files are in:", aqwms_path, "\n")
}