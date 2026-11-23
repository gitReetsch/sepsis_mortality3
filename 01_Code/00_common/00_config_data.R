# *********************************************************************************
# 00_config_data.R
# *********************************************************************************
# 
# Purpose: Define the study period and paths for raw inputs and shared data.
#
# - Set the overall data years.
# - Configure raw GVS and CoD input paths.
# - Define locations for shared data and preparation outputs.
# - Provide a helper for naming common data files.
#
# This script defines configuration only; it does not read data or run preparation.
#
# *********************************************************************************

# Data period covered by the input data
start_year <- 2019L
end_year <- 2024L

# Define folder paths
path_common <- file.path(root, "04_Daten", "common")
path_preparation_results <- file.path(root, "05_Ergebnisse", "preparation")
path_lookups <- file.path(root, "04_Daten", "lookups")
# Persistent lookup tables for hospital and hospital-site IDs.
# Existing tables are updated with new IDs when needed.

# define file paths 
path_diagnoses <- list(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_DIA_2019_250642.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_DIA_2020_250642.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_DIA_2021_250642.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_DIA_2022_250642.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_DIA_2023_250642.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_DIA_2024_250642.csv")
)

path_chop <- list(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_PROC_2019_250642.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_PROC_2020_250642.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_PROC_2021_250642.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_PROC_2022_250642.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_PROC_2023_250642.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_PROC_2024_250642.csv")
)

path_icu_raw <- list(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_ICU_2019_250642.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_ICU_2020_250642.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_ICU_2021_250642.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_ICU_2022_250642.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_ICU_2023_250642.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_ICU_2024_250642.csv")
)

path_discharges <- c(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_ENCOUNTER_2019_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103230.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_ENCOUNTER_2020_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103229.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_ENCOUNTER_2021_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103226.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_ENCOUNTER_2022_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103226.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_ENCOUNTER_2023_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103225.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_ENCOUNTER_2024_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103223.csv")
)

path_cod <- file.path(
  root,
  "03_Inputdaten",
  "TU",
  "Datenlieferung_TU_2019_2024_20260409_Pseudonymize_out_20260421_091332_379_20260421_091438_506_20260421_094438_788.csv"
)


# Build a file path for a shared prepared dataset using its name and the data period.
# Example: common_file("diagnoses") -> diagnoses_2019_2024.fst
common_file <- function(stem) file.path(path_common,
  sprintf("%s_%d_%d.fst", stem, start_year, end_year))

