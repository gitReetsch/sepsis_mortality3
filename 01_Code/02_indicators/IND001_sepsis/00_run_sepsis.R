# ******************************************************************************
# 00_run_sepsis.R
# ******************************************************************************
#
# Purpose: Set up the sepsis analysis and run it.
#
# - Load the shared project setup and data configuration.
# - ....
#
# Inputs:
# - The existing project root and its setup/configuration files.
#
# Outputs:
# - A status message in the console.
# - No analysis datasets, plots, or result files.
#
# Set the project root before running this script.
# Run it from the extracted package root in a fresh R session.
#
# ****************************************************************************** 


# Path r-project (root)
root <- normalizePath("C:/dev/gitRepos/sepsis_mortality3",  winslash = "/",  mustWork = TRUE)

# ****************************************************************************** 
# Indicator analysis parameters ----
# ****************************************************************************** 
indicator_id <- "IND001"
indicator_name <- "sepsis"
indicator_folder <- paste0(indicator_id, "_", indicator_name)
index_start_year <- 2021L
index_end_year <- 2024L
sepsis_lookback_months <- 24L
mortality_windows_days <- c(30L, 60L, 90L, 180L, 365L)

# Define folder paths
path_code <- file.path(root, "01_Code")
path_indicator_data    <- file.path(root, "04_Daten", indicator_folder)
path_indicator_results <- file.path(root, "05_Ergebnisse", indicator_folder)
path_indicator_quarto  <- file.path(root, "06_Quarto", indicator_folder)

# Create the indicator output folders based on folder paths (if they don't already exist)
dir.create(path_indicator_data,    recursive = TRUE, showWarnings = FALSE)
dir.create(path_indicator_results, recursive = TRUE, showWarnings = FALSE)
dir.create(path_indicator_quarto,  recursive = TRUE, showWarnings = FALSE)

# Load sepsis definition
source(file.path(path_code,"02_indicators",indicator_folder,"01_definition.R"), local=TRUE)

# ****************************************************************************** 
# Sourcing common setup ----
# ****************************************************************************** 
# - Checks project folders
# - Checks and loads required packages
# - Sets number formatting, plot theme and date stamp
# - Loads shared functions in 00_common/01_functions.R
#
# ****************************************************************************** 

source(file.path(path_code, "00_common", "02_setup.R"), local=TRUE) 


# ****************************************************************************** 
# Sourcing common config 
# ****************************************************************************** 
# - Sets study period: 2019–2024
# - Defines input and output paths
# - Creates a helper for naming shared data files
# - Configuration only; does not read or process data
#
# ****************************************************************************** 

source(file.path(path_code, "00_common", "00_config_data.R"), local=TRUE)


# ****************************************************************************** 
# Create cohot & analysis dataset
# ****************************************************************************** 
# - Recreate the cohort when needed 
#    - uncomment ans run these lines
# - "03_identify_cases.R" 
#    - links discharges and diagnoses  
#    - creates discharges_flagged in memory
# - "04_cohort.R") 
#    - reduces data to cohort
#    - creates STROBE flow table/chart
#    - saves versioned analysis dataset in path_indicator_data
#
# ****************************************************************************** 

# source(file.path(path_code, "02_indicators", indicator_folder, "02_identify_cases.R"), local = TRUE)
# source(file.path(path_code, "02_indicators", indicator_folder, "03_cohort.R"), local = TRUE)


# Open lis for Paper objects (STILL NEEDED?)
paper_objects <- list()

# Load the saved analysis dataset
analysis_dataset_name <- "sepsis_analysis_2026-11-17"

analysis_dataset <- fst::read_fst(
  file.path(
    path_indicator_data,
    paste0(analysis_dataset_name, ".fst")
  )
)


admin_censor_date <- as.Date("2025-12-31")

source(file.path(path_code, "02_indicators", indicator_folder, "04_mortality_outcomes.R"), local = TRUE)
source(file.path(path_code, "02_indicators", indicator_folder, "05_covariates.R"), local = TRUE)
source(file.path(path_code, "02_indicators", indicator_folder, "06_descriptive_mortality.R"), local = TRUE)


 
# ****************************************************************************** 
# CoD-Analyse
# ****************************************************************************** 
# Path CoD datasaet
path_cod     <- file.path(root, "04_Daten", "common", "cod_2019_2024.fst")
# sourcing script
source(file.path(path_code, "02_indicators", indicator_folder, "07_cod_analysis.R"), local = TRUE)

# ****************************************************************************** 
# Models
# ****************************************************************************** 
source(file.path(path_code, "02_indicators", indicator_folder, "08_models.R"), local = TRUE)


# ******************************************************************************
# Hospital classifications ----
# ******************************************************************************
# - Classifies hospitals for in-hospital and 90-day mortality under different
#   risk-adjustment models.
# - Compares classifications across mortality endpoints and control-limit methods.
# - Saves hospital-level classifications and summaries.
# Input:
# - analysis_dataset and results from Scripts 03–08.
# - minimum_site_volume, the minimum number of sepsis cases for classification.
#
# Output:
# - Hospital classification results in memory and in the results directory.
#
# ******************************************************************************

# Minimale Fallzahlen pro Spital für hospital classifications
minimum_site_volume <- 50L

source(file.path(path_code, "02_indicators", indicator_folder, "09_hospital_comparisons.R"), local = TRUE)


#  ******************************************************************************
# Validation and export ----
# ******************************************************************************
# - Loads the result bundles from Scripts 03–09.
# - Checks bundle metadata and validates key manuscript numbers.
# - Saves paper objects, tables and figures.
# Inputs:
# - Run parameters and result bundles in path_indicator_results.
# Outputs:
# - The paper_objects RDS bundle and exported tables and figures.
#
# ****************************************************************************** 

source(file.path(path_code, "02_indicators", indicator_folder, "11_validate_and_export.R"), local = TRUE)
