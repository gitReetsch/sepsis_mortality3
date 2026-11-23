# =====================================================================
# 01_setup
# =====================================================================

rm(list = ls())
gc()

# Projekt-Root
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Ordnerstruktur bauen
dir.create(file.path(root, "00_Archiv"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "01_Code"),       recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "02_Grundlagen"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "03_Inputdaten"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "04_Daten"),      recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "05_Ergebnisse"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "06_Quarto"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "07_Temp"),       recursive = TRUE, showWarnings = FALSE)

# Outputpfade
path_output          <- normalizePath(file.path(root, "05_Ergebnisse"),  winslash = "/", mustWork = TRUE)

# Functions 
source(file.path(root, "01_Code/00_functions.R"))

# Prefix / suffix
prefix <- NULL 
suffix <- NULL 

# Variablenliste laden
file_path <- file.path(root, "02_Grundlagen", "GVS_hospital_inpatient250642.csv")
varlist <- data.table::fread(file_path)
str(varlist)
varlist[, !c("name_it", "name_fr"), with = FALSE]

cols <- c("select", "table", "name_de", "name_en")   # <- hier deine Spaltennamen

dt <- fread(
  file_path,
  select = cols
)
# View(varlist)

# ===================================================
# Study period
# ===================================================
datestamp           <- format(Sys.Date(), "%Y-%m-%d")

# Data observation period
start_year <- 2019
end_year   <- 2024


# Eligible index hospitalization period
index_start_year <- 2021
index_end_year   <- 2024

# Look-back period for previous sepsis hospitalization
sepsis_lookback_months <- 24

# Mortality windows
mortality_windows_days <- c(30, 60, 90, 180, 365)


# ===================================================
# Input files: list of the paths
# ===================================================

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

path_icu <- list(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_ICU_2019_250642.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_ICU_2020_250642.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_ICU_2021_250642.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_ICU_2022_250642.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_ICU_2023_250642.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_ICU_2024_250642.csv")
)

# file suffixes for discharge table
path_discharges <- c(
  "2019" = file.path(root, "03_Inputdaten", "GVS", "2019", "INPATIENT_ENCOUNTER_2019_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103230.csv"),
  "2020" = file.path(root, "03_Inputdaten", "GVS", "2020", "INPATIENT_ENCOUNTER_2020_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103229.csv"),
  "2021" = file.path(root, "03_Inputdaten", "GVS", "2021", "INPATIENT_ENCOUNTER_2021_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103226.csv"),
  "2022" = file.path(root, "03_Inputdaten", "GVS", "2022", "INPATIENT_ENCOUNTER_2022_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103226.csv"),
  "2023" = file.path(root, "03_Inputdaten", "GVS", "2023", "INPATIENT_ENCOUNTER_2023_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103225.csv"),
  "2024" = file.path(root, "03_Inputdaten", "GVS", "2024", "INPATIENT_ENCOUNTER_2024_250642_KS_GVS_F2F_Key_GVS_DSV250642_Pseudonymize_20260526103223.csv")
)


# ------------------------------------------------------------------------------
# ICD-10 definitions
# ------------------------------------------------------------------------------

# Explicit sepsis codes used for:
# 1. cohort identification
# 2. strong cause-of-death evidence
sepsis_explicit_codes <- c(
  "A021", "A207", "A327", "A391", "A392", "A394",
  "A400", "A401", "A402", "A403", "A408", "A409",
  "A410", "A411", "A412", "A413", "A414",
  "A4151", "A4152", "A4158", "A418", "A419",
  "A427", "B377",
  "P360", "P361", "P362", "P363", "P364", "P365", "P368", "P369",
  "O85", "O753",
  "R650", "R651", "R572",
  "A483", "A499"
)

# Infection codes used for moderate cause-of-death evidence.
# Based on the infection component of the implicit sepsis definition
# in the Swiss Sepsis Report, with predefined empirical additions.
sepsis_infection_prefixes <- c(
  "A010", "A021", "A327",
  "A390", "A391", "A392", "A393", "A394", "A395", "A398", "A399",
  "A400", "A401", "A402", "A403", "A408", "A409",
  "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419",
  "A427",
  "A490", "A491", "A492", "A493", "A498", "A499",
  "B370", "B371", "B372", "B373", "B374", "B375", "B376", "B377",
  "B378", "B379",
  "B950", "B951", "B952", "B953", "B9541", "B9542", "B9548",
  "B955", "B956", "B957", "B9590", "B9591",
  "B962", "B963", "B965", "B966", "B967", "B968",
  "P360", "P361", "P362", "P363", "P364", "P365", "P368", "P369",
  "P375",
  "O85", "O753",
  "U071", "U072",
  "G00", "G01", "G02", "G03",
  "I30", "I32", "I33", "I39", "I40", "I41", "I80",
  "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16",
  "J17", "J18", "J20", "J21", "J22", "J47", "J85", "J86",
  "K35", "K61", "K65",
  "L03", "L04", "L08", "L88",
  "M00", "M01", "M86",
  "N10", "N11", "N12", "N16", "N30", "N34",
  "J69",
  "N390",
  "T814",
  "T826", "T827",
  "T845", "T846", "T847",
  "T857",
  "O86"
)

# Strong evidence against sepsis: external causes of death.
sepsis_against_prefixes_strong <- c(
  "V", "W", "X"
)

# Moderate evidence against sepsis:
# predefined major competing non-infectious causes.
sepsis_against_prefixes_moderate <- c(
  "I21",
  "I71",
  "T79",
  "I60", "I61", "I62"
)



# =====================================================================
# pipeline
# =====================================================================
# 
# source(file.path(root, "01_Code/02_prep_icd.R"))
# source(file.path(root, "01_Code/03_prep_chop.R"))
# source(file.path(root, "01_Code/04_prep_discharges.R"))
# source(file.path(root, "01_Code/05_prep_cod.R"))
# source(file.path(root, "01_Code/06_prep_icu.R"))
# source(file.path(root, "01_Code/08_link_discharges_cod.R"))
# source(file.path(root, "01_Code/09_link_icd_chop.R"))
# source(file.path(root, "01_Code/Ind_001_sepsis.R"))
# 



