# ******************************************************************************
# 03_identify_cases.R
# ******************************************************************************
#
# Purpose: Identify discharge records that meet the explicit ICD sepsis definition.
#
# - Read the prepared discharge and diagnosis data.
# - Apply the sepsis ICD code list using the shared ICD flag function.
# - Join the resulting encounter-level flags to the discharge records.
# - Check the join and report sepsis-flagged encounters by year.
#
# Eligibility criteria are applied in 04_cohort.R.
#
# Inputs:
# - common_file("discharges")
# - common_file("diagnoses")
# - sepsis_indicator_definition from 02_definition.R
#
# Output:
# - discharges_flagged in memory, for use by 04_cohort.R.
# - A console QA table of flagged encounters by year.
# - No data files are written.
#
# ******************************************************************************

# Read the prepared data
discharges <- fst::read_fst(common_file("discharges"), as.data.table = FALSE)
diagnoses <- fst::read_fst(common_file("diagnoses"), as.data.table = FALSE)

# Check that the inputs contain the variables required here and in 04_cohort.R
required_discharge_vars <- c(
  "survey_year",
  "encounter_id",
  "discharge_month",
  "residence_country",
  "service_centre_main",
  "patient_id"
)

required_diagnosis_vars <- c(
  "survey_year",
  "encounter_id",
  "rank",
  "icd_clean",
  "icd_add_clean"
)

missing_discharge_vars <- setdiff(required_discharge_vars, names(discharges))
if (length(missing_discharge_vars) > 0L) {
  stop(
    "Discharge data are missing required variables: ",
    paste(missing_discharge_vars, collapse = ", ")
  )
}

missing_diagnosis_vars <- setdiff(required_diagnosis_vars, names(diagnoses))
if (length(missing_diagnosis_vars) > 0L) {
  stop(
    "Diagnosis data are missing required variables: ",
    paste(missing_diagnosis_vars, collapse = ", ")
  )
}

# Use the year and encounter together because encounter IDs may repeat across years
encounter_key_vars <- c("survey_year", "encounter_id")

if (anyNA(discharges[encounter_key_vars])) {
  stop("Discharge data contain missing year or encounter IDs.")
}

if (anyDuplicated(discharges[encounter_key_vars]) > 0L) {
  stop("Discharge data contain duplicate year/encounter keys.")
}

# Treat blank or whitespace-only patient IDs as missing for the later cohort filter
discharges <- discharges |>
  dplyr::mutate(patient_id = dplyr::na_if(trimws(as.character(patient_id)), ""))

# Apply the exact-code sepsis definition; this returns one row per flagged encounter
sepsis_flags <- create_icd_flag(
  data = diagnoses,
  indicator = sepsis_indicator_definition
)

if (anyNA(sepsis_flags[encounter_key_vars])) {
  stop("Sepsis flags contain missing year or encounter IDs.")
}

if (anyDuplicated(sepsis_flags[encounter_key_vars]) > 0L) {
  stop("Sepsis flags contain duplicate year/encounter keys.")
}

# Attach the newly generated sepsis flags without changing the discharge row count
discharges_flagged <- discharges |>
  dplyr::left_join(
    sepsis_flags,
    by = encounter_key_vars,
    relationship = "one-to-one",
    na_matches = "never"
  ) |>
  dplyr::mutate(
    encounter_key = paste(survey_year, encounter_id, sep = "_")
  )

if (nrow(discharges_flagged) != nrow(discharges)) {
  stop("Joining sepsis flags changed the number of discharge records.")
}

# QA count only: discharge encounters matching the explicit sepsis ICD code list.
# This is before 04_cohort.R applies the discharge, residence, service-centre,
# patient-ID, and index-cohort criteria; it is not the final study cohort.
qa_sepsis_identification <- discharges_flagged |>
  dplyr::summarise(
    n_discharge_records = dplyr::n(),
    n_sepsis_encounters = sum(sepsis_flag > 0L, na.rm = TRUE),
    .by = survey_year
  )

tibble::as_tibble(qa_sepsis_identification) |>
  print(n = Inf)
