# ******************************************************************************
# 03_cohort.R
# ******************************************************************************
#
# Purpose: Define the sepsis study population using a 24-month lookback.
#
# - Calculate the observed 24-month sepsis history from the 2019–2024 records.
# - Select the first eligible index hospitalization per person in 2021–2024.
# - Start the STROBE flow with all discharges in the index period and apply
#   the study-population criteria.
# - Check the resulting cohort and save the flow outputs.
#
# Inputs:
# - discharges_flagged from 03_identify_cases.R
# - start_year, index_start_year, index_end_year and sepsis_lookback_months
#
# Outputs:
# - path_indicator_data: versioned FST file with the selected sepsis analysis dataset.
# - path_indicator_results: STROBE flow chart (PNG) and flow log (XLSX).
# - Console: flow log by year and cohort QA summaries.
# - In memory: flow, sepsis_analysis, sepsis_analysis_all and QA tables.
#
# ******************************************************************************


# ******************************************************************************
# Check input ----
# ******************************************************************************
required_vars <- c(
  "survey_year",
  "encounter_id",
  "discharge_month",
  "residence_country",
  "service_centre_main",
  "patient_id",
  "sepsis_flag",
  "discharge_to_death_days"
)

missing_vars <- setdiff(required_vars, names(discharges_flagged))

if (length(missing_vars) > 0L) {
  stop("discharges_flagged is missing required variable(s): ", paste(missing_vars, collapse = ", "))}

discharges_flagged <- discharges_flagged |>
  dplyr::mutate(
    patient_id = dplyr::na_if(trimws(as.character(patient_id)), ""),
    encounter_key = paste(survey_year, encounter_id, sep = "_")
  )

if (anyNA(discharges_flagged[c("survey_year", "encounter_id")])) {
  stop("Missing survey year or encounter ID in discharges_flagged.")}

if (anyDuplicated(discharges_flagged$encounter_key) > 0L) {
  stop("Encounter key is not unique in discharges_flagged.")}

# ******************************************************************************
# Calculate lookback and index flags using the full observation period ----
# ******************************************************************************

# Build the history from eligible sepsis hospitalizations across all observed years.
# It is used to assess prior sepsis before creating the 2021–2024 flow.
sepsis_history <- discharges_flagged |>
  dplyr::filter(
    !is.na(discharge_month),                         # A month is needed to order hospitalizations and calculate the lookback
    as.character(residence_country) == "8100",       # only Swiss-residence 
    !service_centre_main %in% c("M500", "M950"),     # only acute care service-centre 
    sepsis_flag > 0L,                                # only if sepsis diagnosis present (principal or secondary)
    !is.na(patient_id)                               # patient ID is needed to link hospitalizations over time
  ) |>
  dplyr::mutate(
    sepsis_month = lubridate::ymd(                   # Convert YYYYMM to the first day of that month for date calculations
      paste0(as.character(discharge_month), "01"))
  )

if (anyNA(sepsis_history$sepsis_month)) {            # Catch non-missing month values that could not be parsed as valid dates
  stop("Some eligible sepsis records have an invalid discharge month.")
}

data_start_month_index <- start_year * 12L + 1L      # Month index for January of the first observed data year

sepsis_history <- sepsis_history |>
  dplyr::mutate(
    sepsis_month_index =                            # Convert each month to a consecutive integer for month-difference calculations
      lubridate::year(sepsis_month) * 12L +
      lubridate::month(sepsis_month)
  ) |>
  dplyr::group_by(patient_id) |>                    # Calculate the prior history separately for each patient
  dplyr::arrange(
    sepsis_month_index,
    encounter_id,
    .by_group = TRUE
  ) |>                                               # Order by month; encounter_id breaks ties within a month
  dplyr::mutate(
    sepsis_hospitalization_sequence = dplyr::row_number(), # Number each patient's observed eligible sepsis hospitalization in order
    previous_sepsis_month_index = dplyr::lag(sepsis_month_index), # Month index of the immediately preceding observed sepsis hospitalization
    months_since_previous_sepsis =                    # Gap, in calendar months, from the preceding sepsis hospitalization
      sepsis_month_index - previous_sepsis_month_index,
    lookback_available =                              # TRUE when at least the full lookback period is covered by the source data
      sepsis_month_index - data_start_month_index >= sepsis_lookback_months,
    sepsis_in_lookback24m = dplyr::case_when(         # Flag a prior observed sepsis hospitalization in the lookback window
      !lookback_available ~ NA_integer_,              # Use NA when the data do not cover the full lookback period
      !is.na(months_since_previous_sepsis) &
        months_since_previous_sepsis <= sepsis_lookback_months ~ 1L, # 1 means a prior event occurred within 24 months
      TRUE ~ 0L                                       # 0 means no prior event was observed within 24 months
    ),
    eligible_index_candidate = as.integer(            # Identify cases eligible to become an index hospitalization
      survey_year >= index_start_year &               # Restrict candidate index stays to the configured index period
        survey_year <= index_end_year &
        sepsis_in_lookback24m == 0L                   # Require no observed eligible sepsis stay in the lookback window
    ),
    index_hospitalization = as.integer(               # Select at most one index hospitalization per patient
      eligible_index_candidate == 1L &
        cumsum(eligible_index_candidate) == 1L        # Keep the first eligible candidate in the index period
    )
  ) |>
  dplyr::ungroup()                                    # Remove patient grouping before later joins and summaries

# Confirm that the lookback is observable for every historical sepsis event
# occurring in the configured index period.
index_period_history <- sepsis_history |>
  dplyr::filter(
    survey_year >= index_start_year,
    survey_year <= index_end_year
  )

if (anyNA(index_period_history$sepsis_in_lookback24m)) { # Stop if any index-period event lacks a classifiable lookback
  stop(
    "The 24-month lookback is not fully observable for some index-period ",
    "sepsis records. Check the data start year and index period."
  )
}

# Keep the history variables needed to attach the lookback and index flags
# to the 2021–2024 start dataset.
index_history_vars <- sepsis_history |>
  dplyr::select(
    survey_year,                                     # First part of the encounter key
    encounter_id,                                    # Second part of the encounter key
    sepsis_hospitalization_sequence,                 # Position in the patient's observed sepsis sequence
    previous_sepsis_month_index,                     # Month index of the preceding sepsis hospitalization
    months_since_previous_sepsis,                    # Months since the preceding sepsis hospitalization
    lookback_available,                              # Whether the full lookback window is observable
    sepsis_in_lookback24m,                           # Whether sepsis was observed within that window
    eligible_index_candidate,                        # Whether this event can qualify as an index candidate
    index_hospitalization                            # Whether this event was selected as the patient's index
  )

if (anyDuplicated(index_history_vars[c("survey_year", "encounter_id")]) > 0L) { # Ensure the later join cannot duplicate discharge rows
  stop("Calculated lookback variables have duplicate encounter keys.")
}

# ******************************************************************************
# Define the start population  ----
# ******************************************************************************

# The flow starts with every discharge in 2021–2024. The history variables
# were calculated above using the full 2019–2024 data.
discharges_2021_2024 <- discharges_flagged |>
  dplyr::filter(
    survey_year >= index_start_year,
    survey_year <= index_end_year
  ) |>
  dplyr::left_join(
    index_history_vars,
    by = c("survey_year", "encounter_id"),
    relationship = "one-to-one",
    na_matches = "never"
  )


# ******************************************************************************
# Create STROBE flow data  ----
# ******************************************************************************

flow_result <- strobe_flow(
  input_data = discharges_2021_2024,
  start_label = paste0(
    "All inpatient discharges, ",
    index_start_year, "–", index_end_year
  ),
  by_year = TRUE,
  by_year_var = survey_year,
  steps = list(
    flow_step(
      label = "Discharge month available (indicating a GVS record of type 'A')",
      criterion = !is.na(discharge_month),
      criteria_type = "study population"
    ),
    flow_step(
      label = "Residence in Switzerland",
      criterion = as.character(residence_country) == "8100",
      criteria_type = "study population",
      exclude_label = "Excluded: non-Swiss residents"   
      ),
    flow_step(
      label = "Included acute-care cost centres",
      criterion = !service_centre_main %in% c("M500", "M950"),
      criteria_type = "study population",
      exclude_label = "Excluded: non-acute cost centres (M500, M950)"
      ),
    flow_step(
      label = "Meets the explicit sepsis ICD definition",
      criterion = !is.na(sepsis_flag) & sepsis_flag > 0L,
      criteria_type = "study population",
      exclude_label = "Excluded: no explicit sepsis code"
    ),
    flow_step(
      label = "Patient identifier available for patient-level lookback",
      criterion = !is.na(patient_id),
      criteria_type = "analysis population",
      exclude_label = "Excluded: no valid patient ID"
    ),
    flow_step(
      label = "No sepsis hospitalization in the preceding 24 months",
      criterion = sepsis_in_lookback24m == 0L,
      criteria_type = "analysis population",
      exclude_label = "Excluded: sepsis in prior 24 months"
    ),
    flow_step(
      label = "First eligible sepsis hospitalization per patient",
      criterion = index_hospitalization == 1L,
      criteria_type = "analysis population",
      exclude_label = "Excluded: later eligible admissions"
    )
  )
)

# The flow object is just the log; the filtered data is the analysis dataset
flow <- flow_result$flow_log
sepsis_analysis <- flow_result$output_dataset


# ******************************************************************************
# Cohort QA ----
# ******************************************************************************
sepsis_analysis_all <- sepsis_history

qa_sepsis_sequence <- sepsis_analysis_all |>
  dplyr::summarise(
    n_sepsis_hospitalizations = dplyr::n(),
    n_patients = dplyr::n_distinct(patient_id),
    n_with_sepsis_in_lookback =
      sum(sepsis_in_lookback24m == 1L, na.rm = TRUE),
    n_eligible_index_candidates =
      sum(eligible_index_candidate == 1L),
    n_index_hospitalizations =
      sum(index_hospitalization == 1L),
    n_index_patients = dplyr::n_distinct(
      patient_id[index_hospitalization == 1L]
    )
  )

qa_duplicate_index_patients <- sepsis_analysis_all |>
  dplyr::filter(index_hospitalization == 1L) |>
  dplyr::count(patient_id, name = "n_index_hospitalizations") |>
  dplyr::filter(n_index_hospitalizations > 1L)

qa_index_within_lookback <- sepsis_analysis_all |>
  dplyr::filter(
    index_hospitalization == 1L,
    !is.na(months_since_previous_sepsis),
    months_since_previous_sepsis <= sepsis_lookback_months
  )

qa_index_by_year <- sepsis_analysis_all |>
  dplyr::filter(index_hospitalization == 1L) |>
  dplyr::count(survey_year, name = "n_index_hospitalizations") |>
  dplyr::arrange(survey_year)

# Check same-month sepsis hospitalizations with missing death intervals.
qa_index_ordering <- sepsis_analysis_all |>
  dplyr::group_by(patient_id, discharge_month) |>
  dplyr::summarise(
    n_hospitalizations = dplyr::n(),
    all_missing_death_time = all(is.na(discharge_to_death_days)),
    any_index_hospitalization = any(index_hospitalization == 1L),
    .groups = "drop"
  ) |>
  dplyr::filter(n_hospitalizations > 1L, all_missing_death_time)

qa_index_ordering_summary <- qa_index_ordering |>
  dplyr::summarise(
    ambiguous_patient_months = dplyr::n(),
    ambiguous_hospitalizations = sum(n_hospitalizations),
    percent_hospitalizations = round(
      100 * ambiguous_hospitalizations / nrow(sepsis_analysis_all), 2
    ),
    ambiguous_index_patient_months = sum(any_index_hospitalization),
    percent_index_hospitalizations_affected = round(
      100 * ambiguous_index_patient_months /
        sum(sepsis_analysis_all$index_hospitalization),
      2
    )
  )

qa_hospitalizations_per_patient <- sepsis_analysis_all |>
  dplyr::count(patient_id, name = "n_sepsis_hospitalizations") |>
  dplyr::count(n_sepsis_hospitalizations, name = "n_patients") |>
  dplyr::mutate(
    percent = round(100 * n_patients / sum(n_patients), 1)
  ) |>
  dplyr::arrange(n_sepsis_hospitalizations)

print(tibble::as_tibble(qa_sepsis_sequence), n = Inf)
print(tibble::as_tibble(qa_index_by_year), n = Inf)
print(tibble::as_tibble(qa_index_ordering_summary), n = Inf)
print(tibble::as_tibble(qa_hospitalizations_per_patient), n = Inf)

stopifnot(nrow(qa_duplicate_index_patients) == 0L)
stopifnot(nrow(qa_index_within_lookback) == 0L)


# ******************************************************************************
# Prepare and print the flow log ----
# ******************************************************************************

# Put years in columns and add totals at the right for console review and export.
flow_log_by_year <- flow |>
  dplyr::select(
    flow_step,
    keep_criteria_label,
    keep_criteria,
    criteria_type,
    population,
    exclude_criteria_label,
    year,
    remaining,
    excluded
  ) |>
  tidyr::pivot_longer(
    cols = c(remaining, excluded),
    names_to = "count_type",
    values_to = "n"
  ) |>
  tidyr::pivot_wider(
    id_cols = c(
      flow_step,
      keep_criteria_label,
      keep_criteria,
      criteria_type,
      population,
      exclude_criteria_label
    ),
    names_from = c(count_type, year),
    values_from = n,
    names_glue = "{count_type}_{year}"
  ) |>
  dplyr::mutate(
    remaining_Total = rowSums(
      dplyr::pick(dplyr::starts_with("remaining_")),
      na.rm = TRUE
    ),
    excluded_Total = rowSums(
      dplyr::pick(dplyr::starts_with("excluded_")),
      na.rm = TRUE
    )
  ) |>
  dplyr::select(
    dplyr::everything(),
    remaining_Total,
    excluded_Total
  )

print(tibble::as_tibble(flow_log_by_year), n = Inf, width = Inf)

# Copy in Zwischenablage für Einfügen in Excel
# write.table(
#   as.data.frame(flow_log_by_year),
#   "clipboard",
#   sep = "\t",
#   row.names = FALSE,
#   col.names = TRUE,
#   na = ""
# )

# ******************************************************************************
# Save outputs ----
# ******************************************************************************

flowchart_filepath        <- file.path(path_indicator_results, paste0("flowchart_", indicator_id, "_", datestamp, ".png"))
flow_log_filepath         <- file.path(path_indicator_results, paste0("flowchart_", indicator_id, "_", datestamp, ".xlsx"))
analysis_dataset_filepath <- file.path(path_indicator_data,    paste0("sepsis_analysis_", datestamp, ".fst"))

# Leite den Datensatznamen aus der FST-Datei ab
analysis_dataset_name    <- tools::file_path_sans_ext(basename(analysis_dataset_filepath))

# Path for rds file based on dataset name
rds_object_filepath   <- file.path(path_indicator_results, paste0("03_cohort_rds_objects_", analysis_dataset_name, ".rds"))

# save png
plot_strobe_flow(
  flow_log = flow,
  file_png = flowchart_filepath,
  file_pdf = NULL,
  title = paste0(
    "Flow chart for ", indicator_name,
    " (", index_start_year, "–", index_end_year, ")"
  ),
  by_year = FALSE
)

# save xlsx with flow table
write_excel_tables_update(
  list = list(flow_table = flow_log_by_year),
  file = flow_log_filepath
)

# save analysis dataset as fst
fst::write_fst(sepsis_analysis, analysis_dataset_filepath)

# Save flow data for validation and reporting
saveRDS(
  list(
    metadata = list(
      analysis_dataset_name = analysis_dataset_name,
      indicator_id = indicator_id,
      index_start_year = index_start_year,
      index_end_year = index_end_year,
      datestamp = datestamp
    ),
    cohort_size = nrow(sepsis_analysis),
    flow_log = flow,
    flow_log_by_year = flow_log_by_year
  ),
  file = rds_object_filepath 
)

# final message
message(
  "Cohort construction complete. Analysis population: ",
  nrow(sepsis_analysis),
  " index hospitalizations.\n",
  "Files created:\n",
  " - Analysis dataset: ", analysis_dataset_filepath, "\n",
  " - STROBE flow chart: ", flowchart_filepath, "\n",
  " - Flow log (as xlsx table): ", flow_log_filepath, "\n",
  " - Flow log (as rds object): ", rds_object_filepath 
)
