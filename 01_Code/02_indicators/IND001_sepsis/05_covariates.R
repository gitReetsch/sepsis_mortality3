# ******************************************************************************
# 05_covariates.R
# ******************************************************************************
#
# Purpose:
# - Join encounter-level Charlson and Elixhauser scores.
# - Derive covariates for the sepsis mortality risk-adjustment models.
# - Check covariate completeness and distributions.
#
# Input:
# - analysis_dataset loaded and prepared by 00_run_sepsis.R and prior scripts.
# - comorbidity_scores_<start_year>_<end_year>.fst created by 05_comorbidity.R.
#
# Outputs:
# - analysis_dataset in memory, with risk-adjustment covariates and descriptive variables.
# - QA tables printed to the console and saved in a timestamped RDS file.
#
# ******************************************************************************


# ******************************************************************************
# Check required variables and load comorbidity scores ----
# ******************************************************************************

required_covariate_inputs <- c(
  "survey_year",
  "encounter_id",
  "class_age",
  "gender",
  "admission_type",
  "sepsis_flag",
  "sepsis_trigger_icd"
)

missing_covariate_inputs <- setdiff(required_covariate_inputs,names(analysis_dataset))

if (length(missing_covariate_inputs) > 0L) {
  stop(
    "Missing required variables in analysis_dataset: ",
    paste(missing_covariate_inputs, collapse = ", "),
    call. = FALSE)}

if (any(c("charlson_score", "elixhauser_vw_score") %in% names(analysis_dataset))) {
  stop(
    "Comorbidity score variables already exist in analysis_dataset. ",
    "Check whether scores have already been joined before running 05_covariates.R.",
    call. = FALSE)}

comorbidity_scores_file <- file.path(path_common, 
                                     glue::glue("comorbidity_scores_{start_year}_{end_year}.fst"))

if (!file.exists(comorbidity_scores_file)) {
  stop(
    "Comorbidity score file not found: ",
    comorbidity_scores_file,
    call. = FALSE)}

comorbidity_scores <- fst::read_fst(comorbidity_scores_file)

required_score_variables <- c(
  "survey_year",
  "encounter_id",
  "charlson_score",
  "elixhauser_vw_score"
)

missing_score_variables <- setdiff(
  required_score_variables,
  names(comorbidity_scores)
)

if (length(missing_score_variables) > 0L) {
  stop("Missing required variables in comorbidity score file: ",
       paste(missing_score_variables, collapse = ", "), call. = FALSE )}

if (anyDuplicated(comorbidity_scores[c("survey_year", "encounter_id")]) > 0L) {
  stop("Comorbidity score file contains duplicate survey_year/encounter_id keys.", call. = FALSE)}


# ******************************************************************************
# Join comorbidity scores ----
# ******************************************************************************
n_analysis_before_join <- nrow(analysis_dataset)

analysis_dataset <- analysis_dataset |>
  dplyr::left_join(
    comorbidity_scores |>
      dplyr::select(
        survey_year,
        encounter_id,
        charlson_score,
        elixhauser_vw_score
      ),
    by = c("survey_year", "encounter_id")
  )

if (nrow(analysis_dataset) != n_analysis_before_join) {
  stop("Joining comorbidity scores changed the number of rows in analysis_dataset.",  call. = FALSE )
}

# ******************************************************************************
# Join ICU and mechanical ventilation data for descriptive analyses ----
# ******************************************************************************
icu_file <- file.path(path_common, glue::glue("icu_{start_year}_{end_year}.fst"))

if (!file.exists(icu_file)) {
  stop("ICU data file not found: ", icu_file, call. = FALSE)}

icu_data <- fst::read_fst(icu_file)

icu_required_columns <- c(
  "survey_year",
  "encounter_id",
  "icu_duration",
  "icu_resp_duration"
)

missing_icu_columns <- setdiff(icu_required_columns, names(icu_data))

if (length(missing_icu_columns) > 0L) {
  stop("ICU data are missing required column(s): ", 
       paste(missing_icu_columns, collapse = ", "), 
       call. = FALSE)}

encounter_key <- c("survey_year", "encounter_id")

if (anyNA(icu_data[encounter_key]) || anyNA(analysis_dataset[encounter_key])) {
  stop("Missing survey_year or encounter_id in the ICU linkage.", call. = FALSE)}

if (anyDuplicated(analysis_dataset[encounter_key]) > 0L) {
  stop("Encounter keys are not unique in analysis_dataset.", call. = FALSE)}

if (any(icu_data$icu_duration < 0, na.rm = TRUE) || any(icu_data$icu_resp_duration < 0, na.rm = TRUE)) {
  stop("ICU and ventilation durations must not be negative.", call. = FALSE)}

# Aggregate multiple ICU records to one row per hospitalisation.
icu_encounter <- icu_data |>
  dplyr::summarise(
    icu_hours         = sum(icu_duration, na.rm = TRUE),
    ventilation_hours = sum(icu_resp_duration, na.rm = TRUE),
    .by = c(survey_year, encounter_id)
  ) |>
  dplyr::mutate(
    icu_admission = as.integer(icu_hours > 0),
    mechanical_ventilation = as.integer(ventilation_hours > 0),
    icu_length_group = dplyr::case_when(
      icu_hours == 0  ~ "No ICU",
      icu_hours <= 24 ~ "≤24 hours",
      icu_hours <= 72 ~ "25–72 hours",
      icu_hours <= 168 ~ "73 hours–7 days",
      icu_hours > 168 ~ ">7 days"
    ),
    icu_length_group = factor(
      icu_length_group,
      levels = c(
        "No ICU",
        "≤24 hours",
        "25–72 hours",
        "73 hours–7 days",
        ">7 days"
      ),
      ordered = TRUE
    )
  )

if (anyDuplicated(icu_encounter[encounter_key]) > 0L) {
  stop("ICU data are not unique after aggregation.", call. = FALSE)}

icu_output_columns <- c(
  "icu_hours",
  "ventilation_hours",
  "icu_admission",
  "mechanical_ventilation",
  "icu_length_group",
  "icu_admission_group",
  "mechanical_ventilation_group"
)

if (any(icu_output_columns %in% names(analysis_dataset))) {
  stop("ICU variables already exist in analysis_dataset. Check for a previous ICU join.", call. = FALSE)}

n_before_icu_join <- nrow(analysis_dataset)

analysis_dataset <- analysis_dataset |>
  dplyr::left_join( icu_encounter, by = encounter_key ) |>
  dplyr::mutate(
    # An index hospitalisation with no ICU record is treated as no ICU stay.
    icu_hours = dplyr::coalesce(icu_hours, 0),
    ventilation_hours = dplyr::coalesce(ventilation_hours, 0),
    icu_admission = dplyr::coalesce(icu_admission, 0L),
    mechanical_ventilation = dplyr::coalesce(mechanical_ventilation, 0L),
    icu_length_group = factor(
      dplyr::coalesce(as.character(icu_length_group), "No ICU"),
      levels = c(
        "No ICU",
        "≤24 hours",
        "25–72 hours",
        "73 hours–7 days",
        ">7 days"
      ),
      ordered = TRUE
    ),
    icu_admission_group = factor(
      dplyr::if_else(icu_admission == 1L, "ICU admission", "No ICU admission"),
      levels = c("ICU admission", "No ICU admission")
    ),
    mechanical_ventilation_group = factor(
      dplyr::if_else(
        mechanical_ventilation == 1L,
        "Mechanical ventilation",
        "No mechanical ventilation"
      ),
      levels = c("Mechanical ventilation", "No mechanical ventilation")
    )
  )

if (nrow(analysis_dataset) != n_before_icu_join) {
  stop("ICU linkage changed the number of analysis records.", call. = FALSE)}

# QA summary by year; keep this object for the final validation bundle.
qa_icu_linkage <- analysis_dataset |>
  dplyr::summarise(
    n_hospitalisations         = dplyr::n(),
    n_icu_admissions           = sum(icu_admission),
    pct_icu_admissions         = 100 * mean(icu_admission),
    n_mechanical_ventilation   = sum(mechanical_ventilation),
    pct_mechanical_ventilation = 100 * mean(mechanical_ventilation),
    .by = survey_year
  ) |>
  dplyr::arrange(survey_year)


# ******************************************************************************
# Derive risk-adjustment covariates ----
# ******************************************************************************
analysis_dataset <- analysis_dataset |>
  dplyr::mutate(
    # Age groups
    age_group = dplyr::case_when(
      class_age == "D0T27"  ~ "0–27 days",
      class_age == "D28T36" ~ "28–364 days",
      class_age == "_NA"    ~ "1–4 years",
      class_age == "Y5T14"  ~ "5–14 years",
      class_age == "Y15T39" ~ "15–39 years",
      class_age == "Y40T64" ~ "40–64 years",
      class_age == "YGE65"  ~ "≥65 years",
      TRUE                  ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c(
        "0–27 days",
        "28–364 days",
        "1–4 years",
        "5–14 years",
        "15–39 years",
        "40–64 years",
        "≥65 years"
      ),
      ordered = TRUE
    ),
    # sex    
    sex = dplyr::case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE        ~ NA_character_
    ),
    sex = factor(sex, levels = c("Female", "Male")),
    # diagnosis position    
    sepsis_trigger_position = dplyr::case_when(
      sepsis_flag == 1L ~ "main_diagnosis",
      sepsis_flag == 2L ~ "secondary_diagnosis",
      sepsis_flag == 3L ~ "add_on_to_main_diagnosis",
      TRUE              ~ NA_character_
    ),
    trigger_position_group = dplyr::case_when(
      sepsis_trigger_position == "main_diagnosis" ~ "Primary diagnosis",
      sepsis_trigger_position %in% c(
        "secondary_diagnosis",
        "add_on_to_main_diagnosis"
      ) ~ "Secondary diagnosis or add-on to main diagnosis",
      TRUE ~ NA_character_
    ),
    # sepsis severity
    sepsis_severity_group = dplyr::case_when(
      is.na(sepsis_trigger_icd) ~ NA_character_,
      stringr::str_detect(sepsis_trigger_icd, "R572") ~ "Septic shock",
      stringr::str_detect(sepsis_trigger_icd, "R651") ~ "Severe sepsis / SIRS with organ dysfunction",
      stringr::str_detect(sepsis_trigger_icd, "R650") ~ "SIRS without organ dysfunction",
      TRUE ~ "Other explicit sepsis code"
    ),
    sepsis_severity_group = factor(
      sepsis_severity_group,
      levels = c(
        "Other explicit sepsis code",
        "SIRS without organ dysfunction",
        "Severe sepsis / SIRS with organ dysfunction",
        "Septic shock"
      ),
      ordered = TRUE
    ),
    # Emergency vs. planned admission
    admission_type_group = dplyr::case_when(
      admission_type == 1 ~ "Emergency admission",
      admission_type == 2 ~ "Planned admission",
      TRUE                ~ "Other / unknown"
    ),
    admission_type_group = factor(
      admission_type_group,
      levels = c(
        "Emergency admission",
        "Planned admission",
        "Other / unknown"
      )
    ),
    # Grouped Elixhauser scores (for descriptive summaries, continuous  score is used in the risk-adjustment model)
    elixhauser_vw_group = cut(
      elixhauser_vw_score,
      breaks = c(-Inf, 0, 5, 10, 20, Inf),
      labels = c("≤0", "1–5", "6–10", "11–20", ">20"),
      ordered_result = TRUE
    ),
    # Grouped Charlson scores
    charlson_group = cut(
      charlson_score,
      breaks = c(-Inf, 0, 1, 2, 4, Inf),
      labels = c("0", "1", "2", "3–4", "≥5"),
      ordered_result = TRUE
    )
  )


# ******************************************************************************
# QA: overall distribution of cohort characteristics ----
# ******************************************************************************
profile_columns <- c(
  "age_group",
  "sex",
  "admission_type_group",
  "trigger_position_group",
  "sepsis_severity_group",
  "elixhauser_vw_group",
  "icu_admission_group",
  "mechanical_ventilation_group"
)

if (anyNA(analysis_dataset[profile_columns])) {
  stop("Cohort-profile variables contain missing values.")}

cohort_size <- nrow(analysis_dataset)
qa_cohort_characteristics <- analysis_dataset |>
  dplyr::transmute(
    `Age group`                                         = as.character(age_group),
    `Sex`                                               = as.character(sex),
    `Admission type`                                    = as.character(admission_type_group),
    `Position of sepsis-defining diagnosis`             = dplyr::recode(
      as.character(trigger_position_group),
      "Primary diagnosis"                               = "Principal diagnosis",
      "Secondary diagnosis or add-on to main diagnosis" = "Secondary diagnosis or add-on to the principal diagnosis"
    ),
    `Diagnosis-based sepsis severity proxy`             = as.character(sepsis_severity_group),
    `Elixhauser van Walraven score`                     = as.character(elixhauser_vw_group),
    `ICU admission`                                     = as.character(icu_admission_group),
    `Mechanical ventilation`                            = as.character(mechanical_ventilation_group)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "characteristic",
    values_to = "category"
  ) |>
  dplyr::count(characteristic, category, name = "n") |>
  dplyr::mutate(pct = 100 * n / cohort_size) |> 
  as_tibble()

# ******************************************************************************
# QA - Covariate missingness by year ----
# ******************************************************************************
model_covariates <- c(
  "age_group",
  "sex",
  "elixhauser_vw_score",
  "admission_type_group",
  "trigger_position_group",
  "sepsis_severity_group"
)

qa_covariate_missingness <- analysis_dataset |>
  dplyr::summarise(
    n_records = dplyr::n(),
    dplyr::across(
      dplyr::all_of(model_covariates),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    ),
    n_complete_covariates = sum(
      dplyr::if_all(
        dplyr::all_of(model_covariates),
        ~ !is.na(.x)
      )
    ),
    .by = survey_year
  ) |>
  dplyr::arrange(survey_year) |> 
  as_tibble()

# ******************************************************************************
# QA - Covariate distributions ----
# ******************************************************************************
qa_age_group        <- freq_table(analysis_dataset, age_group, by = survey_year, totals = "both") |> as_tibble()
qa_sex              <- freq_table(analysis_dataset, sex, by = survey_year, totals = "both") |> as_tibble()
qa_admission_type   <- freq_table(analysis_dataset, admission_type_group, by = survey_year, totals = "both") |> as_tibble()
qa_trigger_position <- freq_table(analysis_dataset, trigger_position_group, by = survey_year, totals = "both") |> as_tibble()
qa_sepsis_severity  <- freq_table(analysis_dataset, sepsis_severity_group, by = survey_year, totals = "both") |> as_tibble()
qa_elixhauser_group <- freq_table(analysis_dataset, elixhauser_vw_group, by = survey_year, totals = "both") |> as_tibble()

# ******************************************************************************
# Print in cosole & save RDS ----
# ******************************************************************************
# QA-Objekte zusammenstellen
qa_objects <- list(
  qa_covariate_missingness  = qa_covariate_missingness,
  qa_age_group              = qa_age_group,
  qa_sex                    = qa_sex,
  qa_admission_type         = qa_admission_type,
  qa_trigger_position       = qa_trigger_position,
  qa_sepsis_severity        = qa_sepsis_severity,
  qa_elixhauser_group       = qa_elixhauser_group,
  qa_icu_linkage            = qa_icu_linkage,
  qa_cohort_characteristics = qa_cohort_characteristics
)

# Save covariate-related QA outputs as rds
rds_object_filepath  <- file.path(
  path_indicator_results, 
  glue::glue("05_covariates_rds_objects_{analysis_dataset_name}.rds"))

saveRDS(
  list(
    metadata = list(
      analysis_dataset_name = analysis_dataset_name,
      indicator_id          = indicator_id,
      index_start_year      = index_start_year,
      index_end_year        = index_end_year,
      datestamp             = datestamp
    ),
    qa_objects = qa_objects
  ),
  file = rds_object_filepath 
)


# Dieselben QA-Objekte mit Namen in der Konsole ausgeben
for (object_name in names(qa_objects)) {
  cat("\n---", object_name, "---\n")
  
  x <- qa_objects[[object_name]]
  # if data.frame, then as tibble printen
  if (is.data.frame(x)) {
    print(tibble::as_tibble(x), n = Inf, width = Inf)
  } else {print(x)}
}

# ******************************************************************************
# End of 05_covariates.R ----
# ******************************************************************************
cli::cli_alert_success( "Covariates created & QA outputs saved: {basename(rds_object_filepath)}")


