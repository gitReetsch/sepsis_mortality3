# ******************************************************************************
# 04_mortality_outcomes.R
# ******************************************************************************
#
# Purpose:
# - Define valid death events using GVS discharge records and linked BEVNAT data.
# - Derive mortality outcomes and follow-up times from discharge and admission.
# - Assess negative discharge-to-death intervals and compare death counts.
#
# Inputs:
# - analysis_dataset loaded by 00_run_sepsis.R.
# - admin_censor_date and output metadata/paths defined by the run script.
#
# Outputs:
# - analysis_dataset in memory, with mortality outcomes and follow-up times.
# - outcome_definitions and QA tables in memory.
# - A timestamped RDS bundle with outcome definitions and QA outputs.
#
# ******************************************************************************


# ******************************************************************************
# Define event and time interval variables ----
# ******************************************************************************
# Administrative end of BEVNAT follow-up ('admin_censor_date') is defined in run script
analysis_dataset <- analysis_dataset |>
  dplyr::mutate(
    death_gvs                    = as.integer(!is.na(discharge_decision) & discharge_decision == 5L),     # Death recorded at discharge from the index hospitalisation.
    death_bevnat                 = as.integer(!is.na(discharge_to_death_days)),
    death_in_hospital_index_stay = death_gvs,
    death_event                  = as.integer(death_gvs == 1L |                                           # GVS-Tod zählt immer
                                                (death_bevnat == 1L & discharge_to_death_days >=-1)),     # BEVNAT-only zählt bei einem Intervall ab -1 Tag.
    # discharge_to_death_days_analysis
    discharge_to_death_days_analysis = dplyr::case_when(
      death_gvs == 1L ~ 0,                                                                           # GVS-Todesfälle auf Tag 0 setzen
      death_bevnat == 1L & discharge_to_death_days >= -1 ~ pmax(discharge_to_death_days, 0), # BEVNAT-only intervals ≥ −1 retained; −1 set to 0.
      TRUE ~ NA_real_                                                             
    ),
    # Derive time from admission by adding the index-stay length to the 
    # discharge-to-death interval
    admission_to_death_days_analysis = dplyr::case_when(                                 
      death_event == 1L &
        !is.na(discharge_to_death_days_analysis) &
        !is.na(episode_length) ~
        as.numeric(episode_length) + discharge_to_death_days_analysis,                   
      TRUE ~ NA_real_
    ),
    # Approximate discharge date as the last day of the recorded month.
    discharge_date = lubridate::ceiling_date(
      lubridate::ymd(paste0(discharge_month, "01")),        # paste0(discharge_month, "01") -> "20210301" / ubridate::ymd("20210301") -> 2021-03-01 
      unit = "month") - lubridate::days(1),                 # lubridate::ceiling_date(..., unit = "month") -> 2021-04-01 / dann minus 1 Tag -> 2021-03-31
    # Follow-up time from index discharge:
    # deaths use the BEVNAT interval; non-deaths are censored on 31 Dec 2025.
    followup_time_postdischarge_days = dplyr::case_when(
     death_event == 1L & !is.na(discharge_to_death_days_analysis) ~
       discharge_to_death_days_analysis,
     death_event == 0L ~
       as.numeric(admin_censor_date - discharge_date),
     TRUE ~ NA_real_
   ),
   # Follow-up time from index admission:
   # add the index-stay length for survivors; deaths use admission-to-death time.
    followup_time_postadmission_days = dplyr::case_when(
      death_event == 1L & !is.na(admission_to_death_days_analysis) ~ admission_to_death_days_analysis,
      death_event == 0L & !is.na(episode_length) & !is.na(followup_time_postdischarge_days) ~  as.numeric(episode_length) + followup_time_postdischarge_days,
      TRUE ~ NA_real_
    )
  )

# ******************************************************************************
# Outcome decision
# ******************************************************************************
# Two BEVNAT-only records are not counted as death events:
# one has an interval of -978 days in 2022 and the other -4,034 days in 2024.
# These intervals place the recorded deaths approximately 2.7 and 11.0 years
# before the index discharge, respectively, and are implausible as post-discharge
# deaths. They may indicate incorrect patient linkage, although this cannot be
# confirmed from the available data. GVS deaths during the index hospitalisation
# remain classified as deaths. See the QA tables below.
# Binäre Outcomes für die Modellierung


# ******************************************************************************
# Define binary outcome variables ----
# ******************************************************************************

analysis_dataset <- analysis_dataset |>
  dplyr::mutate(
    death_in_hospital       = as.integer(death_in_hospital_index_stay == 1L),
    death_90d_postadmission = as.integer(
      death_event == 1L &
        !is.na(followup_time_postadmission_days) &
        followup_time_postadmission_days <= 90
      )
    )



# Table with outcome variable definitions 
outcome_definitions <- tibble::tribble(
  ~`Outcome variable`, ~Definition,
  "death_in_hospital",
  "1 if death was recorded at discharge from the index sepsis hospitalisation (GVS discharge_decision = 5); 0 otherwise. Deaths during subsequent hospitalisations are not counted.",
  
  "death_90d_postadmission",
  "1 if a valid death event occurred within 90 days after admission to the index sepsis hospitalisation; 0 otherwise."
)

print(outcome_definitions)



# ******************************************************************************
# Quality Assessment (QA) ----
# ******************************************************************************

# ******************************************************************************
## QA - Classify raw BEVNAT intervals for QA ----
# ******************************************************************************

mortality_qa_data <- analysis_dataset |>
  dplyr::mutate(
    interval_group = dplyr::case_when(
      is.na(discharge_to_death_days) ~ "Missing interval",
      discharge_to_death_days < -30 ~ "More than 30 days before discharge",
      discharge_to_death_days >= -30 & discharge_to_death_days <= -8 ~ "-30 to -8 days",
      discharge_to_death_days >= -7 & discharge_to_death_days <= -2 ~ "-7 to -2 days",
      discharge_to_death_days == -1 ~ "-1 day",
      discharge_to_death_days == 0 ~ "0 days",
      discharge_to_death_days > 0 & discharge_to_death_days <= 30 ~ "1 to 30 days",
      discharge_to_death_days > 30 ~ "More than 30 days"
    ),
    interval_group = factor(
      interval_group,
      levels = c(
        "-1 day",
        "-7 to -2 days",
        "-30 to -8 days",
        "More than 30 days before discharge",
        "0 days",
        "1 to 30 days",
        "More than 30 days",
        "Missing interval"
      )
    )
  )


# ******************************************************************************
## QA - Summarise intervals by year and GVS in-hospital death status ----
# ******************************************************************************

qa_interval_summary <- mortality_qa_data |>
  dplyr::group_by(survey_year, interval_group) |>
  dplyr::summarise(
    n_records = dplyr::n(),
    n_gvs_in_hospital_deaths = sum(death_gvs == 1L),
    n_other_discharges = sum(death_gvs == 0L),
    .groups = "drop"
  ) |>
  dplyr::arrange(survey_year, interval_group)

{cat("\n--- qa_interval_summary ---\n")
  print(tibble::as_tibble(qa_interval_summary), n = Inf, width = Inf)}



# ******************************************************************************
## QA - List BEVNAT-only records excluded from the death outcome ----
# ******************************************************************************
# BEVNAT-only records with implausible intervals below -1 day
qa_excluded_bevnat_records <- mortality_qa_data |>
  dplyr::filter(
    death_gvs == 0L,
    !is.na(discharge_to_death_days),
    discharge_to_death_days < -1
  )|>
  dplyr::select(
    survey_year,
    encounter_id,
    patient_id,
    hospital_id_num,
    discharge_to_death_days
  ) |>
  dplyr::arrange(survey_year, discharge_to_death_days)

{cat("\n--- qa_excluded_bevnat_records ---\n")
 print(tibble::as_tibble(qa_excluded_bevnat_records), n = Inf, width = Inf)}

# ******************************************************************************
## QA - Compare previous and selected death counts ----
# ******************************************************************************

qa_death_count_comparison <- mortality_qa_data |>
  dplyr::summarise(
    n_deaths_under_previous_rule = sum(
      death_gvs == 1L | !is.na(discharge_to_death_days)
    ),
    n_deaths_under_selected_rule = sum(death_event == 1L),
    n_bevnat_only_records_excluded = sum(
      death_gvs == 0L &
        !is.na(discharge_to_death_days) &
        discharge_to_death_days < -1
    ),
    .by = survey_year
  ) |>
  dplyr::arrange(survey_year)

{cat("\n--- qa_death_count_comparison ---\n")
 print(tibble::as_tibble(qa_death_count_comparison), n = Inf, width = Inf)}


# ******************************************************************************
# Save outcome definitions and QA outputs ----
# ******************************************************************************

qa_objects <- list(
  qa_interval_summary = qa_interval_summary,
  qa_excluded_bevnat_records = qa_excluded_bevnat_records,
  qa_death_count_comparison = qa_death_count_comparison
)

rds_object_filepath <- file.path(
  path_indicator_results,
  paste0(
    "04_mortality_outcomes_rds_objects_",
    analysis_dataset_name,
    ".rds"
  )
)

saveRDS(
  list(
    metadata = list(
      analysis_dataset_name = analysis_dataset_name,
      indicator_id = indicator_id,
      index_start_year = index_start_year,
      index_end_year = index_end_year,
      datestamp = datestamp
    ),
    outcome_definitions = outcome_definitions,
    qa_objects = qa_objects
  ),
  file = rds_object_filepath 
)

cli::cli_alert_success(
  "Mortality outcome definitions and QA outputs saved: {basename(rds_object_filepath)}"
)
