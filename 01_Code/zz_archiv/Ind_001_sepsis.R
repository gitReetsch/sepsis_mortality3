# *********************************************************************
# 10_ind001_sepsis.R
# Indicator 001: Sepsis
# *********************************************************************
# Purpose:
# - Define the sepsis cohort from the generic flagged case universe
# - Apply sepsis-specific inclusion/exclusion criteria
# - Create mortality outcomes and readmission sequence variables
# - Create STROBE-style participant flow and QA tables
# - Save the sepsis analysis dataset and supporting outputs
# *********************************************************************
# *********************************************************************
# (0) Prepare data ----
# *********************************************************************
# *********************************************************************
# (0.1) Prep: Parameters, load data & integrity checks ----
# *********************************************************************

# Parameters
indicator_id   <- "ind001"
indicator_name <- "sepsis"

path_discharges_flagged <- file.path(root,"04_Daten", glue::glue("discharges_flagged_{start_year}_{end_year}.fst"))
path_data_out           <- file.path(root,"04_Daten",glue::glue("analysis_{indicator_id}_{indicator_name}_{start_year}_{end_year}.fst"))
flowchart_png           <- file.path(path_output, glue::glue("flowchart_{indicator_id}_{indicator_name}_{start_year}_{end_year}_{datestamp}.png"))
flow_log_excel_file     <- file.path(path_output, glue::glue("flowchart_{indicator_id}_{indicator_name}_{start_year}_{end_year}_{datestamp}.xlsx"))


# Load linked discharges (encounter+cod+icd+chop)
discharges_flagged <- read_fst(path_discharges_flagged) |> 
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_"))  # create unique encounter_id over years

# Basic integrity checks. 
if (anyDuplicated(discharges_flagged$encounter_key) > 0) {
  stop("encounter_key is not unique in discharges_flagged.")}

if (any(is.na(discharges_flagged$survey_year))) {
  stop("Missing survey_year in discharges_flagged.")}


# *********************************************************************
# (0.2) Define analysis cohort (Strobe fflow chart) ----
# *********************************************************************

# Start STROBE-style flow chart
flow <- add_flow_step(
  flow_step = 0,
  data = discharges_flagged,
  output_tibble = "analysis_data",
  keep_criteria_label = "All inpatient records based on input data",
  keep_criteria_type = "source",
  population = "Source population",
  by_year = TRUE,
  by_year_var = survey_year
)

flow <- add_flow_step(
  flow_step = 1,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label = "Discharge month available, indicating a discharge record (Statistikfall = 'A')",
  keep_criteria = !is.na(discharge_month),
  keep_criteria_type = "eligibility"
)

flow <- add_flow_step(
  flow_step = 2,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label = "Country of residence: Switzerland",
  keep_criteria = as.character(residence_country) == "8100",
  keep_criteria_type = "eligibility"
)

flow <- add_flow_step(
  flow_step = 3,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label = "Main service centre acute care; excluding M500 and M950",
  keep_criteria = !service_centre_main %in% c("M500", "M950"),
  keep_criteria_type = "eligibility"
)

flow <- add_flow_step(
  flow_step = 4,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label = "Meets sepsis case definition based on ICD trigger code",
  keep_criteria = !is.na(sepsis_flag) & sepsis_flag > 0,
  keep_criteria_type = "eligibility",
  population = "Study population"
)

flow <- add_flow_step(
  flow_step = 5,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label = "Patient identifier available; required for linkage (BEVNAT, CoD)",
  keep_criteria = !is.na(patient_id),
  keep_criteria_type = "analysis",
  exclude_criteria_label = "Excluded: no valid patient identifier")

# Optional analysis exclusion
# flow <- add_flow_step(
#   flow_step = 6,
#   current_flow = flow,
#   output_tibble = "analysis_data",
#   keep_criteria_label = "No transfer to another acute hospital or acute division of the same hospital.",
#   keep_criteria = is.na(post_discharge_location) |
#     !post_discharge_location %in% c(6, 66),
#   keep_criteria_type = "analysis"
# )

sepsis_analysis_all <- flow$analysis_data

# *********************************************************************
# (0.3) Sepsis-specific outcome variables ----
# *********************************************************************
sepsis_analysis_all <- sepsis_analysis_all |>
  mutate(
    indicator_id   = indicator_id,
    indicator_name = indicator_name,
    
    death_event       = as.integer(death == 1L),     # Death with known time from discharge to death (see also 06_link_discharges_cod)
    in_hospital_death = as.integer(death_gvs == 1L)  # see also 06_link_discharges_cod
  )

# Harmonise discharge-to-death interval for in-hospital deaths
qa_inhospital_death_time <- sepsis_analysis_all |>
  filter(in_hospital_death == 1L,  discharge_to_death_days != 0)

if (nrow(qa_inhospital_death_time) > 0) {
  message(nrow(qa_inhospital_death_time),
          " in-hospital deaths had discharge_to_death_days != 0 and were recoded to 0.")
  print(qa_inhospital_death_time |>
          count(discharge_to_death_days))
  sepsis_analysis_all <- sepsis_analysis_all |>
    mutate(discharge_to_death_days = if_else(in_hospital_death == 1L,0,discharge_to_death_days))}

# A small number of records show an inconsistency between the in-hospital
# death indicator and the recorded discharge-to-death interval (typically
# ±1–2 days). These discrepancies most likely arise from minor differences
# between administrative data sources.
# For analytical consistency, all in-hospital deaths are assigned a
# discharge-to-death interval of 0 days. This affects only the timing
# variable; the death status itself remains unchanged.

# Verify harmonisation
stopifnot(sepsis_analysis_all |>
            filter(in_hospital_death == 1L, discharge_to_death_days != 0) |>
            nrow() == 0)   

# calculate further outcome variables
sepsis_analysis_all <- sepsis_analysis_all |>
  mutate(
    admission_to_death_days = case_when(
      death_event == 1L &
        !is.na(discharge_to_death_days) &
        !is.na(episode_length) ~         # episode_length does also include Temporary discharges (los does exclude that)
        as.numeric(episode_length) + discharge_to_death_days,
      TRUE ~ NA_real_
    ),
    
    qa_missing_death_time  = as.integer(death_event == 1L & is.na(discharge_to_death_days)),
    qa_negative_death_days = as.integer(!is.na(discharge_to_death_days) & discharge_to_death_days < 0),
    qa_cod_without_bevnat  = as.integer(death_cod == 1L & death_bevnat != 1L & death_gvs != 1L),
    qa_missing_los         = as.integer(is.na(los))
  )



# *********************************************************************
# (0.4) Sepsis hospitalization sequence & index hospitalization ----
# *********************************************************************

# Index hospitalization definition:
# - eligible index hospitalizations occur in 2021-2024
# - patients must have no observed sepsis hospitalization during the
#   preceding 24 months
# - 2019-2020 are retained solely to provide the required look-back period
#
# Since hospitalization timing is available at month level, the look-back
# is operationalised using differences between discharge months (see setup script).


sepsis_analysis_all <- sepsis_analysis_all |>
  mutate(
    # gives the first day of the month recorded
    sepsis_month = lubridate::ymd(paste0(discharge_month, "01")),
    # converts year and month into a single consecutive month index,
    # making it easy to calculate differences between months
    sepsis_month_index = lubridate::year(sepsis_month) * 12L + lubridate::month(sepsis_month)
  ) |>
  group_by(patient_id) |>
  arrange(
    sepsis_month_index,
    encounter_id,
    .by_group = TRUE
  ) |>
  mutate(
    # Sequential order of all observed sepsis hospitalizations
    # during the available 2019-2024 observation period.
    sepsis_hospitalization_sequence = row_number(),
    # Immediately preceding observed sepsis hospitalization.
    previous_sepsis_month_index     = lag(sepsis_month_index),
    months_since_previous_sepsis    = sepsis_month_index - previous_sepsis_month_index,
    # Candidate index hospitalization must:
    eligible_index_candidate = as.integer(
      # 1) occur during 2021-2024, and
      survey_year >= index_start_year & survey_year <= index_end_year &
        # 2) have no observed sepsis hospitalization in the preceding 24 months.
        (is.na(previous_sepsis_month_index) | months_since_previous_sepsis > sepsis_lookback_months)
      # With month-level dates, <=24 months is treated conservatively as
      # falling within the look-back period.
    ),
    
    # flags the first eligible hospitalization per patient during 2021-2024 as the index hospitalization
    index_hospitalization = as.integer(
      eligible_index_candidate == 1L &
        cumsum(eligible_index_candidate) == 1L
    )
  ) |>
  ungroup()

# QA: new index hospitalization definition: Number of candidate/index stays
sepsis_analysis_all |>
  summarise(
    n_sepsis_hospitalizations = n(),
    n_patients = n_distinct(patient_id),
    n_eligible_candidates = sum(eligible_index_candidate),
    n_index_hospitalizations = sum(index_hospitalization)
  )

# should result in 0 records
sepsis_analysis_all |>
  filter(index_hospitalization == 1L) |>
  count(patient_id) |>
  filter(n > 1)

# Check (QA): no index hospitalization should have a previous observed
# sepsis hospitalization within the 24-month look-back period
# should result in 0 records
sepsis_analysis_all |>
  filter(
    index_hospitalization == 1L,
    !is.na(months_since_previous_sepsis),
    months_since_previous_sepsis <= sepsis_lookback_months
  )
# Check (QA) Readmission definition: Ambiguous ordering
qa_index_ordering <- sepsis_analysis_all |>
  group_by(patient_id, discharge_month) |>
  summarise(
    n_hospitalizations     = n(),
    all_missing_death_time = all(is.na(discharge_to_death_days)),
    any_index_hospitalization = any(index_hospitalization == 1L),
    .groups = "drop"
  ) |>
  filter(n_hospitalizations > 1, all_missing_death_time)

qa_index_ordering_summary <- qa_index_ordering |>
  summarise(
    ambiguous_patient_months       = n(),
    ambiguous_hospitalizations     = sum(n_hospitalizations),
    percent_hospitalizations       = round(100 * ambiguous_hospitalizations / nrow(sepsis_analysis_all), 2),
    ambiguous_index_patient_months = sum(any_index_hospitalization, na.rm = TRUE),
    percent_index_hospitalizations_affected = round(
      100 * ambiguous_index_patient_months /
        sum(sepsis_analysis_all$index_hospitalization),
      2
    )
  )

print(qa_index_ordering_summary, width = Inf)

cat(
  "\nQA interpretation:\n",
  "- Ambiguous ordering occurs when multiple sepsis hospitalizations share the same discharge month\n",
  "  and all have missing discharge-to-death time.\n",
  "- These cases account for ",
  qa_index_ordering_summary$percent_hospitalizations,
  "% of hospitalizations (",
  qa_index_ordering_summary$ambiguous_hospitalizations,
  " hospitalizations).\n",
  "- Ambiguous patient-months containing an index hospitalization: ",
  qa_index_ordering_summary$ambiguous_index_patient_months,
  " (",
  qa_index_ordering_summary$percent_index_hospitalizations_affected,
  "% of all index hospitalizations).\n",
  sep = ""
)

# Check (QA) Readmission definition: mean Nb. of admissions per patient
qa_hospitalizations_patients <- sepsis_analysis_all |>
  summarise(n_hospitalizations = n(), 
            n_patients = n_distinct(patient_id),
            mean_hospitalizations_per_patient = round(n_hospitalizations / n_patients,2)
  )

print(qa_hospitalizations_patients)

cat(
  "\nQA interpretation:\n",
  "- The analysis cohort contains ",
  scales::comma(qa_hospitalizations_patients$n_hospitalizations),
  " sepsis hospitalizations from ",
  scales::comma(qa_hospitalizations_patients$n_patients),
  " unique patients.\n",
  "- On average, patients experienced ",
  qa_hospitalizations_patients$mean_hospitalizations_per_patient,
  " observed sepsis hospitalizations during the study period.\n",
  sep = ""
)

# Check (QA) Readmission definition: Nb. of admissions per patient
qa_hospitalizations_per_patient <- sepsis_analysis_all |>
  count(patient_id, name = "n_sepsis_hospitalizations") |>
  count(n_sepsis_hospitalizations, name = "n_patients") |>
  mutate(
    percent = round(100 * n_patients / sum(n_patients), 1)
  ) |>
  arrange(n_sepsis_hospitalizations)

print(qa_hospitalizations_per_patient)

cat(
  "\nQA interpretation:\n",
  "- Most patients (",
  qa_hospitalizations_per_patient$percent[qa_hospitalizations_per_patient$n_sepsis_hospitalizations == 1],
  "%) had only one observed sepsis hospitalization during the study period.\n",
  "- ",
  round(100 - qa_hospitalizations_per_patient$percent[qa_hospitalizations_per_patient$n_sepsis_hospitalizations == 1],1),
  "% experienced at least one additional observed sepsis hospitalization.\n",
  sep = ""
)

# Check (QA): index hospitalizations and available look-back
qa_index_by_year <- sepsis_analysis_all |>
  filter(index_hospitalization == 1L) |>
  count(survey_year, name = "n_index_hospitalizations") |>
  arrange(survey_year)

print(qa_index_by_year)

# *********************************************************************
# (0.5) Finalize analysis cohort & flow chart ----
# *********************************************************************

flow$analysis_data <- sepsis_analysis_all


# *********************************************************************
# Restrict analysis period to 2021–2024
# 2019–2020 are used exclusively as the 24-month lookback period
# *********************************************************************

flow <- add_flow_step(
  flow_step = 6,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label =
    "Index period 2021–2024; 2019–2020 used as 24-month lookback period",
  keep_criteria =
    survey_year >= index_start_year &
    survey_year <= index_end_year,
  keep_criteria_type = "analysis",
  exclude_criteria_label =
    "Excluded: outside 2021–2024 index period")


# *********************************************************************
# Retain first eligible index hospitalization per patient
# *********************************************************************

flow <- add_flow_step(
  flow_step = 7,
  current_flow = flow,
  output_tibble = "analysis_data",
  keep_criteria_label =
    "First eligible sepsis hospitalization per patient\n(no observed sepsis hospitalization in preceding 24 months)",
  keep_criteria = index_hospitalization == 1L,
  keep_criteria_type = "analysis",
  population = "Analysis population",
  exclude_criteria_label =
    "Not selected as first eligible index hospitalization"
)


sepsis_analysis <- flow$analysis_data
flow_log <- flow$flow_log

# *********************************************************************
# (0.6) Plot pooled flow chart & save flow data per year ----
# *********************************************************************
#  plot
plot_strobe_flow(
  flow = flow,
  file_png = flowchart_png,
  file_pdf = NULL,
  title = glue::glue(
    "Flow chart for indicator '{indicator_name}' ({indicator_id})"
  ),
  by_year = FALSE,
  # Box colours
  main_box_fill = "white",
  exclusion_box_fill = "white",
  main_box_border = "black",
  exclusion_box_border = "black",
  # Population highlight colours
  color_source_population = "#FCE3DC",
  color_study_population = "#F0E1EB",
  color_analysis_population = "#E5F0F9",
  # Left strip colours for criterion type
  source_strip_fill = "#ED6E4E",
  eligibility_strip_fill = "#DBB5CD",
  analysis_strip_fill = "#005095",
  strip_width = 0.15,
  # legend
  show_legend = TRUE,
  legend_text_size = 3.0,
  legend_y_gap = 1.35,
  # percentages
  show_percentages = TRUE,
  percentage_accuracy = 0.1
)

# Export flow log to Excel
flow_log_by_year <- flow$flow_log |>
  arrange(flow_step, year)

write_excel_tables_update(
  list = list(flow_chart = flow_log_by_year),
  file = flow_log_excel_file
)

# *********************************************************************
# (0.7) Potential underestimation of post-discharge mortality ----
# *********************************************************************

# *********************************************************************
# QA 1: Death information without a known discharge-to-death interval
# *********************************************************************

qa_missing_death_time_final <- sepsis_analysis |>
  filter(
    death_event == 1L,
    is.na(discharge_to_death_days)
  )

stopifnot(nrow(qa_missing_death_time_final) == 0)


# Records identified as deaths only through the Cause of Death Statistics.
# CoD alone is not used to define mortality because no reliable death timing
# is available without confirmation in GVS or BEVNAT.
qa_cod_only_deaths_final <- sepsis_analysis |>
  filter(
    death_cod == 1L,
    death_gvs != 1L,
    death_bevnat != 1L
  )

n_cod_only_deaths <- nrow(qa_cod_only_deaths_final)

# *********************************************************************
# QA 2: Maximum possible underestimation of 30-day post-discharge
# morötality from deaths recorded during a later sepsis hospitalization
# ---- ***************************************************************--

sepsis_analysis_all <- sepsis_analysis_all |>
  mutate(
    confirmed_death = as.integer(
      death_gvs == 1L | death_bevnat == 1L
    )
  )


qa_later_death_not_attributed_30d_postdischarge <- sepsis_analysis_all |>
  group_by(patient_id) |>
  filter(any(index_hospitalization == 1L)) |>
  mutate(
    index_sequence = sepsis_hospitalization_sequence[
      index_hospitalization == 1L
    ][1]
  ) |>
  summarise(
    index_encounter_key = encounter_key[
      index_hospitalization == 1L
    ][1],
    
    index_discharge_month = discharge_month[
      index_hospitalization == 1L
    ][1],
    
    index_confirmed_death = confirmed_death[
      index_hospitalization == 1L
    ][1],
    
    first_later_death_month = {
      x <- discharge_month[
        sepsis_hospitalization_sequence > index_sequence &
          confirmed_death == 1L
      ]
      
      if (length(x) == 0L || all(is.na(x))) {
        NA_integer_
      } else {
        as.integer(min(x, na.rm = TRUE))
      }
    },
    
    .groups = "drop"
  ) |>
  filter(
    index_confirmed_death == 0L,
    !is.na(first_later_death_month)
  ) |>
  mutate(
    index_month_date = lubridate::ymd(
      paste0(index_discharge_month, "01")
    ),
    
    later_death_month_date = lubridate::ymd(
      paste0(first_later_death_month, "01")
    ),
    
    month_difference =
      12L * (
        lubridate::year(later_death_month_date) -
          lubridate::year(index_month_date)
      ) +
      (
        lubridate::month(later_death_month_date) -
          lubridate::month(index_month_date)
      ),
    
    # Conservative upper bound:
    # a death in the same or following calendar month may have occurred
    # within 30 days of the index discharge.
    possibly_within_30d_postdischarge =
      month_difference >= 0L &
      month_difference <= 1L
  )


qa_possible_missed_deaths_30d_postdischarge <-
  qa_later_death_not_attributed_30d_postdischarge |>
  summarise(
    n_index_cases_with_later_death = n(),
    
    possibly_within_30d_postdischarge = sum(
      possibly_within_30d_postdischarge,
      na.rm = TRUE
    ),
    
    cohort_size = nrow(sepsis_analysis),
    
    max_percentage_point_underestimate_30d_postdischarge =
      100 * possibly_within_30d_postdischarge / cohort_size
  )


print(qa_possible_missed_deaths_30d_postdischarge)


cat(
  "\nPotential underestimation of 30-day post-discharge mortality:\n",
  "- Index cases with a confirmed death during a later sepsis hospitalization: ",
  qa_possible_missed_deaths_30d_postdischarge$
    n_index_cases_with_later_death,
  "\n",
  "- Potentially occurring within 30 days of the index discharge: ",
  qa_possible_missed_deaths_30d_postdischarge$
    possibly_within_30d_postdischarge,
  "\n",
  "- Maximum underestimation of 30-day post-discharge mortality: ",
  round(
    qa_possible_missed_deaths_30d_postdischarge$
      max_percentage_point_underestimate_30d_postdischarge,
    3
  ),
  " percentage points\n",
  sep = ""
)


# *********************************************************************
# (1) Descriptive mortality burden ----
# *********************************************************************
# Goal:
# To quantify mortality following sepsis hospitalization using:
# - in-hospital mortality,
# - fixed follow-up periods from hospital discharge, and
# - fixed follow-up periods from hospital admission.
#
# This allows assessment of both:
# 1) mortality missed when observation ends at discharge, and
# 2) the influence of the chosen time origin (admission vs discharge).
# *********************************************************************


# *********************************************************************
# Administrative censoring
# *********************************************************************
# BEVNAT follow-up appears to cover deaths at least until the end of 2025.
admin_censor_date <- as.Date("2025-12-31")


# *********************************************************************
# Construct follow-up time from discharge and from admission
# *********************************************************************
sepsis_mortality <- sepsis_analysis |>
  mutate(
    # Only the discharge month is available.
    # For survivors, use the last day of the discharge month as the
    # approximate discharge date. This yields the shortest possible
    # follow-up time to administrative censoring and is therefore
    # conservative with respect to follow-up completeness.
    discharge_date = lubridate::ceiling_date(
      lubridate::ymd(paste0(discharge_month, "01")),
      unit = "month"
    ) - lubridate::days(1),
    
    # ***************************************************************
    # Follow-up anchored at discharge
    # ***************************************************************
    # - in-hospital deaths have discharge_to_death_days = 0
    # - post-discharge deaths use the recorded discharge-to-death time
    # - survivors are censored at 31 Dec 2025
    followup_time_postdischarge_days = case_when(
      death_event == 1L & !is.na(discharge_to_death_days) ~
        pmax(discharge_to_death_days, 0),
      
      death_event == 0L ~
        as.numeric(admin_censor_date - discharge_date),
      
      TRUE ~ NA_real_
    ),
    
    followup_time_postdischarge_days =
      pmax(followup_time_postdischarge_days, 0),
    
    
    # ***************************************************************
    # Follow-up anchored at admission
    # ***************************************************************
    # For deaths, admission_to_death_days was derived upstream as:
    # episode_length + discharge_to_death_days.
    #
    # For survivors, follow-up from admission is the observed
    # post-discharge follow-up plus the length of the index stay.
    followup_time_postadmission_days = case_when(
      death_event == 1L & !is.na(admission_to_death_days) ~
        pmax(admission_to_death_days, 0),
      
      death_event == 0L &
        !is.na(episode_length) &
        !is.na(followup_time_postdischarge_days) ~
        episode_length + followup_time_postdischarge_days,
      
      TRUE ~ NA_real_
    ),
    
    followup_time_postadmission_days =
      pmax(followup_time_postadmission_days, 0),
    
    death_event = as.integer(death_event == 1L),
    
    death_in_hospital = as.integer(in_hospital_death == 1L)
  )


# *********************************************************************
# QA: Follow-up variables
# *********************************************************************

qa_followup_times <- sepsis_mortality |>
  summarise(
    n = n(),
    missing_postdischarge_followup   = sum(is.na(followup_time_postdischarge_days)),
    missing_postadmission_followup   = sum(is.na(followup_time_postadmission_days)),
    negative_postdischarge_followup  = sum(followup_time_postdischarge_days < 0, na.rm = TRUE),
    negative_postadmission_followup  = sum(followup_time_postadmission_days < 0, na.rm = TRUE),
    admission_before_discharge       = sum(followup_time_postadmission_days < followup_time_postdischarge_days, na.rm = TRUE)
  )

qa_followup_times

# Admission-based follow-up should never be shorter than
# discharge-based follow-up.
stopifnot(
  qa_followup_times$negative_postdischarge_followup == 0,
  qa_followup_times$negative_postadmission_followup == 0,
  qa_followup_times$admission_before_discharge == 0
)

# *********************************************************************
# (1.1) Kaplan-Meier survival from hospital admission ----
# *********************************************************************

km_fit_postadmission <- survival::survfit(
  survival::Surv(
    time = followup_time_postadmission_days,
    event = death_event
  ) ~ 1,
  data = sepsis_mortality
)

km_plot_postadmission <- survminer::ggsurvplot(
  km_fit_postadmission,
  data = sepsis_mortality,
  risk.table = FALSE,
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  conf.int.style = "ribbon",
  censor = TRUE,
  censor.shape = 124,
  censor.size = 1.2,
  legend = "none",
  xlab = "Days since hospital admission",
  ylab = "Survival probability",
  surv.scale = "percent",
  xlim = c(0, 365),
  break.time.by = 30
)

km_plot_postadmission$plot <- km_plot_postadmission$plot +
  geom_vline(
    xintercept = c(30, 90, 180, 365),
    linetype = "dashed",
    linewidth = 0.3,
    colour = "grey60"
  ) +
  labs(
    title = "Overall survival after sepsis hospitalization",
    subtitle = "Survival from hospital admission, index hospitalizations 2021–2024",
    caption = paste0(
      "Administrative censoring: 31 Dec 2025. ",
      "Sources: FSO – GVS, CoD and BEVNAT."
    )
  ) +
  theme(
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0, margin = margin(t = 8)),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  )

km_plot_postadmission


# *********************************************************************
# (1.2) Mortality at fixed time points ----
# *********************************************************************

mortality_windows <- tibble::tibble(window_days = c(30, 60, 90, 180, 365))

# *********************************************************************
# In-hospital mortality
# *********************************************************************

in_hospital_summary <- sepsis_mortality |>
  summarise(
    deaths_in_hospital = sum(death_in_hospital == 1L, na.rm = TRUE),
    mortality_in_hospital = deaths_in_hospital / n(),
    mortality_in_hospital_pct = 100 * mortality_in_hospital
  )

# *********************************************************************
# Mortality by fixed follow-up window
# *********************************************************************

mortality_summary <- mortality_windows |>
  rowwise() |>
  mutate(
    
    # Post-admission mortality
    deaths_postadmission = sum(
      sepsis_mortality$death_event == 1L & sepsis_mortality$followup_time_postadmission_days <= window_days,
      na.rm = TRUE
    ),
    
    mortality_postadmission     = deaths_postadmission / nrow(sepsis_mortality),
    mortality_postadmission_pct = 100 * mortality_postadmission,
    
    # Post-discharge mortality
    deaths_postdischarge = sum(
      sepsis_mortality$death_event == 1L & sepsis_mortality$followup_time_postdischarge_days <= window_days,
      na.rm = TRUE
    ),
    
    mortality_postdischarge     = deaths_postdischarge / nrow(sepsis_mortality),
    mortality_postdischarge_pct = 100 * mortality_postdischarge,
    
    # Difference between discharge- and admission-anchored mortality
    difference_percentage_points = mortality_postdischarge_pct - mortality_postadmission_pct,
    
    # ***************************************************************
    # Coverage of in-hospital deaths by post-admission window
    # ***************************************************************
    
    in_hospital_deaths_total = in_hospital_summary$deaths_in_hospital,
    
    in_hospital_deaths_covered = sum(
      sepsis_mortality$death_in_hospital == 1L & sepsis_mortality$admission_to_death_days <= window_days,
      na.rm = TRUE
    ),
    
    pct_in_hospital_deaths_covered = 100 * in_hospital_deaths_covered / in_hospital_deaths_total,
    
    # ***************************************************************
    # Composition of post-admission mortality
    # ***************************************************************
    pct_postadmission_deaths_in_hospital = 100 * in_hospital_deaths_covered / deaths_postadmission
    
  ) |>
  ungroup()


mortality_summary
in_hospital_summary

# *********************************************************************
# Prepare mortality results for export
# *********************************************************************

mortality_results <- mortality_summary |>
  select(
    window_days,
    
    deaths_postadmission,
    mortality_postadmission,
    mortality_postadmission_pct,
    
    deaths_postdischarge,
    mortality_postdischarge,
    mortality_postdischarge_pct,
    
    difference_percentage_points,
    
    in_hospital_deaths_total,
    in_hospital_deaths_covered,
    pct_in_hospital_deaths_covered,
    pct_postadmission_deaths_in_hospital
  )

mortality_results

# *********************************************************************
# Export mortality results to Excel
# *********************************************************************

mortality_results_excel_file <- file.path(
  path_output,
  glue::glue("mortality_results_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.xlsx")
)

write_excel_tables_update(
  list = list(
    in_hospital_mortality = in_hospital_summary,
    mortality_by_window = mortality_results
  ),
  file = mortality_results_excel_file
)

# *********************************************************************
# (2) Cause-of-death analysis ----
# *********************************************************************
# Goal:
# To describe how recorded causes of death following sepsis
# hospitalization change with increasing time from hospital admission.
#
# Main outputs:
# - Stacked bar chart of cause-of-death coding patterns by time from admission
# - Sepsis-specific codes
# - Infection codes without a sepsis-specific code
# - Competing non-infectious causes / external causes
# - Mixed / unknown / no clear classification
# *********************************************************************


# *********************************************************************
# Sample for manual review
# *********************************************************************

cod_review_sample <- sepsis_analysis |>
  filter(death_event == 1L) |>
  group_by(survey_year) |>
  group_modify(~ slice_sample(.x, n = min(200, nrow(.x)))) |>
  ungroup() |>
  select(
    survey_year,
    encounter_id,
    patient_id,
    sepsis_flag,
    sepsis_trigger_icd,
    grund_ges_t,
    unmitt_ges_t,
    begleit_a_ges_t,
    begleit_b_ges_t,
    endg_u_ges_t
  )


# *********************************************************************
# ICD definitions
# *********************************************************************

cod_sepsis_specific_codes <- sepsis_explicit_codes
cod_infection_prefixes <- sepsis_infection_prefixes
cod_external_cause_prefixes <- sepsis_against_prefixes_strong
cod_competing_cause_prefixes <- sepsis_against_prefixes_moderate

cod_fields <- c(
  "grund_ges_t",
  "unmitt_ges_t",
  "begleit_a_ges_t",
  "begleit_b_ges_t",
  "endg_u_ges_t"
)


# *********************************************************************
# Classify cause-of-death coding patterns
# *********************************************************************

sepsis_analysis <- sepsis_analysis |>
  mutate(
    
    # At least one CoD field contains a sepsis-specific ICD-10 code.
    cod_sepsis_specific_code = as.integer(
      if_any(
        all_of(cod_fields),
        ~ .x %in% cod_sepsis_specific_codes
      )
    ),
    
    # At least one CoD field contains an infection diagnosis.
    # Cases with a sepsis-specific code are subsequently classified
    # in the sepsis-specific category.
    cod_infection_code = as.integer(
      if_any(
        all_of(cod_fields),
        ~ purrr::map_lgl(
          .x,
          \(code) !is.na(code) &&
            any(stringr::str_starts(code, cod_infection_prefixes))
        )
      )
    ),
    
    # External causes
    cod_external_cause = as.integer(
      if_any(
        all_of(cod_fields),
        ~ substr(.x, 1, 1) %in% cod_external_cause_prefixes
      )
    ),
    
    # Prespecified competing non-infectious causes
    cod_competing_noninfectious_cause = as.integer(
      if_any(
        all_of(cod_fields),
        ~ purrr::map_lgl(
          .x,
          \(code) !is.na(code) &&
            any(stringr::str_starts(code, cod_competing_cause_prefixes))
        )
      )
    ),
    
    # Any coding pattern compatible with sepsis or infection
    cod_compatible_with_sepsis_or_infection = as.integer(
      cod_sepsis_specific_code == 1L |
        cod_infection_code == 1L
    ),
    
    # Any coding pattern indicating an external or competing
    # non-infectious cause
    cod_competing_or_external_cause = as.integer(
      cod_external_cause == 1L |
        cod_competing_noninfectious_cause == 1L
    )
  )


# *********************************************************************
# Cause-of-death coding patterns by time from admission to death
# *********************************************************************

cod_evidence_time <- sepsis_analysis |>
  filter(
    death_event == 1L,
    !is.na(admission_to_death_days)
  ) |>
  mutate(
    
    admission_to_death_group = case_when(
      admission_to_death_days <= 30  ~ "0–30 days",
      admission_to_death_days <= 60  ~ "31–60 days",
      admission_to_death_days <= 90  ~ "61–90 days",
      admission_to_death_days <= 180 ~ "91–180 days",
      admission_to_death_days <= 365 ~ "181–365 days",
      TRUE                           ~ "366+ days"
    ),
    
    admission_to_death_group = factor(
      admission_to_death_group,
      levels = c(
        "0–30 days",
        "31–60 days",
        "61–90 days",
        "91–180 days",
        "181–365 days",
        "366+ days"
      )
    ),
    
    cod_record_available = death_cod == 1L,
    
    # CoD record available, but no informative code other than
    # missing/blank values or R99.
    cod_unknown_or_ill_defined =
      cod_record_available &
      if_all(
        all_of(cod_fields),
        ~ is.na(.x) | .x == "" | .x == "R99"
      ),
    
    cod_evidence_group = case_when(
      
      !cod_record_available ~
        "No CoD record available",
      
      # Both a sepsis/infection-compatible code and an external or
      # competing non-infectious cause are present.
      cod_compatible_with_sepsis_or_infection == 1L &
        cod_competing_or_external_cause == 1L ~
        "Mixed coding pattern",
      
      # Sepsis-specific codes take precedence over infection-only codes.
      cod_sepsis_specific_code == 1L ~
        "Sepsis-specific code",
      
      cod_infection_code == 1L ~
        "Infection code without sepsis-specific code",
      
      cod_unknown_or_ill_defined ~
        "Unknown / ill-defined",
      
      cod_external_cause == 1L ~
        "External cause",
      
      cod_competing_noninfectious_cause == 1L ~
        "Competing non-infectious cause",
      
      TRUE ~
        "No clear classification"
    ),
    
    cod_evidence_group = factor(
      cod_evidence_group,
      levels = c(
        "Unknown / ill-defined",
        "No clear classification",
        "External cause",
        "Competing non-infectious cause",
        "Mixed coding pattern",
        "Infection code without sepsis-specific code",
        "Sepsis-specific code"
      )
    )
  )


# *********************************************************************
# Prepare cause-of-death coding-pattern plot
# *********************************************************************

# Distribution of coding categories among deaths with an available CoD record.
# The manuscript figure is restricted to deaths within 365 days of admission.
cod_evidence_plot <- cod_evidence_time |>
  filter(
    cod_record_available,
    admission_to_death_days <= 365
  ) |>
  count(
    admission_to_death_group,
    cod_evidence_group,
    name = "n"
  ) |>
  group_by(admission_to_death_group) |>
  mutate(
    percent_cod_deaths = 100 * n / sum(n)
  ) |>
  ungroup()


# Proportion of all deaths without an available CoD record.
# This is shown separately above the 100% stacked bars.
cod_missing_plot <- cod_evidence_time |>
  filter(
    admission_to_death_days <= 365
  ) |>
  group_by(admission_to_death_group) |>
  summarise(
    n_all = n(),
    n_no_cod = sum(!cod_record_available),
    pct_no_cod = 100 * n_no_cod / n_all,
    .groups = "drop"
  ) |>
  mutate(
    x = as.numeric(admission_to_death_group),
    ymin = 100,
    ymax = 100 + pct_no_cod
  )


# *********************************************************************
# Plot cause-of-death coding patterns by time from admission
# *********************************************************************

p_cod_evidence <- ggplot() +
  
  # 0–100%:
  # distribution of coding categories among deaths with available CoD data
  geom_col(
    data = cod_evidence_plot,
    aes(
      x = admission_to_death_group,
      y = percent_cod_deaths,
      fill = cod_evidence_group
    )
  ) +
  
  # Above 100%:
  # proportion of all deaths without an available CoD record
  geom_rect(
    data = cod_missing_plot,
    aes(
      xmin = x - 0.45,
      xmax = x + 0.45,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "#969696"
  ) +
  
  geom_hline(
    yintercept = 100,
    linewidth = 0.4,
    colour = "black"
  ) +
  
  scale_y_continuous(
    labels = \(x) paste0(x, "%"),
    breaks = seq(0, 150, 25),
    limits = c(0, 150),
    expand = c(0, 0)
  ) +
  
  scale_fill_manual(
    values = c(
      "Unknown / ill-defined" = "#BDBDBD",
      "No clear classification" = "#E5E5E5",
      "External cause" = "#d7191c",
      "Competing non-infectious cause" = "#fdae61",
      "Mixed coding pattern" = "#ffffbf",
      "Infection code without sepsis-specific code" = "#abd9e9",
      "Sepsis-specific code" = "#2c7bb6"
    ),
    breaks = c(
      "Unknown / ill-defined",
      "No clear classification",
      "External cause",
      "Competing non-infectious cause",
      "Mixed coding pattern",
      "Infection code without sepsis-specific code",
      "Sepsis-specific code"
    )
  ) +
  
  guides(
    fill = guide_legend(
      reverse = TRUE,
      nrow = 2,
      byrow = TRUE
    )
  ) +
  
  labs(
    x = "Time from hospital admission to death",
    y = "Percentage of deaths",
    fill = "Cause-of-death coding category",
    title = NULL,
    subtitle = NULL,
    caption = paste0(
      "Coloured bars show the distribution of coding categories among deaths with an available CoD record; ",
      "dark grey above 100% shows the proportion of all deaths in each interval without an available CoD record.\n",
      "Sources: FSO – Healthcare Statistics (GVS), Cause of Death Statistics (CoD), ",
      "Vital Statistics (BEVNAT)."
    )
  ) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )


p_cod_evidence

# *********************************************************************
# (3) Mortality stratification ----
# *********************************************************************
# Goal:
# To identify key predictors of mortality and inform the adjustment
# strategy for hospital comparisons.
#
# Main outputs:
# - Kaplan-Meier curves by age group
# - Sex
# - Sepsis trigger position: primary vs additional diagnosis
# - Sepsis severity / shock if available
# - Emergency admission / pre-admission location
# - Comorbidity burden if constructed
# *********************************************************************

# *********************************************************************
# Link comorbidity scores
# *********************************************************************

path_comorbidity_scores <- file.path(root, "04_Daten", glue::glue("comorbidity_scores_{start_year}_{end_year}.fst"))
comorbidity_scores <- read_fst(path_comorbidity_scores)

# QA: keys must be unique before joining
stopifnot(!anyDuplicated(comorbidity_scores$encounter_key))
stopifnot(!anyDuplicated(sepsis_mortality$encounter_key))

n_before <- nrow(sepsis_mortality)

sepsis_mortality <- sepsis_mortality |>
  left_join(
    comorbidity_scores |>
      select(
        encounter_key,
        charlson_score,
        elixhauser_vw_score
      ),
    by = "encounter_key"
  )

stopifnot(nrow(sepsis_mortality) == n_before)

# QA: missing scores after linkage
qa_comorbidity_linkage <- sepsis_mortality |>
  summarise(
    n = n(),
    missing_charlson = sum(is.na(charlson_score)),
    missing_elixhauser = sum(is.na(elixhauser_vw_score)),
    pct_missing_charlson = round(100 * missing_charlson / n, 2),
    pct_missing_elixhauser = round(100 * missing_elixhauser / n, 2)
  )

qa_comorbidity_linkage # Missings: probably they only have main diagnosis

names(comorbidity_scores)

# *********************************************************************
# Link ICU variables
# *********************************************************************

path_icu <- file.path(root, "04_Daten", glue::glue("icu_{start_year}_{end_year}.fst"))

icu_all <- read_fst(path_icu) |>
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_"))

# Aggregate to encounter level
icu_encounter <- icu_all |>
  summarise(
    icu_hours = sum(icu_duration, na.rm = TRUE),
    ventilation_hours = sum(icu_resp_duration, na.rm = TRUE),
    .by = encounter_key
  ) |>
  mutate(
    icu_admission = as.integer(icu_hours > 0),
    mechanical_ventilation = as.integer(ventilation_hours > 0),
    icu_length_group = case_when(
      icu_hours == 0                  ~ "No ICU",
      icu_hours <= 24                 ~ "≤24 hours",
      icu_hours <= 72                 ~ "25–72 hours",
      icu_hours <= 168                ~ "73 hours–7 days",
      icu_hours > 168                 ~ ">7 days",
      TRUE                            ~ NA_character_
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
  ) |>
  select(
    encounter_key,
    icu_hours,
    ventilation_hours,
    icu_admission,
    mechanical_ventilation,
    icu_length_group
  )

# QA: ICU keys must be unique before joining
stopifnot(!anyDuplicated(icu_encounter$encounter_key))
stopifnot(!anyDuplicated(sepsis_mortality$encounter_key))

n_before <- nrow(sepsis_mortality)

sepsis_mortality <- sepsis_mortality |>
  left_join(
    icu_encounter,
    by = "encounter_key"
  ) |>
  mutate(
    icu_hours = coalesce(icu_hours, 0),
    ventilation_hours = coalesce(ventilation_hours, 0),
    icu_admission = coalesce(icu_admission, 0L),
    mechanical_ventilation = coalesce(mechanical_ventilation, 0L),
    icu_length_group = forcats::fct_explicit_na(
      icu_length_group,
      na_level = "No ICU"
    ),
    icu_length_group = case_when(
      is.na(icu_length_group) ~ "No ICU",
      TRUE                    ~ as.character(icu_length_group)
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
    ),
    icu_admission_group = case_when(
      icu_admission == 1L ~ "ICU admission",
      TRUE               ~ "No ICU admission"
    ),
    mechanical_ventilation_group = case_when(
      mechanical_ventilation == 1L ~ "Mechanical ventilation",
      TRUE                         ~ "No mechanical ventilation"
    )
  )

stopifnot(nrow(sepsis_mortality) == n_before)

# QA: ICU linkage
qa_icu_linkage <- sepsis_mortality |>
  summarise(
    n = n(),
    n_icu_admission = sum(icu_admission == 1L, na.rm = TRUE),
    pct_icu_admission = round(100 * n_icu_admission / n, 1),
    n_mechanical_ventilation = sum(mechanical_ventilation == 1L, na.rm = TRUE),
    pct_mechanical_ventilation = round(100 * n_mechanical_ventilation / n, 1)
  )

qa_icu_linkage

# *********************************************************************
# Create stratification variables
# *********************************************************************

sepsis_mortality <- sepsis_mortality |>
  mutate(
    sepsis_trigger_position = case_when(
      sepsis_flag == 1L ~ "main_diagnosis",
      sepsis_flag == 2L ~ "secondary_diagnosis",
      sepsis_flag == 3L ~ "add_on_to_main_diagnosis",
      TRUE             ~ NA_character_
    ),
    age_group = case_when(
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
    sex = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE        ~ NA_character_
    ),
    
    trigger_position_group = case_when(
      sepsis_trigger_position == "main_diagnosis"  ~ "Primary diagnosis",
      TRUE ~ "Secondary diagnosis or add-on to main diagnosis"
    ),
    
    sepsis_severity_group = case_when(
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
      )
    ),
    admission_type_group = case_when(
      admission_type == 1 ~ "Emergency admission",
      admission_type == 2 ~ "Planned admission",
      TRUE                ~ "Other / unknown"
    ),
    pre_admission_group = case_when(
      is.na(pre_admission_location) ~ "Missing",
      TRUE ~ as.character(pre_admission_location)
    ),
    elixhauser_vw_group = cut(
      elixhauser_vw_score,
      breaks = c(-Inf, 0, 5, 10, 20, Inf),
      labels = c("≤0", "1–5", "6–10", "11–20", ">20"),
      ordered_result = TRUE
    ),
    charlson_group = cut(
      charlson_score,
      breaks = c(-Inf, 0, 1, 2, 4, Inf),
      labels = c("0", "1", "2", "3–4", "≥5"),
      ordered_result = TRUE
    )
  )

# *********************************************************************
# Helper: stratified Kaplan-Meier plot
# *********************************************************************

plot_km_by_group <- function(data, group_var, title) {
  
  group_var <- rlang::ensym(group_var)
  
  km_data <- data |>
    filter(!is.na(!!group_var)) |>
    mutate(km_group = as.factor(!!group_var))
  
  km_fit <- survfit(
    Surv(
      time = pmin(followup_time_postadmission_days, 365),
      event = death_event == 1L &
        followup_time_postadmission_days <= 365
    ) ~ km_group,
    data = km_data
  )
  
  p <- ggsurvplot(
    km_fit,
    data = km_data,
    legend.labs = levels(km_data$km_group),
    ggtheme = theme_bw(),
    xlim = c(0, 365),
    ylim = c(0, 1),
    break.time.by = 30,
    risk.table = FALSE,
    conf.int = TRUE,
    conf.int.alpha = 0.12,
    conf.int.style = "ribbon",
    censor = TRUE,
    censor.shape = 124,
    censor.size = 1.2,
    xlab = "Days since hospital admission",
    ylab = "Survival probability",
    surv.scale = "percent",
    legend.title = NULL
  )$plot +
    geom_vline(
      xintercept = c(30, 60, 90, 180, 365),
      linetype = "dashed",
      linewidth = 0.3,
      colour = "grey60"
    ) +
    labs(
      title = title,
      colour = NULL,
      fill = NULL
    ) +
    guides(
      colour = guide_legend(
        title = NULL,
        nrow = 2,
        byrow = TRUE
      ),
      fill = guide_legend(
        title = NULL,
        nrow = 2,
        byrow = TRUE
      )
    ) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      axis.text = element_text(
        color = "black",
        size = 7
      ),
      axis.title = element_text(
        size = 8,
        color = "black"
      ),
      plot.title = element_text(
        size = 9,
        face = "bold"
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      legend.title = element_blank(),
      legend.spacing.x = unit(0.2, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.35, "lines"),
      legend.key.width = unit(0.8, "lines"),
      legend.spacing.y = unit(0, "pt"),
      legend.margin = margin(
        t = 0, r = 0, b = 0, l = 0
      ),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  p
}
# *********************************************************************
# Kaplan-Meier curves by key strata
# *********************************************************************

p_km_age <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = age_group,
  title = "age group"
)

p_km_sex <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = sex,
  title = "sex"
)

p_km_trigger <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = trigger_position_group,
  title = "diagnosis trigger position"
)

p_km_severity <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = sepsis_severity_group,
  title = "sepsis severity proxy"
)


p_km_admission <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = admission_type_group,
  title = "admission type"
)

p_km_charlson <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = charlson_group,
  title = "Charlson comorbidity score"
)

p_km_elixhauser <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = elixhauser_vw_group,
  title = "Elixhauser van Walraven score"
)
p_km_icu <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = icu_admission_group,
  title = "ICU admission"
)

p_km_icu_length <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = icu_length_group,
  title = "ICU length of stay"
)

p_km_ventilation <- plot_km_by_group(
  data = sepsis_mortality,
  group_var = mechanical_ventilation_group,
  title = "Mechanical ventilation"
)


p_km_strata <-  (p_km_age  | p_km_sex | p_km_severity ) /
  (p_km_admission | p_km_trigger | p_km_elixhauser) /
  (p_km_icu | p_km_icu_length | p_km_ventilation)+
  plot_annotation(
    title = "Survival after sepsis hospitalization by patient and clinical characteristics",
    caption = paste0(
      "Survival measured from hospital admission. ",
      "Follow-up administratively censored at 31 Dec 2025.\n",
      "Sources: FSO - Healthcare Statistics (GVS), ",
      "Cause of Death Statistics (CoD), Vital Statistics (BEVNAT)."
    )
  ) &
  theme(
    plot.caption = element_text(size = 8, hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  )

p_km_strata


# *********************************************************************
# (4) Hospital comparison ----
# *********************************************************************
# Goal:
# To assess whether hospital performance assessment changes when using
# a standardized 90-day post-admission mortality endpoint instead of
# conventional in-hospital mortality.
#
# Secondary comparison:
# - 90-day post-discharge mortality
#
# Main outputs:
# - Crude hospital mortality rates
# - Age/sex-adjusted rates
# - Extended risk-adjusted rates
# - Funnel plots
# - Agreement / reclassification of hospitals:
#   lower than expected / within expected range / higher than expected
# *********************************************************************


# *********************************************************************
# Define mortality outcomes
# *********************************************************************

sepsis_mortality <- sepsis_mortality |>
  mutate(
    
    # Conventional in-hospital mortality
    death_in_hospital = as.integer(
      in_hospital_death == 1L
    ),
    
    # Primary fixed-window outcome:
    # death within 90 days of hospital admission
    death_90d_postadmission = as.integer(
      death_event == 1L &
        followup_time_postadmission_days <= 90
    ),
    
    # Secondary / sensitivity outcome:
    # death within 90 days after hospital discharge
    death_90d_postdischarge = as.integer(
      death_event == 1L &
        followup_time_postdischarge_days <= 90
    )
  )


# *********************************************************************
# QA: outcome frequencies
# *********************************************************************
qa_hospital_outcomes <- tibble::tibble(
  outcome = c(
    "In-hospital mortality",
    "90-day post-admission mortality",
    "90-day post-discharge mortality"
  ),
  deaths = c(
    sum(sepsis_mortality$death_in_hospital, na.rm = TRUE),
    sum(sepsis_mortality$death_90d_postadmission, na.rm = TRUE),
    sum(sepsis_mortality$death_90d_postdischarge, na.rm = TRUE)
  ),
  cohort_size = nrow(sepsis_mortality)
) |>
  mutate(
    mortality = deaths / cohort_size,
    mortality_pct = round(100 * mortality, 1)
  )

qa_hospital_outcomes

# *********************************************************************
# (4.1) Analyze volume per hospital site & define threshold ----
# *********************************************************************

# *********************************************************************
# QA: hospital site mapped to more than one hospital in the same year
# *********************************************************************

qa_site_to_hospital <- sepsis_mortality |>
  distinct(
    survey_year,
    hospital_site_id_num,
    hospital_id_num
  ) |>
  summarise(
    n_hospitals = n_distinct(hospital_id_num),
    hospitals = paste(
      sort(unique(hospital_id_num)),
      collapse = ", "
    ),
    .by = c(survey_year, hospital_site_id_num)
  ) |>
  filter(n_hospitals > 1)

qa_site_to_hospital


# *********************************************************************
# Sepsis case volume per hospital site and year
# *********************************************************************

hospital_site_volume <- sepsis_mortality |>
  count(
    survey_year,
    hospital_site_id_num,
    name = "n_sepsis_cases"
  ) |>
  mutate(
    volume_group = case_when(
      n_sepsis_cases < 5   ~ "<5",
      n_sepsis_cases < 10  ~ "5–9",
      n_sepsis_cases < 100 ~ "10–99",
      n_sepsis_cases < 500 ~ "100–499",
      TRUE                 ~ "≥500"
    ),
    volume_group = factor(
      volume_group,
      levels = c(
        "<5",
        "5–9",
        "10–99",
        "100–499",
        "≥500"
      ),
      ordered = TRUE
    )
  )


# Summary of annual site volumes
hospital_site_volume_summary <- hospital_site_volume |>
  group_by(survey_year) |>
  summarise(
    n_sites = n(),
    median_cases = median(n_sepsis_cases),
    p25 = quantile(n_sepsis_cases, 0.25),
    p75 = quantile(n_sepsis_cases, 0.75),
    min = min(n_sepsis_cases),
    max = max(n_sepsis_cases),
    .groups = "drop"
  )

hospital_site_volume_summary


# Number of hospital sites by annual volume group
site_volume_by_year <- hospital_site_volume |>
  count(
    survey_year,
    volume_group,
    name = "n_hospital_sites"
  ) |>
  tidyr::pivot_wider(
    names_from = volume_group,
    values_from = n_hospital_sites,
    values_fill = 0
  )

site_volume_by_year


# *********************************************************************
# Pooled index sepsis case volume per hospital site, 2021–2024
# *********************************************************************

hospital_site_volume_pooled <- sepsis_mortality |>
  count(
    hospital_site_id_num,
    name = "n_sepsis_cases"
  ) |>
  mutate(
    volume_group = case_when(
      n_sepsis_cases < 5   ~ "<5",
      n_sepsis_cases < 10  ~ "5–9",
      n_sepsis_cases < 100 ~ "10–99",
      n_sepsis_cases < 500 ~ "100–499",
      TRUE                 ~ "≥500"
    ),
    volume_group = factor(
      volume_group,
      levels = c(
        "<5",
        "5–9",
        "10–99",
        "100–499",
        "≥500"
      ),
      ordered = TRUE
    )
  )


site_volume_pooled <- hospital_site_volume_pooled |>
  count(
    volume_group,
    name = "n_hospital_sites"
  ) |>
  tidyr::pivot_wider(
    names_from = volume_group,
    values_from = n_hospital_sites,
    values_fill = 0
  )

site_volume_pooled


# *********************************************************************
# Assess possible minimum-volume thresholds
# *********************************************************************

thresholds <- c(0, 10, 20, 30, 50, 75, 100, 150, 200)

hospital_thresholds_table <- purrr::map_dfr(
  thresholds,
  \(threshold) {
    
    included <- hospital_site_volume_pooled |>
      filter(n_sepsis_cases >= threshold)
    
    tibble::tibble(
      threshold = threshold,
      hospital_sites = nrow(included),
      pct_hospital_sites =
        100 * nrow(included) /
        nrow(hospital_site_volume_pooled),
      
      sepsis_cases =
        sum(included$n_sepsis_cases),
      
      pct_sepsis_cases =
        100 * sum(included$n_sepsis_cases) /
        sum(hospital_site_volume_pooled$n_sepsis_cases)
    )
  }
)

hospital_thresholds_table


# *********************************************************************
# Threshold used for hospital comparison
# *********************************************************************

n_sepsis_threshold <- 50

# Hospital sites with fewer than 50 pooled index sepsis hospitalizations during 2021–2024 
# were excluded from hospital-level comparisons. This threshold retained 116 of 201 hospital 
# sites (57.7%) and 98.2% of all index hospitalizations.

# Summary for selected threshold
selected_threshold_summary <- hospital_thresholds_table |>
  filter(threshold == n_sepsis_threshold)

selected_threshold_summary


# *********************************************************************
# (4.2) Model fitting ----
# *********************************************************************
# Complete-case assessment for risk-adjustment models
qa_model_missing <- sepsis_mortality |>
  summarise(
    n = n(),
    across(
      c(
        death_in_hospital,
        death_90d_postadmission,
        age_group,
        sex,
        elixhauser_vw_score,
        admission_type_group,
        trigger_position_group,
        sepsis_severity_group
      ),
      ~ sum(is.na(.)),
      .names = "missing_{.col}"
    )
  )


qa_model_missing |>
  tidyr::pivot_longer(
    cols = starts_with("missing_"),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  mutate(
    pct_missing = 100 * n_missing / qa_model_missing$n
  )

# model estimation
model_raw_inhospital      <- glm(death_in_hospital ~ 1 , data = sepsis_mortality, family = binomial())
model_raw_90d             <- glm(death_90d_postadmission         ~ 1 ,  data = sepsis_mortality,  family = binomial())
model_age_sex_inhospital  <- glm(death_in_hospital ~ age_group + sex, data = sepsis_mortality,  family = binomial())
model_age_sex_90d         <- glm(death_90d_postadmission         ~ age_group + sex, data = sepsis_mortality,family = binomial())
model_full_inhospital     <- glm(death_in_hospital ~ age_group + sex + elixhauser_vw_score + admission_type_group + trigger_position_group + sepsis_severity_group,  data = sepsis_mortality, family = binomial())
model_full_90d            <- glm(death_90d_postadmission         ~ age_group + sex + elixhauser_vw_score + admission_type_group + trigger_position_group +sepsis_severity_group, data = sepsis_mortality, family = binomial())
model_full2_inhospital    <- glm(death_in_hospital ~ age_group + sex + elixhauser_vw_score + admission_type_group + trigger_position_group + sepsis_severity_group + age_group:elixhauser_vw_score, data = sepsis_mortality, family = binomial())
model_full2_90d           <- glm(death_90d_postadmission         ~ age_group + sex + elixhauser_vw_score + admission_type_group + trigger_position_group + sepsis_severity_group + age_group:elixhauser_vw_score, data = sepsis_mortality, family = binomial())


# Unified model-performance table
model_registry <- tibble(
  outcome = c(
    rep("In-hospital mortality", 4),
    rep("90-day post-admission mortality", 4)
  ),
  model = rep(
    c(
      "Raw",
      "Age and sex",
      "Full model",
      "Full model + interaction"
    ),
    2
  ),
  model_object = list(
    model_raw_inhospital,
    model_age_sex_inhospital,
    model_full_inhospital,
    model_full2_inhospital,
    model_raw_90d,
    model_age_sex_90d,
    model_full_90d,
    model_full2_90d
  )
)

model_performance_table <- model_registry |>
  mutate(
    df = map_dbl(model_object, ~ attr(logLik(.x), "df") ),
    aic = map_dbl(model_object, AIC),
    brier_score = map_dbl(model_object, ~ {
      observed <- model.response(model.frame(.x))
      predicted <- predict(.x, type = "response")
      mean((observed - predicted)^2)
    }
    ),
    tjur_r2 = map_dbl(
      model_object, ~ {
        observed <- model.response(model.frame(.x))
        predicted <- predict(.x, type = "response")
        mean(predicted[observed == 1]) -
          mean(predicted[observed == 0])
      }
    ),
    mcfadden_r2 = map_dbl(
      model_object, ~ {
        null_model <- update(.x, . ~ 1)
        1 - as.numeric(logLik(.x)) / as.numeric(logLik(null_model))
      }
    )
  ) |>
  select(
    outcome,
    model,
    df,
    aic,
    brier_score,
    tjur_r2,
    mcfadden_r2
  )

model_performance_table

# *********************************************************************
# (4.3) Predict expected mortality ----
# *********************************************************************

# Helper: predict expected mortality
predict_expected <- function(data, model, expected_var) {
  data |>
    mutate(
      !!expected_var := predict(
        model,
        newdata = data,
        type = "response"
      )
    )
}

# Predict expected patient-level mortality for each model
sepsis_mortality <- sepsis_mortality |>
  predict_expected(model_raw_inhospital, "expected_raw_inhospital") |>
  predict_expected(model_age_sex_inhospital, "expected_age_sex_inhospital") |>
  predict_expected(model_full_inhospital, "expected_full_inhospital") |>
  predict_expected(model_full2_inhospital, "expected_full2_inhospital") |>
  predict_expected(model_raw_90d, "expected_raw_90d") |>
  predict_expected(model_age_sex_90d, "expected_age_sex_90d") |>
  predict_expected(model_full_90d, "expected_full_90d") |>
  predict_expected(model_full2_90d, "expected_full2_90d")


# *********************************************************************
# QA: predicted mortality
# *********************************************************************
qa_expected_mortality <- sepsis_mortality |>
  summarise(
    across(
      starts_with("expected_"),
      list(
        min = min,
        mean = mean,
        max = max
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("model", ".value"),
    names_pattern = "(.*)_(min|mean|max)"
  )

qa_expected_mortality

# Looks good:
# - Raw models have constant predictions.
# - Mean predicted risk equals observed mortality:
#    - in-hospital: 0.202
#    - 90-day post-admission: 0.279
# - All predictions are within [0, 1].
# - Full models allow substantial variation in individual predicted risk.
# - Adding the age × Elixhauser interaction further widens the predicted-risk range.


# *********************************************************************
# (4.4) Hospital-level aggregation ----
# *********************************************************************

# Helper: aggregate observed and expected mortality by hospital site
aggregate_hospital <- function(data, observed_var, expected_var) {
  
  overall_rate <- mean(data[[observed_var]],  na.rm = TRUE)
  
  data |>
    summarise(
      n_cases         = n(),
      observed_deaths = sum(.data[[observed_var]], na.rm = TRUE),
      expected_deaths = sum(.data[[expected_var]], na.rm = TRUE),
      observed_rate   = observed_deaths / n_cases,
      expected_rate   = expected_deaths / n_cases,
      oe_ratio        = observed_deaths / expected_deaths,
      adjusted_rate   = oe_ratio * overall_rate,
      .by             = hospital_site_id_num
    )
}


# *********************************************************************
# Hospital summaries by outcome and model
# *********************************************************************

hospital_raw_inhospital     <- aggregate_hospital(sepsis_mortality, "death_in_hospital", "expected_raw_inhospital")
hospital_age_sex_inhospital <- aggregate_hospital(sepsis_mortality, "death_in_hospital", "expected_age_sex_inhospital")
hospital_full_inhospital    <- aggregate_hospital(sepsis_mortality,  "death_in_hospital",  "expected_full_inhospital")
hospital_full2_inhospital   <- aggregate_hospital(sepsis_mortality,  "death_in_hospital",  "expected_full2_inhospital")

hospital_raw_90d     <- aggregate_hospital(sepsis_mortality,  "death_90d_postadmission", "expected_raw_90d")
hospital_age_sex_90d <- aggregate_hospital(sepsis_mortality, "death_90d_postadmission", "expected_age_sex_90d")
hospital_full_90d    <- aggregate_hospital(sepsis_mortality, "death_90d_postadmission","expected_full_90d")
hospital_full2_90d   <- aggregate_hospital( sepsis_mortality,  "death_90d_postadmission",  "expected_full2_90d")


# *********************************************************************
# Apply minimum-volume threshold
# *********************************************************************
hospital_sites_included <- hospital_site_volume_pooled |>
  filter(n_sepsis_cases >= n_sepsis_threshold) |>
  select(hospital_site_id_num)

hospital_full_inhospital_analysis <- hospital_full_inhospital |>
  semi_join(
    hospital_sites_included,
    by = "hospital_site_id_num"
  )

hospital_full_90d_analysis <- hospital_full_90d |>
  semi_join(
    hospital_sites_included,
    by = "hospital_site_id_num"
  )


# *********************************************************************
# QA: hospital-level aggregation
# *********************************************************************

qa_hospital_aggregation <- bind_rows(
  
  hospital_full_inhospital_analysis |>
    summarise(
      outcome = "In-hospital mortality",
      n_sites = n(),
      n_cases = sum(n_cases),
      pct_cases = 100 * n_cases / nrow(sepsis_mortality),
      observed_deaths = sum(observed_deaths),
      expected_deaths = sum(expected_deaths),
      oe_ratio = observed_deaths / expected_deaths
    ),
  
  hospital_full_90d_analysis |>
    summarise(
      outcome = "90-day post-admission mortality",
      n_sites = n(),
      n_cases = sum(n_cases),
      pct_cases = 100 * n_cases / nrow(sepsis_mortality),
      observed_deaths = sum(observed_deaths),
      expected_deaths = sum(expected_deaths),
      oe_ratio = observed_deaths / expected_deaths
    )
)

qa_hospital_aggregation

# One important point: because you first fit the models on all 66,732 patients 
# and only afterwards restrict hospital comparisons to the 116 sites meeting the 
# volume threshold, observed_deaths and expected_deaths among those 116 sites 
# do not necessarily have to be exactly equal. The overall fitted cohort 
# has O/E ≈ 1; the restricted subset can deviate slightly from 1.
# So for QA here, I mainly want to see:
# - n_sites = 116
# - n_cases = 65,502
# - pct_cases ≈ 98.2%
# - plausible O/E values close to 1.


# *********************************************************************
# (4.5) Compare hospital estimates between adjustment models ----
# *********************************************************************
# Questions:
# 1. Does the age × Elixhauser interaction materially change hospital estimates?
# 2. Should the interaction model be retained for the hospital comparison?
#
# Comparison restricted to hospital sites meeting the minimum volume threshold.
# *********************************************************************


# *********************************************************************
# In-hospital mortality
# *********************************************************************

hospital_compare_inhospital <- hospital_full_inhospital |>
  select(
    hospital_site_id_num,
    adjusted_full = adjusted_rate,
    expected_full = expected_rate
  ) |>
  left_join(
    hospital_full2_inhospital |>
      select(
        hospital_site_id_num,
        adjusted_full2 = adjusted_rate,
        expected_full2 = expected_rate
      ),
    by = "hospital_site_id_num"
  ) |>
  semi_join(
    hospital_sites_included,
    by = "hospital_site_id_num"
  ) |>
  mutate(
    adjusted_difference = adjusted_full2 - adjusted_full,
    expected_difference = expected_full2 - expected_full,
    adjusted_abs_difference = abs(adjusted_difference),
    expected_abs_difference = abs(expected_difference)
  )


# *********************************************************************
# 90-day post-admission mortality
# *********************************************************************

hospital_compare_90d <- hospital_full_90d |>
  select(
    hospital_site_id_num,
    adjusted_full = adjusted_rate,
    expected_full = expected_rate
  ) |>
  left_join(
    hospital_full2_90d |>
      select(
        hospital_site_id_num,
        adjusted_full2 = adjusted_rate,
        expected_full2 = expected_rate
      ),
    by = "hospital_site_id_num"
  ) |>
  semi_join(
    hospital_sites_included,
    by = "hospital_site_id_num"
  ) |>
  mutate(
    adjusted_difference = adjusted_full2 - adjusted_full,
    expected_difference = expected_full2 - expected_full,
    adjusted_abs_difference = abs(adjusted_difference),
    expected_abs_difference = abs(expected_difference)
  )


# *********************************************************************
# Summarise differences between models
# *********************************************************************

qa_model_comparison <- bind_rows(
  
  hospital_compare_inhospital |>
    summarise(
      outcome = "In-hospital mortality",
      n_hospitals = n(),
      correlation_adjusted =
        cor(adjusted_full, adjusted_full2),
      median_adjusted_difference =
        median(adjusted_abs_difference),
      max_adjusted_difference =
        max(adjusted_abs_difference),
      median_expected_difference =
        median(expected_abs_difference),
      max_expected_difference =
        max(expected_abs_difference)
    ),
  
  hospital_compare_90d |>
    summarise(
      outcome = "90-day post-admission mortality",
      n_hospitals = n(),
      correlation_adjusted =
        cor(adjusted_full, adjusted_full2),
      median_adjusted_difference =
        median(adjusted_abs_difference),
      max_adjusted_difference =
        max(adjusted_abs_difference),
      median_expected_difference =
        median(expected_abs_difference),
      max_expected_difference =
        max(expected_abs_difference)
    )
)

qa_model_comparison


# *********************************************************************
# Hospitals with largest differences
# *********************************************************************

qa_model_comparison_inhospital_outliers <-
  hospital_compare_inhospital |>
  left_join(
    hospital_site_volume_pooled,
    by = "hospital_site_id_num"
  ) |>
  arrange(desc(adjusted_abs_difference))

qa_model_comparison_90d_outliers <-
  hospital_compare_90d |>
  left_join(
    hospital_site_volume_pooled,
    by = "hospital_site_id_num"
  ) |>
  arrange(desc(adjusted_abs_difference))


qa_model_comparison_inhospital_outliers
qa_model_comparison_90d_outliers

# Interpretation:
# The age × Elixhauser interaction improved patient-level model fit only
# modestly and had little impact on hospital-level adjusted mortality estimates.
# Correlations between the Full and Full + interaction models were 0.996
# for both outcomes, and median absolute differences in adjusted mortality
# were approximately 0.1–0.2 percentage points.
# Therefore, subsequent hospital comparisons use the simpler Full model
# without the interaction term.


# *********************************************************************
# (4.6) Funnel plots using FunnelPlotR ----
# *********************************************************************
library(FunnelPlotR)

# *********************************************************************
# Prepare hospital-level input for FunnelPlotR
# *********************************************************************
funnel_inhospital_data <- hospital_full_inhospital |>
  semi_join(hospital_sites_included,  by = "hospital_site_id_num"
  ) |>
  transmute(
    hospital_site_id_num = as.character(hospital_site_id_num),
    observed_deaths,
    expected_deaths
  )

funnel_90d_data <- hospital_full_90d |>
  semi_join(hospital_sites_included, by = "hospital_site_id_num"
  ) |>
  transmute(
    hospital_site_id_num = as.character(hospital_site_id_num),
    observed_deaths,
    expected_deaths
  )


# *********************************************************************
# Check for overdispersion
# *********************************************************************
# data_type = "SR": indirectly standardised ratio (observed / expected)
# draw_adjusted = TRUE: additionally calculate overdispersion-adjusted limits

fp_inhospital_od <- funnel_plot(
  .data = funnel_inhospital_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE
)

fp_90d_od <- funnel_plot(
  .data = funnel_90d_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE
)


qa_funnel_overdispersion <- tibble::tibble(
  outcome = c(
    "In-hospital mortality",
    "90-day post-admission mortality"
  ),
  phi = c(
    phi(fp_inhospital_od),
    phi(fp_90d_od)
  ),
  tau2 = c(
    tau2(fp_inhospital_od),
    tau2(fp_90d_od)
  ),
  n_outliers = c(
    nrow(outliers(fp_inhospital_od)),
    nrow(outliers(fp_90d_od))
  )
)

qa_funnel_overdispersion

# Overdiespersion Interpretation:
# Moderate overdispersion was observed for in-hospital mortality
# (phi = 1.32; tau² = 0.0036), whereas no overdispersion was detected
# for 90-day post-admission mortality (phi = 0.88; tau² = 0).
#
# For consistency, subsequent hospital classification uses the
# overdispersion-adjusted funnel framework for both outcomes.
# Where tau² = 0, the adjusted limits effectively coincide with
# the unadjusted Poisson limits.


# *********************************************************************
# Final funnel plots
# *********************************************************************

fp_inhospital_adj <- funnel_plot(
  .data = funnel_inhospital_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 99,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE,
  title = "In-hospital mortality after sepsis hospitalization",
  multiplier = 100,
  x_label = "Expected deaths",
  y_label = "Observed-to-expected mortality ratio (%)",
  plot_cols = c(
    "#ED6E4E",
    "#ED6E4E",
    "#005095",
    "#005095"
  ),
  theme = theme_bw()
)

fp_90d_adj <- funnel_plot(
  .data = funnel_90d_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 99,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE,
  title = "90-day post-admission mortality after sepsis hospitalization",
  multiplier = 100,
  x_label = "Expected deaths",
  y_label = "Observed-to-expected mortality ratio (%)",
  plot_cols = c(
    "#ED6E4E",
    "#ED6E4E",
    "#005095",
    "#005095"
  ),
  theme = theme_bw()
)


p_funnel_inhospital_adj <- fp_inhospital_adj$plot +
  coord_cartesian(ylim = c(0, 325))

p_funnel_90d_adj <- fp_90d_adj$plot +
  coord_cartesian(ylim = c(0, 325))

p_funnel_inhospital_adj
p_funnel_90d_adj


# Control-limit interpretation:
# 95% limits are used for the primary hospital classification because they are
# more sensitive to differences between mortality definitions and therefore
# better suited to assessing reclassification. However, with 95% limits some
# hospitals are expected to fall outside the limits by chance alone.
#
# The 99.8% limits are shown as a more conservative reference ("alarm limits"):
# hospitals outside these limits provide much stronger evidence of exceptional
# variation, but substantially fewer hospitals are expected to be flagged.
#
# Accordingly, hospital reclassification is based on the 95%
# overdispersion-adjusted control limits, while both 95% and 99.8% limits are
# displayed in the funnel plots.

# *********************************************************************
# Extract outlier classifications and source data for later comparison
# *********************************************************************

funnel_outliers_inhospital <- outliers(fp_inhospital_adj)
funnel_outliers_90d <- outliers(fp_90d_adj)

funnel_data_inhospital <- source_data(fp_inhospital_adj)
funnel_data_90d <- source_data(fp_90d_adj)



# *********************************************************************
# (4.7) Reclassification of hospitals: ----
#       in-hospital vs 90-day post-admission mortality
# *********************************************************************
# Main question:
# Does hospital performance classification change when using
# 90-day post-admission mortality instead of in-hospital mortality?
#
# Classification is based on the 95% overdispersion-adjusted
# funnel limits for both outcomes.
# *********************************************************************


# *********************************************************************
# Helper: classify hospital sites relative to funnel limits
# *********************************************************************

classify_funnel <- function(funnel_object, lower_limit, upper_limit) {
  
  source_data(funnel_object) |>
    as_tibble() |>
    transmute(
      hospital_site_id_num = as.integer(as.character(group)),
      oe_ratio = rr,
      category = case_when(
        rr * 100 < .data[[lower_limit]] ~ "Lower than expected",
        rr * 100 > .data[[upper_limit]] ~ "Higher than expected",
        TRUE ~ "Within control limits"
      )
    )
}


category_levels <- c(
  "Lower than expected",
  "Within control limits",
  "Higher than expected"
)


# *********************************************************************
# Classify hospitals for each mortality outcome
# *********************************************************************

funnel_class_inhospital <- classify_funnel(
  fp_inhospital_adj,
  lower_limit = "OD95LCL",
  upper_limit = "OD95UCL"
) |>
  rename(
    oe_ratio_inhospital = oe_ratio,
    category_inhospital = category
  )


funnel_class_90d <- classify_funnel(
  fp_90d_adj,
  lower_limit = "OD95LCL",
  upper_limit = "OD95UCL"
) |>
  rename(
    oe_ratio_90d = oe_ratio,
    category_90d = category
  )


# *********************************************************************
# Compare classifications
# *********************************************************************

hospital_reclassification <- funnel_class_inhospital |>
  inner_join(
    funnel_class_90d,
    by = "hospital_site_id_num"
  ) |>
  mutate(
    category_inhospital = factor(category_inhospital, levels = category_levels),
    category_90d        = factor(category_90d, levels = category_levels),
    changed_category    = category_inhospital != category_90d
  )


# *********************************************************************
# 3 × 3 reclassification table
# *********************************************************************

table_reclassification <- hospital_reclassification |>
  count(category_inhospital, category_90d, name = "n_hospitals" ) |>
  tidyr::complete(
    category_inhospital = factor(category_levels, levels = category_levels),
    category_90d        = factor(category_levels, levels = category_levels),
    fill                = list(n_hospitals = 0)
  )


table_reclassification_wide <- table_reclassification |>
  tidyr::pivot_wider(
    names_from = category_90d,
    values_from = n_hospitals,
    values_fill = 0
  )


# *********************************************************************
# Overall reclassification summary
# *********************************************************************

summary_reclassification <- hospital_reclassification |>
  summarise(
    n_hospitals          = n(),
    same_category        = sum(!changed_category),
    changed_category     = sum(changed_category),
    pct_changed_category = round(100 * changed_category / n_hospitals, 1)
  )


table_reclassification_wide
summary_reclassification


# *********************************************************************
# reclassification direction
# *********************************************************************
reclassification_directions <- hospital_reclassification |>
  filter(changed_category) |>
  count(
    category_inhospital,
    category_90d,
    name = "n_hospitals"
  ) |>
  arrange(desc(n_hospitals))

reclassification_directions

# Interpretation:
# Among 116 hospital sites included in the comparison, 101 (87.1%)
# remained in the same performance category when using 90-day
# post-admission instead of in-hospital mortality, whereas 15 (12.9%)
# changed category.
#
# Five hospitals moved from lower-than-expected mortality to within
# control limits, three moved from within control limits to
# lower-than-expected mortality, five moved from within control limits
# to higher-than-expected mortality, and two moved from
# higher-than-expected mortality to within control limits.
#
# Thus, replacing in-hospital mortality with a standardized 90-day
# post-admission endpoint materially changed the comparative assessment
# for approximately one in eight hospital sites.




# *********************************************************************
# (4.8) Hospital classification under alternative ----
#       risk-adjustment specifications
# *********************************************************************

# This analysis addresses two related questions:
#
# 1) Does the extent of reclassification between in-hospital and
#    90-day post-admission mortality depend on the risk-adjustment model?
#
# 2) Does risk adjustment itself change how hospitals are classified
#    for 90-day post-admission mortality?


# *********************************************************************
# (4.8.1) Helper function: ----
# Classification under alternative risk-adjustment models
# *********************************************************************

calculate_reclassification <- function(
    hospital_inhospital,
    hospital_90d,
    model_name
) {
  
  make_funnel <- function(hospital_data) {
    
    funnel_data <- hospital_data |>
      semi_join(
        hospital_sites_included,
        by = "hospital_site_id_num"
      ) |>
      transmute(
        hospital_site_id_num =
          as.character(hospital_site_id_num),
        observed_deaths,
        expected_deaths
      )
    
    funnel_plot(
      .data = funnel_data,
      numerator = observed_deaths,
      denominator = expected_deaths,
      group = hospital_site_id_num,
      data_type = "SR",
      limit = 95,
      label = NA,
      draw_unadjusted = FALSE,
      draw_adjusted = TRUE,
      multiplier = 100
    )
  }
  
  
  funnel_inhospital <- make_funnel(
    hospital_inhospital
  )
  
  funnel_90d <- make_funnel(
    hospital_90d
  )
  
  
  classification_inhospital <- classify_funnel(
    funnel_inhospital,
    lower_limit = "OD95LCL",
    upper_limit = "OD95UCL"
  ) |>
    select(
      hospital_site_id_num,
      category_inhospital = category
    )
  
  
  classification_90d <- classify_funnel(
    funnel_90d,
    lower_limit = "OD95LCL",
    upper_limit = "OD95UCL"
  ) |>
    select(
      hospital_site_id_num,
      category_90d = category
    )
  
  
  classification_inhospital |>
    inner_join(
      classification_90d,
      by = "hospital_site_id_num"
    ) |>
    mutate(
      model = model_name,
      category_inhospital = factor(
        category_inhospital,
        levels = category_levels
      ),
      category_90d = factor(
        category_90d,
        levels = category_levels
      ),
      changed_category =
        category_inhospital != category_90d
    )
}


# *********************************************************************
# (4.8.2) Generate classifications for Raw, Age/Sex and Full models ----
# *********************************************************************

hospital_reclassification_models <- bind_rows(
  
  calculate_reclassification(
    hospital_inhospital = hospital_raw_inhospital,
    hospital_90d = hospital_raw_90d,
    model_name = "Raw"
  ),
  
  calculate_reclassification(
    hospital_inhospital = hospital_age_sex_inhospital,
    hospital_90d = hospital_age_sex_90d,
    model_name = "Age and sex"
  ),
  
  calculate_reclassification(
    hospital_inhospital = hospital_full_inhospital,
    hospital_90d = hospital_full_90d,
    model_name = "Full model"
  )
)


# *********************************************************************
# (4.8.3) Endpoint reclassification under alternative ----
#         risk-adjustment models
# *********************************************************************
#
# Compact main-text table:
# In-hospital vs 90-day post-admission mortality under
# Raw, Age/Sex and Full adjustment.

summary_reclassification_models <- hospital_reclassification_models |>
  summarise(
    n_hospitals = n(),
    same_category = sum(!changed_category),
    pct_same_category =
      100 * same_category / n_hospitals,
    reclassified = sum(changed_category),
    pct_reclassified =
      100 * reclassified / n_hospitals,
    .by = model
  ) |>
  mutate(
    model = factor(
      model,
      levels = c(
        "Raw",
        "Age and sex",
        "Full model"
      )
    )
  ) |>
  arrange(model)

summary_reclassification_models


# *********************************************************************
# (4.8.4) Identity / overlap of endpoint-reclassified hospitals ----
# *********************************************************************
#
# This assesses whether the same hospital sites are affected when
# changing from in-hospital to 90-day mortality under different
# risk-adjustment specifications.

reclassification_identity <- hospital_reclassification_models |>
  select(
    hospital_site_id_num,
    model,
    changed_category
  ) |>
  pivot_wider(
    names_from = model,
    values_from = changed_category
  )


overlap_reclassification_models <- bind_rows(
  
  tibble(
    comparison = "Raw vs Age and sex",
    reclassified_model_1 =
      sum(reclassification_identity$Raw),
    reclassified_model_2 =
      sum(reclassification_identity$`Age and sex`),
    reclassified_in_both =
      sum(
        reclassification_identity$Raw &
          reclassification_identity$`Age and sex`
      ),
    model_1_only =
      sum(
        reclassification_identity$Raw &
          !reclassification_identity$`Age and sex`
      ),
    model_2_only =
      sum(
        !reclassification_identity$Raw &
          reclassification_identity$`Age and sex`
      )
  ),
  
  tibble(
    comparison = "Raw vs Full model",
    reclassified_model_1 =
      sum(reclassification_identity$Raw),
    reclassified_model_2 =
      sum(reclassification_identity$`Full model`),
    reclassified_in_both =
      sum(
        reclassification_identity$Raw &
          reclassification_identity$`Full model`
      ),
    model_1_only =
      sum(
        reclassification_identity$Raw &
          !reclassification_identity$`Full model`
      ),
    model_2_only =
      sum(
        !reclassification_identity$Raw &
          reclassification_identity$`Full model`
      )
  ),
  
  tibble(
    comparison = "Age and sex vs Full model",
    reclassified_model_1 =
      sum(reclassification_identity$`Age and sex`),
    reclassified_model_2 =
      sum(reclassification_identity$`Full model`),
    reclassified_in_both =
      sum(
        reclassification_identity$`Age and sex` &
          reclassification_identity$`Full model`
      ),
    model_1_only =
      sum(
        reclassification_identity$`Age and sex` &
          !reclassification_identity$`Full model`
      ),
    model_2_only =
      sum(
        !reclassification_identity$`Age and sex` &
          reclassification_identity$`Full model`
      )
  )
)

overlap_reclassification_models


# *********************************************************************
# (4.8.5) Prepare 90-day classifications across adjustment models ----
# *********************************************************************
#
# This analysis keeps the mortality endpoint fixed at 90 days and
# examines whether the hospital classification itself changes as
# the degree of risk adjustment changes.

library(ggalluvial)

classification_90d_models <- hospital_reclassification_models |>
  select(
    hospital_site_id_num,
    model,
    category_90d
  ) |>
  pivot_wider(
    names_from = model,
    values_from = category_90d
  ) |>
  mutate(
    across(
      c(
        Raw,
        `Age and sex`,
        `Full model`
      ),
      ~ factor(
        .x,
        levels = category_levels,
        labels = c(
          "Lower",
          "Within limits",
          "Higher"
        )
      )
    )
  )


# *********************************************************************
# (4.8.6) Quantify classification changes due to risk adjustment ----
# *********************************************************************

n_changed_raw_full <- classification_90d_models |>
  summarise(
    n = sum(Raw != `Full model`)
  ) |>
  pull(n)

n_changed_age_sex_full <- classification_90d_models |>
  summarise(
    n = sum(`Age and sex` != `Full model`)
  ) |>
  pull(n)

n_hospitals <- nrow(classification_90d_models)


summary_risk_adjustment_90d <- tibble(
  comparison = c(
    "Raw vs Full model",
    "Age and sex vs Full model"
  ),
  n_hospitals = n_hospitals,
  n_same = c(
    sum(
      classification_90d_models$Raw ==
        classification_90d_models$`Full model`
    ),
    sum(
      classification_90d_models$`Age and sex` ==
        classification_90d_models$`Full model`
    )
  ),
  n_changed = c(
    n_changed_raw_full,
    n_changed_age_sex_full
  )
) |>
  mutate(
    pct_changed =
      100 * n_changed / n_hospitals
  )

summary_risk_adjustment_90d


# *********************************************************************
# (4.8.7) Alluvial plots: ----
# Effect of risk adjustment on 90-day hospital classification
# *********************************************************************

classification_colors <- c(
  "Lower" = "#2c7bb6",
  "Within limits" = "#D9D9D9",
  "Higher" = "#d7191c"
)


# *********************************************************************
# Plot 1: Raw vs fully adjusted model
# *********************************************************************

p_alluvial_raw_full <- ggplot(
  classification_90d_models,
  aes(
    axis1 = Raw,
    axis2 = `Full model`,
    y = 1
  )
) +
  ggalluvial::geom_alluvium(
    aes(fill = Raw),
    width = 0.12,
    alpha = 0.60,
    knot.pos = 0.4
  ) +
  
  # Label flows corresponding to hospitals changing category
  ggplot2::geom_text(
    data = classification_90d_models |>
      filter(Raw != `Full model`),
    stat = "flow",
    aes(
      label = paste0(
        "n = ",
        after_stat(count)
      )
    ),
    size = 3,
    fontface = "bold"
  ) +
  
  ggalluvial::geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 0.16,
    colour = NA
  ) +
  
  ggplot2::geom_text(
    stat = "stratum",
    aes(
      label = after_stat(stratum)
    ),
    size = 3.2,
    fontface = "bold"
  ) +
  
  scale_x_discrete(
    limits = c(
      "Raw",
      "Full model"
    ),
    labels = c(
      "Raw",
      "Fully adjusted"
    ),
    expand = c(0.12, 0.12)
  ) +
  
  scale_fill_manual(
    values = classification_colors,
    drop = FALSE
  ) +
  
  labs(
    title = "A. Raw vs fully adjusted",
    subtitle = paste0(
      n_changed_raw_full,
      " of ",
      n_hospitals,
      " hospital sites changed classification (",
      round(
        100 * n_changed_raw_full /
          n_hospitals,
        1
      ),
      "%)"
    ),
    x = NULL,
    y = "Hospital sites",
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
  theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor =
      element_blank(),
    legend.position =
      "none",
    
    plot.title = element_text(
      size = 11,
      face = "bold",
      margin = margin(b = 3)
    ),
    
    plot.subtitle = element_text(
      size = 9,
      colour = "grey35",
      margin = margin(b = 8)
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "bold"
    ),
    
    axis.text.y =
      element_text(size = 8),
    
    axis.title.y =
      element_text(size = 9),
    
    plot.margin =
      margin(8, 12, 8, 8)
  )


# *********************************************************************
# Plot 2: Age- and sex-adjusted vs fully adjusted model
# *********************************************************************

p_alluvial_age_sex_full <- ggplot(
  classification_90d_models,
  aes(
    axis1 = `Age and sex`,
    axis2 = `Full model`,
    y = 1
  )
) +
  ggalluvial::geom_alluvium(
    aes(fill = `Age and sex`),
    width = 0.12,
    alpha = 0.60,
    knot.pos = 0.4
  ) +
  
  # Label flows corresponding to hospitals changing category
  ggplot2::geom_text(
    data = classification_90d_models |>
      filter(
        `Age and sex` !=
          `Full model`
      ),
    stat = "flow",
    aes(
      label = paste0(
        "n = ",
        after_stat(count)
      )
    ),
    size = 3,
    fontface = "bold"
  ) +
  
  ggalluvial::geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 0.16,
    colour = NA
  ) +
  
  ggplot2::geom_text(
    stat = "stratum",
    aes(
      label = after_stat(stratum)
    ),
    size = 3.2,
    fontface = "bold"
  ) +
  
  scale_x_discrete(
    limits = c(
      "Age and sex",
      "Full model"
    ),
    labels = c(
      "Age and sex",
      "Fully adjusted"
    ),
    expand = c(0.12, 0.12)
  ) +
  
  scale_fill_manual(
    values = classification_colors,
    drop = FALSE
  ) +
  
  labs(
    title =
      "B. Age- and sex-adjusted vs fully adjusted",
    
    subtitle = paste0(
      n_changed_age_sex_full,
      " of ",
      n_hospitals,
      " hospital sites changed classification (",
      round(
        100 * n_changed_age_sex_full /
          n_hospitals,
        1
      ),
      "%)"
    ),
    
    x = NULL,
    y = "Hospital sites",
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
  theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor =
      element_blank(),
    legend.position =
      "none",
    
    plot.title = element_text(
      size = 11,
      face = "bold",
      margin = margin(b = 3)
    ),
    
    plot.subtitle = element_text(
      size = 9,
      colour = "grey35",
      margin = margin(b = 8)
    ),
    
    axis.text.x = element_text(
      size = 10,
      face = "bold"
    ),
    
    axis.text.y =
      element_text(size = 8),
    
    axis.title.y =
      element_text(size = 9),
    
    plot.margin =
      margin(8, 8, 8, 12)
  )


# *********************************************************************
# (4.8.8) Combine both alluvial plots ----
# *********************************************************************

p_alluvial_risk_adjustment <-
  p_alluvial_raw_full |
  p_alluvial_age_sex_full


p_alluvial_risk_adjustment <-
  p_alluvial_risk_adjustment +
  patchwork::plot_annotation(
    
    title =
      "Hospital classification under alternative risk-adjustment specifications",
    
    caption = paste0(
      "Hospital sites were classified as having lower-than-expected mortality, ",
      "mortality within the 95% control limits, or higher-than-expected mortality. ",
      "Numbers on changing flows indicate the number of hospital sites moving ",
      "between classification categories."
    ),
    
    theme = theme(
      
      plot.title = element_text(
        size = 13,
        face = "bold",
        margin = margin(b = 8)
      ),
      
      plot.caption = element_text(
        size = 8,
        hjust = 0,
        colour = "grey35",
        margin = margin(t = 10)
      )
    )
  )


p_alluvial_risk_adjustment


# *********************************************************************
# Interpretation
# *********************************************************************
#
# The analyses distinguish two effects of risk adjustment.
#
# First, the proportion of hospitals reclassified when replacing
# in-hospital mortality with 90-day post-admission mortality can be
# compared across the Raw, Age/Sex and Full models.
#
# Second, with the mortality endpoint held constant at 90 days,
# hospital classification itself can change when progressively more
# detailed case-mix adjustment is applied.
#
# The alluvial plots illustrate both the extent and direction of these
# changes and therefore show whether risk adjustment materially affects
# how individual hospitals are judged.


# *********************************************************************
# (5.0) Profile of reclassified hospitals ----
# *********************************************************************
#
# Goal:
# To assess whether hospitals changing performance category when
# replacing in-hospital mortality with 90-day post-admission mortality
# show a recognizable hospital-level profile.
#
# Primary comparison:
# - Reclassified hospital sites
# - Hospital sites remaining in the same category
#
# Reclassification is based on the primary fully adjusted models
# for in-hospital and 90-day post-admission mortality, using
# overdispersion-adjusted 95% control limits.
# *********************************************************************


# *********************************************************************
# (5.1) Prepare hospital-level characteristics ----
# *********************************************************************

hospital_profile <- sepsis_mortality |>
  semi_join(
    hospital_sites_included,
    by = "hospital_site_id_num"
  ) |>
  mutate(
    # Death within 90 days after admission but out-of-hospital (after discharge)
    death_90d_after_discharge   = as.integer(death_90d_postadmission == 1L & death_in_hospital == 0L),
    # Transfer to another acute hospital / acute division
    discharge_to_acute_transfer = as.integer(post_discharge_location %in% c(6, 66)    )
  ) |>
  summarise(
    # Volume
    n_cases = n(),
    # Length of stay
    mean_episode_length         = mean(episode_length,na.rm = TRUE),
    median_episode_length       = median(episode_length,na.rm = TRUE),
    # Mortality
    deaths_inhospital           = sum(death_in_hospital,na.rm = TRUE),
    deaths_90d                  = sum(death_90d_postadmission,na.rm = TRUE),
    deaths_90d_after_discharge  = sum(death_90d_after_discharge,na.rm = TRUE),
    mortality_inhospital_pct    = 100 * deaths_inhospital / n_cases,
    mortality_90d_pct           = 100 * deaths_90d / n_cases,
    postdischarge_90d_death_pct = 100 * deaths_90d_after_discharge / n_cases,
    # Among all 90-day deaths: proportion occurring after discharge
    postdischarge_share_of_90d_deaths_pct = 100 * deaths_90d_after_discharge / deaths_90d,
    # Transfer pattern
    acute_transfer_pct         = 100 * mean(discharge_to_acute_transfer, na.rm = TRUE),
    .by = hospital_site_id_num
  )

# *********************************************************************
# (5.2) Add primary reclassification status and direction ----
# *********************************************************************

category_rank <- c(
  "Lower than expected" = 1,
  "Within control limits" = 2,
  "Higher than expected" = 3
)

hospital_profile <- hospital_profile |>
  left_join(
    hospital_reclassification |>
      select(
        hospital_site_id_num,
        category_inhospital,
        category_90d,
        changed_category
      ),
    by = "hospital_site_id_num"
  ) |>
  mutate(
    category_inhospital_num = unname(
      category_rank[as.character(category_inhospital)]
    ),
    category_90d_num = unname(
      category_rank[as.character(category_90d)]
    ),
    
    reclassification_direction = case_when(
      category_90d_num > category_inhospital_num ~
        "Toward higher mortality classification",
      category_90d_num < category_inhospital_num ~
        "Toward lower mortality classification",
      TRUE ~
        "No change"
    ),
    
    reclassification_direction = factor(
      reclassification_direction,
      levels = c(
        "No change",
        "Toward higher mortality classification",
        "Toward lower mortality classification"
      )
    )
  )

# QA: should be 101 / 10 / 5
hospital_profile |>
  count(reclassification_direction)


# *********************************************************************
# (5.3) Hospital profile by direction of reclassification ----
# *********************************************************************

hospital_profile_by_direction <- hospital_profile |>
  summarise(
    n_hospitals = n(),
    
    # Volume
    median_cases = median(n_cases, na.rm = TRUE),
    q1_cases = quantile(n_cases, 0.25, na.rm = TRUE),
    q3_cases = quantile(n_cases, 0.75, na.rm = TRUE),
    
    # Length of stay
    median_los = median(median_episode_length, na.rm = TRUE),
    q1_los = quantile(median_episode_length, 0.25, na.rm = TRUE),
    q3_los = quantile(median_episode_length, 0.75, na.rm = TRUE),
    
    # Share of 90-day deaths occurring after discharge
    median_postdischarge_share = median(
      postdischarge_share_of_90d_deaths_pct,
      na.rm = TRUE
    ),
    q1_postdischarge_share = quantile(
      postdischarge_share_of_90d_deaths_pct, 0.25, na.rm = TRUE
    ),
    q3_postdischarge_share = quantile(
      postdischarge_share_of_90d_deaths_pct, 0.75, na.rm = TRUE
    ),
    
    # Transfer to another acute-care hospital
    median_acute_transfer = median(acute_transfer_pct, na.rm = TRUE),
    q1_acute_transfer = quantile(acute_transfer_pct, 0.25, na.rm = TRUE),
    q3_acute_transfer = quantile(acute_transfer_pct, 0.75, na.rm = TRUE),
    
    .by = reclassification_direction
  )


# *********************************************************************
# (5.4) Publication table ----
# *********************************************************************

hospital_profile_table <- hospital_profile_by_direction |>
  transmute(
    reclassification_direction,
    n_hospitals,
    
    `Eligible sepsis hospitalizations, median (IQR)` =
      sprintf(
        "%.0f (%.0f–%.0f)",
        median_cases, q1_cases, q3_cases
      ),
    
    `Median length of stay, days, median (IQR)` =
      sprintf(
        "%.1f (%.1f–%.1f)",
        median_los,
        q1_los,
        q3_los
      ),
    
    `90-day deaths occurring after discharge, %, median (IQR)` =
      sprintf(
        "%.1f (%.1f–%.1f)",
        median_postdischarge_share,
        q1_postdischarge_share,
        q3_postdischarge_share
      ),
    
    `Transfer to another acute-care hospital, %, median (IQR)` =
      sprintf(
        "%.1f (%.1f–%.1f)",
        median_acute_transfer,
        q1_acute_transfer,
        q3_acute_transfer
      )
  )

hospital_profile_table |>
  print(width = Inf)


# *********************************************************************
# (5.5) Individual reclassified hospitals ----
# *********************************************************************

hospital_profile_reclassified <- hospital_profile |>
  filter(
    changed_category
  ) |>
  arrange(
    category_inhospital,
    category_90d,
    desc(n_cases)
  )

hospital_profile_reclassified


# *********************************************************************
# (6.0) Sensitivity analysis: ----
#       Conventional Poisson control limits
# *********************************************************************
## Primary hospital classifications use overdispersion-adjusted 95%
# control limits. This sensitivity analysis examines whether the
# reclassification result changes when conventional Poisson limits
# are used instead.

# *********************************************************************
# (6.1) Funnel plots using conventional Poisson limits ----
# *********************************************************************

fp_inhospital_poisson <- funnel_plot(
  .data = funnel_inhospital_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = FALSE,
  multiplier = 100
)


fp_90d_poisson <- funnel_plot(
  .data = funnel_90d_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = FALSE,
  multiplier = 100
)


# *********************************************************************
# 6.2) Classify hospitals using conventional Poisson limits
# *********************************************************************

sensitivity_class_inhospital_poisson <- classify_funnel(
  fp_inhospital_poisson,
  lower_limit = "LCL95",
  upper_limit = "UCL95"
) |>
  rename(
    category_inhospital = category
  )


sensitivity_class_90d_poisson <- classify_funnel(
  fp_90d_poisson,
  lower_limit = "LCL95",
  upper_limit = "UCL95"
) |>
  rename(
    category_90d = category
  )


# *********************************************************************
# (6.3) Endpoint reclassification using conventional Poisson limits ----
# *********************************************************************

hospital_reclassification_poisson <-
  sensitivity_class_inhospital_poisson |>
  select(
    hospital_site_id_num,
    category_inhospital
  ) |>
  inner_join(
    sensitivity_class_90d_poisson |>
      select(
        hospital_site_id_num,
        category_90d
      ),
    by = "hospital_site_id_num"
  ) |>
  mutate(
    category_inhospital = factor(
      category_inhospital,
      levels = category_levels
    ),
    category_90d = factor(
      category_90d,
      levels = category_levels
    ),
    changed_category =
      category_inhospital !=
      category_90d
  )


summary_reclassification_poisson <-
  hospital_reclassification_poisson |>
  summarise(
    n_hospitals = n(),
    same_category =
      sum(!changed_category),
    changed_category =
      sum(changed_category),
    pct_changed_category =
      round(
        100 *
          changed_category /
          n_hospitals,
        1
      )
  )


table_reclassification_poisson <-
  hospital_reclassification_poisson |>
  count(
    category_inhospital,
    category_90d,
    name = "n_hospitals"
  ) |>
  tidyr::complete(
    category_inhospital = factor(
      category_levels,
      levels = category_levels
    ),
    category_90d = factor(
      category_levels,
      levels = category_levels
    ),
    fill = list(
      n_hospitals = 0
    )
  ) |>
  pivot_wider(
    names_from = category_90d,
    values_from = n_hospitals,
    values_fill = 0
  )


summary_reclassification_poisson
table_reclassification_poisson

# *********************************************************************
# (6.4) Sensitivity of 90-day mortality classification ----
#       to control-limit method
# *********************************************************************
#
# This analysis holds constant:
# - the mortality endpoint: 90-day post-admission mortality
# - the risk-adjustment model: fully adjusted model
#
# It compares hospital mortality classification under:
# - overdispersion-adjusted 95% control limits
# - conventional Poisson 95% control limits
# *********************************************************************

classification_90d_control_limits <-
  funnel_class_90d |>
  select(
    hospital_site_id_num,
    category_overdispersion = category_90d
  ) |>
  inner_join(
    sensitivity_class_90d_poisson |>
      select(
        hospital_site_id_num,
        category_poisson = category_90d
      ),
    by = "hospital_site_id_num"
  ) |>
  mutate(
    category_overdispersion = factor(
      category_overdispersion,
      levels = category_levels
    ),
    category_poisson = factor(
      category_poisson,
      levels = category_levels
    ),
    changed_category =
      category_overdispersion != category_poisson
  )


# *********************************************************************
# Summary
# *********************************************************************

summary_control_limits_90d <-
  classification_90d_control_limits |>
  summarise(
    n_hospitals = n(),
    same_category = sum(!changed_category),
    changed_category = sum(changed_category),
    pct_changed_category = round(
      100 * changed_category / n_hospitals,
      1
    )
  )


# *********************************************************************
# 3 × 3 classification table
# *********************************************************************

table_control_limits_90d <-
  classification_90d_control_limits |>
  count(
    category_overdispersion,
    category_poisson,
    name = "n_hospitals"
  ) |>
  tidyr::complete(
    category_overdispersion = factor(
      category_levels,
      levels = category_levels
    ),
    category_poisson = factor(
      category_levels,
      levels = category_levels
    ),
    fill = list(
      n_hospitals = 0
    )
  )


table_control_limits_90d_wide <-
  table_control_limits_90d |>
  tidyr::pivot_wider(
    names_from = category_poisson,
    values_from = n_hospitals,
    values_fill = 0
  )


# *********************************************************************
# Direction of classification changes
# *********************************************************************

control_limit_change_directions_90d <-
  classification_90d_control_limits |>
  filter(changed_category) |>
  count(
    category_overdispersion,
    category_poisson,
    name = "n_hospitals"
  ) |>
  arrange(desc(n_hospitals))


# *********************************************************************
# Print results
# *********************************************************************

summary_control_limits_90d
table_control_limits_90d_wide
control_limit_change_directions_90d

# *********************************************************************
# Interpretation
# *********************************************************************
#
# Using conventional Poisson control limits, 18 of 116 hospital sites
# (15.5%) changed category when replacing in-hospital mortality with
# 90-day post-admission mortality, compared with 15 of 116 (12.9%)
# in the primary overdispersion-adjusted analysis.
#
# Thus, the main finding of endpoint-related hospital reclassification
# was robust to the method used to define the funnel-plot control limits,
# although conventional Poisson limits produced somewhat more
# reclassification.






# *********************************************************************
# (7.0) Export main results ----
# *********************************************************************

# *********************************************************************
# (7.1) ICD definitions ----
# *********************************************************************

supplement_icd_definitions <- bind_rows(
  tibble(
    definition_order = 1L,
    category_order = 1L,
    definition = "Sepsis-specific coding",
    evidence_category = "Sepsis-specific ICD-10 codes",
    code = cod_sepsis_specific_codes,
    matching = "Exact code"
  ),
  tibble(
    definition_order = 2L,
    category_order = 1L,
    definition = "Infection coding without sepsis-specific code",
    evidence_category = "Infection codes",
    code = cod_infection_prefixes,
    matching = "Code prefix"
  ),
  tibble(
    definition_order = 3L,
    category_order = 1L,
    definition = "External cause",
    evidence_category = "External causes",
    code = cod_external_cause_prefixes,
    matching = "Chapter prefix"
  ),
  tibble(
    definition_order = 4L,
    category_order = 1L,
    definition = "Competing non-infectious cause",
    evidence_category = "Major competing non-infectious causes",
    code = cod_competing_cause_prefixes,
    matching = "Code prefix"
  )
)

# *********************************************************************
# (7.2) Cohort characteristics ----
# *********************************************************************

cohort_characteristics <- bind_rows(
  sepsis_mortality |> count(age_group, name = "n") |>
    mutate(variable = "Age group", level = as.character(age_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(sex, name = "n") |>
    mutate(variable = "Sex", level = as.character(sex)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(admission_type_group, name = "n") |>
    mutate(variable = "Admission type", level = as.character(admission_type_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(trigger_position_group, name = "n") |>
    mutate(variable = "Sepsis trigger position", level = as.character(trigger_position_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(sepsis_severity_group, name = "n") |>
    mutate(variable = "Sepsis severity proxy", level = as.character(sepsis_severity_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(elixhauser_vw_group, name = "n") |>
    mutate(variable = "Elixhauser van Walraven score", level = as.character(elixhauser_vw_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(icu_admission_group, name = "n") |>
    mutate(variable = "ICU admission", level = as.character(icu_admission_group)) |>
    select(variable, level, n),
  
  sepsis_mortality |> count(mechanical_ventilation_group, name = "n") |>
    mutate(variable = "Mechanical ventilation", level = as.character(mechanical_ventilation_group)) |>
    select(variable, level, n)
) |>
  mutate(
    percent = 100 * n / nrow(sepsis_mortality)
  )

# *********************************************************************
# (7.3) Model fit tables ----
# *********************************************************************

model_fit_lrt_90d <- as_tibble(
  anova(
    model_raw_90d,
    model_age_sex_90d,
    model_full_90d,
    model_full2_90d,
    test = "Chisq"
  ),
  rownames = "model_step"
)

model_fit_lrt_inhospital <- as_tibble(
  anova(
    model_raw_inhospital,
    model_age_sex_inhospital,
    model_full_inhospital,
    model_full2_inhospital,
    test = "Chisq"
  ),
  rownames = "model_step"
)


# *********************************************************************
# (7.4) Export tables to Excel ----
# *********************************************************************

main_results_file <- file.path(
  path_output,
  glue::glue("main_results_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.xlsx")
)

write_excel_tables_update(
  list(
    
  
    
    # Cohort characteristics
    cohort_characteristics = cohort_characteristics,
    
    # Definitions and descriptive mortality results
    supplement_icd_definitions = supplement_icd_definitions,
    in_hospital_mortality = in_hospital_summary,
    mortality_by_window = mortality_results,
    
    # Model QA and model fit
    qa_model_missing = qa_model_missing,
    qa_expected_mortality = qa_expected_mortality,
    model_performance = model_performance_table,
    model_fit_lrt_90d = model_fit_lrt_90d,
    model_fit_lrt_inhospital = model_fit_lrt_inhospital,
    
    # Hospital volume and inclusion threshold
    hospital_thresholds = hospital_thresholds_table,
    site_volume_by_year = site_volume_by_year,
    site_volume_pooled = site_volume_pooled,
    
    # Funnel-plot QA
    funnel_overdispersion = qa_funnel_overdispersion,
    
    # Primary hospital comparison:
    # In-hospital vs 90-day post-admission mortality
    reclassification_table = table_reclassification_wide,
    reclassification_summary = summary_reclassification,
    reclassification_long = table_reclassification,
    hospital_reclassification = hospital_reclassification,
    
    # Hospital classification under alternative risk-adjustment specifications
    reclassification_models_summary = summary_reclassification_models,
    reclassification_models_overlap = overlap_reclassification_models,
    risk_adjustment_90d_summary = summary_risk_adjustment_90d,
    classification_90d_models = classification_90d_models,
    
    # Sensitivity analysis: conventional Poisson control limits
    reclassification_poisson_table = table_reclassification_poisson,
    reclassification_poisson_summary = summary_reclassification_poisson,
    
    # Hospital profiles by reclassification status
    hospital_profile_table = hospital_profile_table,
    hospital_profile_reclassified = hospital_profile_reclassified,
    
    # Hospital-level results
    hospital_full_inhospital = hospital_full_inhospital,
    hospital_full_90d = hospital_full_90d,
    
    # Full vs Full + interaction specification check
    hospital_compare_inhospital = hospital_compare_inhospital,
    hospital_compare_90d = hospital_compare_90d
  ),
  file = main_results_file
)

message(glue::glue("Output saved: {basename(main_results_file)}"))


# *********************************************************************
# (7.5) Export figures ----
# *********************************************************************

# Overall survival curve
ggsave(
  file.path(path_output, glue::glue("survival_curve_overall_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  km_plot_postadmission$plot,
  width = 9,
  height = 5.2,
  dpi = 500,
  bg = "white"
)

# Survival curves by patient / hospitalization characteristics
ggsave(
  file.path(path_output, glue::glue("survival_curve_by_strata_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  p_km_strata,
  width = 12,
  height = 15,
  dpi = 500,
  bg = "white"
)

# Cause-of-death evidence
ggsave(
  file.path(path_output, glue::glue("stackedbar_cod_evidence_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  p_cod_evidence,
  width = 9,
  height = 5.2,
  dpi = 500,
  bg = "white"
)

# Funnel plot: in-hospital mortality
ggsave(
  file.path(path_output, glue::glue("funnel_inhospital_adjusted_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  p_funnel_inhospital_adj,
  width = 8,
  height = 6,
  dpi = 500,
  bg = "white"
)

# Funnel plot: 90-day post-admission mortality
ggsave(
  file.path(path_output, glue::glue("funnel_90d_postadmission_adjusted_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  p_funnel_90d_adj,
  width = 8,
  height = 6,
  dpi = 500,
  bg = "white"
)

# Hospital classification under alternative risk adjustment
ggsave(
  file.path(path_output, glue::glue("alluvial_risk_adjustment_90d_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.png")),
  p_alluvial_risk_adjustment,
  width = 12,
  height = 6.5,
  dpi = 500,
  bg = "white"
)


# *********************************************************************
# (7.6) Save reusable R objects for paper generation ----
# *********************************************************************

paper_objects_file <- file.path(
  path_output,
  glue::glue("paper_objects_{indicator_id}_{indicator_name}_{index_start_year}_{index_end_year}_{datestamp}.rds")
)

saveRDS(
  list(
    
    # ***************************************************************--
    # Cohort characteristics
    # ***************************************************************--
    cohort_characteristics = cohort_characteristics,
    
    # ***************************************************************--
    # Cohort flow
    # ***************************************************************--
    flowchart_png = flowchart_png,
    flow_log = flow_log,
    flow = flow,
    
    # ***************************************************************--
    # Overall Kaplan-Meier objects
    # ***************************************************************--
    km_fit_postadmission = km_fit_postadmission,
    km_plot_postadmission = km_plot_postadmission,
    p_km_postadmission = km_plot_postadmission$plot,
    
    # ***************************************************************--
    # Stratified Kaplan-Meier plot objects
    # ***************************************************************--
    p_km_age = p_km_age,
    p_km_sex = p_km_sex,
    p_km_trigger = p_km_trigger,
    p_km_severity = p_km_severity,
    p_km_admission = p_km_admission,
    p_km_charlson = p_km_charlson,
    p_km_elixhauser = p_km_elixhauser,
    p_km_icu = p_km_icu,
    p_km_icu_length = p_km_icu_length,
    p_km_ventilation = p_km_ventilation,
    p_km_strata = p_km_strata,
    
    # ***************************************************************--
    # FunnelPlotR objects
    # ***************************************************************--
    fp_inhospital_adj = fp_inhospital_adj,
    fp_90d_adj = fp_90d_adj,
    fp_inhospital_poisson = fp_inhospital_poisson,
    fp_90d_poisson = fp_90d_poisson,
    
    # ***************************************************************--
    # Extracted funnel ggplot objects
    # ***************************************************************--
    p_funnel_inhospital_adj = p_funnel_inhospital_adj,
    p_funnel_90d_adj = p_funnel_90d_adj,
    
    # ***************************************************************--
    # Risk-adjustment plots
    # ***************************************************************--
    p_alluvial_raw_full = p_alluvial_raw_full,
    p_alluvial_age_sex_full = p_alluvial_age_sex_full,
    p_alluvial_risk_adjustment = p_alluvial_risk_adjustment,
    
    # ***************************************************************--
    # Final tables and figure data
    # ***************************************************************--
    supplement_icd_definitions = supplement_icd_definitions,
    in_hospital_mortality = in_hospital_summary,
    mortality_by_window = mortality_results,
    cod_evidence_plot = cod_evidence_plot,
    p_cod_evidence = p_cod_evidence,
    
    # ***************************************************************--
    # Primary hospital reclassification
    # ***************************************************************--
    table_reclassification = table_reclassification_wide,
    table_reclassification_long = table_reclassification,
    summary_reclassification = summary_reclassification,
    hospital_reclassification = hospital_reclassification,
    
    # ***************************************************************--
    # Hospital classification under alternative risk-adjustment models
    # ***************************************************************--
    hospital_reclassification_models = hospital_reclassification_models,
    summary_reclassification_models = summary_reclassification_models,
    overlap_reclassification_models = overlap_reclassification_models,
    classification_90d_models = classification_90d_models,
    summary_risk_adjustment_90d = summary_risk_adjustment_90d,
    
    # ***************************************************************--
    # Sensitivity analysis: conventional Poisson limits
    # ***************************************************************--
    table_reclassification_poisson = table_reclassification_poisson,
    summary_reclassification_poisson = summary_reclassification_poisson,
    hospital_reclassification_poisson = hospital_reclassification_poisson,
    
    # ***************************************************************--
    # Sensitivity analysis: control-limit method for 90-day mortality
    # ***************************************************************--
    classification_90d_control_limits = classification_90d_control_limits,
    summary_control_limits_90d = summary_control_limits_90d,
    table_control_limits_90d = table_control_limits_90d_wide,
    control_limit_change_directions_90d = control_limit_change_directions_90d,
    
    # ***************************************************************--
    # QA tables
    # ***************************************************************--
    qa_model_missing = qa_model_missing,
    qa_expected_mortality = qa_expected_mortality,
    qa_funnel_overdispersion = qa_funnel_overdispersion,
    hospital_thresholds_table = hospital_thresholds_table,
    site_volume_by_year = site_volume_by_year,
    site_volume_pooled = site_volume_pooled,
    
    # ***************************************************************--
    # Model fit
    # ***************************************************************--
    model_performance_table = model_performance_table,
    model_fit_lrt_90d = model_fit_lrt_90d,
    model_fit_lrt_inhospital = model_fit_lrt_inhospital,
    
    
    # ***************************************************************--
    # Full vs Full + interaction specification check
    # ***************************************************************--
    hospital_compare_inhospital = hospital_compare_inhospital,
    hospital_compare_90d = hospital_compare_90d,
    
    # ***************************************************************--
    # Hospital profiles by reclassification status
    # ***************************************************************--
    hospital_profile = hospital_profile,
    hospital_profile_by_direction = hospital_profile_by_direction,
    hospital_profile_table = hospital_profile_table,
    hospital_profile_reclassified = hospital_profile_reclassified,
    # ***************************************************************--
    # Hospital summaries
    # ***************************************************************--
    hospital_full_inhospital = hospital_full_inhospital,
    hospital_full_90d = hospital_full_90d,
    
    hospital_full_inhospital_analysis = hospital_full_inhospital |>
      semi_join(hospital_sites_included, by = "hospital_site_id_num"),
    
    hospital_full_90d_analysis = hospital_full_90d |>
      semi_join(hospital_sites_included, by = "hospital_site_id_num")
  ),
  paper_objects_file
)

message(glue::glue("R objects saved: {basename(paper_objects_file)}"))

