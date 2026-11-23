# ******************************************************************************
# 06_descriptive_mortality.R
# ******************************************************************************
#
# Purpose:
# - Summarise observed mortality from admission and discharge.
# - Calculate crude and Kaplan-Meier mortality at follow-up windows.
# - Plot overall survival and survival stratified by key covariates.
#
# Inputs:
# - analysis_dataset after 05_mortality_outcomes.R and 06_covariates.R.
#
# Outputs:
# - mortality_burden table in memory and printed to the console.
# - Overall and covariate-stratified Kaplan-Meier plots in memory.
# - script saves an RDS bundle with objects relevant for the paper
#
# ******************************************************************************


# ******************************************************************************
# Check input and define follow-up windows ----
# ******************************************************************************

required_mortality_vars <- c(
  "death_event",
  "death_in_hospital_index_stay",
  "followup_time_postdischarge_days",
  "followup_time_postadmission_days",
  "age_group",
  "sex",
  "admission_type_group",
  "trigger_position_group",
  "sepsis_severity_group",
  "charlson_group",
  "elixhauser_vw_group"
)

missing_mortality_vars <- setdiff(
  required_mortality_vars,
  names(analysis_dataset)
)

if (length(missing_mortality_vars) > 0L) {
  stop(
    "Missing required variables in analysis_dataset: ",
    paste(missing_mortality_vars, collapse = ", "),
    call. = FALSE
  )
}

mortality_windows_days <- c(30L, 60L, 90L, 180L, 365L)
max_followup_days <- max(mortality_windows_days)


# ******************************************************************************
# Overall observed mortality and Kaplan-Meier estimates ----
# ******************************************************************************

summarise_mortality_anchor <- function(data, time_var, anchor) {
  followup_time <- data[[time_var]]
  
  km_data <- data |>
    dplyr::filter(!is.na(.data[[time_var]])) |>
    dplyr::mutate(
      km_time = pmin(.data[[time_var]], max_followup_days),
      km_event = death_event == 1L & .data[[time_var]] <= max_followup_days
    )
  
  km_fit <- survival::survfit(
    survival::Surv(km_time, km_event) ~ 1,
    data = km_data
  )
  
  km_at_windows <- summary(km_fit, times = mortality_windows_days)
  
  n_deaths <- vapply(
    mortality_windows_days,
    function(day) {
      sum(data$death_event == 1L & !is.na(followup_time) & followup_time <= day)
    },
    numeric(1)
  )
  
  n_evaluable <- vapply(
    mortality_windows_days,
    function(day) {
      sum(
        !is.na(followup_time) &
          (
            (data$death_event == 1L & followup_time <= day) |
              followup_time >= day
          )
      )
    },
    numeric(1)
  )
  
  mortality_table <- tibble::tibble(
    anchor = anchor,
    followup_days = km_at_windows$time,
    n_at_risk = km_at_windows$n.risk,
    n_evaluable = n_evaluable,
    deaths = n_deaths,
    crude_mortality_pct = 100 * n_deaths / n_evaluable,
    km_survival_pct = 100 * km_at_windows$surv,
    km_mortality_pct = 100 * (1 - km_at_windows$surv)
  )
  
  list(table = mortality_table, fit = km_fit, data = km_data)
}

postdischarge_results <- summarise_mortality_anchor(
  analysis_dataset,
  "followup_time_postdischarge_days",
  "Post-discharge"
)

postadmission_results <- summarise_mortality_anchor(
  analysis_dataset,
  "followup_time_postadmission_days",
  "Post-admission"
)

n_cohort <- nrow(analysis_dataset)
n_in_hospital_deaths <- sum(
  analysis_dataset$death_in_hospital_index_stay == 1L,
  na.rm = TRUE
)

mortality_in_hospital <- tibble::tibble(
  anchor = "In-hospital",
  followup_days = NA_integer_,
  n_at_risk = n_cohort,
  n_evaluable = n_cohort,
  deaths = n_in_hospital_deaths,
  crude_mortality_pct = 100 * n_in_hospital_deaths / n_cohort,
  km_survival_pct = NA_real_,
  km_mortality_pct = NA_real_
)

mortality_burden <- dplyr::bind_rows(
  mortality_in_hospital,
  postdischarge_results$table,
  postadmission_results$table
)

print(tibble::as_tibble(mortality_burden), n = Inf, width = Inf)


# ******************************************************************************
# In-hospital death coverage by post-admission window ----
# ******************************************************************************
n_in_hospital_deaths_with_known_timing <- sum(
  analysis_dataset$death_in_hospital_index_stay == 1L & 
    !is.na(analysis_dataset$admission_to_death_days_analysis),
  na.rm = TRUE
)

in_hospital_deaths_covered <- vapply(
  mortality_windows_days,
  function(day) {
    sum(
      analysis_dataset$death_in_hospital_index_stay == 1L &
        !is.na(analysis_dataset$admission_to_death_days_analysis) &
        analysis_dataset$admission_to_death_days_analysis <= day,
      na.rm = TRUE
    )
  },
  numeric(1)
)

inhospital_death_coverage <- tibble::tibble(
  followup_days = mortality_windows_days,
  in_hospital_deaths_total = n_in_hospital_deaths,
  in_hospital_deaths_with_known_timing =
    n_in_hospital_deaths_with_known_timing,
  in_hospital_deaths_covered = in_hospital_deaths_covered,
  deaths_postadmission = postadmission_results$table$deaths
) |>
  dplyr::mutate(
    pct_in_hospital_deaths_covered =
      100 * in_hospital_deaths_covered / in_hospital_deaths_total,
    deaths_after_discharge =
      deaths_postadmission - in_hospital_deaths_covered,
    pct_deaths_after_discharge = dplyr::if_else(
      deaths_postadmission > 0,
      100 * deaths_after_discharge / deaths_postadmission,
      NA_real_
    )
  )

print(tibble::as_tibble(inhospital_death_coverage), n = Inf, width = Inf)
# in_hospital_deaths_with_known_timing is a QA count of in-hospital deaths with 
# a non-missing admission_to_death_days_analysis value—that is, deaths for 
# which episode_length is available. Comparing it with in_hospital_deaths_total 
# shows whether every in-hospital death has a time value for the window 
# calculation.


# ******************************************************************************
# Plot overall Kaplan-Meier curves ----
# ******************************************************************************

make_km_plot <- function(km_fit, km_data, title, x_label, show_legend = FALSE) {
  p <- survminer::ggsurvplot(
    km_fit,
    data = km_data,
    conf.int = TRUE,
    risk.table = FALSE,
    censor = TRUE,
    xlim = c(0, max_followup_days),
    ylim = c(0, 1),
    break.time.by = 30,
    surv.scale = "percent",
    legend = if (show_legend) "bottom" else "none",
    legend.title = NULL,
    ggtheme = ggplot2::theme_bw(),
    xlab = x_label,
    ylab = "Survival probability"
  )$plot +
    ggplot2::geom_vline(
      xintercept = c(30, 90, 180, 365),
      linetype = "dashed",
      linewidth = 0.3,
      colour = "grey60"
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  p
}

p_km_overall_postdischarge <- make_km_plot(
  postdischarge_results$fit,
  postdischarge_results$data,
  "Overall survival after discharge",
  "Days since discharge"
)

p_km_overall_postadmission <- make_km_plot(
  postadmission_results$fit,
  postadmission_results$data,
  "Overall survival after admission",
  "Days since admission"
)

print(p_km_overall_postdischarge)
print(p_km_overall_postadmission)


# ******************************************************************************
# Kaplan-Meier curves by covariate, anchored at admission ----
# ******************************************************************************

plot_km_by_group <- function(data, group_var, title) {
  km_data <- data |>
    dplyr::filter(
      !is.na(followup_time_postadmission_days),
      !is.na(.data[[group_var]])
    ) |>
    dplyr::mutate(
      km_time = pmin(
        followup_time_postadmission_days,
        max_followup_days
      ),
      km_event = death_event == 1L &
        followup_time_postadmission_days <= max_followup_days,
      km_group = as.factor(.data[[group_var]])
    )
  
  km_fit <- survival::survfit(
    survival::Surv(km_time, km_event) ~ km_group,
    data = km_data
  )
  
  make_km_plot(
    km_fit,
    km_data,
    title,
    "Days since admission",
    show_legend = TRUE
  )
}

p_km_age_group <- plot_km_by_group(analysis_dataset, "age_group", "Age group")
p_km_sex <- plot_km_by_group(analysis_dataset, "sex", "Sex")
p_km_admission_type <- plot_km_by_group(analysis_dataset, "admission_type_group", "Admission type")
p_km_trigger_position <- plot_km_by_group(analysis_dataset, "trigger_position_group", "Sepsis trigger position")
p_km_sepsis_severity <- plot_km_by_group(analysis_dataset, "sepsis_severity_group", "Sepsis severity proxy")
p_km_charlson <- plot_km_by_group(analysis_dataset, "charlson_group", "Charlson score")
p_km_elixhauser <- plot_km_by_group(analysis_dataset, "elixhauser_vw_group", "Elixhauser van Walraven score")

print(p_km_age_group)
print(p_km_sex)
print(p_km_admission_type)
print(p_km_trigger_position)
print(p_km_sepsis_severity)
print(p_km_charlson)
print(p_km_elixhauser)

# ******************************************************************************
# Save descriptive mortality objects ----
# ******************************************************************************

rds_object_filepath <- file.path(
  path_indicator_results,
  paste0(
    "06_descriptive_mortality_rds_objects_",
    analysis_dataset_name,
    ".rds"
  )
)

descriptive_mortality_objects <- list(
  metadata = list(
    analysis_dataset_name = analysis_dataset_name,
    indicator_id = indicator_id,
    index_start_year = index_start_year,
    index_end_year = index_end_year,
    datestamp = datestamp
  ),
  mortality_burden = mortality_burden,
  inhospital_death_coverage = inhospital_death_coverage,
  plots = list(
    overall_postdischarge = p_km_overall_postdischarge,
    overall_postadmission = p_km_overall_postadmission,
    age_group = p_km_age_group,
    sex = p_km_sex,
    admission_type = p_km_admission_type,
    trigger_position = p_km_trigger_position,
    sepsis_severity = p_km_sepsis_severity,
    charlson = p_km_charlson,
    elixhauser = p_km_elixhauser
  )
)

saveRDS(
  descriptive_mortality_objects,
  file = rds_object_filepath
)

cli::cli_alert_success(
  "Descriptive mortality bundle saved: {basename(rds_object_filepath)}"
)