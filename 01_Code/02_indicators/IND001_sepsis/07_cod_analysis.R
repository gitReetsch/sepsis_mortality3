# ******************************************************************************
# 07_cod_analysis.R
# ******************************************************************************
#
# Purpose:
# - Classify Cause of Death (CoD) coding patterns among deaths in the sepsis cohort.
# - Describe how coding patterns vary by time from index admission to death.
#
# Inputs:
# - path_cod wird im run script definiert
# - analysis_dataset after 04_mortality_outcomes.R
# - CoD code vectors from 01_definition.R:
#   sepsis_explicit_codes
#   sepsis_infection_prefixes
#   sepsis_against_prefixes_strong
#   sepsis_against_prefixes_moderate
#
# Outputs:
# - cod_evidence_summary and QA summaries in memory and printed to console.
# - p_cod_evidence plot in memory.
# - RDS bundle with the summary tables and plot.
#
# This analysis describes recorded CoD coding patterns. It does not establish
# whether an individual death was caused by sepsis.
#
# ******************************************************************************


# ******************************************************************************
# Link CoD records to the saved analysis cohort ----
# ******************************************************************************
cod_fields <- c(
  "grund_ges_t",
  "unmitt_ges_t",
  "begleit_a_ges_t",
  "begleit_b_ges_t",
  "endg_u_ges_t"
)

cod_data <- fst::read_fst(path_cod)

discharge_cod <- analysis_dataset |>
  dplyr::mutate(patient_id = dplyr::na_if(trimws(as.character(patient_id)), "")) |>
  dplyr::left_join(cod_data, by = c("patient_id" = "ahv_n")) |>
  dplyr::mutate(match_cod = as.integer(!is.na(ereignis_ges_d)))

if (nrow(discharge_cod) != nrow(analysis_dataset)) {
  stop("Joining CoD data changed the number of analysis records.", call. = FALSE)
  }

message("CoD records linked to ", sum(discharge_cod$match_cod == 1L, na.rm = TRUE),
        " of ", nrow(analysis_dataset),  " analysis records.")

# ******************************************************************************
# Classify cause-of-death coding patterns ----
# ******************************************************************************
# Classify CoD coding patterns among cohort deaths.
cod_deaths <- discharge_cod |>
  dplyr::filter(death_event == 1L) |>
  dplyr::mutate(
    cod_record_available = match_cod == 1L,
    cod_sepsis_specific_code = dplyr::if_any(
      dplyr::all_of(cod_fields),  ~ .x %in% sepsis_explicit_codes ),
    cod_infection_code = 
      dplyr::if_any(dplyr::all_of(cod_fields),~ purrr::map_lgl(.x,\(code) !is.na(code) &&
          any(stringr::str_starts(code, sepsis_infection_prefixes)))),
    cod_external_cause = dplyr::if_any(
      dplyr::all_of(cod_fields),~ substr(.x, 1, 1) %in% sepsis_against_prefixes_strong),
    cod_competing_noninfectious_cause = dplyr::if_any(dplyr::all_of(cod_fields),
      ~ purrr::map_lgl(.x,\(code) !is.na(code) && any(stringr::str_starts(code, sepsis_against_prefixes_moderate)))),
    cod_unknown_or_ill_defined =cod_record_available & 
      dplyr::if_all(dplyr::all_of(cod_fields),~ is.na(.x) | .x == "R99"),
    cod_evidence_group = dplyr::case_when(
      !cod_record_available ~ "No CoD record available",
      (cod_sepsis_specific_code | cod_infection_code) &
        (cod_external_cause | cod_competing_noninfectious_cause) ~  "Mixed coding pattern",
      cod_sepsis_specific_code ~  "Sepsis-specific code",
      cod_infection_code ~ "Infection code without sepsis-specific code",
      cod_unknown_or_ill_defined ~ "Unknown / ill-defined",
      cod_external_cause ~ "External cause",
      cod_competing_noninfectious_cause ~ "Competing non-infectious cause",
      TRUE ~ "No clear classification"
    )
  )


# ******************************************************************************
# Group deaths by time from admission ----
# ******************************************************************************
admission_time_levels <- c(
  "0–30 days",
  "31–60 days",
  "61–90 days",
  "91–180 days",
  "181–365 days",
  "366+ days"
)

cod_evidence_time <- cod_deaths |>
  dplyr::filter(
    !is.na(admission_to_death_days_analysis),
    admission_to_death_days_analysis >= 0
  ) |>
  dplyr::mutate(
    admission_to_death_group = dplyr::case_when(
      admission_to_death_days_analysis <= 30  ~ "0–30 days",
      admission_to_death_days_analysis <= 60  ~ "31–60 days",
      admission_to_death_days_analysis <= 90  ~ "61–90 days",
      admission_to_death_days_analysis <= 180 ~ "91–180 days",
      admission_to_death_days_analysis <= 365 ~ "181–365 days",
      TRUE                                    ~ "366+ days"
    ),
    admission_to_death_group = factor(
      admission_to_death_group,
      levels = admission_time_levels
    )
  )


# CoD categories among deaths with a CoD record
cod_evidence_summary <- cod_evidence_time |>
  dplyr::filter(cod_record_available) |>
  dplyr::count(admission_to_death_group, cod_evidence_group, name = "n") |>
  dplyr::group_by(admission_to_death_group) |>
  dplyr::mutate(
    percent = 100 * n / sum(n)
  ) |>
  dplyr::ungroup()

# Share of all deaths without a CoD record, shown separately in the plot
cod_missing_summary <- cod_evidence_time |>
  dplyr::group_by(admission_to_death_group) |>
  dplyr::summarise(
    n_deaths = dplyr::n(),
    n_without_cod = sum(!cod_record_available),
    percent_without_cod = 100 * n_without_cod / n_deaths,
    .groups = "drop"
  )

print(tibble::as_tibble(cod_evidence_summary), n = Inf, width = Inf)
print(tibble::as_tibble(cod_missing_summary), n = Inf, width = Inf)


# ******************************************************************************
# Plot through 365 days after admission ----
# ******************************************************************************
# Legend and stack order: dark blue first, then light blue through grey.
cod_colours <- c(
  "Sepsis-specific code" = "#2C7BB6",
  "Infection code without sepsis-specific code" = "#ABD9E9",
  "Mixed coding pattern" = "#FFFFBF",
  "Competing non-infectious cause" = "#FDAE61",
  "External cause" = "#D7191C",
  "No clear classification" = "#E5E5E5",
  "Unknown / ill-defined" = "#BDBDBD"
)

cod_plot_categories <- cod_evidence_summary |>
  dplyr::filter(admission_to_death_group != "366+ days") |>
  dplyr::mutate(
    x = as.integer(admission_to_death_group),
    cod_evidence_group = factor(
      cod_evidence_group,
      levels = names(cod_colours)
    )
  )

cod_plot_missing <- cod_missing_summary |>
  dplyr::filter(admission_to_death_group != "366+ days") |>
  dplyr::mutate(
    x = as.integer(admission_to_death_group),
    ymin = 100,
    ymax = 100 + percent_without_cod
  )

p_cod_evidence <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = cod_plot_categories,
    ggplot2::aes(
      x = x,
      y = percent,
      fill = cod_evidence_group
    ),
    position = ggplot2::position_stack(reverse = TRUE)
  ) +
  ggplot2::geom_rect(
    data = cod_plot_missing,
    ggplot2::aes(
      xmin = x - 0.45,
      xmax = x + 0.45,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "#969696"
  ) +
  ggplot2::geom_hline(
    yintercept = 100,
    linewidth = 0.4,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:5,
    labels = admission_time_levels[1:5],
    limits = c(0.5, 5.5)
  ) +
  ggplot2::scale_y_continuous(
    labels = \(x) paste0(x, "%"),
    breaks = seq(0, 150, by = 25),
    limits = c(0, 150),
    expand = c(0, 0)
  ) +
  ggplot2::scale_fill_manual(
    values = cod_colours,
    breaks = names(cod_colours),
    drop = FALSE
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  ggplot2::labs(
    x = "Time from admission to death",
    y = "Percentage of deaths",
    fill = "Cause-of-death coding category",
    caption = paste0(
      "Cause-of-death coding patterns by time from admission for index sepsis hospitalisations, 2021–2024.\n",
      "Deaths occurring within 365 days of admission were assigned to mutually exclusive categories ",
      "using the code groups in Supplementary Table F1 and the hierarchical rules in Section F.\n",
      "Coloured segments show the distribution among deaths with an available CoD record and sum to 100% ",
      "within each interval. The dark-grey segment above 100% shows the percentage without an available CoD record.\n",
      "Coding patterns do not establish whether deaths were attributable to sepsis. Sources: Swiss Federal ",
      "Statistical Office — Health Services Statistics (GVS), Cause of Death Statistics (CoD), and Vital Statistics (BEVNAT)."
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  )

print(p_cod_evidence)

# ******************************************************************************
# Save cause-of-death analysis objects ----
# ******************************************************************************

rds_object_filepath <- file.path(
  path_indicator_results,
  paste0(
    "07_cod_analysis_rds_objects_",
    analysis_dataset_name,
    ".rds"
  )
)

cod_analysis_objects <- list(
  metadata = list(
    analysis_dataset_name = analysis_dataset_name,
    indicator_id = indicator_id,
    index_start_year = index_start_year,
    index_end_year = index_end_year,
    datestamp = datestamp
  ),
  cod_evidence_summary = cod_evidence_summary,
  cod_missing_summary = cod_missing_summary,
  p_cod_evidence = p_cod_evidence
)

saveRDS(
  cod_analysis_objects,
  file = rds_object_filepath
)

cli::cli_alert_success(
  "Cause-of-death analysis bundle saved: {basename(rds_object_filepath)}"
)
