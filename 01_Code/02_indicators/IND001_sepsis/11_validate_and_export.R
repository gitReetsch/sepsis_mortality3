# ******************************************************************************
# 11_validate_and_export.R
# ******************************************************************************
#
# Purpose:
# - Load and validate the RDS bundles from Scripts 03–09.
# - Derive paper_numbers and check internal consistency.
# - Save the original bundles and paper_numbers in a final RDS file.
#
# Inputs:
# - Run parameters from 00_run_sepsis.R.
# - RDS bundles from Scripts 03–09 in path_indicator_results.
#
# Outputs:
# - Stops if a bundle is missing, metadata are inconsistent,
#   or internal count checks fail.
# - A final RDS bundle in path_indicator_results.
#
# Run after the result-producing scripts have completed.
#
# ******************************************************************************

# ******************************************************************************
# Check required run parameters ----
# ******************************************************************************

required_parameters <- c(
  "path_indicator_results",
  "analysis_dataset_name",
  "indicator_id",
  "indicator_name",
  "index_start_year",
  "index_end_year",
  "datestamp"
)

missing_parameters <- required_parameters[
  !vapply(required_parameters, exists, logical(1), inherits = TRUE)
]

if (length(missing_parameters) > 0L) {
  stop(
    "Missing run parameter(s): ",
    paste(missing_parameters, collapse = ", ")
  )
}

if (!dir.exists(path_indicator_results)) {
  stop("Results directory does not exist: ", path_indicator_results)
}


# ******************************************************************************
# Load result bundles from Scripts 03–09 ----
# ******************************************************************************
bundle_filepaths <- c(
  "03_cohort"                = file.path(path_indicator_results, paste0("03_cohort_rds_objects_", analysis_dataset_name, ".rds")),
  "04_mortality_outcomes"    = file.path(path_indicator_results,  paste0("04_mortality_outcomes_rds_objects_", analysis_dataset_name, ".rds")),
  "05_covariates"            = file.path(path_indicator_results, paste0("05_covariates_rds_objects_", analysis_dataset_name, ".rds")),
  "06_descriptive_mortality" = file.path(path_indicator_results, paste0("06_descriptive_mortality_rds_objects_", analysis_dataset_name, ".rds")),
  "07_cod_analysis"          = file.path(path_indicator_results, paste0("07_cod_analysis_rds_objects_", analysis_dataset_name, ".rds")),
  "08_models"                = file.path(path_indicator_results, paste0("08_models_rds_objects_", analysis_dataset_name, ".rds")),
  "09_hospital_comparisons"  = file.path(path_indicator_results, paste0("09_hospital_comparisons_rds_objects_", analysis_dataset_name, ".rds")    )
  )

missing_bundles <- bundle_filepaths[!file.exists(bundle_filepaths)]

if (length(missing_bundles) > 0L) {
  stop("Required RDS bundle(s) not found: ", paste(basename(missing_bundles), collapse = ", "))}

result_bundles <- lapply(bundle_filepaths, readRDS)


# ******************************************************************************
# Check bundle metadata ----
# ******************************************************************************
expected_metadata <- list(
  analysis_dataset_name = analysis_dataset_name,
  indicator_id = indicator_id,
  index_start_year = index_start_year,
  index_end_year = index_end_year
)

metadata_matches <- c(
  "03_cohort"                = identical(result_bundles[["03_cohort"]]$metadata[names(expected_metadata)],  expected_metadata),
  "04_mortality_outcomes"    = identical(result_bundles[["04_mortality_outcomes"]]$metadata[names(expected_metadata)], expected_metadata ),
  "05_covariates"            = identical(result_bundles[["05_covariates"]]$metadata[names(expected_metadata)], expected_metadata),
  "06_descriptive_mortality" = identical(result_bundles[["06_descriptive_mortality"]]$metadata[names(expected_metadata)], expected_metadata),
  "07_cod_analysis"          = identical(result_bundles[["07_cod_analysis"]]$metadata[names(expected_metadata)], expected_metadata),
  "08_models"                = identical(result_bundles[["08_models"]]$metadata[names(expected_metadata)], expected_metadata),
  "09_hospital_comparisons"  = identical(result_bundles[["09_hospital_comparisons"]]$metadata[names(expected_metadata)],expected_metadata)
)

if (!all(metadata_matches)) {
  stop("Metadata mismatch in bundle(s): ",paste(names(metadata_matches)[!metadata_matches], collapse = ", "))}

# ******************************************************************************
# Derive and validate key paper numbers ----
# ******************************************************************************
cohort_bundle      <- result_bundles[["03_cohort"]]
outcome_bundle     <- result_bundles[["04_mortality_outcomes"]]
covariate_bundle   <- result_bundles[["05_covariates"]]
descriptive_bundle <- result_bundles[["06_descriptive_mortality"]]
cod_bundle         <- result_bundles[["07_cod_analysis"]]
model_bundle       <- result_bundles[["08_models"]]
hospital_bundle    <- result_bundles[["09_hospital_comparisons"]]
outcome_summary    <- hospital_bundle$qa_hospital_outcomes

deaths_inhospital <- outcome_summary$deaths[outcome_summary$outcome == "In-hospital mortality"]
deaths_90d <- outcome_summary$deaths[outcome_summary$outcome == "90-day post-admission mortality"]
cohort_size <- cohort_bundle$cohort_size
cohort_size_in_outcome_summary <- unique(outcome_summary$cohort_size)

if (length(cohort_size_in_outcome_summary) != 1L || cohort_size != cohort_size_in_outcome_summary) {
  stop("Cohort size differs between the cohort and outcome summaries.")}

deaths_90d_descriptive <- descriptive_bundle$mortality_burden |>
  dplyr::filter(anchor == "Post-admission",  followup_days == 90) |>
  dplyr::pull(deaths)

if (length(deaths_90d_descriptive) != 1L ||
    deaths_90d != deaths_90d_descriptive) {
  stop("The 90-day death count differs between the hospital outcome summary ",
       "and descriptive mortality summary.")}

hospital_cases <- hospital_bundle$qa_hospital_aggregation |>
  dplyr::filter(
    outcome == "90-day post-admission mortality",
    model == "Full model"
  ) |>
  dplyr::pull(n_cases)

strobe_flow_numbers <- cohort_bundle$flow_log_by_year |>
  dplyr::select(
    keep_criteria_label,
    exclude_criteria_label,
    remaining_Total,
    excluded_Total
  )

death_count_qa <- outcome_bundle$qa_objects$qa_death_count_comparison

deaths_bevnat_only_excluded <- sum(
  death_count_qa$n_bevnat_only_records_excluded,
  na.rm = TRUE
)

hospital_classification_counts <- hospital_bundle$hospital_reclassification |>
  dplyr::select(
    hospital_site_id_num,
    category_inhospital,
    category_90d
  ) |>
  tidyr::pivot_longer(
    cols = c(category_inhospital, category_90d),
    names_to = "endpoint",
    values_to = "classification"
  ) |>
  dplyr::mutate(
    endpoint = dplyr::recode(
      endpoint,
      category_inhospital = "In-hospital mortality",
      category_90d = "90-day post-admission mortality"
    )
  ) |>
  dplyr::count(endpoint, classification, name = "n_sites")

paper_numbers <- list(
  cohort_size = as.integer(cohort_size),
  
  # Counts from the updated STROBE flow
  strobe_flow = strobe_flow_numbers,
  
  # cohort characteristics
  cohort_characteristics =covariate_bundle$qa_objects$qa_cohort_characteristics,
  
  # inhospital_death_coverage
  inhospital_death_coverage = descriptive_bundle$inhospital_death_coverage,
  
  # Updated death definition
  deaths_bevnat_only_excluded = as.integer(deaths_bevnat_only_excluded),
  deaths_inhospital = as.integer(deaths_inhospital),
  mortality_inhospital_pct =round(100 * deaths_inhospital / cohort_size, 1),
  deaths_90d = as.integer(deaths_90d),
  mortality_90d_pct = round(100 * deaths_90d / cohort_size, 1),
  
  # model performance & hospital classification numbers
  model_performance = model_bundle$model_performance_table,
  funnel_overdispersion = hospital_bundle$qa_funnel_overdispersion,
  hospital_classification_counts = hospital_classification_counts,
  reclassification_summary = hospital_bundle$summary_reclassification,
  reclassification_directions = hospital_bundle$reclassification_directions,
  reclassification_matrix =  hospital_bundle$table_reclassification_long,
  hospital_profiles_by_direction = hospital_bundle$hospital_profile_by_direction,
  
  # sensitivity analysis (reclassifications when moving from inhospital to 90d)
  reclassification_by_model =  hospital_bundle$summary_reclassification_models,
  reclassification_poisson =  hospital_bundle$summary_reclassification_poisson,
  
  # sensitivity analysis (reclassifications when comparing risk adjustment models)
  reclassification_at_90d =  hospital_bundle$summary_risk_adjustment_90d |>
    dplyr::mutate(pct_changed = round(pct_changed, 1)),
  control_limits_at_90d =hospital_bundle$summary_control_limits_90d,
  
  # Other Results figures
  mortality_burden = descriptive_bundle$mortality_burden,
  hospital_sites = nrow(hospital_bundle$hospital_sites_included),
  hospital_cases = as.integer(hospital_cases),
  sites_reclassified_endpoints = hospital_bundle$summary_reclassification$changed_category[[1L]],
  pct_sites_reclassified_endpoints = hospital_bundle$summary_reclassification$pct_changed_category[[1L]]
)

paper_numbers # -------> check manually
print(paper_numbers$cohort_characteristics, n=Inf)
print(paper_numbers$inhospital_death_coverage, n=Inf, width=Inf)

# ******************************************************************************
# save all bundels + paper_numbers ----
# ******************************************************************************
paper_objects <- list(
  metadata = list(
    analysis_dataset_name = analysis_dataset_name,
    indicator_id = indicator_id,
    indicator_name = indicator_name,
    index_start_year = index_start_year,
    index_end_year = index_end_year,
    datestamp = datestamp
  ),
  paper_numbers = paper_numbers,
  result_bundles = result_bundles
)

rds_object_filepath <- file.path(
  path_indicator_results,
  paste0("paper_objects_", analysis_dataset_name, ".rds")
)
saveRDS(
  paper_objects,
  file = rds_object_filepath
) 

cli::cli_alert_success(
  "Paper objects saved: {basename(rds_object_filepath)}"
)

