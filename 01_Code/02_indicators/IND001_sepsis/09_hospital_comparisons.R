# ******************************************************************************
# 09_hospital_comparisons.R 
# ******************************************************************************
# Purpose:
# - Compare in-hospital with 90-day post-admission mortality across sites.
# - Assess sensitivity to risk-adjustment and funnel-limit specifications.
# - Create the hospital-comparison tables and figures used in the paper
#   and Supplementary Sections C and H–K.
#
# Input:
# - analysis_dataset from the preceding cohort and model scripts. It must
#   contain the outcomes, expected risks, site identifiers and profile fields
#   listed below.
#
# Output:
# - A dated RDS bundle of hospital-comparison paper objects in path_output.
# ******************************************************************************

# ******************************************************************************
# Script-specific dependencies and required inputs ----
# ******************************************************************************

# Check packages
required_packages <- c("FunnelPlotR", "ggalluvial", "patchwork")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0L) {
  stop( "Required package(s) not installed: ",  paste(missing_packages, collapse = ", "))}

# Ckeck analysis dataset
if (!exists("analysis_dataset", inherits = TRUE)) {
  stop("Object 'analysis_dataset' is missing. Run the cohort and model scripts first.")}

# model prediction map
model_prediction_map <- tibble::tribble(
  ~outcome,                          ~model,                     ~observed_var,            ~expected_var,
  "In-hospital mortality",           "Raw",                      "death_in_hospital",       "expected_raw_inhospital",
  "In-hospital mortality",           "Age and sex",              "death_in_hospital",       "expected_age_sex_inhospital",
  "In-hospital mortality",           "Full model",               "death_in_hospital",       "expected_full_inhospital",
  "In-hospital mortality",           "Full model + interaction", "death_in_hospital",       "expected_full2_inhospital",
  "90-day post-admission mortality", "Raw",                      "death_90d_postadmission", "expected_raw_90d",
  "90-day post-admission mortality", "Age and sex",              "death_90d_postadmission", "expected_age_sex_90d",
  "90-day post-admission mortality", "Full model",               "death_90d_postadmission", "expected_full_90d",
  "90-day post-admission mortality", "Full model + interaction", "death_90d_postadmission", "expected_full2_90d"
)

# ckeck variables
required_columns <- c(
  "survey_year", "hospital_site_id_num", "hospital_id_num",
  "episode_length", "post_discharge_location",
  model_prediction_map$observed_var,
  model_prediction_map$expected_var) |>
  unique()

missing_columns <- setdiff(required_columns, names(analysis_dataset))
if (length(missing_columns) > 0L) {
  stop( "analysis_dataset is missing required column(s): ", paste(missing_columns, collapse = ", "))}



outcome_columns <- unique(model_prediction_map$observed_var) # outcome variables
invalid_outcomes <- vapply(
  analysis_dataset[outcome_columns],                         # choose outcome variables in analysis dataset?
  \(x) anyNA(x) || any(!x %in% 0:1, na.rm = TRUE),           # only values 0 or 1?
  logical(1)
)

if (any(invalid_outcomes)) {
  stop("Hospital-comparison outcomes must be complete binary variables coded 0/1.")}

expected_columns <- unique(model_prediction_map$expected_var) # variables for expected values
invalid_predictions <- vapply(
  analysis_dataset[expected_columns],                         # choose variables in analysis dataset?
  \(x) any(!is.finite(x) | x < 0 | x > 1),                    # only values between 0 and 1?
  logical(1)
)

if (any(invalid_predictions)) {
  stop("Expected risks must be finite probabilities between 0 and 1.")}


# ******************************************************************************
# Outcome and volume summaries: Supplementary Table C1 ----
# ******************************************************************************

qa_hospital_outcomes <- tibble::tibble(
  outcome = c(
    "In-hospital mortality",
    "90-day post-admission mortality"
  ),
  deaths = c(
    sum(analysis_dataset$death_in_hospital),
    sum(analysis_dataset$death_90d_postadmission)
  ),
  cohort_size = nrow(analysis_dataset)
) |>
  dplyr::mutate(
    mortality = deaths / cohort_size,
    mortality_pct = round(100 * mortality, 1)
  )

# check hospital sites (wenn tabelle leer = innerhalb keines einzelnen Jahres 
# ist ein hospital_site_id_num mehreren hospital_id_num zugeordnet.)
qa_site_to_hospital <- analysis_dataset |>
  dplyr::distinct(survey_year, hospital_site_id_num, hospital_id_num) |>
  dplyr::summarise(
    n_hospitals = dplyr::n_distinct(hospital_id_num),
    hospitals = paste(sort(unique(hospital_id_num)), collapse = ", "),
    .by = c(survey_year, hospital_site_id_num)
  ) |>
  dplyr::filter(n_hospitals > 1L)

# Number of sepsis hospitalisations by hospital site
hospital_site_volume <- analysis_dataset |>
  dplyr::count(survey_year, hospital_site_id_num, name = "n_sepsis_cases") |>
  dplyr::mutate(
    volume_group = dplyr::case_when(
      n_sepsis_cases < 5L   ~ "<5",
      n_sepsis_cases < 10L  ~ "5–9",
      n_sepsis_cases < 100L ~ "10–99",
      n_sepsis_cases < 500L ~ "100–499",
      TRUE                  ~ "≥500"
    ),
    volume_group = factor(
      volume_group,
      levels = c("<5", "5–9", "10–99", "100–499", "≥500"),
      ordered = TRUE
    )
  )

hospital_site_volume_summary <- hospital_site_volume |>
  dplyr::summarise(
    n_sites = dplyr::n(),
    median_cases = median(n_sepsis_cases),
    p25 = stats::quantile(n_sepsis_cases, 0.25),
    p75 = stats::quantile(n_sepsis_cases, 0.75),
    min = min(n_sepsis_cases),
    max = max(n_sepsis_cases),
    .by = survey_year
  )

site_volume_by_year <- hospital_site_volume |>
  dplyr::count(survey_year, volume_group, name = "n_hospital_sites") |>
  tidyr::pivot_wider(
    names_from = volume_group,
    values_from = n_hospital_sites,
    values_fill = 0
  )

# pooling over Years
hospital_site_volume_pooled <- analysis_dataset |>
  dplyr::count(hospital_site_id_num, name = "n_sepsis_cases") |>
  dplyr::mutate(
    volume_group = dplyr::case_when(
      n_sepsis_cases < 5L   ~ "<5",
      n_sepsis_cases < 10L  ~ "5–9",
      n_sepsis_cases < 100L ~ "10–99",
      n_sepsis_cases < 500L ~ "100–499",
      TRUE                  ~ "≥500"
    ),
    volume_group = factor(
      volume_group,
      levels = c("<5", "5–9", "10–99", "100–499", "≥500"),
      ordered = TRUE
    )
  )

site_volume_pooled <- hospital_site_volume_pooled |>
  dplyr::count(volume_group, name = "n_hospital_sites") |>
  tidyr::pivot_wider(
    names_from = volume_group,
    values_from = n_hospital_sites,
    values_fill = 0
  )

# Table by thresholds for supplements
thresholds <- sort(unique(c(
  0L, 10L, 20L, 30L, 50L, 75L, 100L, 150L, 200L,
  minimum_site_volume # make sure that this value is part of the list
)))

hospital_thresholds_table <- purrr::map_dfr(thresholds, function(threshold) {
  included <- hospital_site_volume_pooled |>
    dplyr::filter(n_sepsis_cases >= threshold)
  
  tibble::tibble(
    threshold = threshold,
    hospital_sites = nrow(included),
    pct_hospital_sites = 100 * nrow(included) / nrow(hospital_site_volume_pooled),
    sepsis_cases = sum(included$n_sepsis_cases),
    pct_sepsis_cases = 100 * sum(included$n_sepsis_cases) /
      sum(hospital_site_volume_pooled$n_sepsis_cases)
  )
})

selected_threshold_summary <- hospital_thresholds_table |>
  dplyr::filter(threshold == minimum_site_volume)

# IDs der sites inluded
hospital_sites_included <- hospital_site_volume_pooled |>
  dplyr::filter(n_sepsis_cases >= minimum_site_volume) |>
  dplyr::select(hospital_site_id_num)


# ******************************************************************************
# Hospital aggregation: using full cohort as reference population  ----
# ******************************************************************************

# O: beobachtete Todesfälle (observed_deaths)
# E: Summe der individuellen vorhergesagten Todesrisiken (expected_deaths)
# O/E: beobachtete geteilt durch erwartete Todesfälle
# adjusted rate: O/E multipliziert mit der Mortalität der gesamten Kohorte

# helper function to aggregate values by hospital site
aggregate_hospital <- function(data, observed_var, expected_var) {
  # The reference rate is the mortality rate in the full analysis cohort.
  # The minimum-volume threshold is applied after aggregation. This preserves
  # the original indirect-standardisation reference for every eligible site.
  overall_rate <- mean(data[[observed_var]])
  
  data |>
    dplyr::summarise(
      n_cases = dplyr::n(),
      observed_deaths = sum(.data[[observed_var]]),
      expected_deaths = sum(.data[[expected_var]]),
      observed_rate = observed_deaths / n_cases,
      expected_rate = expected_deaths / n_cases,
      oe_ratio = observed_deaths / expected_deaths,
      adjusted_rate = oe_ratio * overall_rate,
      .by = hospital_site_id_num
    )
}

# Table with all hospital results
# Dieser Code erstellt eine gemeinsame Tabelle mit den Spitalergebnissen für alle acht Kombinationen aus Endpunkt und Modell
# - purrr::pmap_dfr() geht die Zeilen von model_prediction_map der Reihe nach durch. Für jede Zeile ruft es die Funktion mit 
#   den vier Werten outcome, model, observed_var und expected_var auf.
# - aggregate_hospital() berechnet anhand der angegebenen beobachteten und erwarteten Variablen die Kennzahlen für jeden 
#   Spitalstandort.
# - mutate() ergänzt den Ergebnissen den Endpunkt- und Modellnamen. .before = 1 setzt diese beiden Spalten an den Tabellenanfang.
# - Das _dfr in pmap_dfr() bedeutet: Die acht Ergebnis-Tabellen werden zeilenweise zu einer Tabelle zusammengefügt.

hospital_summaries_all <- purrr::pmap_dfr(
  model_prediction_map,
  function(outcome, model, observed_var, expected_var) {
    aggregate_hospital(
      data = analysis_dataset,
      observed_var = observed_var,
      expected_var = expected_var
    ) |>
      dplyr::mutate(outcome = outcome, model = model, .before = 1)
  }
)

# add number of cases per hospital information
hospital_summaries <- hospital_summaries_all |>
  dplyr::left_join(hospital_site_volume_pooled, by = "hospital_site_id_num")

# filter for hospitals with number of cases above threshold
hospital_summaries_analysis <- hospital_summaries |>
  dplyr::semi_join(hospital_sites_included, by = "hospital_site_id_num")


# checking hospital aggregation after applying the minimum site-volume threshold.
qa_hospital_aggregation <- hospital_summaries_analysis |>
  dplyr::summarise(
    n_sites = dplyr::n(),
    n_cases = sum(n_cases),
    pct_cases = 100 * n_cases / nrow(analysis_dataset),
    observed_deaths = sum(observed_deaths),
    expected_deaths = sum(expected_deaths),
    oe_ratio = observed_deaths / expected_deaths,
    .by = c(outcome, model)
  ) |>
  dplyr::arrange(outcome, model)

# - 116 sites and 65,502 cases are included for each model. That is 98.16% of the 
#   full cohort of 66,732; the remaining 1,230 cases were treated at sites below 
#   the threshold.
# - observed_deaths is the number of deaths among those 65,502 cases. It is the 
#   same across models, as expected: the observed outcome does not change when 
#   the risk-adjustment model changes.
# - expected_deaths is the sum of the model’s predicted risks for those cases.
# - oe_ratio is observed deaths divided by expected deaths. Values near 1 mean 
#   the combined observed and expected totals across these sites are very 
#   similar.


# ******************************************************************************
# Full versus interaction model: check whether hospital estimates change ----
# ******************************************************************************
hospital_compare_interaction <- hospital_summaries_analysis |>
  dplyr::filter(model %in% c("Full model", "Full model + interaction")) |>
  dplyr::select(outcome, hospital_site_id_num,  model,  adjusted_rate,  expected_rate ) |>
  tidyr::pivot_wider(
    id_cols = c(outcome, hospital_site_id_num),
    names_from = model,
    values_from = c(adjusted_rate, expected_rate),
    names_sep = "_"
  ) |>
  dplyr::mutate(
    # Difference is interaction model minus full model, in percentage points.
    adjusted_difference_pp = 100 * (`adjusted_rate_Full model + interaction` - `adjusted_rate_Full model` ),
    expected_difference_pp = 100 * (`expected_rate_Full model + interaction` - `expected_rate_Full model` ),
    adjusted_abs_difference_pp = abs(adjusted_difference_pp),
    expected_abs_difference_pp = abs(expected_difference_pp)
    )

qa_model_comparison <- hospital_compare_interaction |>
  dplyr::summarise(
    n_sites = dplyr::n(),
    correlation_adjusted = stats::cor(
      `adjusted_rate_Full model`,
      `adjusted_rate_Full model + interaction`
    ),
    median_abs_adjusted_difference_pp = stats::median(adjusted_abs_difference_pp),
    max_abs_adjusted_difference_pp = max(adjusted_abs_difference_pp),
    median_abs_expected_difference_pp = stats::median(expected_abs_difference_pp),
    max_abs_expected_difference_pp = max(expected_abs_difference_pp),
    .by = outcome
  )

# Die Korrelation von 0,996 zeigt, dass die Standortwerte über beide Modelle 
# hinweg stark zusammenhängen. Sie bedeutet aber nicht, dass sich die Werte für 
# jeden Standort kaum ändern. Der Median liegt bei rund 0,14 Prozentpunkten; die 
# grösste Änderung beträgt dagegen 3,43 bzw. 3,82 Prozentpunkte.


# Rank sites by the absolute change in adjusted rate and retain volume context.
qa_hospital_model_comparison_ranked <- hospital_compare_interaction |>
  dplyr::left_join(
    hospital_site_volume_pooled |>
      dplyr::select(hospital_site_id_num, n_sepsis_cases, volume_group),
    by = "hospital_site_id_num"
  ) |>
  dplyr::arrange(outcome, dplyr::desc(adjusted_abs_difference_pp))

# Beim ersten Standort in der sortierten 90-Tage-Tabelle (ID 345) sinkt die 
# adjustierte Rate beispielsweise von 46,0 % auf 42,2 %. Das sind −3,8 Prozentpunkte.
# Die erwartete Rate steigt gleichzeitig von 8,26 % auf 9,01 %. Da die beobachtete 
# Zahl der Todesfälle gleich bleibt, führt die höhere erwartete Zahl zu einem 
# niedrigeren O/E-Verhältnis und damit zu einer niedrigeren adjustierten Rate.



# ******************************************************************************
# Funnel plots: original 95% / 99.8% display and adjusted 95% classification ----
# ******************************************************************************
# Prep data for funnel plot
funnel_inhospital_data <- hospital_summaries_analysis |>
  dplyr::filter(
    outcome == "In-hospital mortality",
    model == "Full model"
  ) |>
  dplyr::transmute(
    hospital_site_id_num = as.character(hospital_site_id_num),
    observed_deaths,
    expected_deaths
  )

funnel_90d_data <- hospital_summaries_analysis |>
  dplyr::filter(
    outcome == "90-day post-admission mortality",
    model == "Full model"
  ) |>
  dplyr::transmute(
    hospital_site_id_num = as.character(hospital_site_id_num),
    observed_deaths,
    expected_deaths
  )
# Funnel plots: 95% limits and OD-adjusted classification ----
fp_inhospital_adj <- FunnelPlotR::funnel_plot(
  .data = funnel_inhospital_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE,
  multiplier = 100
)

fp_90d_adj <- FunnelPlotR::funnel_plot(
  .data = funnel_90d_data,
  numerator = observed_deaths,
  denominator = expected_deaths,
  group = hospital_site_id_num,
  data_type = "SR",
  limit = 95,
  label = NA,
  draw_unadjusted = TRUE,
  draw_adjusted = TRUE,
  multiplier = 100
)



# Check for overdispersion  
qa_funnel_overdispersion <- tibble::tibble(
  outcome = c(
    "In-hospital mortality",
    "90-day post-admission mortality"
  ),
  phi = c(
    FunnelPlotR::phi(fp_inhospital_adj),
    FunnelPlotR::phi(fp_90d_adj)
  ),
  tau2 = c(
    FunnelPlotR::tau2(fp_inhospital_adj),
    FunnelPlotR::tau2(fp_90d_adj)
  ),
  n_outliers = c(
    nrow(FunnelPlotR::outliers(fp_inhospital_adj)),
    nrow(FunnelPlotR::outliers(fp_90d_adj))
  )
)

funnel_outliers_inhospital <- FunnelPlotR::outliers(fp_inhospital_adj) |>
  tibble::as_tibble()

funnel_outliers_90d <- FunnelPlotR::outliers(fp_90d_adj) |>
  tibble::as_tibble()

# phi und tau2 beschreiben die Streuung zwischen den Spitälern; n_outliers zählt 
# die von FunnelPlotR als Ausreisser markierten Sites. limit = 95 legt dabei 
# die 95%-Grenze fest
# Bei der In-hospital-Mortalität zeigt sich zusätzliche Variation zwischen den 
# Spitälern, weshalb die OD-korrigierten Kontrollgrenzen weiter ausfallen. Bei 
# der 90-Tage-Mortalität wurde keine zusätzliche Überstreuung geschätzt; die 
# OD-korrigierten Grenzen entsprechen daher praktisch den Poisson-Grenzen.



# Customize funnelplots
funnel_data_inhospital <- FunnelPlotR::source_data(fp_inhospital_adj) |>
  tibble::as_tibble()

p_funnel_inhospital_adj <- ggplot2::ggplot(
  funnel_data_inhospital, ggplot2::aes(x = denominator, y = rr * 100)) +
  ggplot2::geom_point(size = 1.7, alpha = 0.75) +
  ggplot2::geom_line(ggplot2::aes(y = LCL95, colour = "Conventional Poisson"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = UCL95, colour = "Conventional Poisson"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = OD95LCL, colour = "Adjusted for overdispersion"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = OD95UCL, colour = "Adjusted for overdispersion"), linewidth = 0.7) +
  ggplot2::geom_hline(yintercept = 100, linewidth = 0.4, colour = "grey40") +  
  ggplot2::scale_colour_manual(name = "95% control limits", 
                              values = c("Conventional Poisson" = "#005095", "Adjusted for overdispersion" = "#ED6E4E")) +
  ggplot2::labs(
    title = "In-hospital mortality after sepsis hospitalisation",
    x = "Expected deaths",
    y = "Observed-to-expected mortality ratio (%)"
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 250)) +
  ggplot2::theme_bw()

funnel_data_90d <- FunnelPlotR::source_data(fp_90d_adj) |>
  tibble::as_tibble()

p_funnel_90d_adj <- ggplot2::ggplot(funnel_data_90d, ggplot2::aes(x = denominator, y = rr * 100)) +
  ggplot2::geom_point(size = 1.7, alpha = 0.75) +
  ggplot2::geom_line(ggplot2::aes(y = LCL95, colour = "Conventional Poisson"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = UCL95, colour = "Conventional Poisson"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = OD95LCL, colour = "Adjusted for overdispersion"), linewidth = 0.7) +
  ggplot2::geom_line(ggplot2::aes(y = OD95UCL, colour = "Adjusted for overdispersion"), linewidth = 0.7) +
  ggplot2::geom_hline(yintercept = 100, linewidth = 0.4, colour = "grey40") +
  ggplot2::scale_colour_manual(name = "95% control limits", 
                               values = c("Conventional Poisson" = "#005095","Adjusted for overdispersion" = "#ED6E4E")) +
  ggplot2::labs(
    title = "90-day post-admission mortality after sepsis hospitalisation",
    x = "Expected deaths",
    y = "Observed-to-expected mortality ratio (%)"
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 250)) +
  ggplot2::theme_bw()


classify_funnel <- function(funnel_object, lower_limit, upper_limit) {
  FunnelPlotR::source_data(funnel_object) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      hospital_site_id_num = as.integer(as.character(group)),
      oe_ratio = rr,
      category = dplyr::case_when(
        rr * 100 < .data[[lower_limit]] ~ "Lower than expected",
        rr * 100 > .data[[upper_limit]] ~ "Higher than expected",
        TRUE ~ "Within control limits"
      )
    )
}

# classification tables
category_levels <- c(
  "Lower than expected",
  "Within control limits",
  "Higher than expected"
)

funnel_class_inhospital <- classify_funnel(
  fp_inhospital_adj, "OD95LCL", "OD95UCL"
) |>
  dplyr::rename(
    oe_ratio_inhospital = oe_ratio,
    category_inhospital = category
  )

funnel_class_90d <- classify_funnel(
  fp_90d_adj, "OD95LCL", "OD95UCL"
) |>
  dplyr::rename(
    oe_ratio_90d = oe_ratio,
    category_90d = category
  )

# Analyze hospital REclassification when moving from inhospital to 90d-postadmission
hospital_reclassification <- funnel_class_inhospital |>
  dplyr::inner_join(funnel_class_90d, by = "hospital_site_id_num") |>
  dplyr::mutate(
    category_inhospital = factor(category_inhospital, levels = category_levels),
    category_90d = factor(category_90d, levels = category_levels),
    changed_category = category_inhospital != category_90d
  ) |>
  dplyr::left_join(hospital_site_volume_pooled, by = "hospital_site_id_num")

# hospital reclassification table
table_reclassification <- hospital_reclassification |>
  dplyr::count(category_inhospital, category_90d, name = "n_hospitals") |>
  tidyr::complete(
    category_inhospital = factor(category_levels, levels = category_levels),
    category_90d = factor(category_levels, levels = category_levels),
    fill = list(n_hospitals = 0L)
  )

# hospital reclassification table wide
table_reclassification_wide <- table_reclassification |>
  tidyr::pivot_wider(
    names_from = category_90d,
    values_from = n_hospitals,
    values_fill = 0L
  )

# summary reclassification table wide
summary_reclassification <- hospital_reclassification |>
  dplyr::summarise(
    n_hospitals = dplyr::n(),
    same_category = sum(!changed_category),
    changed_category = sum(changed_category),
    pct_changed_category = round(100 * changed_category / n_hospitals, 1)
  )

# reclassification directions
reclassification_directions <- hospital_reclassification |>
  dplyr::filter(changed_category) |>
  dplyr::count(category_inhospital, category_90d, name = "n_hospitals") |>
  dplyr::arrange(dplyr::desc(n_hospitals))


# ******************************************************************************
# Supplementary Table I1: endpoint reclassification by risk model ----
# ******************************************************************************

make_adjusted_funnel <- function(hospital_data) {
  # Daten für Funnel rausziehen
  funnel_data <- hospital_data |>
    # Nur Standorte ab dem festgelegten Mindestvolumen behalten
    dplyr::semi_join(hospital_sites_included, by = "hospital_site_id_num") |>
    # Nur die für den Funnel-Plot benötigten Variablen weitergeben
    dplyr::transmute(hospital_site_id_num = as.character(hospital_site_id_num), 
                     observed_deaths, expected_deaths)
  
  # Funnel plot inklusive Definition der Outlier
  FunnelPlotR::funnel_plot(
    .data = funnel_data,
    numerator = observed_deaths,                       # Beobachtete Todesfälle
    denominator = expected_deaths,                     # Erwartete Todesfälle
    group = hospital_site_id_num,                      # Jeder Standort bildet eine Gruppe
    data_type = "SR",                                  # O/E-Verhältnis: standardisierte Rate
    limit = 95,                                        # 95%-Kontrollgrenzen verwenden
    label = NA,                                        # Keine Standorte im Plot beschriften
    draw_unadjusted = FALSE,                           # Keine Poisson-Grenzen berechnen
    draw_adjusted = TRUE,                              # OD-korrigierte Grenzen berechnen
    multiplier = 100                                   # Verhältnis und Grenzen auf Prozent skalieren
  )
}


calculate_reclassification <- function(hospital_inhospital, hospital_90d, model_name) {
  # FunnelPlotR-Objekt mit OD-korrigierten 95%-Grenzen für die Klassifikation
  funnel_inhospital <- make_adjusted_funnel(hospital_inhospital)  
  funnel_90d <- make_adjusted_funnel(hospital_90d)                
  
  # Klassifikation für beide Endpunkte berechnen: inhospital
  class_inhospital <- classify_funnel(funnel_inhospital,"OD95LCL","OD95UCL") |>
    dplyr::select(hospital_site_id_num, category_inhospital = category)
  class_90d <- classify_funnel(funnel_90d, "OD95LCL", "OD95UCL") |>
    dplyr::select(hospital_site_id_num, category_90d = category)
  
  # Join classifications and define REclassifications  
  class_inhospital |>
    dplyr::inner_join(class_90d,  by = "hospital_site_id_num") |>
    dplyr::mutate(
      model = model_name,   # Modellname für spätere Tabellen ergänzen
      category_inhospital = factor(category_inhospital, levels = category_levels),
      category_90d = factor(category_90d, levels = category_levels),
      changed_category = category_inhospital != category_90d
    )
}


get_hospital_model <- function(outcome, model) {
  hospital_summaries |>
    dplyr::filter(
      .data$outcome == .env$outcome,
      .data$model == .env$model
    ) |>
    dplyr::select(
      hospital_site_id_num,
      observed_deaths,
      expected_deaths
    )
}

# Anwendung der Funktionen
# - get_hospital_model() wählt Outcome und Modell aus hospital_summaries aus
# - make_adjusted_funnel() berechnet die OD-korrigierten 95%-Grenzen
# - calculate_reclassification() vergleicht die Standortklassifikation
#   zwischen In-hospital- und 90-Tage-Mortalität innerhalb eines Modells


# Define Reclassifications by model
hospital_reclassification_models <- dplyr::bind_rows(
  calculate_reclassification(
    get_hospital_model("In-hospital mortality", "Raw"),
    get_hospital_model("90-day post-admission mortality", "Raw"), 
    "Raw"
  ),
  calculate_reclassification(
    get_hospital_model("In-hospital mortality", "Age and sex"),
    get_hospital_model("90-day post-admission mortality", "Age and sex"), 
    "Age and sex"
  ),
  calculate_reclassification(
    get_hospital_model("In-hospital mortality", "Full model"),
    get_hospital_model("90-day post-admission mortality", "Full model"), 
    "Full model"
  )
)

# Summary of Reclassifications by model
summary_reclassification_models <- hospital_reclassification_models |>
  dplyr::summarise(
    n_hospitals       = dplyr::n(),
    same_category     = sum(!changed_category),
    pct_same_category = 100 * same_category / n_hospitals,
    reclassified      = sum(changed_category),
    pct_reclassified  = 100 * reclassified / n_hospitals,
    .by               = model
  ) |>
  dplyr::mutate(model = factor(model, levels = c("Raw", "Age and sex", "Full model"))) |>
  dplyr::arrange(model)

# Table of Reclassifications 
table_reclassification_models <- hospital_reclassification_models |>
  dplyr::count(model, category_inhospital, category_90d, name = "n_hospitals") |>
  tidyr::complete(
    model,
    category_inhospital = factor(category_levels, levels = category_levels),
    category_90d = factor(category_levels, levels = category_levels),
    fill = list(n_hospitals = 0L)
  ) |>
  tidyr::pivot_wider(
    names_from = category_90d,
    values_from = n_hospitals,
    values_fill = 0L
  )

# Which hospitals are reclassified between inhospital and 90d under which ajustment model
reclassification_identity <- hospital_reclassification_models |>
  dplyr::select(hospital_site_id_num, model, changed_category) |>
  tidyr::pivot_wider(names_from = model, values_from = changed_category)


# TRUE means the site changed category between mortality endpoints
reclassified_raw     <- reclassification_identity$Raw
reclassified_age_sex <- reclassification_identity$`Age and sex`
reclassified_full    <- reclassification_identity$`Full model`

# summary table to show overlap of reclassifications
overlap_reclassification_models <- tibble::tibble(
  comparison = c(
    "Raw vs Age and sex",
    "Raw vs Full model",
    "Age and sex vs Full model"
  ),
  reclassified_model_1 = c(
    sum(reclassified_raw),
    sum(reclassified_raw),
    sum(reclassified_age_sex)
  ),
  reclassified_model_2 = c(
    sum(reclassified_age_sex),
    sum(reclassified_full),
    sum(reclassified_full)
  ),
  reclassified_in_both = c(
    sum(reclassified_raw & reclassified_age_sex),
    sum(reclassified_raw & reclassified_full),
    sum(reclassified_age_sex & reclassified_full)
  ),
  model_1_only = c(
    sum(reclassified_raw & !reclassified_age_sex),
    sum(reclassified_raw & !reclassified_full),
    sum(reclassified_age_sex & !reclassified_full)
  ),
  model_2_only = c(
    sum(!reclassified_raw & reclassified_age_sex),
    sum(!reclassified_raw & reclassified_full),
    sum(!reclassified_age_sex & reclassified_full)
  )
)


# ******************************************************************************
# Supplementary Table J1 and Figure 3: risk adjustment at fixed 90-day endpoint ----
# ******************************************************************************
# chose relevant classification and transpose table into wide format
classification_90d_models <- hospital_reclassification_models |>
  dplyr::select(hospital_site_id_num, model, category_90d) |>
  tidyr::pivot_wider(names_from = model, values_from = category_90d)

# create summary table to show reclassification under different adjustments
summary_risk_adjustment_90d <- tibble::tibble(
  comparison = c("Raw vs Full model", "Age and sex vs Full model"),
  n_sites = nrow(classification_90d_models),
  n_same = c(
    sum(classification_90d_models$Raw == classification_90d_models$`Full model`),
    sum(classification_90d_models$`Age and sex` == classification_90d_models$`Full model`)
  ),
  n_changed = c(
    sum(classification_90d_models$Raw != classification_90d_models$`Full model`),
    sum(classification_90d_models$`Age and sex` != classification_90d_models$`Full model`)
  )
) |>
  dplyr::mutate(pct_changed = 100 * n_changed / n_sites)

# shorten labels for plot later on
classification_90d_models <- classification_90d_models |>
  dplyr::mutate(
    dplyr::across(
      c(Raw, `Age and sex`, `Full model`),
      ~ factor(.x, levels = category_levels,labels = c("Lower", "Within limits", "Higher"))
    )
  )

# define colors
classification_colors <- c(
  "Lower" = "#2c7bb6",
  "Within limits" = "#D9D9D9",
  "Higher" = "#d7191c"
)
# function make_alluvial to create alluvial plot
make_alluvial <- function(data, first_model, first_label, plot_title, n_changed) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      axis1 = .data[[first_model]],
      axis2 = .data[["Full model"]],
      y = 1
    )
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = .data[[first_model]]),
      width = 0.12,
      alpha = 0.60,
      knot.pos = 0.4
    ) +
    ggalluvial::geom_stratum(
      ggplot2::aes(fill = ggplot2::after_stat(stratum)),
      width = 0.16,
      colour = NA
    ) +
    ggalluvial::stat_stratum(
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      geom = "text",
      size = 3.2,
      fontface = "bold"
    ) +
    ggplot2::scale_x_discrete(
      limits = c(first_model, "Full model"),
      labels = c(first_label, "Fully adjusted"),
      expand = c(0.12, 0.12)
    ) +
    ggplot2::scale_fill_manual(values = classification_colors, drop = FALSE) +
    ggplot2::labs(
      title = plot_title,
      subtitle = paste0(
        n_changed, " of ", nrow(data), " hospital sites changed classification (",
        round(100 * n_changed / nrow(data), 1), "%)"
      ),
      x = NULL,
      y = "Hospital sites",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
}

# key numbers to use in plot
n_changed_raw_full <- summary_risk_adjustment_90d$n_changed[[1L]]
n_changed_age_sex_full <- summary_risk_adjustment_90d$n_changed[[2L]]

# alluvial plot raw vs. full
p_alluvial_raw_full <- make_alluvial(
  data        = classification_90d_models, 
  first_model = "Raw", 
  first_label = "Raw", 
  plot_title  = "A. Raw vs fully adjusted", 
  n_changed   =  n_changed_raw_full)

# alluvial plot age_sex vs. full
p_alluvial_age_sex_full <- make_alluvial(
  data        = classification_90d_models, 
  first_model = "Age and sex", 
  first_label = "Age and sex", 
  plot_title  = "B. Age- and sex-adjusted vs fully adjusted", 
  n_changed   =  n_changed_age_sex_full)

# patchwork alluvial plots togehther
p_alluvial_risk_adjustment <- patchwork::wrap_plots(
  p_alluvial_raw_full, 
  p_alluvial_age_sex_full, 
  nrow = 1
  ) + 
  patchwork::plot_annotation(
  title = "Hospital classification under alternative risk-adjustment specifications",
  caption = paste(
    "Classifications use fully adjusted-model funnel plots with overdispersion-adjusted",
    "95% control limits. Numbers in subtitles show sites whose classification changed."
  )
)


# ******************************************************************************
# Supplementary Table H1: descriptive profiles by direction of reclassification
# ******************************************************************************
hospital_profile <- analysis_dataset |>
  dplyr::semi_join(hospital_sites_included, by = "hospital_site_id_num") |>
  dplyr::mutate(
    # Todesfall innerhalb von 90 Tagen nach Aufnahme, aber nicht im Indexspital
    death_90d_after_discharge = death_90d_postadmission == 1L & death_in_hospital == 0L,
    # Codes 6 und 66: anderes Akutspital bzw. Akutabteilung desselben Betriebs
    acute_care_transfer = post_discharge_location %in% c(6L, 66L)
  ) |>
  dplyr::summarise(
    n_cases = dplyr::n(),
    median_episode_length = stats::median(episode_length, na.rm = TRUE),
    # Anteil der 90-Tage-Todesfälle, die nach dem Austritt eintraten
    postdischarge_share_of_90d_deaths_pct = dplyr::if_else(
      sum(death_90d_postadmission) > 0,
      100 * sum(death_90d_after_discharge) / sum(death_90d_postadmission),
      NA_real_
    ),
    # Anteil aller Indexaufnahmen mit Austrittscode 6 oder 66
    acute_care_transfer_pct = 100 * mean(acute_care_transfer),
    .by = hospital_site_id_num
  ) |>
  dplyr::left_join(
    hospital_reclassification |>
      dplyr::select(
        hospital_site_id_num,
        category_inhospital,
        category_90d,
        changed_category
      ),
    by = "hospital_site_id_num"
  )

# Kategorien in eine Rangfolge übersetzen, um die Bewegungsrichtung zu bestimmen
category_rank <- c(
  "Lower than expected" = 1L,
  "Within control limits" = 2L,
  "Higher than expected" = 3L
)

hospital_profile <- hospital_profile |>
  dplyr::mutate(
    category_inhospital_num    = unname(category_rank[as.character(category_inhospital)]),
    category_90d_num           = unname(category_rank[as.character(category_90d)]),
    reclassification_direction = dplyr::case_when(
      is.na(category_inhospital_num) | is.na(category_90d_num) ~ NA_character_,
      category_90d_num > category_inhospital_num ~"Toward higher mortality classification",
      category_90d_num < category_inhospital_num ~"Toward lower mortality classification",
      TRUE ~ "No change"
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

hospital_profile_by_direction <- hospital_profile |>
  dplyr::summarise(
    n_hospitals                = dplyr::n(),
    median_cases               = stats::median(n_cases, na.rm = TRUE),
    q1_cases                   = stats::quantile(n_cases, 0.25, na.rm = TRUE),
    q3_cases                   = stats::quantile(n_cases, 0.75, na.rm = TRUE),
    median_los                 = stats::median(median_episode_length, na.rm = TRUE),
    q1_los                     = stats::quantile(median_episode_length, 0.25, na.rm = TRUE),
    q3_los                     = stats::quantile(median_episode_length, 0.75, na.rm = TRUE),
    median_postdischarge_share = stats::median(postdischarge_share_of_90d_deaths_pct, na.rm = TRUE),
    q1_postdischarge_share     = stats::quantile(postdischarge_share_of_90d_deaths_pct, 0.25, na.rm = TRUE),
    q3_postdischarge_share     = stats::quantile(postdischarge_share_of_90d_deaths_pct, 0.75, na.rm = TRUE),
    median_acute_transfer      = stats::median(acute_care_transfer_pct, na.rm = TRUE),
    q1_acute_transfer          = stats::quantile(acute_care_transfer_pct, 0.25, na.rm = TRUE),
    q3_acute_transfer          = stats::quantile(acute_care_transfer_pct, 0.75, na.rm = TRUE),
    .by = reclassification_direction
  )

hospital_profile_table <- hospital_profile_by_direction |>
  dplyr::transmute(
    reclassification_direction,
    n_hospitals,
    `Eligible sepsis hospitalisations, median (IQR)` = sprintf("%.0f (%.0f–%.0f)", median_cases, q1_cases, q3_cases),
    `Median length of stay, days (IQR)`              = sprintf("%.1f (%.1f–%.1f)", median_los, q1_los, q3_los),
    `90-day deaths after discharge, %, median (IQR)` = sprintf("%.1f (%.1f–%.1f)", median_postdischarge_share, q1_postdischarge_share, q3_postdischarge_share),
    `Transfer to another acute-care hospital or an acute-care unit within the same institution, %, median (IQR)` = 
      sprintf("%.1f (%.1f–%.1f)", median_acute_transfer, q1_acute_transfer, q3_acute_transfer)
  )

hospital_profile_reclassified <- hospital_profile |>
  dplyr::filter(changed_category) |>
  dplyr::arrange(category_inhospital, category_90d, dplyr::desc(n_cases))

# ******************************************************************************
# Supplementary Table I2: endpoint reclassification using Poisson 95% limits ----
# ******************************************************************************
# funnel plot with 95% Poisson control limits --> inhospital
fp_inhospital_poisson <- FunnelPlotR::funnel_plot(
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

# funnel plot with 95% Poisson control limits --> 90d
fp_90d_poisson <- FunnelPlotR::funnel_plot(
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

# classification hospitals with 95% Poisson control limits --> inhospital
sensitivity_class_inhospital_poisson <- classify_funnel(
  fp_inhospital_poisson, "LCL95", "UCL95") |>
  dplyr::rename(category_inhospital = category)

# classification hospitals with 95% Poisson control limits --> 90d
sensitivity_class_90d_poisson <- classify_funnel(
  fp_90d_poisson, "LCL95", "UCL95") |>
  dplyr::rename(category_90d = category)

# check nb of records before join_by()
expected_sites   <- sort(as.integer(hospital_sites_included$hospital_site_id_num))
inhospital_sites <- sort(as.integer(sensitivity_class_inhospital_poisson$hospital_site_id_num))
sites_90d        <- sort(as.integer(sensitivity_class_90d_poisson$hospital_site_id_num))

if (!identical(inhospital_sites, expected_sites) ||!identical(sites_90d, expected_sites)) {
  stop("Poisson classifications do not contain exactly the expected hospital sites.")}
cli::cli_alert_success("Both Poisson classifications contain all expected hospital sites.")

# join and analyze REclassification of hospitals when using 95% Poisson control limits and swithichng from inhospital to 90d
hospital_reclassification_poisson <- sensitivity_class_inhospital_poisson |>
  dplyr::select(hospital_site_id_num, category_inhospital) |>
  dplyr::inner_join(sensitivity_class_90d_poisson |>
      dplyr::select(hospital_site_id_num, category_90d),  by = "hospital_site_id_num") |>
  dplyr::mutate(
    category_inhospital = factor(category_inhospital, levels = category_levels),
    category_90d = factor(category_90d, levels = category_levels),
    changed_category = category_inhospital != category_90d
  )

# summary of  REclassification
summary_reclassification_poisson <- hospital_reclassification_poisson |>
  dplyr::summarise(
    n_hospitals = dplyr::n(),
    same_category = sum(!changed_category),
    changed_category = sum(changed_category),
    pct_changed_category = round(100 * changed_category / n_hospitals, 1)
  )


# REclassification table
table_reclassification_poisson <- hospital_reclassification_poisson |>
  dplyr::count(category_inhospital, category_90d, name = "n_hospitals") |>
  tidyr::complete(
    category_inhospital = factor(category_levels, levels = category_levels),
    category_90d = factor(category_levels, levels = category_levels),
    fill = list(n_hospitals = 0L)
  ) |>
  tidyr::pivot_wider(
    names_from = category_90d,
    values_from = n_hospitals,
    values_fill = 0L
  )

# ******************************************************************************
# Supplementary Table K1: 90-day classification under Poisson vs OD-adjusted 95% limits ----
# ******************************************************************************
# check number of sites before join 
sites_90d_od <- sort(as.integer(funnel_class_90d$hospital_site_id_num))
if (!identical(inhospital_sites, expected_sites) || !identical(sites_90d, expected_sites) ||!identical(sites_90d_od, expected_sites)) {
  stop("A classification does not contain exactly the expected hospital sites.")}
cli::cli_alert_success("All classifications contain the expected hospital sites.")

# join and analyze REclassification of hospitals when using 95% Poisson control limits instead of OD limits
classification_90d_control_limits <- funnel_class_90d |>
  dplyr::select(hospital_site_id_num, category_overdispersion = category_90d) |>
  dplyr::inner_join(sensitivity_class_90d_poisson |>
      dplyr::select(hospital_site_id_num, category_poisson = category_90d), by = "hospital_site_id_num") |>
  dplyr::mutate(
    category_overdispersion = factor(category_overdispersion, levels = category_levels),
    category_poisson = factor(category_poisson, levels = category_levels),
    changed_category = category_overdispersion != category_poisson
  )

# summary of REclassification 
summary_control_limits_90d <- classification_90d_control_limits |>
  dplyr::summarise(
    n_hospitals = dplyr::n(),
    same_category = sum(!changed_category),
    changed_category = sum(changed_category),
    pct_changed_category = round(100 * changed_category / n_hospitals, 1)
  )

# REclassification table
table_control_limits_90d <- classification_90d_control_limits |>
  dplyr::count(category_overdispersion, category_poisson, name = "n_hospitals") |>
  tidyr::complete(
    category_overdispersion = factor(category_levels, levels = category_levels),
    category_poisson = factor(category_levels, levels = category_levels),
    fill = list(n_hospitals = 0L)
  )

# REclassification table transposed to wide
table_control_limits_90d_wide <- table_control_limits_90d |>
  tidyr::pivot_wider(
    names_from = category_poisson,
    values_from = n_hospitals,
    values_fill = 0L
  )

# List sites whose classification changes between Poisson and OD-adjusted limits 
control_limit_change_directions_90d <- classification_90d_control_limits |>
  dplyr::filter(changed_category) |>
  dplyr::count(category_overdispersion, category_poisson, name = "n_hospitals") |>
  dplyr::arrange(dplyr::desc(n_hospitals))
  # --> Empty table cause no category changes


# ******************************************************************************
# Hospital classification overview: RDS and XLSX ----
# ******************************************************************************
# this table contains all the classifications by hospital site id as well as 
# some descfriptive measures by hospital

# create hospital_site_characteristics
hospital_site_characteristics <- analysis_dataset |>
  dplyr::summarise(
    n_deaths_inhospital     = sum(death_in_hospital, na.rm = TRUE),
    n_deaths_90d            = sum(death_90d_postadmission, na.rm = TRUE),
    pct_age_65plus          = 100 * mean(age_group == "≥65 years", na.rm = TRUE),
    pct_men                 = 100 * mean(sex == "Male", na.rm = TRUE),
    pct_emergency_admission = 100 * mean(admission_type_group == "Emergency admission", na.rm = TRUE),
    pct_principal_diagnosis = 100 * mean(trigger_position_group == "Primary diagnosis", na.rm = TRUE),
    pct_septic_shock        = 100 * mean(sepsis_severity_group == "Septic shock", na.rm = TRUE),
    mean_elixhauser         = mean(elixhauser_vw_score, na.rm = TRUE),
    .by = hospital_site_id_num
  )

# transpose table with classification by adjustment model
classification_by_model <- hospital_reclassification_models |>
  dplyr::mutate(
    model = dplyr::case_when(
      model == "Raw" ~ "raw",
      model == "Age and sex" ~ "age_sex",
      model == "Full model" ~ "full"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = hospital_site_id_num,
    names_from = model,
    values_from = c(category_inhospital, category_90d),
    names_glue = "{.value}_{model}"
  )

# transpose table with classification by cl method
classification_poisson <- hospital_reclassification_poisson |>
  dplyr::transmute(
    hospital_site_id_num,
    category_inhospital_full_poisson = category_inhospital,
    category_90d_full_poisson = category_90d
  )

# join & add 'classification_status'
hospital_classification_overview <- 
  hospital_site_volume_pooled |>
  dplyr::left_join(hospital_site_characteristics, by = "hospital_site_id_num") |>
  dplyr::left_join(classification_by_model, by = "hospital_site_id_num") |>
  dplyr::left_join(classification_poisson,  by = "hospital_site_id_num") |>
  dplyr::mutate(
    classification_status = dplyr::if_else(
      n_sepsis_cases >= minimum_site_volume,
      "Classified",
      paste0("Not classified (<", minimum_site_volume, " cases)")
    )
  )

# check so a site above the threshold can’t silently have a missing classification:
classification_columns <- grep("^category_", names(hospital_classification_overview), value = TRUE)
eligible_site_rows     <-  hospital_classification_overview$n_sepsis_cases >= minimum_site_volume

if (anyNA(hospital_classification_overview[ eligible_site_rows, classification_columns])) {
  stop("One or more eligible sites are missing a classification.")
}

# set output path
path_output <- path_indicator_results
if (!dir.exists(path_output)) { stop("Output directory does not exist: ", path_output)}


# save rds & xlsx
hospital_classification_overview_rds_filepath <- file.path(
  path_output,
  paste0("hospital_classification_overview_", analysis_dataset_name,".rds")
  )

saveRDS(
  hospital_classification_overview,
  file = hospital_classification_overview_rds_filepath
)

hospital_classification_overview_xlsx_filepath <- file.path(
  path_output,
  paste0("hospital_classification_overview_", analysis_dataset_name,".xlsx")
)

write_excel_tables_update(
  list = list(
    hospital_classification_overview = hospital_classification_overview
  ),
  file = hospital_classification_overview_xlsx_filepath
)

# ******************************************************************************
# Save reusable paper objects ----
# ******************************************************************************
# check for necessary parameters 
required_output_parameters <- c(
  "analysis_dataset_name",
  "indicator_id", "indicator_name",
  "index_start_year", "index_end_year",
  "datestamp", "path_indicator_results"
)

missing_output_parameters <- required_output_parameters[
  !vapply(required_output_parameters, exists, logical(1), inherits = TRUE)
]

if (length(missing_output_parameters) > 0L) {
  stop("Missing output parameter(s): ", paste(missing_output_parameters, collapse = ", ") )}


rds_object_filepath <- file.path(
  path_indicator_results,
  paste0(
    "09_hospital_comparisons_rds_objects_",
    analysis_dataset_name,
    ".rds"
  )
)

# save RDS file
saveRDS(
  list(
    metadata = list(
      analysis_dataset_name = analysis_dataset_name,
      indicator_id = indicator_id,
      indicator_name = indicator_name,
      index_start_year = index_start_year,
      index_end_year = index_end_year,
      datestamp = datestamp
    ),
    # Outcome and volume QA; Supplementary Table C1
    qa_hospital_outcomes = qa_hospital_outcomes,
    qa_site_to_hospital = qa_site_to_hospital,
    hospital_site_volume = hospital_site_volume,
    hospital_site_volume_summary = hospital_site_volume_summary,
    site_volume_by_year = site_volume_by_year,
    hospital_site_volume_pooled = hospital_site_volume_pooled,
    site_volume_pooled = site_volume_pooled,
    hospital_thresholds_table = hospital_thresholds_table,
    selected_threshold_summary = selected_threshold_summary,
    hospital_sites_included = hospital_sites_included,
    hospital_summaries = hospital_summaries,
    hospital_summaries_analysis = hospital_summaries_analysis,
    qa_hospital_aggregation = qa_hospital_aggregation,

    # Funnel plots, QA and primary endpoint reclassification
    fp_inhospital_adj = fp_inhospital_adj,
    fp_90d_adj = fp_90d_adj,
    fp_inhospital_poisson = fp_inhospital_poisson,
    fp_90d_poisson = fp_90d_poisson,
    p_funnel_inhospital_adj = p_funnel_inhospital_adj,
    p_funnel_90d_adj = p_funnel_90d_adj,
    funnel_data_inhospital = funnel_data_inhospital,
    funnel_data_90d = funnel_data_90d,
    funnel_outliers_inhospital = funnel_outliers_inhospital,
    funnel_outliers_90d = funnel_outliers_90d,
    qa_funnel_overdispersion = qa_funnel_overdispersion,
    table_reclassification = table_reclassification_wide,
    table_reclassification_long = table_reclassification,
    summary_reclassification = summary_reclassification,
    reclassification_directions = reclassification_directions,
    hospital_reclassification = hospital_reclassification,
    
    # Full versus interaction model check
    hospital_compare_interaction = hospital_compare_interaction,
    qa_model_comparison = qa_model_comparison,
    qa_hospital_model_comparison_ranked = qa_hospital_model_comparison_ranked,
    
    # Sensitivity: endpoint comparison by model; Supplementary Table I1
    hospital_reclassification_models = hospital_reclassification_models,
    summary_reclassification_models = summary_reclassification_models,
    table_reclassification_models = table_reclassification_models,
    overlap_reclassification_models = overlap_reclassification_models,
    
    # Sensitivity: risk adjustment at fixed 90-day endpoint; Figure 3/Table J1
    classification_90d_models = classification_90d_models,
    summary_risk_adjustment_90d = summary_risk_adjustment_90d,
    p_alluvial_raw_full = p_alluvial_raw_full,
    p_alluvial_age_sex_full = p_alluvial_age_sex_full,
    p_alluvial_risk_adjustment = p_alluvial_risk_adjustment,
    
    # Descriptive hospital profiles; Supplementary Table H1
    hospital_profile = hospital_profile,
    hospital_profile_by_direction = hospital_profile_by_direction,
    hospital_profile_table = hospital_profile_table,
    hospital_profile_reclassified = hospital_profile_reclassified,
    
    # Sensitivity: Poisson endpoint comparison (I2) and fixed endpoint (K1)
    hospital_reclassification_poisson = hospital_reclassification_poisson,
    summary_reclassification_poisson = summary_reclassification_poisson,
    table_reclassification_poisson = table_reclassification_poisson,
    classification_90d_control_limits = classification_90d_control_limits,
    summary_control_limits_90d = summary_control_limits_90d,
    table_control_limits_90d = table_control_limits_90d_wide,
    control_limit_change_directions_90d = control_limit_change_directions_90d,
    
    #hospital_classification_overview
    hospital_classification_overview = hospital_classification_overview
  ),
  file = rds_object_filepath
)

cli::cli_alert_success("Hospital-comparison paper objects saved: {basename(rds_object_filepath)}")
