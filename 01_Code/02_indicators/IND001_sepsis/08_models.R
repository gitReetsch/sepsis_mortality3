# ******************************************************************************
# 08_models.R
# ******************************************************************************
#
# Purpose:
# - Fit unadjusted, age/sex-adjusted, fully adjusted and interaction models.
# - Calculate model performance and patient-level expected mortality.
#
# Inputs:
# - analysis_dataset after 04_mortality_outcomes.R and 05_covariates.R
# - path_indicator_results and analysis_dataset_name from 00_run_sepsis.R
#
# Outputs:
# - analysis_dataset with model outcomes and expected mortality.
# - Model performance and QA summaries in memory and printed to console.
# - RDS bundle with models and performance results.
#
# ******************************************************************************

# ******************************************************************************
# QA: missing model variables ----
# ******************************************************************************
qa_model_missing <- analysis_dataset |>
  dplyr::summarise(
    n = dplyr::n(),
    dplyr::across(
      dplyr::all_of(c(
        "death_in_hospital",
        "death_90d_postadmission",
        "age_group",
        "sex",
        "elixhauser_vw_score",
        "admission_type_group",
        "trigger_position_group",
        "sepsis_severity_group"
      )),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    )
  )

print(tibble::as_tibble(qa_model_missing), width = Inf)


# ******************************************************************************
# Fit models ----
# ******************************************************************************
model_raw_inhospital <- stats::glm(
  death_in_hospital ~ 1,
  data = analysis_dataset,
  family = stats::binomial()
)

model_raw_90d <- stats::glm(
  death_90d_postadmission ~ 1,
  data = analysis_dataset,
  family = stats::binomial()
)

model_age_sex_inhospital <- stats::glm(
  death_in_hospital ~ age_group + sex,
  data = analysis_dataset,
  family = stats::binomial()
)

model_age_sex_90d <- stats::glm(
  death_90d_postadmission ~ age_group + sex,
  data = analysis_dataset,
  family = stats::binomial()
)

model_full_inhospital <- stats::glm(
  death_in_hospital ~
    age_group + sex + elixhauser_vw_score +
    admission_type_group + trigger_position_group +
    sepsis_severity_group,
  data = analysis_dataset,
  family = stats::binomial()
)

model_full_90d <- stats::glm(
  death_90d_postadmission ~
    age_group + sex + elixhauser_vw_score +
    admission_type_group + trigger_position_group +
    sepsis_severity_group,
  data = analysis_dataset,
  family = stats::binomial()
)

model_full2_inhospital <- stats::glm(
  death_in_hospital ~
    age_group + sex + elixhauser_vw_score +
    admission_type_group + trigger_position_group +
    sepsis_severity_group +
    age_group:elixhauser_vw_score,
  data = analysis_dataset,
  family = stats::binomial()
)

model_full2_90d <- stats::glm(
  death_90d_postadmission ~
    age_group + sex + elixhauser_vw_score +
    admission_type_group + trigger_position_group +
    sepsis_severity_group +
    age_group:elixhauser_vw_score,
  data = analysis_dataset,
  family = stats::binomial()
)


# ******************************************************************************
# Model performance ----
# ******************************************************************************
# Einordnung der Modellkennzahlen:
#
# - AIC vergleicht Modellfit und Modellkomplexität. Ein niedrigerer Wert
#   spricht für den besseren relativen Fit unter Berücksichtigung der Anzahl
#   der Parameter. Für die In-hospital-Mortalität sinkt der AIC vom
#   Alters-/Geschlechtsmodell (64'965) zum Vollmodell (59'632); mit Interaktion
#   sinkt er weiter auf 59'267. Modelle nur innerhalb desselben Endpunkts
#   vergleichen.
#
# - Der Brier-Score misst den mittleren quadratischen Fehler der vorhergesagten
#   Wahrscheinlichkeiten. Ein niedrigerer Wert ist besser. Für die
#   In-hospital-Mortalität sinkt er vom Alters-/Geschlechtsmodell (0.157) zum
#   Vollmodell (0.144). Die Interaktion verändert ihn gerundet kaum (0.144).
#   Der Brier-Score erfasst die Gesamtgüte der Wahrscheinlichkeitsvorhersagen,
#   ist aber keine reine Kalibrierungskennzahl.
#
# - Tjur-R² misst die Trennung zwischen den beobachteten Todesfällen und
#   Überlebenden: Es ist die mittlere vorhergesagte Mortalität bei Verstorbenen
#   minus jene bei Überlebenden. Höhere Werte bedeuten stärkere Diskrimination.
#   Für die In-hospital-Mortalität steigt Tjur-R² vom Alters-/Geschlechtsmodell
#   (0.0269) zum Vollmodell (0.112); mit Interaktion beträgt es 0.114.
#
# - McFadden-R² beschreibt die Verbesserung der Log-Likelihood gegenüber dem
#   Nullmodell mit Interzept allein. Höhere Werte zeigen einen besseren
#   relativen Modellfit, sind aber nicht als Anteil erklärter Varianz zu lesen.
#   Das Nullmodell hat definitionsgemäss den Wert 0.
#
# - df gibt die Zahl der geschätzten Modellparameter einschliesslich Interzept
#   an; es ist kein Gütemass. Das Interaktionsmodell hat mehr Parameter als
#   das Vollmodell.
#
# - Kalibrierung bedeutet, dass vorhergesagte Risiken mit den beobachteten
#   Häufigkeiten übereinstimmen. Wenn ein Modell für eine Gruppe beispielsweise
#   20 % Mortalitätsrisiko vorhersagt, sollte die beobachtete Mortalität in
#   dieser Gruppe ungefähr 20 % betragen.
#
# - Die Tabelle enthält noch keine direkte Kalibrierungsanalyse. Dafür sollten
#   Kalibrierungsplots sowie Kalibrierungsinterzept und -steigung ergänzt
#   werden. Dass die mittlere vorhergesagte Mortalität der beobachteten
#   Gesamtrate entspricht, genügt als Kalibrierungsnachweis nicht.
#
# - Die gezeigten Kennzahlen stammen aus denselben Daten, mit denen die Modelle
#   geschätzt wurden. Sie beschreiben daher die In-sample-Performance. Vor
#   einem Einsatz für Benchmarking sind insbesondere Kalibrierung, zeitliche
#   Stabilität und Übertragbarkeit weiter zu prüfen.

model_registry <- tibble::tibble(
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
  dplyr::mutate(
    df = purrr::map_dbl(
      model_object,
      ~ attr(stats::logLik(.x), "df")
    ),
    aic = purrr::map_dbl(model_object, stats::AIC),
    brier_score = purrr::map_dbl(
      model_object,
      ~ {
        observed <- stats::model.response(stats::model.frame(.x))
        predicted <- stats::predict(.x, type = "response")
        mean((observed - predicted)^2)
      }
    ),
    tjur_r2 = purrr::map_dbl(
      model_object,
      ~ {
        observed <- stats::model.response(stats::model.frame(.x))
        predicted <- stats::predict(.x, type = "response")
        mean(predicted[observed == 1L]) -
          mean(predicted[observed == 0L])
      }
    ),
    mcfadden_r2 = purrr::map_dbl(
      model_object,
      ~ {
        null_model <- stats::update(.x, . ~ 1)
        1 - as.numeric(stats::logLik(.x)) /
          as.numeric(stats::logLik(null_model))
      }
    )
  ) |>
  dplyr::select(
    outcome,
    model,
    df,
    aic,
    brier_score,
    tjur_r2,
    mcfadden_r2
  )

print(tibble::as_tibble(model_performance_table), n = Inf, width = Inf)


# ******************************************************************************
# Predict expected mortality ----
# ******************************************************************************
predict_expected <- function(data, model, expected_var) {
  data |>
    dplyr::mutate(
      !!expected_var := stats::predict(
        model,
        newdata = data,
        type = "response"
      )
    )
}

analysis_dataset <- analysis_dataset |>
  predict_expected(model_raw_inhospital, "expected_raw_inhospital") |>
  predict_expected(model_age_sex_inhospital, "expected_age_sex_inhospital") |>
  predict_expected(model_full_inhospital, "expected_full_inhospital") |>
  predict_expected(model_full2_inhospital, "expected_full2_inhospital") |>
  predict_expected(model_raw_90d, "expected_raw_90d") |>
  predict_expected(model_age_sex_90d, "expected_age_sex_90d") |>
  predict_expected(model_full_90d, "expected_full_90d") |>
  predict_expected(model_full2_90d, "expected_full2_90d")


# ******************************************************************************
# QA: expected mortality ----
# ******************************************************************************
# Die mittlere vorhergesagte Mortalität entspricht in jedem Modell der beobachteten 
# Sterblichkeit im Gesamtkollektiv: 20,2 % für In-hospital-Mortalität und 27,9 % 
# für 90-Tage-Mortalität. Das ist bei Modellen mit Interzept und Auswertung im 
# selben Datensatz zu erwarten.
# 
# Die adjustierten Modelle unterscheiden sich darin, wie stark sie das individuelle 
# Risiko variieren lassen. Mit den zusätzlichen Kovariaten wird die Spanne breiter; 
# im Interaktionsmodell reicht sie bis 85,7 % bzw. 90,7 %. Das zeigt eine stärkere 
# Risikostreuung, aber für sich allein keine bessere Modellgüte oder Kalibrierung. 
# Dafür sind die separat berechneten Performance-Kennzahlen relevant.

qa_expected_mortality <- analysis_dataset |>
  dplyr::summarise(
    dplyr::across(
      dplyr::starts_with("expected_"),
      list(min = min, mean = mean, max = max)
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = c("model", "outcome", ".value"),
    names_pattern = "^expected_(raw|age_sex|full2|full)_(inhospital|90d)_(min|mean|max)$"
  ) |>
  dplyr::mutate(
    model = dplyr::recode(
      model,
      raw = "Raw",
      age_sex = "Age and sex",
      full = "Full model",
      full2 = "Full model + interaction"
    ),
    outcome = dplyr::recode(
      outcome,
      inhospital = "In-hospital mortality",
      `90d` = "90-day post-admission mortality"
    ),
    dplyr::across(c(min, mean, max), ~ round(100 * .x, 1))
  ) |>
  dplyr::rename(
    min_pct = min,
    mean_pct = mean,
    max_pct = max
  ) |>
  dplyr::arrange(outcome, model)

print(tibble::as_tibble(qa_expected_mortality), n = Inf, width = Inf)



# ******************************************************************************
# Save model objects ----
# ******************************************************************************

rds_object_filepath <- file.path(
  path_indicator_results,
  paste0(
    "08_models_rds_objects_",
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
    model_registry = model_registry,
    model_performance_table = model_performance_table,
    qa_model_missing = qa_model_missing,
    qa_expected_mortality = qa_expected_mortality
  ),
  file = rds_object_filepath
)

cli::cli_alert_success(
  "Model bundle saved: {basename(rds_object_filepath)}"
)
