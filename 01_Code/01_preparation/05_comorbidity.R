# ******************************************************************************
# 05_comorbidity.R
# ******************************************************************************
#
# Purpose: Calculate encounter-level Charlson and Elixhauser comorbidity scores.
#
# - Use secondary ICD-10 diagnoses and the Quan mappings.
# - Assign a score of zero to encounters without qualifying secondary diagnoses.
# - Save the scores for use in downstream analyses.
#
# Outputs:
# - path_common: comorbidity_scores FST file for the configured study period,
#   saved using common_file().
# - Console: score summaries and comorbidity-flag prevalences.
# - diagnostics.pdf: Charlson and Elixhauser score histograms, when run through
#   00_run_preparation.R with the PDF graphics device open.
#
# ******************************************************************************

# ******************************************************************************
# Information about the used R package 
# - https://ellessenne.github.io/comorbidity/
# - vignette("B-comorbidity-scores", package = "comorbidity")
# - vignette("A-introduction", package = "comorbidity")
# ******************************************************************************

# ******************************************************************************
# Read diagnoses and identify encounters ----
# ******************************************************************************

icd_all <- read_fst(common_file("diagnoses"))

if (anyNA(icd_all$survey_year) || anyNA(icd_all$encounter_id) ||
    any(trimws(as.character(icd_all$encounter_id)) == "", na.rm = TRUE)) {
  stop("Diagnosis data contain missing encounter identifiers.")
}

all_icd_encounters <- icd_all |>
  distinct(survey_year, encounter_id) |>
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_"))

if (anyDuplicated(all_icd_encounters$encounter_key)) {
  stop("Encounter keys are not unique.")
}


# ******************************************************************************
# Prepare secondary diagnosis codes ----
# ******************************************************************************
dx_comorbidity_long <- icd_all |>
  filter(rank > 1) |>  # filter(rank > 1) schliesst nur die Hauptdiagnose mit rank == 1
  transmute(
    survey_year,
    encounter_id,
    code = na_if(trimws(as.character(icd_clean)), "")
  ) |>
  filter(!is.na(code)) |>
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_")) |>
  distinct(encounter_key, code) |>
  select(encounter_key, code)


# ******************************************************************************
# Calculate comorbidity flags and scores ----
# ******************************************************************************
charlson_flags <- comorbidity::comorbidity(
  x = dx_comorbidity_long,
  id = "encounter_key",
  code = "code",
  map = "charlson_icd10_quan",
  assign0 = TRUE,
  labelled = FALSE
)

elixhauser_flags <- comorbidity::comorbidity(
  x = dx_comorbidity_long,
  id = "encounter_key",
  code = "code",
  map = "elixhauser_icd10_quan",
  assign0 = TRUE,
  labelled = FALSE
)

charlson_scores <- tibble(
  encounter_key = charlson_flags$encounter_key,
  charlson_score = comorbidity::score(
    x = charlson_flags,
    weights = "quan",
    assign0 = TRUE
  )
)

elixhauser_scores <- tibble(
  encounter_key = elixhauser_flags$encounter_key,
  elixhauser_vw_score = comorbidity::score(
    x = elixhauser_flags,
    weights = "vw",
    assign0 = TRUE
  )
)


# ******************************************************************************
# Combine scores for all encounters ----
# ******************************************************************************
comorbidity_scores <- all_icd_encounters |>
  left_join(charlson_scores, by = "encounter_key", relationship = "one-to-one") |>
  left_join(elixhauser_scores, by = "encounter_key", relationship = "one-to-one") |>
  mutate(
    charlson_score = coalesce(charlson_score, 0),
    elixhauser_vw_score = coalesce(elixhauser_vw_score, 0)
  ) |>
  select(
    survey_year,
    encounter_id,
    encounter_key,
    charlson_score,
    elixhauser_vw_score
  )

if (nrow(comorbidity_scores) != nrow(all_icd_encounters) ||
    anyDuplicated(comorbidity_scores$encounter_key)) {
  stop("Comorbidity scores do not contain exactly one row per encounter.")
}


# ******************************************************************************
# Save scores and review distributions ----
# ******************************************************************************
output_fst <- common_file("comorbidity_scores")
write_fst(comorbidity_scores, output_fst)

message("Output saved: ", basename(output_fst))

print(summary(comorbidity_scores$charlson_score))
hist(
  comorbidity_scores$charlson_score,
  breaks = 30,
  main = "Charlson score",
  xlab = "Score"
)

print(summary(comorbidity_scores$elixhauser_vw_score))
hist(
  comorbidity_scores$elixhauser_vw_score,
  breaks = 50,
  main = "Elixhauser van Walraven score",
  xlab = "Score"
)

flag_prevalence <- bind_rows(
  charlson_flags |>
    summarise(across(-encounter_key, \(x) mean(x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "comorbidity", values_to = "prevalence") |>
    mutate(index = "Charlson"),
  elixhauser_flags |>
    summarise(across(-encounter_key, \(x) mean(x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "comorbidity", values_to = "prevalence") |>
    mutate(index = "Elixhauser")
) |>
  select(index, comorbidity, prevalence)

print(flag_prevalence)


