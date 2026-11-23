# ===================================================
# Diagnoses
# ===================================================

# ---------------------------------------------------
# read data
# ---------------------------------------------------
# generate empty list 
diagnosis_filelist <- list() 

for (year_analysis in start_year:end_year) {
  message(glue::glue("Reading diagnoses {year_analysis}"))
  
  # get filepath from list defined in setup
  diagnosis_filelist[[as.character(year_analysis)]] <-
    fread(path_diagnoses[[as.character(year_analysis)]]) |> # read in data
    janitor::clean_names()                                  # clean names
}

# combine all years into one dataset
icd_all <- bind_rows(diagnosis_filelist)

# have a glimpse
glimpse(icd_all)

# ===================================================
# Descriptive analysis raw data
# ===================================================
# ICD codes by year
freq_table(data = icd_all, var = icd, by = survey_year, totals = "both")     |>   arrange(desc(Total)) |> head()
freq_table(data = icd_all, var = icd_add, by = survey_year, totals = "both") |>   arrange(desc(Total)) |> head()
freq_table(data = icd_all, var = poa, by = survey_year, totals = "both") 


# ===================================================
# standardise ICD codes for indicator matching
# ===================================================
icd_all <- icd_all |>
  mutate(
    icd_clean = icd |>
      str_to_upper() |>              # convert to upper case
      str_remove_all("\\.") |>       # remove dots
      str_remove_all("\\s+"),        # remove spaces
    
    icd_add_clean = icd_add |>
      str_to_upper() |>              # convert to upper case
      str_remove_all("\\.") |>       # remove dots
      str_remove_all("\\s+")         # remove spaces
  )


# =====================================================================
# Comorbidity flags: Charlson and Elixhauser, Quan ICD-10 algorithm
# =====================================================================

library(comorbidity)

# Use secondary diagnoses only:
# rank > 1 = diagnoses after the main diagnosis.
dx_comorbidity_long <- icd_all |>
  filter(rank > 1) |>
  transmute(
    survey_year,
    encounter_id,
    code = icd_clean
  ) |>
  filter(!is.na(code), code != "")

# Unique encounter key for package input
dx_comorbidity_long <- dx_comorbidity_long |>
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_")) |>
  distinct(encounter_key, code)

# Charlson flags
charlson_flags <- comorbidity(
  x = dx_comorbidity_long,
  id = "encounter_key",
  code = "code",
  map = "charlson_icd10_quan",
  assign0 = TRUE,
  labelled = FALSE
)

# Elixhauser flags: contains the binary indicators (0/1) for each Elixhauser comorbidity.
elixhauser_flags <- comorbidity(
  x = dx_comorbidity_long,
  id = "encounter_key",
  code = "code",
  map = "elixhauser_icd10_quan",
  assign0 = TRUE,
  labelled = FALSE
)

# charlson Scores
charlson_scores <- charlson_flags |>
  mutate(
    charlson_score = score(
      x = charlson_flags,
      weights = "quan",
      assign0 = TRUE
    )
  ) |>
  select(encounter_key, charlson_score)

# van Walraven weighted Elixhauser score
elixhauser_scores <- elixhauser_flags |>
  select(encounter_key) |>
  mutate(
    elixhauser_vw_score = score(
      elixhauser_flags,
      weights = "vw",
      assign0 = TRUE
    )
  )

charlson_flags  |> count(encounter_key) |> filter(n > 1)
elixhauser_flags |> count(encounter_key) |> filter(n > 1)
charlson_scores |> count(encounter_key) |> filter(n > 1)
elixhauser_scores |> count(encounter_key) |> filter(n > 1)

charlson_flags  |> nrow()
elixhauser_flags |> nrow()
charlson_scores |> nrow()
elixhauser_scores |> nrow()

charlson_flags  |> head()
elixhauser_flags |> head()
charlson_scores |> head()
elixhauser_scores |> head()

# # Combine
# comorbidity_flags <- charlson_flags |>
#   left_join(elixhauser_flags, by = "encounter_key", suffix = c("_charlson", "_elixhauser")) |>
#   left_join(charlson_scores, by = "encounter_key") |>
#   left_join(elixhauser_scores, by = "encounter_key") |>
#   separate(
#     encounter_key,
#     into = c("survey_year", "encounter_id"),
#     sep = "_",
#     convert = TRUE,
#     remove = FALSE
#   )
# 
# write_fst(
#   comorbidity_flags,
#   file.path(root, "04_Daten", glue::glue("comorbidity_{start_year}_{end_year}.fst"))
# )

# Combine (only scores)
# Combine scores and assign 0 to encounters without secondary comorbidity codes
all_icd_encounters <- icd_all |>
  distinct(survey_year, encounter_id) |>
  mutate(encounter_key = paste(survey_year, encounter_id, sep = "_"))

comorbidity_scores <- all_icd_encounters |>
  left_join(charlson_scores, by = "encounter_key") |>
  left_join(elixhauser_scores, by = "encounter_key") |>
  mutate(
    charlson_score = coalesce(charlson_score, 0),
    elixhauser_vw_score = coalesce(elixhauser_vw_score, 0)
  ) |>
  select(
    encounter_key,
    survey_year,
    encounter_id,
    charlson_score,
    elixhauser_vw_score
  )

output_fst <- file.path(
  root,
  "04_Daten",
  glue::glue("comorbidity_scores_{start_year}_{end_year}.fst")
)

write_fst(comorbidity_scores, output_fst)

message(glue::glue("Output saved: {basename(output_fst)}"))


# Validierung
# Erwartung: Charlson: viele 0er, danach rasch abfallend.
summary(comorbidity_scores$charlson_score)
hist(comorbidity_scores$charlson_score, breaks = 30)

# Erwartung: van Walraven: Werte typischerweise zwischen etwa −10 und >40, die meisten zwischen 0 und 10.
summary(comorbidity_scores$elixhauser_vw_score)
hist(comorbidity_scores$elixhauser_vw_score, breaks = 50)

# Erwartung (wenn es nur Akutsspitäler wären): Diabetes	10–25 %, CHF	5–15 %, CKD	5–15 %, Metastasen	1–5 %
charlson_flags |> summarise(across(-encounter_key, mean))
elixhauser_flags |>  summarise(across(-encounter_key, mean))

# ===================================================
# Indicator definition
# ===================================================

# indicator_list is a list of lists (every indicator is defined by a list itself)
indicator_list <- list(
  # -------------------------------------------------
  # sepsis
  # -------------------------------------------------
  sepsis = list(                        # name of indicator 
    codes =  sepsis_explicit_codes,     # see setup file
    source_var = "icd",                 # variable where to search for 'codes' 
    flag_var = "sepsis_flag",           # name of flag variable
    trigger_var = "sepsis_trigger_icd"  # name of variable with codes which triggered the flag
  )
  
  
  # -------------------------------------------------
  # next indicator
  # -------------------------------------------------
  
  # ,
  # name_indicator_2 = list(              # name of indicator 
  #   codes = c(),                        # relevant codes for definition    
  #   source_var = "icd",                 # variable where to search for 'codes'  
  #   flag_var = "sepsis_flag",           # name of flag variable
  #   trigger_var = "sepsis_trigger_icd"  # name of variable with codes which triggered the flag
  # )

)


# ===================================================
# flag function 
# ===================================================
create_icd_flag <- function(data, indicator) {
  
  data |>
    mutate(
      flag_value = case_when(
        icd_clean %in% indicator$codes & rank == 1 ~ 1L, # main diagnosis
        icd_clean %in% indicator$codes & rank > 1  ~ 2L, # secondary diagnosis
        icd_add_clean %in% indicator$codes         ~ 3L, # add-on to main diagnosis
        TRUE                                       ~ NA_integer_
      ),
      trigger_code = case_when(
        icd_clean %in% indicator$codes     ~ icd_clean,
        icd_add_clean %in% indicator$codes ~ icd_add_clean,
        TRUE                               ~ NA_character_
      )
    ) |>
    filter(!is.na(flag_value)) |>
    summarise(
      flag_value = min(flag_value),
      trigger_code = paste(sort(unique(trigger_code)), collapse = ";"),
      .by = c(survey_year, encounter_id)
    ) |>
    rename(
      !!indicator$flag_var := flag_value,
      !!indicator$trigger_var := trigger_code
    )
}

# apply flag function to all indicators in indicator_list
indicator_list <- lapply(
  indicator_list,
  \(indicator) create_icd_flag(icd_all, indicator)
)

# combine all indicator flags by encounter
flag_icd <- purrr::reduce(
  indicator_list,
  full_join,
  by = c("survey_year", "encounter_id")
)

# ===================================================
# Descriptive analysis indicator flags
# ===================================================

freq_table(data = flag_icd, var = survey_year)
freq_table(data = flag_icd, var = sepsis_flag, by = survey_year, totals = "both")


# ===================================================
# Save
# ===================================================

output_fst <- file.path(root,"04_Daten", glue::glue("flag_icd_{start_year}_{end_year}.fst"))
write_fst(flag_icd, output_fst)
message(glue::glue("Output saved: {basename(output_fst)}"))
