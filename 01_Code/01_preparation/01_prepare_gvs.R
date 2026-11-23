# ******************************************************************************
# 01_prepare_gvs
# ******************************************************************************
# 
# Purpose: Prepare GVS discharge, diagnosis, procedure, and ICU data.
#
# - Read and combine the annual GVS input files.
# - Create and update stable numeric IDs for hospitals and hospital sites.
# - Derive and standardise variables used in later analyses.
# - Run descriptive summaries and data quality checks.
# - Save the prepared datasets as fst files.
#
# Outputs:
# - path_common: discharges, diagnoses, procedures, and icu FST files for the
#   configured study period, saved using common_file().
# - path_lookups: persistent hospital and site lookup RDS files, reused and
#   updated to preserve numeric IDs across runs.
#
# ******************************************************************************


# ******************************************************************************
# DISCHARGES ----
# ______________________________________________________________________________

# ******************************************************************************
## Read a data ----
# ******************************************************************************
# generate empty list 
discharge_filelist <- list() 

for (year_analysis in start_year:end_year) {
  message(glue::glue("Reading discharge {year_analysis}"))
  
  # get filepath from list defined in setup
  discharge_filelist[[as.character(year_analysis)]] <-
    fread(path_discharges[[as.character(year_analysis)]]) |>  # read in data
    janitor::clean_names()                                    # clean names
}

# Combine all years after harmonising the data types of variables that differ across years
discharges_all <- discharge_filelist |>
  purrr::map(~ .x |>
               mutate(
                 across(
                   any_of(c(
                     "residence_country",
                     "nationality",
                     "splg"
                   )),
                   as.character
                 )
               )
  ) |>
  bind_rows()

glimpse(discharges_all)

# ******************************************************************************
## Create ID for combination of hospital and site ----
# ******************************************************************************
discharges_all <- discharges_all |>
  mutate(hospital_site_id = paste(hospital_id, local_id, sep = "_") )

# Stable numeric IDs for hospitals and hospital sites
lookup_dir <- path_lookups

if (!dir.exists(lookup_dir)) {
  dir.create(lookup_dir, recursive = TRUE)
}

hospital_lookup_file <- file.path(lookup_dir, "hospital_lookup.rds")
hospital_site_lookup_file <- file.path(lookup_dir, "hospital_site_lookup.rds")

# Helper: update or create stable lookup
update_stable_lookup <- function(data, id_var, lookup_file, num_var) {
  
  current_ids <- data |>
    distinct({{ id_var }}) |>                                                 # eindeutige IDs aus aktuellem Datensatz
    filter(!is.na({{ id_var }}), {{ id_var }} != "") |>                       # fehlende IDs ausschliessen
    arrange({{ id_var }})                                                     # deterministische Reihenfolge
  
  if (file.exists(lookup_file)) {                                             # bestehende Lookup-Tabelle laden
    lookup <- readRDS(lookup_file)
  } else {
    lookup <- current_ids |>
      mutate("{num_var}" := row_number())                                     # erste fortlaufende Numerierung erzeugen
    
    saveRDS(lookup, lookup_file)                                              # Lookup dauerhaft speichern
    return(lookup)                                                            # Funktion beenden
  }
  
  new_ids <- current_ids |>
    anti_join(
      lookup,
      by = rlang::as_name(rlang::enquo(id_var))
    )                                                                         # nur bisher unbekannte IDs behalten
  
  if (nrow(new_ids) > 0) {                                                    # falls neue IDs gefunden wurden
    
    new_ids <- new_ids |>
      mutate(
        "{num_var}" := max(lookup[[num_var]], na.rm = TRUE) +
          row_number()
      )                                                                       # neue Nummern an bestehende anhÃ¤ngen
    
    lookup <- bind_rows(lookup, new_ids) |>                                   # alte und neue IDs zusammenfÃ¼hren
      arrange(.data[[num_var]])                                               # nach numerischer ID sortieren
    
    saveRDS(lookup, lookup_file)                                              # aktualisierte Lookup-Tabelle speichern
  }
  
  lookup                                                                      # Lookup-Tabelle zurÃ¼ckgeben
}


# Create / update lookup tables
hospital_lookup <- update_stable_lookup(
  data = discharges_all,
  id_var = hospital_id,
  lookup_file = hospital_lookup_file,
  num_var = "hospital_id_num"
)

hospital_site_lookup <- update_stable_lookup(
  data = discharges_all,
  id_var = hospital_site_id,
  lookup_file = hospital_site_lookup_file,
  num_var = "hospital_site_id_num"
)

# Join numeric IDs to data
discharges_all <- discharges_all |>
  left_join(hospital_lookup, by = "hospital_id") |>
  left_join(hospital_site_lookup, by = "hospital_site_id")


# Checks

# Pro hospital_id nur eine hospital_id_num?
check_hospital <- discharges_all |>
  group_by(hospital_id) |>
  summarise(
    n_ids = n_distinct(hospital_id_num),
    .groups = "drop"
  ) |>
  filter(n_ids > 1)

if (nrow(check_hospital) > 0) {
  stop("Error: hospital_id not mapped unambiguously to hospital_id_num")
}


# Pro hospital_site_id nur eine hospital_site_id_num?
check_hospital_site <- discharges_all |>
  group_by(hospital_site_id) |>
  summarise(
    n_ids = n_distinct(hospital_site_id_num),
    .groups = "drop"
  ) |>
  filter(n_ids > 1)

if (nrow(check_hospital_site) > 0) {
  stop("Error: hospital_site_id not mapped unambiguously to hospital_site_id_num")
}


# Sind die hospital_id(_num) Ã¼ber mehrere Jahre konsistent?
cases_hospital_year <- discharges_all |>
  group_by(survey_year, hospital_id_num) |>
  summarise(
    n_cases = n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = survey_year,
    values_from = n_cases,
    values_fill = 0
  ) |>
  arrange(hospital_id_num)

cases_hospital_year


# Sind die hospital_site_id(_num) Ã¼ber mehrere Jahre konsistent?
cases_hospital_site_year <- discharges_all |>
  group_by(survey_year, hospital_site_id_num) |>
  summarise(
    n_cases = n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = survey_year,
    values_from = n_cases,
    values_fill = 0
  ) |>
  arrange(hospital_site_id_num)

cases_hospital_site_year


# Gehört ein Standort innerhalb eines Jahres eindeutig zu einem Spital?
check_site_hospital <- discharges_all |>
  distinct(
    survey_year,
    hospital_site_id_num,
    hospital_id_num
  ) |>
  group_by(survey_year, hospital_site_id_num) |>
  summarise(
    n_hospitals = n_distinct(hospital_id_num),
    hospitals = paste(sort(unique(hospital_id_num)), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(n_hospitals > 1)

if (nrow(check_site_hospital) > 0) {
  stop("Error: hospital_site_id_num maps to multiple hospital_id_num within the same year")
}

# Delete original IDs
discharges_all <- discharges_all |>
  select(-hospital_id, -local_id, -hospital_site_id)

# ******************************************************************************
## Descriptive analysis raw data -----
# ******************************************************************************
freq_table(data=discharges_all, var=discharge_month)
freq_table(data=discharges_all, var=service_centre_main, by=survey_year, totals="both")
freq_table(data=discharges_all, var=residence_country, by=survey_year, totals="both")
freq_table(data=discharges_all, var=residence_canton, totals="both")
freq_table(data=discharges_all, var=class_age, by=survey_year, totals="both")
freq_table(data=discharges_all, var=gender, by=survey_year, totals="both")
freq_table(data=discharges_all, var=insurance_class, by=survey_year, totals="both")
freq_table(data=discharges_all, var=room_class, by=survey_year, totals="both")
freq_table(data=discharges_all, var=admission_type, by=survey_year, totals="both")
freq_table(data=discharges_all, var=discharge_decision, by=survey_year, totals="both") # 5  = gestorben


# ******************************************************************************
## Calculate general variables ----
# ******************************************************************************
# length of stay (los)
discharges_all %>%
  summarise(
    n = n(),
    n_na = sum(is.na(los_swissdrg)),
    mean = mean(los_swissdrg, na.rm = TRUE),
    median = median(los_swissdrg, na.rm = TRUE),
    sd = sd(los_swissdrg, na.rm = TRUE),
    min = min(los_swissdrg, na.rm = TRUE),
    max = max(los_swissdrg, na.rm = TRUE)
  )

# Missings bei LOS betreffen ausschliesslich Fälle im Jahr 2024
discharges_all |>
  mutate(
    los_missing = is.na(los_swissdrg)
  ) |>
  count(survey_year, los_missing) |>
  tidyr::pivot_wider(
    names_from = los_missing,
    values_from = n,
    values_fill = 0
  )

library(ggplot2)
ggplot(discharges_all, aes(x = los_swissdrg)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 100)) +
  labs(
    title = "Verteilung der Variable (x-Achse bei 100 Tagen abgeschnitten)",
    subtitle = paste("NA:", sum(is.na(discharges_all$los_swissdrg)))
  )

# calculate los
discharges_all <- discharges_all %>% 
  mutate(los = ifelse(is.na(los_swissdrg), NA,  # Fehlende Werte bleiben fehlend
                      pmax(los_swissdrg, 1)))   # Alle Werte kleiner als 1 werden auf 1 gesetzt
# check berechnete Variable
discharges_all %>%
  summarise(
    n = n(),
    n_na = sum(is.na(los)),
    mean = mean(los, na.rm = TRUE),
    median = median(los, na.rm = TRUE),
    sd = sd(los, na.rm = TRUE),
    min = min(los, na.rm = TRUE),
    max = max(los, na.rm = TRUE)
  )

# ******************************************************************************
## Data quality checks variables ----
# ******************************************************************************
# QA: episode length
# Basic distribution and missingness
qa_episode_length <- discharges_all |>
  summarise(
    n = n(),
    n_missing = sum(is.na(episode_length)),
    pct_missing = round(100 * n_missing / n, 2),
    mean = mean(episode_length, na.rm = TRUE),
    median = median(episode_length, na.rm = TRUE),
    sd = sd(episode_length, na.rm = TRUE),
    min = min(episode_length, na.rm = TRUE),
    max = max(episode_length, na.rm = TRUE)
  )

print(qa_episode_length)

# Missingness by year
qa_episode_length_missing_year <- discharges_all |>
  mutate(episode_length_missing = is.na(episode_length)) |>
  count(survey_year, episode_length_missing) |>
  tidyr::pivot_wider(
    names_from = episode_length_missing,
    values_from = n,
    values_fill = 0
  )

print(qa_episode_length_missing_year)

# QA: episode length versus LOS
qa_episode_vs_los <- discharges_all |>
  mutate(
    episode_los_difference = episode_length - los
  ) |>
  summarise(
    n_complete = sum(!is.na(episode_length) & !is.na(los)),
    n_equal = sum(episode_length == los, na.rm = TRUE),
    n_episode_longer = sum(episode_length > los, na.rm = TRUE),
    n_episode_shorter = sum(episode_length < los, na.rm = TRUE),
    mean_difference = mean(episode_los_difference, na.rm = TRUE),
    median_difference = median(episode_los_difference, na.rm = TRUE),
    max_difference = max(episode_los_difference, na.rm = TRUE)
  )

print(qa_episode_vs_los)

# inspect cases when episode_length < los
qa_episode_los_difference <- discharges_all |>
  mutate(
    episode_los_difference = episode_length - los
  ) |>
  count(
    survey_year,
    difference_group = case_when(
      is.na(los) ~ "LOS missing",
      episode_los_difference < 0 ~ "episode_length < los",
      episode_los_difference == 0 ~ "equal",
      episode_los_difference > 0 ~ "episode_length > los"
    )
  ) |>
  group_by(survey_year) |>
  mutate(percent = 100 * n / sum(n)) |>
  ungroup()

print(qa_episode_los_difference)

discharges_all |>
  filter(!is.na(los), episode_length < los) |>
  summarise(
    n = n(),
    mean_difference = mean(episode_length - los),
    median_difference = median(episode_length - los),
    min_difference = min(episode_length - los),
    max_difference = max(episode_length - los)
  )

# Coclusions:
# episode_length was complete for all records.
# When episode_length was shorter than LOS, the difference was uniformly -1 day,
# consistent with a difference in counting convention rather than implausible values.
# episode_length was longer than LOS in a subset of cases, as expected when the
# administrative hospital episode includes periods not counted in SwissDRG LOS.

# Episode length should not be negative
if (any(discharges_all$episode_length < 0, na.rm = TRUE)) {
  warning("Negative values detected in episode_length.")
}

# Data quality check: discharge_month
n_missing <- sum(is.na(discharges_all$discharge_month))

if (n_missing > 0) {
  stop(
    glue::glue(
      "{n_missing} records without discharge_month found. Dataset may contain non-discharge records."
    )
  )
} else {
  message(
    glue::glue(
      "Data quality check passed: discharge_month available for all {scales::comma(nrow(discharges_all))} records."
    )
  )
}

# ******************************************************************************
## Save fst----
# ******************************************************************************
# Save fst of analysis dataset
discharges_all_reduced <- discharges_all  
  # %>% select(hospital_id_num, 
  #       hospital_site_id_num
  #        patient_id,
  #        gender,
  #        insurance_class,
  #        discharge_decision,
  #        discharge_month,
  #        post_discharge_location,
  #        post_discharge_treatment,
  #        discharge_to_death_days)

head(discharges_all_reduced)

# speichern
output_fst <- common_file("discharges")
write_fst(discharges_all_reduced, output_fst)
message(glue("Output saved: {basename(output_fst)}"))




# ******************************************************************************
# DIAGNOSES ----
# ______________________________________________________________________________

# ******************************************************************************
## Read data ----
# ******************************************************************************
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

# ******************************************************************************
## Descriptive analysis raw data ----
# ******************************************************************************

# ICD codes by year
freq_table(
  data = icd_all |>
    mutate(survey_year = factor(survey_year, levels = start_year:end_year)),
  var = icd, by = survey_year, totals = "both")

freq_table(  
  data = icd_all |>
    mutate(survey_year = factor(survey_year, levels = start_year:end_year)),
  var = icd, by = survey_year, totals = "both")     |>   arrange(desc(Total)) |> head()

freq_table(
  data = icd_all |>
    mutate(survey_year = factor(survey_year, levels = start_year:end_year)),
    var = icd_add, by = survey_year, totals = "both") |>   arrange(desc(Total)) |> head()

freq_table(
  data = icd_all |>
    mutate(survey_year = factor(survey_year, levels = start_year:end_year)),
  var = poa, by = survey_year, totals = "both") 

# ******************************************************************************
## Standardise ICD codes for indicator matching ----
# ******************************************************************************
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

# ******************************************************************************
## save fst ----
# ******************************************************************************
output_fst <- common_file("diagnoses")
write_fst(icd_all, output_fst)
message(glue("Output saved: {basename(output_fst)}"))

# ******************************************************************************
# PROCEDURES (CHOP) ----
# ______________________________________________________________________________

# ******************************************************************************
## Read data ----
# ******************************************************************************
# generate empty list 
chop_filelist <- list() 

for (year_analysis in start_year:end_year) {
  message(glue::glue("Reading CHOP {year_analysis}"))
  
  # get filepath from list defined in setup
  chop_filelist[[as.character(year_analysis)]] <-
    fread(path_chop[[as.character(year_analysis)]]) |>  # read in data
    janitor::clean_names()                              # clean names
}

# combine all years into one dataset
chop_all <- bind_rows(chop_filelist)

# have a glimpse
glimpse(chop_all)

# ******************************************************************************
## Descriptive analysis raw data ----
# ******************************************************************************
# wrapped in print() to makes its result display when the script is run with source()
# 
freq_table(
  data = chop_all |> mutate(survey_year = factor(survey_year,levels = start_year:end_year)),
  var = chop,
  by = survey_year,
  totals = "both"
) |>
  arrange(desc(Total)) |>
  head(20)

freq_table(data = chop_all, var = survey_year)

# ******************************************************************************
## Standardise CHOP codes for indicator matching ----
# ******************************************************************************
chop_all <- chop_all |>
  mutate(
    chop_clean = chop |>
      str_to_upper() |>              # convert to upper case
      str_remove_all("\\.") |>       # remove dots
      str_remove_all("\\s+")         # remove spaces
  )

# ******************************************************************************
## Save fst ----
# ******************************************************************************
output_fst <- common_file("procedures")
write_fst(chop_all, output_fst)
message(glue("Output saved: {basename(output_fst)}"))

# ******************************************************************************
# ICU ----
# ______________________________________________________________________________

# ******************************************************************************
## Read data ----
# ******************************************************************************
# generate empty list 
icu_filelist <- list() 

for (year_analysis in start_year:end_year) {
  message(glue::glue("Reading ICU {year_analysis}"))
  
  # get filepath from list defined in setup
  icu_filelist[[as.character(year_analysis)]] <-
    fread(path_icu_raw[[as.character(year_analysis)]]) |>  # read in data
    janitor::clean_names()                                   # clean names
}

# combine all years into one dataset after harmonising changing code variables
icu_all <- icu_filelist |>
  bind_rows()

glimpse(icu_all)


# ******************************************************************************
## Save fst ----
# ******************************************************************************
output_fst <- common_file("icu")
write_fst(icu_all, output_fst)
message(glue("Output saved: {basename(output_fst)}"))


# ******************************************************************************
# Completion message ----
# ______________________________________________________________________________

message(
  "Script '01_prepare_gvs.R' completed. Frequency tables are not printed when the script is sourced; run those commands interactively to inspect them."
)