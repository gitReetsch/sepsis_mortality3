# ===================================================
# Link discharges with cause of death data
# ===================================================

# ---------------------------------------------------
# read data
# ---------------------------------------------------
discharges_all <- read_fst(file.path(root, "04_Daten", glue("discharges_{start_year}_{end_year}.fst")))
cod_all <- read_fst(file.path(root, "04_Daten", glue("cod_{start_year}_{end_year}.fst")))

glimpse(discharges_all)
glimpse(cod_all)


# ===================================================
# prepare linkage keys
# ===================================================
discharges_all <- discharges_all |>
  mutate(patient_id = na_if(str_trim(as.character(patient_id)), ""))

cod_all <- cod_all |>
  mutate(ahv_n = na_if(str_trim(as.character(ahv_n)), ""))


# ===================================================
# QA before linkage
# ===================================================

qa_linkage_input <- tibble(
  dataset = c("discharges", "cod"),
  n_rows = c(nrow(discharges_all), nrow(cod_all)),
  n_missing_link_id = c(
    sum(is.na(discharges_all$patient_id)),
    sum(is.na(cod_all$ahv_n))
  ),
  n_distinct_link_id = c(
    n_distinct(discharges_all$patient_id, na.rm = TRUE),
    n_distinct(cod_all$ahv_n, na.rm = TRUE)
  )
)

print(qa_linkage_input)

# bis wann reichen todesdaten der BEVNAT
analysis_followup <- discharges_all |>
  mutate(
    discharge_date =ymd(paste0(discharge_month, "01")),
    observed_death_date = discharge_date + discharge_to_death_days
  )

followup_summary <- analysis_followup |>
  summarise(
    earliest_possible_last_death  = max(discharge_date + discharge_to_death_days, na.rm = TRUE),
    maximum_followup_days         = max(discharge_to_death_days, na.rm = TRUE)
  )

followup_summary

# ===================================================
# check uniqueness of CoD linkage key
# ===================================================

cod_duplicates <- cod_all |>
  filter(!is.na(ahv_n)) |>
  count(ahv_n, name = "n_cod_records") |>
  filter(n_cod_records > 1)

if (nrow(cod_duplicates) > 0) {
  stop(
    glue(
      "{nrow(cod_duplicates)} duplicate CoD linkage IDs detected. ",
      "Linkage would duplicate discharge records. Stopping."
    )
  )
}

# if warning triggered, keep all CoD records for now; duplicates are documented but not removed


# ===================================================
# link CoD data to discharges
# ===================================================

discharges_cod <- discharges_all |>
  left_join(
    cod_all,
    by = c("patient_id" = "ahv_n"),      # link hospital discharges to CoD records
    relationship = "many-to-one",        # each patient has at most one CoD record
    na_matches = "never"                 # do not match missing linkage IDs
  )

glimpse(discharges_cod)

# Add one row-count check after linkage
if (nrow(discharges_cod) != nrow(discharges_all)) {
  stop("Linkage changed the number of discharge records.")}

# Check how many hospital records could be linked to CoD data.
discharges_cod |>
  summarise(
    n = n(),
    matched_cod = sum(!is.na(death_date)),
    pct_match = round(100 * matched_cod / n, 2)
  )


# ===================================================
# derive death timing
# ===================================================

# Harmonise time-to-death information for later indicator analyses.
discharges_cod <- discharges_cod |>
  mutate(
    discharge_to_death_days_raw = discharge_to_death_days,
    discharge_to_death_days = case_when(
      !is.na(discharge_to_death_days_raw) ~ as.numeric(discharge_to_death_days_raw),
      discharge_decision == 5 ~ 0,
      TRUE ~ NA_real_
    )
  )


# ===================================================
# create death flags
# ===================================================

# Define source-specific death flags and the analytical death flag.
discharges_cod <- discharges_cod |>
  mutate(
    death_gvs = as.integer(!is.na(discharge_decision) & discharge_decision == 5),
    death_bevnat = as.integer(!is.na(discharge_to_death_days_raw)),
    death_cod = as.integer(!is.na(death_date)),
    death_any_source = as.integer(death_gvs == 1 | death_bevnat == 1 | death_cod == 1),
    death = as.integer(death_gvs == 1 | death_bevnat == 1)
  )


# ===================================================
# Validate overlap of death sources by residence
# ===================================================

validation_death_sources <- discharges_cod |>
  mutate(
    residence_group = if_else(
      as.character(residence_country) == "8100",
      "Swiss residents",
      "Non-Swiss residents"
    )
  ) |>
  count(
    residence_group,
    death_gvs,
    death_bevnat,
    death_cod,
    death_any_source,
    death,
    name = "n"
  ) |>
  arrange(
    residence_group,
    desc(n)
  )

validation_death_sources

# ===================================================
# Validate overlap of death sources
# ===================================================

# Check how deaths are identified across the three available data sources.
validation_death_sources <- discharges_cod |>
  group_by(
    death_gvs,
    death_bevnat,
    death_cod,
    death_any_source,
    death
  ) |>
  summarise(
    n = n(),
    n_non_swiss = sum(as.character(residence_country) != "8100", na.rm = TRUE),
    pct_non_swiss = 100 * n_non_swiss / n,
    .groups = "drop"
  ) |>
  mutate(
    interpretation = case_when(
      death_gvs == 0 & death_bevnat == 0 & death_cod == 0 ~ "No death recorded in any data source.",
      death_gvs == 0 & death_bevnat == 1 & death_cod == 1 ~ "Out-of-hospital death consistently identified by BEVNAT and the cause-of-death statistics.",
      death_gvs == 0 & death_bevnat == 1 & death_cod == 0 ~ "Out-of-hospital death without a CoD match.",
      death_gvs == 1 & death_bevnat == 1 & death_cod == 1 ~ "In-hospital death consistently identified across all three sources.",
      death_gvs == 1 & death_bevnat == 0 & death_cod == 0 ~ "In-hospital death without BEVNAT or CoD record.",
      death_gvs == 1 & death_bevnat == 1 & death_cod == 0 ~ "In-hospital death with BEVNAT but without CoD record.",
      death_gvs == 0 & death_bevnat == 0 & death_cod == 1 ~ "CoD-linked death without corresponding death information in GVS or BEVNAT for this hospital record.",
      death_gvs == 1 & death_bevnat == 0 & death_cod == 1 ~ "In-hospital death confirmed by both GVS and the cause-of-death statistics, but without BEVNAT death information.",
      TRUE ~ "Unexpected combination of death source flags."
    ),
    validation_findings = case_when(
      death_gvs == 0 & death_bevnat == 0 & death_cod == 0 ~ "No further validation required.",
      death_gvs == 0 & death_bevnat == 1 & death_cod == 1 ~ "No further validation required.",
      death_gvs == 0 & death_bevnat == 1 & death_cod == 0 ~ "According to clarification with GESV, the BEVNAT linkage used death information up to and including 2025, whereas CoD data were only available through 2024 for this analysis. This explains why out-of-hospital deaths may be present in BEVNAT but not in CoD. This pattern is supported by the validation plot below.",
      death_gvs == 1 & death_bevnat == 1 & death_cod == 1 ~ "No further validation required.",
      death_gvs == 1 & death_bevnat == 0 & death_cod == 0 ~ "Validation showed that 97.9% of these records lacked a linkage identifier (patient_id), preventing linkage to the CoD statistics.",
      death_gvs == 1 & death_bevnat == 1 & death_cod == 0 ~ "According to clarification with GESV, the BEVNAT linkage used death information up to and including 2025, whereas CoD data were only available through 2024 for this analysis. This explains why in-hospital deaths with BEVNAT information may be present without a CoD match. This pattern is supported by the validation plot below.",
      death_gvs == 0 & death_bevnat == 0 & death_cod == 1 ~ "Validation showed that these 833 hospital records corresponded to only 161 individuals, reflecting multiple hospitalisations per person. Person-level consistency checks confirmed that both CoD and BEVNAT death information is consistently propagated to all hospital records of linked individuals. Consequently, the absence of a BEVNAT flag cannot be explained by person-level linkage to another hospital record. The remaining discrepancy could not be resolved using the available data.",
      death_gvs == 1 & death_bevnat == 0 & death_cod == 1 ~ "This combination is internally consistent between GVS and the cause-of-death statistics but indicates a missing BEVNAT linkage. As the date of death is available from the CoD statistics, the time from discharge to death can be calculated retrospectively if required for indicator construction.",
      TRUE ~ "Review required."
    )
  ) |>
  arrange(desc(n))


# ===================================================
# Show validation table
# ===================================================

# Display the validation table together with the analytical death definition.
validation_readme <- tibble::tribble(
  ~Section, ~Variable, ~Definition, ~Description,
  
  "Definition of input variables used to define death",
  "death_gvs",
  "as.integer(!is.na(discharge_decision) & discharge_decision == 5)",
  "Death recorded in the hospital discharge statistics (GVS).",
  
  "Definition of input variables used to define death",
  "death_bevnat",
  "as.integer(!is.na(discharge_to_death_days_raw))",
  "Death identified through the BEVNAT linkage.",
  
  "Definition of input variables used to define death",
  "death_cod",
  "as.integer(!is.na(death_date))",
  "Death identified through the cause-of-death statistics.",
  
  "Definition of input variables used to define death",
  "death_any_source",
  "as.integer(death_gvs == 1 | death_bevnat == 1 | death_cod == 1)",
  "Validation flag indicating that at least one data source identified a death.",
  
  "Definition of target variable 'death'",
  "death",
  "as.integer(death_gvs == 1 | death_bevnat == 1)",
  "Analytical death flag used in all subsequent analyses. In-hospital deaths are identified by discharge_decision == 5. Out-of-hospital deaths are identified by a non-missing discharge_to_death_days (original variable, suffix '_raw'). CoD-only deaths are intentionally excluded because the temporal relationship to the index hospitalisation cannot be established reliably."
)


# ===================================================
# Checks / proofs
# ===================================================

# Check whether GVS-only deaths are explained by missing linkage identifiers.
validation_gvs_only_missing_id <- discharges_cod |>
  filter(
    death_gvs == 1,
    death_bevnat == 0,
    death_cod == 0
  ) |>
  summarise(
    n = n(),
    missing_patient_id = sum(is.na(patient_id)),
    pct_missing_patient_id = round(100 * mean(is.na(patient_id)), 1)
  )

print(validation_gvs_only_missing_id)


# Check how many persons are represented by CoD-only hospital records.
validation_cod_only_persons <- discharges_cod |>
  filter(
    death_gvs == 0,
    death_bevnat == 0,
    death_cod == 1
  ) |>
  summarise(
    n_records = n(),
    n_persons = n_distinct(patient_id),
    mean_records_per_person = round(n() / n_distinct(patient_id), 2)
  )

print(validation_cod_only_persons)


# Check how many persons are represented by GVS and CoD records without BEVNAT.
validation_gvs_cod_without_bevnat <- discharges_cod |>
  filter(
    death_gvs == 1,
    death_bevnat == 0,
    death_cod == 1
  ) |>
  summarise(
    n_records = n(),
    n_persons = n_distinct(patient_id),
    mean_records_per_person = round(n() / n_distinct(patient_id), 2)
  )

print(validation_gvs_cod_without_bevnat)


# Check whether BEVNAT death information is consistently attached to all hospital records of a linked person.
validation_bevnat_consistency <- discharges_cod |>
  filter(!is.na(patient_id)) |>
  group_by(patient_id) |>
  summarise(
    min_bevnat = min(death_bevnat, na.rm = TRUE),
    max_bevnat = max(death_bevnat, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) |>
  count(
    min_bevnat,
    max_bevnat
  )

print(validation_bevnat_consistency)


# Check whether CoD death information is consistently attached to all hospital records of a linked person.
validation_cod_consistency <- discharges_cod |>
  filter(!is.na(patient_id)) |>
  group_by(patient_id) |>
  summarise(
    min_cod = min(death_cod, na.rm = TRUE),
    max_cod = max(death_cod, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) |>
  count(
    min_cod,
    max_cod
  )

print(validation_cod_consistency)


# ===================================================
# Check: BEVNAT deaths without CoD match
# ===================================================

# Validate whether missing CoD matches are explained by the limited CoD observation period (only Swiss residents).
validation_bevnat_cod_match <- discharges_cod |>
  filter(
    death_bevnat == 1,
    as.character(residence_country) == "8100",
    !is.na(patient_id)  
    ) |>
  mutate(death_days_group = cut(
          discharge_to_death_days_raw,
          breaks = c(-Inf, 0, 30, 60, 90, 180, 365, 730, 1095, Inf),
          right = FALSE,
          labels = c(
            "<0 days",
            "0-29 days",
            "30-59 days",
            "60-89 days",
            "90-179 days",
            "180-364 days",
            "365-729 days",
            "730-1094 days",
            "1095+ days"
          ))) |>
  group_by(survey_year, discharge_month,death_days_group) |>
  summarise(
    n_cases = n(),
    matched_cod = sum(death_cod == 1, na.rm = TRUE),
    no_match_cod = sum(death_cod == 0, na.rm = TRUE),
    match_rate = matched_cod / n_cases,
    .groups = "drop"
  ) |>
  mutate(
    match_class = cut(
      match_rate,
      breaks = c(0, 0.5, 0.8, 0.9, 0.95, 0.99, 1.00001),
      labels = c("<50%", "50-79%", "80-89%", "90-94%", "95-99%", "100%"),
      include.lowest = TRUE
    ),
    discharge_month_num = as.integer(substr(as.character(discharge_month), 5, 6))
  )

plot_bevnat_cod_match <- ggplot(
  validation_bevnat_cod_match,
  aes(
    x = factor(
      discharge_month_num,
      levels = 1:12,
      labels = month.abb
    ),
    y = death_days_group,
    fill = match_class
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = ifelse(
        match_rate < 1,
        paste0(
          scales::percent(match_rate, accuracy = 1),
          "\n(",
          scales::comma(no_match_cod),
          ")"
        ),
        ""
      )
    ),
    size = 3,
    lineheight = 0.9
  ) +
  facet_wrap(~survey_year, ncol = 3) +
  labs(
    title = "Validation of BEVNAT–CoD linkage",
    subtitle = "Only Swiss residents with valid patient_id: Cells show CoD match rate of ; numbers in brackets indicate BEVNAT deaths without CoD match",
    x = "Discharge month",
    y = "Days from discharge to death",
    fill = "CoD match rate"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

plot_bevnat_cod_match

ggsave(filename = file.path(root,"05_Ergebnisse", glue("validation_bevnat_cod_match_{datestamp}.png")),
  plot = plot_bevnat_cod_match, width = 12, height = 8, dpi = 300)

# ===================================================
# save validation output
# ===================================================

# Store all CoD linkage validation outputs in one Excel workbook.
validation_output_file <- file.path(
  root,
  "05_Ergebnisse",
  glue("validation_cod_linkage_{datestamp}.xlsx")
)

write_excel_tables_update(
  list(
    readme = validation_readme,
    death_sources = validation_death_sources,
    gvs_only_missing_id = validation_gvs_only_missing_id,
    cod_only_persons = validation_cod_only_persons,
    gvs_cod_without_bevnat = validation_gvs_cod_without_bevnat,
    bevnat_consistency = validation_bevnat_consistency,
    cod_consistency = validation_cod_consistency,
    bevnat_cod_match = validation_bevnat_cod_match
  ),
  file = validation_output_file
)

# Add validation plot as separate worksheet
plot_file <- file.path(
  root,
  "05_Ergebnisse",
  glue::glue("validation_bevnat_cod_match_{datestamp}.png")
)

write_excel_tables_update(
  list = list(
    bevnat_cod_plot = tibble::tibble(note = "")
  ),
  file = validation_output_file,
  autostyle = FALSE
)

wb <- openxlsx::loadWorkbook(validation_output_file)

openxlsx::insertImage(
  wb,
  sheet = "bevnat_cod_plot",
  file = plot_file,
  startRow = 1,
  startCol = 1,
  width = 12,
  height = 8
)

openxlsx::saveWorkbook(
  wb,
  validation_output_file,
  overwrite = TRUE
)

message(glue::glue("Output saved: {basename(validation_output_file)}"))


# ===================================================
# save linked data
# ===================================================
output_fst <- file.path(root,"04_Daten",glue("discharges_cod_{start_year}_{end_year}.fst"))
write_fst(discharges_cod, output_fst)
message(glue("Output saved: {basename(output_fst)}"))
