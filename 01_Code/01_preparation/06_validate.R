# ******************************************************************************
# 06_validate.R
# ******************************************************************************
#
# Purpose: Validate how GVS and BEVNAT death records match the CoD data.
#
# - Temporarily link prepared discharge data to prepared CoD data.
# - Summarise the overlap of death information across the three sources.
# - Examine CoD match rates among BEVNAT deaths for Swiss residents.
# - Save validation tables and a diagnostic plot for this run.
#
# The joined encounter data are used for validation only and are not saved.
#
# Outputs:
# - path_output/validation_cod_linkage_<datestamp>.xlsx: validation tables,
#   including death-source overlap and CoD match-rate counts.
# - path_output/validation_bevnat_cod_match_<datestamp>.png: CoD match-rate
#   heatmap among BEVNAT-linked discharge records.
# - diagnostics.pdf: the heatmap, when run through 00_run_preparation.R.
#
# The temporary discharge–CoD join is kept in memory and is not saved as a
# shared dataset.
#
# ******************************************************************************


# ******************************************************************************
# Read prepared data and prepare linkage IDs ----
# ******************************************************************************

discharges_all <- read_fst(common_file("discharges")) |>
  mutate(patient_id = na_if(trimws(as.character(patient_id)), "") )

cod_all <- read_fst(common_file("cod")) |>
  mutate(ahv_n = na_if(trimws(as.character(ahv_n)), ""))

if (anyDuplicated(discharges_all[c("survey_year", "encounter_id")])) {
  stop("Duplicate discharge encounter keys detected.")
}

cod_duplicates <- cod_all |>
  filter(!is.na(ahv_n)) |>
  count(ahv_n) |>
  filter(n > 1)

if (nrow(cod_duplicates) > 0L) {
  stop(
    nrow(cod_duplicates),
    " duplicate non-missing CoD linkage IDs detected. ",
    "The validation join could duplicate discharge records."
  )
}

# ******************************************************************************
# Temporarily link CoD data and derive source flags ----
# ******************************************************************************
n_discharges_before <- nrow(discharges_all)

discharges_cod <- discharges_all |>
  left_join(
    cod_all,
    by = c("patient_id" = "ahv_n"),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(
    death_gvs = as.integer(
      !is.na(discharge_decision) & discharge_decision == 5
    ),
    death_bevnat = as.integer(!is.na(discharge_to_death_days)),
    death_cod = as.integer(!is.na(death_date)),
    death_any_source = as.integer(
      death_gvs == 1L | death_bevnat == 1L | death_cod == 1L
    ),
    death = as.integer(death_gvs == 1L | death_bevnat == 1L)
  )

if (nrow(discharges_cod) != n_discharges_before) {
  stop("The CoD join changed the number of discharge records.")
}


# ******************************************************************************
# Summarise overlap between death sources ----
# ******************************************************************************

death_source_patterns <- tibble::tribble(
  ~death_gvs, ~death_bevnat, ~death_cod, ~pattern,
  0L, 0L, 0L, "No death flag",
  0L, 0L, 1L, "CoD only",
  0L, 1L, 0L, "BEVNAT only",
  0L, 1L, 1L, "BEVNAT and CoD",
  1L, 0L, 0L, "GVS only",
  1L, 0L, 1L, "GVS and CoD",
  1L, 1L, 0L, "GVS and BEVNAT",
  1L, 1L, 1L, "All three sources"
)

validation_death_sources <- discharges_cod |>
  mutate(
    residence_group = case_when(
      as.character(residence_country) == "8100" ~ "Swiss residents",
      !is.na(residence_country) ~ "Non-Swiss residents",
      TRUE ~ "Unknown residence"
    )
  ) |>
  count(
    residence_group,
    death_gvs,
    death_bevnat,
    death_cod,
    name = "n_records"
  ) |>
  left_join(
    death_source_patterns,
    by = c("death_gvs", "death_bevnat", "death_cod")
  ) |>
  group_by(residence_group) |>
  mutate(
    percent_within_residence = round(100 * n_records / sum(n_records), 2)
  ) |>
  ungroup() |>
  arrange(residence_group, desc(n_records))


# ******************************************************************************
# Check selected source combinations ----
# ******************************************************************************

validation_gvs_only_missing_id <- discharges_cod |>
  filter(
    death_gvs == 1L,
    death_bevnat == 0L,
    death_cod == 0L
  ) |>
  summarise(
    n_records = n(),
    n_missing_patient_id = sum(is.na(patient_id)),
    percent_missing_patient_id = round(100 * mean(is.na(patient_id)), 1)
  )

validation_cod_only_persons <- discharges_cod |>
  filter(
    death_gvs == 0L,
    death_bevnat == 0L,
    death_cod == 1L
  ) |>
  summarise(
    n_records = n(),
    n_persons = n_distinct(patient_id, na.rm = TRUE),
    mean_records_per_person = if (n_persons == 0L) {
      NA_real_
    } else {
      round(n_records / n_persons, 2)
    }
  )

validation_gvs_cod_without_bevnat <- discharges_cod |>
  filter(
    death_gvs == 1L,
    death_bevnat == 0L,
    death_cod == 1L
  ) |>
  summarise(
    n_records = n(),
    n_persons = n_distinct(patient_id, na.rm = TRUE),
    mean_records_per_person = if (n_persons == 0L) {
      NA_real_
    } else {
      round(n_records / n_persons, 2)
    }
  )


# ******************************************************************************
# Check CoD matches among BEVNAT deaths in Swiss residents ----
# ******************************************************************************

validation_bevnat_cod_match <- discharges_cod |>
  filter(
    death_bevnat == 1L,
    as.character(residence_country) == "8100",
    !is.na(patient_id)
  ) |>
  mutate(
    death_days_group = case_when(
      discharge_to_death_days <= -31 ~ "31+ days before discharge",
      discharge_to_death_days < 0    ~ "1–30 days before discharge",
      discharge_to_death_days < 30   ~ "0–29 days",
      discharge_to_death_days < 60   ~ "30–59 days",
      discharge_to_death_days < 90   ~ "60–89 days",
      discharge_to_death_days < 180  ~ "90–179 days",
      discharge_to_death_days < 365  ~ "180–364 days",
      discharge_to_death_days < 730  ~ "365–729 days",
      discharge_to_death_days < 1095 ~ "730–1094 days",
      TRUE                           ~ "1095+ days"
    ),
    death_days_group = factor(
      death_days_group,
      levels = c(
        "31+ days before discharge",
        "1–30 days before discharge",
        "0–29 days",
        "30–59 days",
        "60–89 days",
        "90–179 days",
        "180–364 days",
        "365–729 days",
        "730–1094 days",
        "1095+ days"
      )
    )
  ) |>
  summarise(
    n_cases = n(),
    matched_cod = sum(death_cod == 1L),
    no_match_cod = sum(death_cod == 0L),
    match_rate = matched_cod / n_cases,
    .by = c(survey_year, discharge_month, death_days_group)
  ) |>
  mutate(
    match_class = cut(
      match_rate,
      breaks = c(0, 0.5, 0.8, 0.9, 0.95, 0.99, 1.00001),
      labels = c("<50%", "50–79%", "80–89%", "90–94%", "95–99%", "100%"),
      include.lowest = TRUE
    ),
    discharge_month_num = as.integer(
      substr(as.character(discharge_month), 5, 6)
    )
  )

plot_bevnat_cod_match <- ggplot(
  validation_bevnat_cod_match,
  aes(
    x = factor(discharge_month_num, levels = 1:12, labels = month.abb),
    y = death_days_group,
    fill = match_class
  )
) +
  
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = paste0(
        scales::percent(match_rate, accuracy = 1),
        "\n(",
        scales::comma(n_cases, accuracy=1),
        ")"
      )
    ),
    size = 2.8,
    lineheight = 0.9
  ) +
  facet_wrap(~survey_year, ncol = 3) +
  scale_fill_manual(
    values = c(
      "<50%" = "#d73027",
      "50–79%" = "#fc8d59",
      "80–89%" = "#fee08b",
      "90–94%" = "#d9ef8b",
      "95–99%" = "#91cf60",
      "100%" = "#1a9850"
    ),
    drop = FALSE
  ) +
  labs(
    title = "CoD match rate among BEVNAT deaths",
    subtitle = paste(
      "Cell labels show match rate and number of linked discharge records (n)."
    ),
    x = "Discharge month",
    y = "Days from discharge to death",
    fill = "CoD match rate",
    caption = paste(
        "Each tile shows the CoD match rate and the number of BEVNAT-linked discharge records (n).",
        "The analysis is restricted to Swiss residents with a non-missing patient ID; CoD matches use the GVS patient ID and CoD AHV identifier.",
        "The lower CoD match rates at long positive intervals are likely explained, at least partly, by the different coverage periods: BEVNAT includes deaths through 2025, whereas the CoD data end in 2024.",
        "Negative intervals near zero, especially −1 day, may reflect differences in date definitions or recording between BEVNAT and GVS.",
        "The very low CoD match rates for intervals more than 30 days before discharge may indicate discordant GVS–BEVNAT linkage. This warrants caution because, before 2024, GVS used a separate anonymised linkage code rather than AHV; the cause has not been confirmed."
        , sep = "\n")
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(plot_bevnat_cod_match)


# ******************************************************************************
# Save validation outputs ----
# ******************************************************************************

validation_readme <- tibble::tribble(
  ~variable, ~definition, ~description,
  "death_gvs",
  "discharge_decision == 5",
  "Death recorded in the GVS discharge data.",
  "death_bevnat",
  "!is.na(discharge_to_death_days)",
  "Death identified through the BEVNAT-linked interval already present in the discharge data.",
  "death_cod",
  "!is.na(death_date)",
  "Death identified through the cause-of-death data after linkage by patient ID.",
  "death_any_source",
  "death_gvs == 1 | death_bevnat == 1 | death_cod == 1",
  "Validation flag showing that at least one source identified a death.",
  "death",
  "death_gvs == 1 | death_bevnat == 1",
  "Analytical death definition. CoD-only records are excluded because their timing relative to the hospital encounter cannot be established reliably."
)

validation_plot_note <- tibble::tibble(
  note = "The CoD match-rate plot is embedded below."
)

validation_output_file <- file.path(
  path_output,
  glue::glue("validation_cod_linkage_{datestamp}.xlsx")
)

plot_file <- file.path(
  path_output,
  glue::glue("validation_bevnat_cod_match_{datestamp}.png")
)

ggsave(
  filename = plot_file,
  plot = plot_bevnat_cod_match,
  width = 12,
  height = 8,
  dpi = 300
)

write_excel_tables_update(
  list(
    readme = validation_readme,
    death_source_overlap = validation_death_sources,
    gvs_only_missing_id = validation_gvs_only_missing_id,
    cod_only_persons = validation_cod_only_persons,
    gvs_cod_without_bevnat = validation_gvs_cod_without_bevnat,
    bevnat_cod_match = validation_bevnat_cod_match,
    bevnat_cod_plot = validation_plot_note
  ),
  file = validation_output_file
)

wb <- openxlsx::loadWorkbook(validation_output_file)

openxlsx::insertImage(
  wb,
  sheet = "bevnat_cod_plot",
  file = plot_file,
  startRow = 3,
  startCol = 1,
  width = 12,
  height = 8
)

openxlsx::saveWorkbook(
  wb,
  validation_output_file,
  overwrite = TRUE
)

message(
  "Validation outputs saved: ",
  basename(validation_output_file),
  " and ",
  basename(plot_file)
)

# Checks
#
# 1. Input keys and join integrity
# - Discharge encounter keys (survey_year + encounter_id) must be unique.
# - Non-missing CoD linkage IDs (ahv_n) must be unique.
# - Joining CoD to discharges must not increase the number of discharge records.
# - Blank linkage IDs are treated as missing and therefore do not match.
#
# 2. Overlap between death sources
# It counts records for each combination of:
#   - GVS: discharge_decision == 5
# - BEVNAT: discharge_to_death_days is present
# - CoD: a linked death_date is present
# The counts are split by residence group and shown as percentages within each group.
#
# 3. Selected mismatches
# - For GVS-only deaths: how many have a missing patient_id?
#   - For CoD-only deaths: how many discharge records and how many distinct people?
#   - For GVS-and-CoD deaths without BEVNAT: the same record and person counts.
#
# 4. CoD matching among BEVNAT deaths
# For Swiss residents with a BEVNAT death interval and a valid patient_id, 
# it calculates CoD match rates by survey year, discharge month and interval-to-death group. 
# The plot shows the match rate and number of unmatched records.
#
# Outputs
# It saves an Excel workbook with the validation tables and embedded plot, plus a separate PNG plot. 
# The joined discharge data are temporary; they are not saved.
# 
# The script does not validate the correctness of the BEVNAT interval itself or
# independently verify each death date. It uses the interval already stored in 
# he discharge data and checks whether the person matches CoD.
