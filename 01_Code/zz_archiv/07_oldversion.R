# ------------------------------------------------------------------------------
# Load & join data  
# ------------------------------------------------------------------------------

# Create a List for datasets per year
sepsis_cases_ <- list()

cod_data <- read_fst(file.path(root, "04_Daten", "cod_2019_2024.fst")) 
path_discharges  <- file.path(root, "04_Daten", glue("discharges_2019_2024.fst"))

for (year_analysis in 2019:2024) {
  
  path_icd         <- file.path(root, "04_Daten", glue("flag_icd_{year_analysis}.fst"))
  path_discharges  <- file.path(root, "04_Daten", glue("discharges_{year_analysis}.fst"))
  
  icd_flag   <- read_fst(path_icd)
  discharges <- read_fst(path_discharges) |>
    mutate(patient_id = na_if(trimws(patient_id), ""))
  
  sepsis_cases_[[as.character(year_analysis)]] <- discharges |>
    left_join(icd_flag, by = "encounter_id") |>
    filter(flag_sepsis > 0) |>                                    # 1=sepsis in diagnosis variable, 2= sepsis in "Zusatz zu Hauptdiagnose" 
    mutate(
      year_analysis = year_analysis,
      indicator = "sepsis"
    ) |>
    left_join(cod_data, by = c("patient_id" = "ahv_n")) |>
    mutate(match_cod = if_else(!is.na(ereignis_ges_d), 1L, 0L)    # flag that match with CoD was found
    )
}


# --------------------------------------------
# Create one dataset (harmonize formats)
# --------------------------------------------

# Overview of list
purrr::imap(sepsis_cases_, ~ tibble(
  year = .y,
  n_rows = nrow(.x),
  n_cols = ncol(.x)
)) |> 
  bind_rows()


# Check for format inconsistencies
purrr::map_dfr(
  sepsis_cases_,
  ~ tibble::tibble(
    variable = names(.x),
    class = purrr::map_chr(.x, ~ class(.x)[1])
  ),
  .id = "year"
) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = class
  ) |> print(n=Inf)

# Create one dataset (harmonize formats)
sepsis_cases <- sepsis_cases_ |>
  purrr::map(~ .x |>
               mutate(
                 across(c(residence_country), as.character),
                 across(c(nationality), as.character),
                 across(c(splg), as.character)
               )
  ) |>
  bind_rows()

# Have a look
glimpse(sepsis_cases)
freq_table(data=sepsis_cases, var=nationality)


# ------------------------------------------------------------------------------
# Validation Linkage
# ------------------------------------------------------------------------------

# Todesfälle im Spital
linkage_inhospital <- sepsis_cases |>
  filter(discharge_decision == 5) |>
  mutate(
    match_bevnat = !is.na(discharge_to_death_days),
    match_cod = match_cod == 1
  ) |>
  count(match_bevnat, match_cod)
linkage_inhospital


# Werte für discharge_to_death_days bei Todesfällen im Spital
sepsis_cases |>
  filter(discharge_decision == 5) |>
  freq_table(var=discharge_to_death_days)

# Todesfälle ausserhalb Spital
linkage_outofhospital_pct <- sepsis_cases |>
  filter(!is.na(discharge_to_death_days)) |>
  summarise(
    n = n(),
    cod_match = sum(match_cod == 1),
    pct_match = round(100 * cod_match / n, 1),
    .by = survey_year
  )
linkage_outofhospital_pct

sepsis_cases |> 
  filter(!is.na(discharge_to_death_days),
         discharge_decision != 5) |> 
  freq_table(discharge_to_death_days)



test <- sepsis_cases %>%
  filter(match_cod == 1L & is.na(discharge_to_death_days)) |> 
  select(discharge_to_death_days, survey_year, discharge_month, nationality, residence_country, ereignis_ges_d  )

test
glimpse(cod_data)

# Plot
library(dplyr)
library(ggplot2)
library(discharge)

tab_match_heatmap <- sepsis_cases |>
  filter(
    !is.na(discharge_to_death_days),
    discharge_decision != 5
  ) |>
  mutate(
    death_days_group = cut(
      discharge_to_death_days,
      breaks = c(0, 30, 60, 90, 180, 365, Inf),
      right = FALSE,
      labels = c(
        "0-29 Tage",
        "30-59 Tage",
        "60-89 Tage",
        "90-179 Tage",
        "180-364 Tage",
        "365+ Tage"
      )
    )
  ) |>
  group_by(year_analysis, discharge_month, death_days_group) |>
  summarise(
    n_cases = n(),
    matched_cod = sum(match_cod == 1, na.rm = TRUE),
    no_match_cod = sum(match_cod == 0, na.rm = TRUE),
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

ggplot(
  tab_match_heatmap,
  aes(
    x = factor(discharge_month_num,
               levels = 1:12,
               labels = month.abb),
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
          "\n(", scales::comma(no_match_cod), ")"
        ),
        ""
      )
    ),
    size = 3,
    lineheight = 0.9
  ) +
  facet_wrap(~year_analysis, ncol = 3) +
  scale_fill_manual(
    values = c(
      "<50%"   = "#d73027",
      "50-79%" = "#fc8d59",
      "80-89%" = "#fee08b",
      "90-94%" = "#d9ef8b",
      "95-99%" = "#91cf60",
      "100%"   = "#1a9850"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Erfolgreicher Match mit CoD-Dtaen nach Austrittsmonat und Anzahl Tagen zwischen Austritt und Todesfall",
    subtitle = "Nur Todesfälle nach Spitalaustritt, in Klammer Anzahl unmatched Todesfälle",
    x = "Austrittsmonat",
    y = "Tage zwischen Austritt und Tod",
    fill = "Matchrate"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank())

# gemäss Abklärung mit Gian-Paolo ist dieser wert geringer, weil BEVNAT inkl. 2025 verwendet wurde 
# und somit bereits Todes Fälle aus 2025 enthält


# ===================================================
# Exclusion criteria (and Log)
# ===================================================
analysis_data <- sepsis_cases

log_exclusion_analysis <- analysis_data %>%
  count(year_analysis, name = "included_start") %>%
  mutate(
    criteria = "cases with trigger ICD for sepsis",
    excluded = 0L,
    included = included_start
  ) %>%
  rename(year = year_analysis) %>%
  select(criteria, year, excluded, included)

# 1) CoD-Match, aber kein BEVNAT-Todesdatum
n_vorher <- analysis_data %>%
  count(year_analysis, name = "n_before")

analysis_data <- analysis_data %>%
  filter(!(match_cod == 1L & is.na(discharge_to_death_days)))

n_nachher <- analysis_data %>%
  count(year_analysis, name = "included")

log_exclusion_analysis <- bind_rows(
  log_exclusion_analysis,
  n_vorher %>%
    full_join(n_nachher, by = "year_analysis") %>%
    mutate(
      criteria = "exclude CoD matches without BEVNAT match (discharge_to_death_days = NA)",
      included = coalesce(included, 0L),
      excluded = n_before - included
    ) %>%
    rename(year = year_analysis) %>%
    select(criteria, year, excluded, included)
)

# print and save log exclusion
print(log_exclusion_analysis)
save(
  log_exclusion_analysis,
  file = file.path(path_output, glue::glue("log_exclusion_analysis_{datestamp}.RData"))
)

# final analysis data
sepsis_cases_analysis <- analysis_data

# ------------------------------------------------------------------------------
# Datenaufbereitung
# ------------------------------------------------------------------------------
glimpse(sepsis_cases)

sepsis_cases <- sepsis_cases %>%
  mutate(
    death = as.integer(
      discharge_decision == 5 |
        !is.na(discharge_to_death_days) 
    )
  )

# ------------------------------------------------------------------------------
# Datenaufbereitung
# ------------------------------------------------------------------------------
# unberücksichtigt bleiben somit die 5 Fälle mit Match bei CoD, bei denen aber discharge_to_death_days fehlt