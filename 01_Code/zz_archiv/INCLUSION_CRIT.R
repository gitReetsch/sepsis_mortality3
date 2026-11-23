

discharges_all <- discharges_all |>
  mutate(
    death_hosp   = discharge_decision == 5
    death_bevnat = !is.na(discharge_to_death_days)
    death_cod    = match_cod == 1
    death        = ...
     ) 



# ===================================================
# Inclusion criteria
# ===================================================
# init
hosp <- discharges_all

log_inclusion <- tibble(
  criteria = "Start",
  year=year_analysis,
  excluded = 0,
  included = nrow(hosp)
)

# 1
n_vorher <- nrow(hosp)

hosp <- hosp %>% filter(!is.na(discharge_month))
log_inclusion <- bind_rows(
  log_inclusion,
  tibble(
    criteria = "discharge month ne missing",
    year=year_analysis,
    excluded = n_vorher - nrow(hosp),
    included = nrow(hosp)
  )
)

# 2
n_vorher <- nrow(hosp)

hosp <- hosp %>% filter(residence_country == "8100") 
log_inclusion <- bind_rows(
  log_inclusion,
  tibble(
    criteria = "country of residence = Switzerland",
    year=year_analysis,
    excluded = n_vorher - nrow(hosp),
    included = nrow(hosp)
  )
)

# 3
n_vorher <- nrow(hosp)

hosp <- hosp %>% filter(!service_centre_main %in% c("M500", "M950"))
log_inclusion <- bind_rows(
  log_inclusion,
  tibble(
    criteria = "main service center not 'M500' or 'M950'",
    year=year_analysis,
    excluded = n_vorher - nrow(hosp),
    included = nrow(hosp)
  )
)

# 4
n_vorher <- nrow(hosp)

hosp <- hosp %>%
  mutate(patient_id = na_if(trimws(patient_id), "")) %>% # auch strings mit nur leerezeichen filtern
  filter(!is.na(patient_id))

log_inclusion <- bind_rows(
  log_inclusion,
  tibble(
    criteria = "patient_id available",
    year = year_analysis,
    excluded = n_vorher - nrow(hosp),
    included = nrow(hosp)
  )
)

# 5
n_vorher <- nrow(hosp)

hosp <- hosp %>%
  filter(
    is.na(post_discharge_location) |       # auch NAs behalten
      !post_discharge_location %in% c(6, 66))

log_inclusion <- bind_rows(
  log_inclusion,
  tibble(
    criteria = "no transfers to other acute care hospital (post_discharge_location=6) or other acute care division of the same hospital (post_discharge_location=66) (if not excluded the case would be counted twice, assumed the patient is recorded again)",
    year=year_analysis,
    excluded = n_vorher - nrow(hosp),
    included = nrow(hosp)
  )
)
# 1  = Zuhause
# 2  = Krankenheim, Pflegeheim
# 3  = Altersheim, andere sozialmed. Institution
# 4  = Psychiatrische Klinik, anderer Betrieb
# 44 = Psychiatrische Abteilung/Klinik, gleicher Betrieb
# 5  = Rehabilitationsklinik, anderer Betrieb
# 55 = Rehabilitationsabteilung/-klinik, gleicher Betrieb
# 6  = anderes Krankenhaus (Akutspital) oder Geburtshaus
# 66 = Akutabteilung/-klinik,  gleicher Betrieb
# 7  = Strafvollzugsanstalt
# 8  = andere
# 9  = unbekannt
# 0  = Todesfall


# =====================================================================
# Save inclusion log
# =====================================================================
# print final table
print(log_inclusion)

# save
output_log <- file.path(path_output, glue("log_inclusion_{year_analysis}_{datestamp}.RData"))
save(log_inclusion, file = output_log)
message(glue("Inclusion log saved: {basename(output_log)}"))

