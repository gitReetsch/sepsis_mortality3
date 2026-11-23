
# ------------------------------------------------------------------------------
# Test linkage 
# ------------------------------------------------------------------------------
# Discharge data 
file_path <- file.path(root, "03_Inputdaten", "INPATIENT_ENCOUNTER_2024_250642.csv")
hosp_raw <- data.table::fread(file_path)
glimpse(hosp_raw)
head(hosp_raw)
colnames(hosp_raw) <- tolower(colnames(hosp_raw))

hosp_raw %>%
  summarise(
    missing = sum(is.na(patient_id) | patient_id == ""),
    non_missing = sum(!(is.na(patient_id) | patient_id == ""))
  )

hosp_raw %>%
  mutate(id_length = nchar(patient_id)) %>%
  count(id_length)

freq_table(data=hosp_raw, var=discharge_decision) # 5  = gestorben

hosp_raw %>% distinct(patient_id)

inhospital_deaths <- hosp_raw %>% 
  filter(discharge_decision==5 & !(is.na(patient_id) | patient_id == "")) 

freq_table(data=inhospital_deaths, var=discharge_month) 
freq_table(data=inhospital_deaths, var=discharge_to_death_days)

out_of_hospital_deaths <- hosp_raw %>% 
  filter(
    discharge_decision != 5,
    !is.na(discharge_to_death_days),
    !is.na(patient_id),
    trimws(patient_id) != ""
  )

freq_table(data=out_of_hospital_deaths, var=discharge_month) %>% write_clip()
freq_table(data=out_of_hospital_deaths, var=discharge_to_death_days) %>% write_clip()

out_of_hospital_deaths <- out_of_hospital_deaths %>%
  mutate(
    discharge_to_death_days_cat = case_when(
      discharge_to_death_days <= -6 ~ "less than -6 days",
      discharge_to_death_days >= -5 & discharge_to_death_days <= -1 ~ "-5 to -1 days",
      discharge_to_death_days == 0 ~ "0 days",
      discharge_to_death_days >= 1 & discharge_to_death_days <= 30 ~ "1–30 days",
      discharge_to_death_days >= 31 & discharge_to_death_days <= 60 ~ "31–60 days",
      discharge_to_death_days >= 61 & discharge_to_death_days <= 90 ~ "61–90 days",
      discharge_to_death_days > 90 ~ "more than 90 days",
      TRUE ~ NA_character_
    ),
    discharge_to_death_days_cat = factor(
      discharge_to_death_days_cat,
      levels = c(
        "less than -6 days",
        "-5 to -1 days",
        "0 days",
        "1–30 days",
        "31–60 days",
        "61–90 days",
        "more than 90 days"
      )
    )
  )
  

freq_table(data= out_of_hospital_deaths, var = discharge_to_death_days_cat) %>%
  write_clip()


# CoD Data
file_path <- file.path(root, "03_Inputdaten", "Datenlieferung_TU_2019_2024_20260409_Pseudonymize_out_20260421_091332_379_20260421_091438_506_20260421_094438_788.csv")
cod_data <- data.table::fread(file_path)
glimpse(cod_data)
colnames(cod_data) <- tolower(colnames(cod_data))
head(cod_data)
cod_data %>%
  summarise(
    missing = sum(is.na(ahv_n) | ahv_n == ""),
    non_missing = sum(!(is.na(ahv_n) | ahv_n == ""))
  ) 

cod_data %>%
  mutate(id_length = nchar(ahv_n)) %>%
  count(id_length)

cod_data <- cod_data %>% 
  mutate(
    death_date = dmy(ereignis_ges_d, locale = "C"), # EREIGNIS_GES_D = Verstorbene Person: Todesdatum
    death_year = year(death_date)
  )


cod_data %>% 
  select(ereignis_ges_d, death_date, death_year) %>% 
  head()

freq_table(data=cod_data, var=death_year)

# Test Linkage inhospital_deaths
test_link <- inhospital_deaths %>%
  left_join(cod_data, by = c("patient_id" = "ahv_n"))

glimpse(test_link)

test_link %>%
  summarise(
    total = n(),
    matched = sum(!is.na(death_year)),
    not_matched = sum(is.na(death_year))
  )

# Test Linkage out_of_hospital_deaths
test_link2 <- out_of_hospital_deaths %>%
  left_join(cod_data, by = c("patient_id" = "ahv_n"))

glimpse(test_link2)

test_link2 %>%
  summarise(
    total = n(),
    matched = sum(!is.na(death_year)),
    not_matched = sum(is.na(death_year))
  ) %>%
  write_clip()

test_link2 %>%
  group_by(discharge_month) %>%
  summarise(
    total = n(),
    matched = sum(!is.na(death_year)),
    not_matched = sum(is.na(death_year)),
    match_rate = round(matched / total * 100, 1),
    .groups = "drop"
  ) %>%
  write_clip()


test_link2 %>%
  group_by(discharge_to_death_days_cat) %>%
  summarise(
    total = n(),
    matched = sum(!is.na(death_year)),
    not_matched = sum(is.na(death_year)),
    match_rate = round(matched / total * 100, 1),
    .groups = "drop"
  ) %>%
  write_clip()




