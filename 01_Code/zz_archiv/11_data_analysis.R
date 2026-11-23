

# ------------------------------------------------------------------------------
# readmissions
# ------------------------------------------------------------------------------
sepsis_cases_indexed <- sepsis_cases %>%
  filter(patient_id != "") %>% # nur gültige Patienten berücksichtigen
  mutate(
    # Replace NA with Inf so that missing values are treated as "largest"
    # -> they will come first when sorting descending
    discharge_to_death_days_sort = if_else(
      is.na(discharge_to_death_days),
      Inf,
      discharge_to_death_days
    ),
    
    # Random number used to break ties when all other sorting variables are equal
    rand_tie = runif(n())
  ) %>%
  
  group_by(patient_id) %>%  # define readmission sequence within each patient
  
  arrange(
    discharge_month,  
    # Primary ordering: chronological by discharge month (ascending)
    
    desc(discharge_to_death_days_sort),  
    # Within the same month:
    # smaller discharge_to_death_days should get a higher index (later episode)
    # -> therefore sort descending so smaller values come later
    
    rand_tie,  
    # Final tie-breaker:
    # if same month AND same discharge_to_death_days,
    # assign random order
    
    .by_group = TRUE
  ) %>%
  
  mutate(
    # Assign sequential index per patient based on the defined order
    readmission_index = row_number()
  ) %>%
  
  ungroup() %>%
  
  # Remove helper variables used only for sorting
  select(-discharge_to_death_days_sort, -rand_tie)

glimpse(sepsis_cases_indexed)


sepsis_cases_indexed %>% 
  freq_table(var=readmission_index) %>% 
  arrange(desc(readmission_index))

sepsis_cases <- sepsis_cases_indexed

# ------------------------------------------------------------------------------
# in-hospital mortality 
# ------------------------------------------------------------------------------

sepsis_cases <- sepsis_cases %>% 
  mutate(
    in_hospital_death=ifelse(discharge_decision==5,1,0),
    death=ifelse(is.na(discharge_to_death_days),0,1))

sepsis_cases %>%
  count(in_hospital_death, death) %>%
  group_by(in_hospital_death) %>%
  mutate(percent = n / sum(n) * 100)

sepsis_cases %>% 
  filter(death==0 & in_hospital_death==1) %>% 
  select(discharge_decision, post_discharge_location, post_discharge_treatment, hospital_id_num, discharge_month) %>% 
  head()
  
# Rekodierung 1
sepsis_cases <- sepsis_cases %>%
  mutate(
    death = if_else(
      death == 0 & in_hospital_death == 1,
      1,
      death
    ),
    discharge_to_death_days = if_else(
      death == 1 & in_hospital_death == 1,
      0,
      discharge_to_death_days
    )
  )

# Rekodierung 2
sepsis_cases %>%
  filter(in_hospital_death==1) %>% 
  count(in_hospital_death, discharge_to_death_days) %>%
  group_by(in_hospital_death) %>%
  mutate(percent = n / sum(n) * 100)

sepsis_cases <- sepsis_cases %>%
  mutate(
    discharge_to_death_days = if_else(
      in_hospital_death == 1,
      0,
      discharge_to_death_days
    )
  )

# ------------------------------------------------------------------------------
# Kaplan Meier Curve
# ------------------------------------------------------------------------------

library(survival)
library(survminer)
surv_obj <- Surv(
  time = sepsis_cases$discharge_to_death_days,
  event = sepsis_cases$death
)
km_fit <- survfit(surv_obj ~ 1)
ggsurvplot(
  km_fit,
  data = sepsis_cases,
  risk.table = TRUE,      # zeigt "number at risk"
  conf.int = TRUE,        # Konfidenzintervalle
  xlab = "Days since discharge",
  ylab = "Survival probability"
)
