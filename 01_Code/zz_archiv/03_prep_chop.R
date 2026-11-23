# ===================================================
# Procedures (CHOP)
# ===================================================

# ---------------------------------------------------
# read data
# ---------------------------------------------------
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

# ===================================================
# Descriptive analysis raw data
# ===================================================
freq_table(
  data = chop_all |> mutate(survey_year = factor(survey_year,levels = start_year:end_year)),
  var = chop,
  by = survey_year,
  totals = "both"
) |>
  arrange(desc(Total)) |>
  head(20)

freq_table(data = chop_all, var = survey_year)

# ===================================================
# standardise CHOP codes for indicator matching
# ===================================================
chop_all <- chop_all |>
  mutate(
    chop_clean = chop |>
      str_to_upper() |>              # convert to upper case
      str_remove_all("\\.") |>       # remove dots
      str_remove_all("\\s+")         # remove spaces
  )


# ===================================================
# Indicator definition
# ===================================================

# indicator_list is a list of lists (every indicator is defined by a list itself)
indicator_list <- list(
  
  # -------------------------------------------------
  # test indicator
  # -------------------------------------------------

  testindic = list(                        # name of indicator
    codes = c("94A120", "94A1B0"),                     # relevant CHOP codes for definition
    source_var = "chop",                   # variable where to search for 'codes'
    flag_var = "testindic_flag",           # name of flag variable
    trigger_var = "testindic_trigger_chop" # name of variable with codes which triggered the flag
  )
  
  # -------------------------------------------------
  # next indicator
  # -------------------------------------------------
  #,
  # name_indicator_1 = list(              # name of indicator 
  #   codes = c(),                        # relevant CHOP codes for definition
  #   source_var = "chop",                # variable where to search for 'codes'
  #   flag_var = "indicator_flag",        # name of flag variable
  #   trigger_var = "indicator_trigger_chop" # name of variable with codes which triggered the flag
  # )
  
)


# ===================================================
# flag function 
# ===================================================

create_chop_flag <- function(data, indicator) {
  
  data |>
    mutate(
      flag_value = case_when(
        chop_clean %in% indicator$codes ~ 1L,  # CHOP match
        TRUE                            ~ NA_integer_
      ),
      trigger_code = case_when(
        chop_clean %in% indicator$codes ~ chop_clean,  # matching CHOP code
        TRUE                            ~ NA_character_
      )
    ) |>
    filter(!is.na(flag_value)) |>                                       # keep matching records
    summarise(
      flag_value = min(flag_value),                                     # any CHOP match flags encounter
      trigger_code = paste(sort(unique(trigger_code)), collapse = ";"), # store all trigger codes
      .by = c(survey_year, encounter_id)
    ) |>
    rename(
      !!indicator$flag_var := flag_value,
      !!indicator$trigger_var := trigger_code 
    )
}


# ===================================================
# Create indicator flags
# ===================================================

  # apply flag function to all indicators in indicator_list
  indicator_flags <- lapply(indicator_list,\(indicator) create_chop_flag(chop_all, indicator))
  
  # combine all indicator flags by encounter
  flag_chop <- purrr::reduce(
    indicator_flags,
    full_join,
    by = c("survey_year", "encounter_id")
  )


# ===================================================
# Descriptive analysis indicator flags
# ===================================================

freq_table(data = flag_chop, var = survey_year)


# ===================================================
# Save
# ===================================================

output_fst <- file.path(
  root,
  "04_Daten",
  glue::glue("flag_chop_{start_year}_{end_year}.fst")
)

write_fst(flag_chop, output_fst)

message(glue::glue("Output saved: {basename(output_fst)}"))

