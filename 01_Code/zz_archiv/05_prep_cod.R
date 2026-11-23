# ===================================================
# Cause of death data
# ===================================================

# ---------------------------------------------------
# read data
# ---------------------------------------------------

file_path_cod <- file.path(
  root,
  "03_Inputdaten",
  "TU",
  "Datenlieferung_TU_2019_2024_20260409_Pseudonymize_out_20260421_091332_379_20260421_091438_506_20260421_094438_788.csv"
)

cod_all <- data.table::fread(file_path_cod) |>
  janitor::clean_names()

glimpse(cod_all)

# ===================================================
# format variables
# ===================================================

cod_all <- cod_all |>
  mutate(
    ahv_n = na_if(str_trim(as.character(ahv_n)), ""),
    death_date = lubridate::dmy(ereignis_ges_d, locale = "C"),
    death_year = lubridate::year(death_date)
  )


# ===================================================
# QA
# ===================================================

freq_table(data = cod_all, var = death_year)
freq_table(data = cod_all, var = grund_ges_t) |>
  arrange(desc(n)) |>
  head(10)

cod_all |>
  summarise(
    n = n(),
    n_missing_ahv_n = sum(is.na(ahv_n)),
    n_missing_death_date = sum(is.na(death_date)),
    n_distinct_persons = n_distinct(ahv_n, na.rm = TRUE)
  )

cod_all |>
  count(death_year) |>
  arrange(death_year)


# ===================================================
# data quality checks
# ===================================================

if (any(is.na(cod_all$ahv_n))) {
  warning("CoD data contains records without ahv_n.")
}

if (any(is.na(cod_all$death_date))) {
  warning("CoD data contains records without valid death_date.")
}


# ===================================================
# save
# ===================================================

output_fst <- file.path(
  root,
  "04_Daten",
  glue::glue("cod_{start_year}_{end_year}.fst")
)

write_fst(cod_all, output_fst)

message(glue::glue("Output saved: {basename(output_fst)}"))
