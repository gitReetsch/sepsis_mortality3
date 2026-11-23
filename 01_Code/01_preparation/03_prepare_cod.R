# ******************************************************************************
# 03_prepare_cod.R
# ******************************************************************************
#
# Purpose: Prepare cause-of-death data for linkage with GVS encounter data.
#
# - Read and combine the cause-of-death input data.
# - Standardise the linkage ID and derive death date and year.
# - Summarise the data and check for missing linkage IDs and invalid dates.
# - Save the prepared data as an fst file.
#
# Outputs:
# - path_common: the cleaned and prepared cod FST file for the configured
#   study period, saved using common_file().
# - Console: CoD preparation summaries, QA results, and any warnings.
#
# ******************************************************************************

# Read and clean the CoD input file
cod_all <- data.table::fread(path_cod) |>
  janitor::clean_names()

cod_code_vars <- c(
  "grund_ges_t",
  "unmitt_ges_t",
  "begleit_a_ges_t",
  "begleit_b_ges_t",
  "endg_u_ges_t"
)

cod_all <- cod_all |>
  mutate(
    ahv_n = na_if(str_trim(as.character(ahv_n)), ""),
    across(
      all_of(cod_code_vars),
      ~ na_if(str_trim(as.character(.x)), "")
    ),
    death_date = lubridate::dmy(ereignis_ges_d, locale = "C"),
    death_year = lubridate::year(death_date)
  )

# Review the prepared data
glimpse(cod_all)

print(
  freq_table(
    data = cod_all,
    var = death_year,
    totals = "none"
  )
)

print(
  freq_table(
    data = cod_all,
    var = grund_ges_t,
    totals = "none"
  ) |>
    arrange(desc(n)) |>
    head(10)
)

cod_qa_summary <- cod_all |>
  summarise(
    n = n(),
    n_missing_ahv_n = sum(is.na(ahv_n)),
    n_missing_death_date = sum(is.na(death_date)),
    n_distinct_persons = n_distinct(ahv_n, na.rm = TRUE)
  )

print(cod_qa_summary)

# Warn about missing linkage IDs or unparseable death dates
if (cod_qa_summary$n_missing_ahv_n > 0L) {
  warning("CoD data contains records without ahv_n.")
}

if (cod_qa_summary$n_missing_death_date > 0L) {
  warning("CoD data contains records without a valid death date.")
}

# Save the prepared CoD data
output_fst <- common_file("cod")
write_fst(cod_all, output_fst)
message(glue::glue("Output saved: {basename(output_fst)}"))
