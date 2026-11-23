# ---------------------------------------------------
# read data
# ---------------------------------------------------
# generate empty list 
icu_filelist <- list() 

for (year_analysis in start_year:end_year) {
  message(glue::glue("Reading ICU {year_analysis}"))
  
  # get filepath from list defined in setup
  icu_filelist[[as.character(year_analysis)]] <-
    fread(path_icu[[as.character(year_analysis)]]) |>  # read in data
    janitor::clean_names()                                   # clean names
}

# combine all years into one dataset after harmonising changing code variables
icu_all <- icu_filelist |>
  bind_rows()

glimpse(icu_all)


# speichern
output_fst <- file.path(
  root,
  "04_Daten",
  glue("icu_{start_year}_{end_year}.fst")
)

write_fst(icu_all, output_fst)
message(glue("Output saved: {basename(output_fst)}"))
