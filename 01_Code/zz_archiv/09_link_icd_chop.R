# ===================================================
# Link discharges with ICD and CHOP indicator flags
# ===================================================

# ---------------------------------------------------
# read data
# ---------------------------------------------------

discharges_cod <- read_fst(
  file.path(root, "04_Daten", glue("discharges_cod_{start_year}_{end_year}.fst"))
)

flag_icd <- read_fst(
  file.path(root, "04_Daten", glue("flag_icd_{start_year}_{end_year}.fst"))
)

flag_chop <- read_fst(
  file.path(root, "04_Daten", glue("flag_chop_{start_year}_{end_year}.fst"))
)



# ===================================================
# QA before linkage
# ===================================================

qa_flag_input <- tibble(
  dataset = c("discharges_cod", "flag_icd", "flag_chop"),
  n_rows = c(nrow(discharges_cod), nrow(flag_icd), nrow(flag_chop)),
  n_encounters = c(
    n_distinct(discharges_cod$encounter_id),
    n_distinct(flag_icd$encounter_id),
    n_distinct(flag_chop$encounter_id)
  )
)

print(qa_flag_input)

# Check uniqueness of linkage keys
check_icd_duplicates <- flag_icd |>
  count(survey_year, encounter_id, name = "n") |>
  filter(n > 1)
if (nrow(check_icd_duplicates) > 0) { stop("ICD flag dataset contains duplicate survey_year / encounter_id combinations.")}

check_chop_duplicates <- flag_chop |>
  count(survey_year, encounter_id, name = "n") |>
  filter(n > 1)
if (nrow(check_chop_duplicates) > 0) {stop("CHOP flag dataset contains duplicate survey_year / encounter_id combinations.")}


# ===================================================
# link flags
# ===================================================

n_before <- nrow(discharges_cod)

discharges_flagged <- discharges_cod |>
  left_join(
    flag_icd,
    by = c("survey_year", "encounter_id"),
    relationship = "one-to-one"
  ) |>
  left_join(
    flag_chop,
    by = c("survey_year", "encounter_id"),
    relationship = "one-to-one"
  )

n_after <- nrow(discharges_flagged)

if (n_before != n_after) {stop(glue("Linkage failed: {scales::comma(n_before)} records before join, {scales::comma(n_after)} after join."))
} else {message(glue("Linkage check passed: {scales::comma(n_after)} records before and after left join."))}

glimpse(discharges_flagged)


# ===================================================
# save
# ===================================================

output_fst <- file.path(
  root,
  "04_Daten",
  glue("discharges_flagged_{start_year}_{end_year}.fst")
)

write_fst(discharges_flagged, output_fst)

message(glue("Output saved: {basename(output_fst)}"))
