# ******************************************************************************
# ICD-10-GM code definitions
# ******************************************************************************

# ******************************************************************************
# Supplementary Table A1: Explicit sepsis definition ----
# ******************************************************************************
sepsis_a1_code_groups <- list(
  "Sepsis-specific codes" = c(
    "A021", "A207", "A327", "A391", "A392", "A394",
    "A400", "A401", "A402", "A403", "A408", "A409",
    "A410", "A411", "A412", "A413", "A414",
    "A4151", "A4152", "A4158", "A418", "A419",
    "A427", "B377",
    "P360", "P361", "P362", "P363", "P364", "P365", "P368", "P369",
    "O85", "O753"
  ),
  "Systemic inflammatory response syndrome (SIRS)" = c("R650", "R651"),
  "Septic shock" = "R572",
  "Toxic shock syndrome" = "A483",
  "Bacteraemia" = "A499"
)
print(sepsis_a1_code_groups)

# Flat vector to use in identification of sepsis cases
sepsis_explicit_codes <- unlist(sepsis_a1_code_groups,  use.names = FALSE)
sepsis_explicit_codes

# ******************************************************************************
# Supplementary Table A2: Infection and implicit organ-dysfunction codes ----
# ******************************************************************************
sepsis_a2_code_groups <- list(
  "Pathogen-specific bacterial, viral and fungal infections" = c(
    "A010", "A021", "A327",
    "A390", "A391", "A392", "A393", "A394", "A395", "A398", "A399",
    "A400", "A401", "A402", "A403", "A408", "A409",
    "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419",
    "A427",
    "A490", "A491", "A492", "A493", "A498", "A499",
    "B370", "B371", "B372", "B373", "B374", "B375", "B376", "B377",
    "B378", "B379",
    "B950", "B951", "B952", "B953", "B9541", "B9542", "B9548",
    "B955", "B956", "B957", "B9590", "B9591",
    "B962", "B963", "B965", "B966", "B967", "B968",
    "P360", "P361", "P362", "P363", "P364", "P365", "P368", "P369",
    "P375",
    "O85", "O753",
    "U071", "U072"
  ),
  "Infection codes in other groups" = c(
    "G00", "G01", "G02", "G03",
    "I30", "I32", "I33", "I39", "I40", "I41", "I80",
    "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16",
    "J17", "J18", "J20", "J21", "J22", "J47", "J85", "J86",
    "K35", "K61", "K65",
    "L03", "L04", "L08", "L88",
    "M00", "M01", "M86",
    "N10", "N11", "N12", "N16", "N30", "N34"
  ),
  "Implicit organ dysfunction" = c(
    "I958", "R031", "R570", "R579", "P290",
    "J80", "J9600", "J9601", "J9609", "J9690", "J9691", "J9699",
    "P285",
    "G931", "G934",
    "F050", "F051", "F058", "F059",
    "P914", "P915", "P916", "P919",
    "R400", "R401", "R402", "R410",
    "N170", "N171", "N172", "N178", "N179", "N19", "P960", "R34",
    "K720", "K7271", "K7272", "K7273", "K7274", "K7279", "K729", "K769",
    "D618", "D619", "D695", "D696", "P60", "P610"
  )
)

print(sepsis_a2_code_groups)

# Infection groups from A2, kept separate from organ-dysfunction codes.
sepsis_a2_infection_codes <- unlist(
  sepsis_a2_code_groups[
    c("Pathogen-specific bacterial, viral and fungal infections",
      "Infection codes in other groups")
  ],
  use.names = FALSE
)




# ******************************************************************************
# Additional infection codes used in the cause-of-death analysis ----
# ******************************************************************************
sepsis_cod_additional_infection_codes <- c(
  "J69",
  "N390",
  "T814",
  "T826", "T827",
  "T845", "T846", "T847",
  "T857",
  "O86"
)

# Flat vector used by the existing CoD analysis.
# Organ-dysfunction codes from A2 are intentionally not included.
sepsis_infection_prefixes <- c(
  sepsis_a2_infection_codes,
  sepsis_cod_additional_infection_codes
)

# ******************************************************************************
# Cause-of-death code groups used in Supplementary Table F1 and CoD analysis ----
# ******************************************************************************

# Strong evidence against sepsis: external causes of death.
sepsis_against_prefixes_strong <- c("V", "W", "X")

# Moderate evidence against sepsis: prespecified competing causes.
sepsis_against_prefixes_moderate <- c(
  "I21", "I71", "T79", "I60", "I61", "I62"
)
# Named groups that Quarto can use to generate Supplementary Table F1.
sepsis_f1_code_groups <- list(
  "Codes included in the explicit administrative sepsis definition" =
    sepsis_explicit_codes,
  "Infection codes" =
    sepsis_infection_prefixes,
  "External-cause codes" =
    sepsis_against_prefixes_strong,
  "Prespecified competing non-infectious cause codes" =
    sepsis_against_prefixes_moderate
)
print(sepsis_f1_code_groups)


# ******************************************************************************
# Configuration for cohort identification ----
# ******************************************************************************
# Used by create_icd_flag() to identify qualifying diagnoses and create
# encounter-level flags: 1 = principal, 2 = secondary, 3 = add-on.
sepsis_indicator_definition <- list(
  codes = sepsis_explicit_codes,
  flag_var = "sepsis_flag",
  trigger_var = "sepsis_trigger_icd"
)

