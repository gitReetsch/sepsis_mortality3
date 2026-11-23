# *********************************************************************************
# 03_setup.R
# *********************************************************************************
#
# Purpose: Prepare a consistent R session for the project scripts.
# - Check the project structure.
# - Check and load required packages.
# - Load shared functions.
# - Set common options and the plot theme.
#
# Important: Run from the project root (open the .Rproj first).
#
# *********************************************************************************


# Run from the project root (open the .Rproj first).
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# check required folders
required_dirs <- c(
  file.path(root, "01_Code", "00_common"),
  file.path(root, "03_Inputdaten")
)

missing_dirs <- required_dirs[!dir.exists(required_dirs)]

if (length(missing_dirs) > 0L) {
  stop(
    "Open the R project and run from its project root. Missing directory/ies: ",
    paste(missing_dirs, collapse = ", ")
  )
}

# define code directory
code_dir <- file.path(root, "01_Code")

# chne fu
functions_file <- file.path(
  code_dir,
  "00_common",
  "01_functions.R"
)

if (!file.exists(functions_file)) {
  stop("Common functions file not found: ", functions_file)
}

# check for required packages
packages <- c(
  "data.table", "dplyr", "fst", "glue", "stringr", "tidyr",
  "lubridate", "ggplot2", "openxlsx", "janitor", "purrr",
  "rlang", "scales", "forcats", "cli"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed: ",
    paste(missing_packages, collapse = ", ")
  )
}

# check for required package versions
if (utils::packageVersion("dplyr") < "1.1.1") {
  stop("dplyr >= 1.1.1 is required.")
}

for (pkg in packages) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}


# options for numbers
options(scipen = 999)

# ggplot theme
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 9, base_family = "sans")
)

# datestamp
datestamp <- format(Sys.Date(), "%Y-%m-%d")

# source functions
source(functions_file, local = TRUE)
