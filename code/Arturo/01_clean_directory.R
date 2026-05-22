# =============================================================================
# 01_clean_directory.R
# Clean California school and district directory files
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/directory/
# =============================================================================

library(tidyverse)
library(janitor)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/directory"
clean_dir <- "data/cleaned/directory"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

district_file <- file.path(raw_dir, "DistrictSites2324_-4467924829629724752.csv")
school_file   <- file.path(raw_dir, "SchoolSites2425_5959976271030172217.csv")

# -----------------------------------------------------------------------------
# 2. Read raw data as character
# -----------------------------------------------------------------------------
# Important: CDS codes and district codes must stay as strings to preserve
# leading zeros.

district_raw <- read_csv(
  district_file,
  col_types = cols(.default = col_character())
)

school_raw <- read_csv(
  school_file,
  col_types = cols(.default = col_character())
)

# -----------------------------------------------------------------------------
# 3. Helper functions used only inside this script
# -----------------------------------------------------------------------------

clean_id <- function(x, width = NULL) {
  x <- str_remove_all(as.character(x), "[^0-9]")
  x <- na_if(x, "")
  if (!is.null(width)) {
    x <- str_pad(x, width = width, side = "left", pad = "0")
  }
  x
}

to_number <- function(x) {
  parse_number(as.character(x))
}

standardize_cds_fields <- function(df) {
  df %>%
    mutate(
      cds_code = clean_id(cds_code, width = 14),
      county_code = str_sub(cds_code, 1, 2),
      district_code_5 = str_sub(cds_code, 3, 7),
      school_code_7 = str_sub(cds_code, 8, 14),
      county_district_code_7 = str_sub(cds_code, 1, 7),
      district_cds_code = paste0(county_district_code_7, "0000000")
    )
}

# -----------------------------------------------------------------------------
# 4. Clean district lookup
# -----------------------------------------------------------------------------

district_clean <- district_raw %>%
  clean_names() %>%
  standardize_cds_fields() %>%
  mutate(
    source_file = "DistrictSites2324_-4467924829629724752.csv",
    directory_file_year = "2023-24",
    source_academic_year = academic_year,
    latitude = to_number(latitude),
    longitude = to_number(longitude),
    enroll_total = to_number(enroll_total),
    enroll_charter = to_number(enroll_charter),
    enroll_non_charter = to_number(enroll_non_charter),
    english_learners = to_number(english_learners),
    foster = to_number(foster),
    homeless = to_number(homeless),
    socioeconomically_disadvantaged = to_number(socioeconomically_disadvantaged),
    students_with_disabilities = to_number(students_with_disabilities)
  ) %>%
  transmute(
    source_file,
    directory_file_year,
    source_academic_year,
    county_code,
    county_name,
    district_code_5,
    county_district_code_7,
    district_cds_code,
    cds_code,
    district_name,
    district_type,
    grade_low,
    grade_high,
    assistance_status,
    street,
    city,
    zip,
    region,
    locale,
    latitude,
    longitude,
    enroll_total,
    enroll_charter,
    enroll_non_charter,
    english_learners,
    foster,
    homeless,
    socioeconomically_disadvantaged,
    students_with_disabilities
  ) %>%
  distinct()

# -----------------------------------------------------------------------------
# 5. Clean school lookup
# -----------------------------------------------------------------------------

school_clean <- school_raw %>%
  clean_names() %>%
  standardize_cds_fields() %>%
  mutate(
    source_file = "SchoolSites2425_5959976271030172217.csv",
    directory_file_year = "2024-25",
    source_academic_year = academic_year,
    is_active = status == "Active",
    is_charter = charter == "Y",
    latitude = to_number(latitude),
    longitude = to_number(longitude),
    enroll_total = to_number(enroll_total),
    english_learner = to_number(english_learner),
    foster = to_number(foster),
    homeless = to_number(homeless),
    socioeconomically_disadvantaged = to_number(socioeconomically_disadvantaged),
    students_with_disabilities = to_number(students_with_disabilities),
    free_reduced_meal_eligible = to_number(free_reduced_meal_eligible)
  ) %>%
  transmute(
    source_file,
    directory_file_year,
    source_academic_year,
    county_code,
    county_name,
    district_code_5,
    county_district_code_7,
    district_cds_code,
    district_name,
    school_code_7,
    cds_code,
    school_name,
    school_type,
    status,
    is_active,
    school_level,
    grade_low,
    grade_high,
    charter,
    is_charter,
    funding_type,
    title_i,
    dass,
    street,
    city,
    zip,
    state,
    region,
    locale,
    latitude,
    longitude,
    enroll_total,
    english_learner,
    foster,
    homeless,
    socioeconomically_disadvantaged,
    students_with_disabilities,
    free_reduced_meal_eligible
  ) %>%
  distinct()

# -----------------------------------------------------------------------------
# 6. QC checks
# -----------------------------------------------------------------------------

qc_summary <- tibble(
  dataset = c("district_lookup", "school_lookup"),
  raw_rows = c(nrow(district_raw), nrow(school_raw)),
  cleaned_rows = c(nrow(district_clean), nrow(school_clean)),
  unique_cds_codes = c(
    n_distinct(district_clean$cds_code, na.rm = TRUE),
    n_distinct(school_clean$cds_code, na.rm = TRUE)
  ),
  duplicate_cds_codes = c(
    sum(duplicated(district_clean$cds_code)),
    sum(duplicated(school_clean$cds_code))
  ),
  missing_cds_codes = c(
    sum(is.na(district_clean$cds_code)),
    sum(is.na(school_clean$cds_code))
  ),
  missing_names = c(
    sum(is.na(district_clean$district_name)),
    sum(is.na(school_clean$school_name))
  )
)

cat("\n================ DIRECTORY QC SUMMARY ================\n")
print(qc_summary)

cat("\n================ DISTRICT TYPES ================\n")
district_clean %>%
  count(district_type, sort = TRUE) %>%
  print(n = 50)

cat("\n================ SCHOOL TYPES ================\n")
school_clean %>%
  count(school_type, sort = TRUE) %>%
  print(n = 50)

cat("\n================ SCHOOL STATUS ================\n")
school_clean %>%
  count(status, sort = TRUE) %>%
  print(n = 20)

cat("\n================ DISTRICT SOURCE ACADEMIC YEARS ================\n")
district_clean %>%
  count(source_academic_year, sort = TRUE) %>%
  print(n = 20)

cat("\n================ SCHOOL SOURCE ACADEMIC YEARS ================\n")
school_clean %>%
  count(source_academic_year, sort = TRUE) %>%
  print(n = 20)

# -----------------------------------------------------------------------------
# 7. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(district_clean, file.path(clean_dir, "district_lookup.csv"))
write_csv(school_clean, file.path(clean_dir, "school_lookup.csv"))
write_csv(qc_summary, file.path(clean_dir, "directory_qc_summary.csv"))

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "district_lookup.csv"), "\n")
cat(file.path(clean_dir, "school_lookup.csv"), "\n")
cat(file.path(clean_dir, "directory_qc_summary.csv"), "\n")