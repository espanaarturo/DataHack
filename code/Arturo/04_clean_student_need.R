# =============================================================================
# 04_clean_student_need.R
# Clean cumulative enrollment and homeless enrollment files
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/student_need/
# =============================================================================

library(tidyverse)
library(janitor)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/student_need"
clean_dir <- "data/cleaned/student_need"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. File specs based on actual file inspection
# -----------------------------------------------------------------------------

cenroll_specs <- tribble(
  ~school_year, ~file_name,
  "2019_20",    "cenroll1920.txt",
  "2020_21",    "cenroll2021.txt",
  "2021_22",    "cenroll2122.txt",
  "2022_23",    "cenroll2223.txt",
  "2023_24",    "cenroll2324.txt",
  "2024_25",    "cenroll2425-v2.txt"
)

hse_specs <- tribble(
  ~school_year, ~file_name,
  "2019_20",    "hse1920.txt",
  "2020_21",    "hse2021.txt",
  "2021_22",    "hse2122.txt",
  "2022_23",    "hse2223.txt",
  "2023_24",    "hse2324.txt",
  "2024_25",    "hse2425.txt"
)

# -----------------------------------------------------------------------------
# 3. Helper functions
# -----------------------------------------------------------------------------

clean_id <- function(x, width = NULL) {
  x <- as.character(x)
  x <- str_remove_all(x, "[^0-9]")
  x <- na_if(x, "")
  
  if (!is.null(width)) {
    x <- str_pad(x, width = width, side = "left", pad = "0")
  }
  
  x
}

to_number <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- na_if(x, "")
  x <- na_if(x, "N/A")
  x <- na_if(x, "NA")
  x <- na_if(x, "--")
  x <- na_if(x, "*")
  x <- na_if(x, "†")
  x <- na_if(x, "null")
  suppressWarnings(parse_number(x))
}

clean_cde_names <- function(df) {
  df %>%
    rename_with(~ str_replace_all(.x, "\ufeff", "")) %>%
    clean_names()
}

add_district_ids <- function(df) {
  df %>%
    mutate(
      county_code = clean_id(county_code, width = 2),
      district_code = clean_id(district_code, width = 5),
      school_code = "0000000",
      county_district_code_7 = paste0(county_code, district_code),
      cds_code = paste0(county_code, district_code, school_code),
      district_cds_code = cds_code
    )
}

# -----------------------------------------------------------------------------
# 4. Clean one cumulative enrollment file
# -----------------------------------------------------------------------------

clean_one_cenroll <- function(school_year, file_name) {
  
  message("Cleaning cumulative enrollment file: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_tsv(
    file_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    clean_cde_names()
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      school_year = school_year,
      academic_year = str_squish(academic_year),
      aggregate_level = str_squish(aggregate_level),
      county_name = str_squish(county_name),
      district_name = str_squish(district_name),
      school_name = str_squish(school_name),
      charter = str_squish(charter),
      reporting_category = str_squish(reporting_category),
      cumulative_enrollment = to_number(cumulative_enrollment)
    ) %>%
    # District-level rows only
    filter(
      aggregate_level == "D",
      charter == "All"
    ) %>%
    add_district_ids() %>%
    filter(
      !is.na(county_code),
      !is.na(district_code),
      str_detect(county_code, "^[0-9]{2}$"),
      str_detect(district_code, "^[0-9]{5}$")
    ) %>%
    transmute(
      source_file,
      school_year,
      academic_year,
      aggregate_level,
      county_code,
      district_code,
      school_code,
      county_district_code_7,
      cds_code,
      district_cds_code,
      county_name,
      district_name,
      charter,
      reporting_category,
      cumulative_enrollment
    )
  
  cleaned
}

# -----------------------------------------------------------------------------
# 5. Clean one homeless enrollment file
# -----------------------------------------------------------------------------

clean_one_hse <- function(school_year, file_name) {
  
  message("Cleaning homeless enrollment file: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_tsv(
    file_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    clean_cde_names()
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      school_year = school_year,
      academic_year = str_squish(academic_year),
      aggregate_level = str_squish(aggregate_level),
      county_name = str_squish(county_name),
      district_name = str_squish(district_name),
      school_name = str_squish(school_name),
      charter_school = str_squish(charter_school),
      dass = str_squish(dass),
      reporting_category = str_squish(reporting_category),
      cumulative_enrollment = to_number(cumulative_enrollment),
      homeless_student_enrollment = to_number(homeless_student_enrollment),
      temporarily_doubled_up = to_number(temporarily_doubled_up),
      temporary_shelters = to_number(temporary_shelters),
      hotels_motels = to_number(hotels_motels),
      temporarily_unsheltered = to_number(temporarily_unsheltered),
      missing_unknown = to_number(missing_unknown),
      temporarily_doubled_up_percent = to_number(temporarily_doubled_up_percent),
      temporary_shelters_percent = to_number(temporary_shelters_percent),
      hotels_motels_percent = to_number(hotels_motels_percent),
      temporarily_unsheltered_percent = to_number(temporarily_unsheltered_percent),
      missing_unknown_percent = to_number(missing_unknown_percent)
    ) %>%
    # District-level rows only, all charter status, all DASS status
    filter(
      aggregate_level == "D",
      charter_school == "All",
      dass == "All"
    ) %>%
    add_district_ids() %>%
    filter(
      !is.na(county_code),
      !is.na(district_code),
      str_detect(county_code, "^[0-9]{2}$"),
      str_detect(district_code, "^[0-9]{5}$")
    ) %>%
    mutate(
      homeless_rate = 100 * homeless_student_enrollment / cumulative_enrollment
    ) %>%
    transmute(
      source_file,
      school_year,
      academic_year,
      aggregate_level,
      county_code,
      district_code,
      school_code,
      county_district_code_7,
      cds_code,
      district_cds_code,
      county_name,
      district_name,
      charter_school,
      dass,
      reporting_category,
      cumulative_enrollment,
      homeless_student_enrollment,
      homeless_rate,
      temporarily_doubled_up,
      temporary_shelters,
      hotels_motels,
      temporarily_unsheltered,
      missing_unknown,
      temporarily_doubled_up_percent,
      temporary_shelters_percent,
      hotels_motels_percent,
      temporarily_unsheltered_percent,
      missing_unknown_percent
    )
  
  cleaned
}

# -----------------------------------------------------------------------------
# 6. Clean all files
# -----------------------------------------------------------------------------

cumulative_enrollment_district_long <- pmap_dfr(
  cenroll_specs,
  clean_one_cenroll
)

homeless_enrollment_district_long <- pmap_dfr(
  hse_specs,
  clean_one_hse
)

# -----------------------------------------------------------------------------
# 7. Create total-only district files
# -----------------------------------------------------------------------------
# ReportingCategory == "TA" is the total/all-students category in CDE files.

cumulative_enrollment_district_total <- cumulative_enrollment_district_long %>%
  filter(reporting_category == "TA") %>%
  transmute(
    source_file,
    school_year,
    academic_year,
    county_code,
    district_code,
    school_code,
    county_district_code_7,
    cds_code,
    district_cds_code,
    county_name,
    district_name,
    district_cumulative_enrollment = cumulative_enrollment
  )

homeless_enrollment_district_total <- homeless_enrollment_district_long %>%
  filter(reporting_category == "TA") %>%
  transmute(
    source_file,
    school_year,
    academic_year,
    county_code,
    district_code,
    school_code,
    county_district_code_7,
    cds_code,
    district_cds_code,
    county_name,
    district_name,
    district_cumulative_enrollment_hse = cumulative_enrollment,
    homeless_student_enrollment,
    homeless_rate,
    temporarily_doubled_up,
    temporary_shelters,
    hotels_motels,
    temporarily_unsheltered,
    missing_unknown,
    temporarily_doubled_up_percent,
    temporary_shelters_percent,
    hotels_motels_percent,
    temporarily_unsheltered_percent,
    missing_unknown_percent
  )

# -----------------------------------------------------------------------------
# 8. QC checks
# -----------------------------------------------------------------------------

student_need_qc_summary <- bind_rows(
  cumulative_enrollment_district_long %>%
    summarize(
      dataset = "cumulative_enrollment_district_long",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code),
      duplicate_year_cds_category = sum(duplicated(paste(school_year, cds_code, reporting_category))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_enrollment = sum(is.na(cumulative_enrollment))
    ),
  cumulative_enrollment_district_total %>%
    summarize(
      dataset = "cumulative_enrollment_district_total",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code),
      duplicate_year_cds_category = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_enrollment = sum(is.na(district_cumulative_enrollment))
    ),
  homeless_enrollment_district_long %>%
    summarize(
      dataset = "homeless_enrollment_district_long",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code),
      duplicate_year_cds_category = sum(duplicated(paste(school_year, cds_code, reporting_category))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_enrollment = sum(is.na(cumulative_enrollment))
    ),
  homeless_enrollment_district_total %>%
    summarize(
      dataset = "homeless_enrollment_district_total",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code),
      duplicate_year_cds_category = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_enrollment = sum(is.na(district_cumulative_enrollment_hse))
    )
)

student_need_year_qc <- bind_rows(
  cumulative_enrollment_district_total %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "cumulative_enrollment_district_total"),
  homeless_enrollment_district_total %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "homeless_enrollment_district_total")
) %>%
  select(dataset, school_year, rows) %>%
  arrange(dataset, school_year)

student_need_reporting_category_qc <- bind_rows(
  cumulative_enrollment_district_long %>%
    count(reporting_category, name = "rows") %>%
    mutate(dataset = "cumulative_enrollment_district_long"),
  homeless_enrollment_district_long %>%
    count(reporting_category, name = "rows") %>%
    mutate(dataset = "homeless_enrollment_district_long")
) %>%
  select(dataset, reporting_category, rows) %>%
  arrange(dataset, reporting_category)

cat("\n================ STUDENT NEED QC SUMMARY ================\n")
print(student_need_qc_summary, width = Inf)

cat("\n================ STUDENT NEED YEAR QC ================\n")
print(student_need_year_qc, n = Inf)

cat("\n================ STUDENT NEED REPORTING CATEGORY QC ================\n")
print(student_need_reporting_category_qc, n = Inf)

# -----------------------------------------------------------------------------
# 9. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(
  cumulative_enrollment_district_long,
  file.path(clean_dir, "cumulative_enrollment_district_long.csv")
)

write_csv(
  cumulative_enrollment_district_total,
  file.path(clean_dir, "cumulative_enrollment_district_total.csv")
)

write_csv(
  homeless_enrollment_district_long,
  file.path(clean_dir, "homeless_enrollment_district_long.csv")
)

write_csv(
  homeless_enrollment_district_total,
  file.path(clean_dir, "homeless_enrollment_district_total.csv")
)

write_csv(
  student_need_qc_summary,
  file.path(clean_dir, "student_need_qc_summary.csv")
)

write_csv(
  student_need_year_qc,
  file.path(clean_dir, "student_need_year_qc.csv")
)

write_csv(
  student_need_reporting_category_qc,
  file.path(clean_dir, "student_need_reporting_category_qc.csv")
)

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "cumulative_enrollment_district_long.csv"), "\n")
cat(file.path(clean_dir, "cumulative_enrollment_district_total.csv"), "\n")
cat(file.path(clean_dir, "homeless_enrollment_district_long.csv"), "\n")
cat(file.path(clean_dir, "homeless_enrollment_district_total.csv"), "\n")
cat(file.path(clean_dir, "student_need_qc_summary.csv"), "\n")
cat(file.path(clean_dir, "student_need_year_qc.csv"), "\n")
cat(file.path(clean_dir, "student_need_reporting_category_qc.csv"), "\n")