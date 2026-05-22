# =============================================================================
# 05_clean_outcomes_assessments.R
# Clean California Dashboard Academic Indicator files for ELA and Math
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/outcomes_assessments/
# =============================================================================

library(tidyverse)
library(readxl)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/outcomes_assessments"
clean_dir <- "data/cleaned/outcomes_assessments"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. File specs based on actual workbook inspection
# -----------------------------------------------------------------------------

assessment_specs <- tribble(
  ~subject, ~school_year, ~file_name,              ~sheet_name,
  "ela",    "2018_19",    "eladownload2019.xlsx",  "Sheet1",
  "ela",    "2021_22",    "eladownload2022.xlsx",  "Sheet1",
  "ela",    "2022_23",    "eladownload2023.xlsx",  "ELADownload2023",
  "ela",    "2023_24",    "eladownload2024.xlsx",  "Sheet1",
  "ela",    "2024_25",    "eladownload2025.xlsx",  "Sheet1",
  
  "math",   "2018_19",    "mathdownload2019.xlsx", "Sheet1",
  "math",   "2021_22",    "mathdownload2022.xlsx", "Sheet1",
  "math",   "2022_23",    "mathdownload2023.xlsx", "MathDownload2023",
  "math",   "2023_24",    "mathdownload2024.xlsx", "Sheet1",
  "math",   "2024_25",    "mathdownload2025.xlsx", "Sheet1"
)

# Note:
# There are no 2019-20 or 2020-21 Dashboard Academic Indicator files here due to
# COVID testing/accountability disruptions. The available sequence is:
# 2018-19, 2021-22, 2022-23, 2023-24, 2024-25.

# -----------------------------------------------------------------------------
# 3. Helper functions
# -----------------------------------------------------------------------------

clean_column_names <- function(x) {
  x %>%
    str_replace_all("\ufeff", "") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

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

add_assessment_ids <- function(df) {
  df %>%
    mutate(
      cds_code = clean_id(cds, width = 14),
      county_code = str_sub(cds_code, 1, 2),
      district_code = str_sub(cds_code, 3, 7),
      school_code = str_sub(cds_code, 8, 14),
      county_district_code_7 = str_sub(cds_code, 1, 7),
      district_cds_code = paste0(county_district_code_7, "0000000")
    )
}

# -----------------------------------------------------------------------------
# 4. Clean one assessment file
# -----------------------------------------------------------------------------

clean_one_assessment <- function(subject, school_year, file_name, sheet_name) {
  
  message("Cleaning assessment file: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  # Some 2023+ Excel files have formatting down to the bottom of the sheet.
  # The range limit avoids accidentally reading 1,048,576 blank rows.
  raw <- read_excel(
    path = file_path,
    sheet = sheet_name,
    n_max = 250000,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  names(raw) <- clean_column_names(names(raw))
  
  # Drop empty columns created by reading the wide A:AK range.
  raw <- raw %>%
    select(where(~ !all(is.na(.x) | .x == "")))
  
  needed_cols <- c(
    "cds",
    "rtype",
    "schoolname",
    "districtname",
    "countyname",
    "charter_flag",
    "coe_flag",
    "dass_flag",
    "studentgroup",
    "currdenom",
    "currstatus",
    "priordenom",
    "priorstatus",
    "change",
    "statuslevel",
    "changelevel",
    "color",
    "box",
    "currnsizemet",
    "priornsizemet",
    "accountabilitymet",
    "hscutpoints",
    "curradjustment",
    "prioradjustment",
    "pairshare_method",
    "notestflag",
    "prate_enrolled",
    "prate_tested",
    "prate",
    "numprloss",
    "currprate_enrolled",
    "currprate_tested",
    "currprate",
    "currnumprloss",
    "currdenom_withoutprloss",
    "currstatus_withoutprloss",
    "priorprate_enrolled",
    "priorprate_tested",
    "priorprate",
    "priornumprloss",
    "priordenom_withoutprloss",
    "priorstatus_withoutprloss",
    "indicator",
    "reportingyear"
  )
  
  missing_cols <- setdiff(needed_cols, names(raw))
  
  if (length(missing_cols) > 0) {
    raw[missing_cols] <- NA_character_
  }
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      source_sheet = sheet_name,
      subject = subject,
      school_year = school_year,
      rtype = str_squish(rtype),
      school_name = str_squish(schoolname),
      district_name = str_squish(districtname),
      county_name = str_squish(countyname),
      charter_flag = str_squish(charter_flag),
      coe_flag = str_squish(coe_flag),
      dass_flag = str_squish(dass_flag),
      student_group = str_squish(studentgroup),
      reporting_year = to_number(reportingyear)
    ) %>%
    add_assessment_ids() %>%
    # District-level rows only for our district-year LCFF project
    filter(
      rtype == "D",
      !is.na(cds_code),
      str_detect(cds_code, "^[0-9]{14}$")
    ) %>%
    mutate(
      current_denominator = to_number(currdenom),
      current_distance_from_standard = to_number(currstatus),
      prior_denominator = to_number(priordenom),
      prior_distance_from_standard = to_number(priorstatus),
      distance_change = to_number(change),
      status_level = to_number(statuslevel),
      change_level = to_number(changelevel),
      color = to_number(color),
      box = to_number(box),
      current_n_size_met = to_number(currnsizemet),
      prior_n_size_met = to_number(priornsizemet),
      accountability_met = to_number(accountabilitymet),
      
      # Participation rate fields vary by year.
      participation_rate_enrolled = coalesce(
        to_number(currprate_enrolled),
        to_number(prate_enrolled)
      ),
      participation_rate_tested = coalesce(
        to_number(currprate_tested),
        to_number(prate_tested)
      ),
      participation_rate = coalesce(
        to_number(currprate),
        to_number(prate)
      ),
      participation_rate_loss_count = coalesce(
        to_number(currnumprloss),
        to_number(numprloss)
      ),
      denominator_without_pr_loss = coalesce(
        to_number(currdenom_withoutprloss),
        to_number(currdenom_withoutprloss)
      ),
      status_without_pr_loss = coalesce(
        to_number(currstatus_withoutprloss),
        to_number(currstatus_withoutprloss)
      ),
      indicator = if_else(
        is.na(indicator) | indicator == "",
        subject,
        str_squish(indicator)
      )
    ) %>%
    transmute(
      source_file,
      source_sheet,
      subject,
      school_year,
      reporting_year,
      rtype,
      county_code,
      district_code,
      school_code,
      county_district_code_7,
      cds_code,
      district_cds_code,
      county_name,
      district_name,
      school_name,
      charter_flag,
      coe_flag,
      dass_flag,
      student_group,
      current_denominator,
      current_distance_from_standard,
      prior_denominator,
      prior_distance_from_standard,
      distance_change,
      status_level,
      change_level,
      color,
      box,
      current_n_size_met,
      prior_n_size_met,
      accountability_met,
      hscutpoints,
      pairshare_method,
      notestflag,
      participation_rate_enrolled,
      participation_rate_tested,
      participation_rate,
      participation_rate_loss_count,
      denominator_without_pr_loss,
      status_without_pr_loss,
      indicator
    )
  
  cleaned
}

# -----------------------------------------------------------------------------
# 5. Clean all files
# -----------------------------------------------------------------------------

academic_indicator_district_long <- pmap_dfr(
  assessment_specs,
  clean_one_assessment
)

# Main district total file: all students only
academic_indicator_district_total <- academic_indicator_district_long %>%
  filter(student_group == "ALL") %>%
  transmute(
    source_file,
    source_sheet,
    subject,
    school_year,
    reporting_year,
    county_code,
    district_code,
    school_code,
    county_district_code_7,
    cds_code,
    district_cds_code,
    county_name,
    district_name,
    student_group,
    current_denominator,
    current_distance_from_standard,
    prior_denominator,
    prior_distance_from_standard,
    distance_change,
    status_level,
    change_level,
    color,
    box,
    current_n_size_met,
    prior_n_size_met,
    accountability_met,
    participation_rate_enrolled,
    participation_rate_tested,
    participation_rate,
    participation_rate_loss_count,
    denominator_without_pr_loss,
    status_without_pr_loss
  )

# -----------------------------------------------------------------------------
# 6. QC checks
# -----------------------------------------------------------------------------

academic_indicator_qc_summary <- bind_rows(
  academic_indicator_district_long %>%
    summarize(
      dataset = "academic_indicator_district_long",
      rows = n(),
      school_years = n_distinct(school_year),
      subjects = n_distinct(subject),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_subject_year_cds_group = sum(
        duplicated(paste(subject, school_year, cds_code, student_group))
      ),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_current_denominator = sum(is.na(current_denominator)),
      missing_current_distance_from_standard = sum(is.na(current_distance_from_standard))
    ),
  academic_indicator_district_total %>%
    summarize(
      dataset = "academic_indicator_district_total",
      rows = n(),
      school_years = n_distinct(school_year),
      subjects = n_distinct(subject),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_subject_year_cds_group = sum(
        duplicated(paste(subject, school_year, cds_code, student_group))
      ),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_current_denominator = sum(is.na(current_denominator)),
      missing_current_distance_from_standard = sum(is.na(current_distance_from_standard))
    )
)

academic_indicator_year_qc <- bind_rows(
  academic_indicator_district_long %>%
    count(subject, school_year, name = "rows") %>%
    mutate(dataset = "academic_indicator_district_long"),
  academic_indicator_district_total %>%
    count(subject, school_year, name = "rows") %>%
    mutate(dataset = "academic_indicator_district_total")
) %>%
  select(dataset, subject, school_year, rows) %>%
  arrange(dataset, subject, school_year)

academic_indicator_student_group_qc <- academic_indicator_district_long %>%
  count(subject, student_group, name = "rows") %>%
  arrange(subject, desc(rows))

cat("\n================ ACADEMIC INDICATOR QC SUMMARY ================\n")
print(academic_indicator_qc_summary, width = Inf)

cat("\n================ ACADEMIC INDICATOR YEAR QC ================\n")
print(academic_indicator_year_qc, n = Inf)

cat("\n================ ACADEMIC INDICATOR STUDENT GROUP QC ================\n")
print(academic_indicator_student_group_qc, n = Inf)

# -----------------------------------------------------------------------------
# 7. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(
  academic_indicator_district_long,
  file.path(clean_dir, "academic_indicator_district_long.csv")
)

write_csv(
  academic_indicator_district_total,
  file.path(clean_dir, "academic_indicator_district_total.csv")
)

write_csv(
  academic_indicator_qc_summary,
  file.path(clean_dir, "academic_indicator_qc_summary.csv")
)

write_csv(
  academic_indicator_year_qc,
  file.path(clean_dir, "academic_indicator_year_qc.csv")
)

write_csv(
  academic_indicator_student_group_qc,
  file.path(clean_dir, "academic_indicator_student_group_qc.csv")
)

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "academic_indicator_district_long.csv"), "\n")
cat(file.path(clean_dir, "academic_indicator_district_total.csv"), "\n")
cat(file.path(clean_dir, "academic_indicator_qc_summary.csv"), "\n")
cat(file.path(clean_dir, "academic_indicator_year_qc.csv"), "\n")
cat(file.path(clean_dir, "academic_indicator_student_group_qc.csv"), "\n")