# =============================================================================
# 06_clean_outcomes_graduation.R
# Clean Adjusted Cohort Graduation Rate and Outcome Data files
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/outcomes_graduation/
# =============================================================================

library(tidyverse)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/outcomes_graduation"
clean_dir <- "data/cleaned/outcomes_graduation"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. File specs based on actual file inspection
# -----------------------------------------------------------------------------

acgr_specs <- tribble(
  ~school_year, ~file_name,
  "2019_20",    "acgr20.txt",
  "2020_21",    "acgr21.txt",
  "2021_22",    "acgr22-v3.txt",
  "2022_23",    "acgr23-v2.txt",
  "2023_24",    "acgr24.txt",
  "2024_25",    "acgr25.txt"
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
# 4. Clean one ACGR file
# -----------------------------------------------------------------------------

clean_one_acgr <- function(school_year, file_name) {
  
  message("Cleaning graduation file: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_tsv(
    file_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8"),
    name_repair = "minimal"
  )
  
  raw <- raw %>%
    rename_with(~ str_replace_all(.x, "\ufeff", ""))
  
  n_cols <- ncol(raw)
  
  if (n_cols == 34) {
    
    names(raw) <- c(
      "academic_year",
      "aggregate_level",
      "county_code",
      "district_code",
      "school_code",
      "county_name",
      "district_name",
      "school_name",
      "charter_school",
      "dass",
      "reporting_category",
      "cohort_students",
      "regular_hs_diploma_graduates_count",
      "regular_hs_diploma_graduates_rate",
      "met_uc_csu_grad_reqs_count",
      "met_uc_csu_grad_reqs_rate",
      "seal_of_biliteracy_count",
      "seal_of_biliteracy_rate",
      "golden_state_seal_merit_diploma_count",
      "golden_state_seal_merit_diploma_rate",
      "chspe_completer_count",
      "chspe_completer_rate",
      "adult_ed_hs_diploma_count",
      "adult_ed_hs_diploma_rate",
      "sped_certificate_count",
      "sped_certificate_rate",
      "ged_completer_count",
      "ged_completer_rate",
      "other_transfer_count",
      "other_transfer_rate",
      "dropout_count",
      "dropout_rate",
      "still_enrolled_count",
      "still_enrolled_rate"
    )
    
    raw <- raw %>%
      mutate(
        graduates_meeting_local_requirements_exemption_count = NA_character_,
        graduates_meeting_local_requirements_exemption_rate = NA_character_,
        cpp_completer_count = NA_character_,
        cpp_completer_rate = NA_character_
      )
    
  } else if (n_cols == 36) {
    
    names(raw) <- c(
      "academic_year",
      "aggregate_level",
      "county_code",
      "district_code",
      "school_code",
      "county_name",
      "district_name",
      "school_name",
      "charter_school",
      "dass",
      "reporting_category",
      "cohort_students",
      "regular_hs_diploma_graduates_count",
      "regular_hs_diploma_graduates_rate",
      "met_uc_csu_grad_reqs_count",
      "met_uc_csu_grad_reqs_rate",
      "seal_of_biliteracy_count",
      "seal_of_biliteracy_rate",
      "golden_state_seal_merit_diploma_count",
      "golden_state_seal_merit_diploma_rate",
      "graduates_meeting_local_requirements_exemption_count",
      "graduates_meeting_local_requirements_exemption_rate",
      "cpp_completer_count",
      "cpp_completer_rate",
      "adult_ed_hs_diploma_count",
      "adult_ed_hs_diploma_rate",
      "sped_certificate_count",
      "sped_certificate_rate",
      "ged_completer_count",
      "ged_completer_rate",
      "other_transfer_count",
      "other_transfer_rate",
      "dropout_count",
      "dropout_rate",
      "still_enrolled_count",
      "still_enrolled_rate"
    )
    
    raw <- raw %>%
      mutate(
        chspe_completer_count = NA_character_,
        chspe_completer_rate = NA_character_
      )
    
  } else {
    stop("Unexpected number of columns in ", file_name, ": ", n_cols)
  }
  
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
      reporting_category = str_squish(reporting_category)
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
      cohort_students = to_number(cohort_students),
      
      regular_hs_diploma_graduates_count =
        to_number(regular_hs_diploma_graduates_count),
      regular_hs_diploma_graduates_rate =
        to_number(regular_hs_diploma_graduates_rate),
      
      met_uc_csu_grad_reqs_count =
        to_number(met_uc_csu_grad_reqs_count),
      met_uc_csu_grad_reqs_rate =
        to_number(met_uc_csu_grad_reqs_rate),
      
      seal_of_biliteracy_count =
        to_number(seal_of_biliteracy_count),
      seal_of_biliteracy_rate =
        to_number(seal_of_biliteracy_rate),
      
      golden_state_seal_merit_diploma_count =
        to_number(golden_state_seal_merit_diploma_count),
      golden_state_seal_merit_diploma_rate =
        to_number(golden_state_seal_merit_diploma_rate),
      
      chspe_completer_count =
        to_number(chspe_completer_count),
      chspe_completer_rate =
        to_number(chspe_completer_rate),
      
      graduates_meeting_local_requirements_exemption_count =
        to_number(graduates_meeting_local_requirements_exemption_count),
      graduates_meeting_local_requirements_exemption_rate =
        to_number(graduates_meeting_local_requirements_exemption_rate),
      
      cpp_completer_count =
        to_number(cpp_completer_count),
      cpp_completer_rate =
        to_number(cpp_completer_rate),
      
      adult_ed_hs_diploma_count =
        to_number(adult_ed_hs_diploma_count),
      adult_ed_hs_diploma_rate =
        to_number(adult_ed_hs_diploma_rate),
      
      sped_certificate_count =
        to_number(sped_certificate_count),
      sped_certificate_rate =
        to_number(sped_certificate_rate),
      
      ged_completer_count =
        to_number(ged_completer_count),
      ged_completer_rate =
        to_number(ged_completer_rate),
      
      other_transfer_count =
        to_number(other_transfer_count),
      other_transfer_rate =
        to_number(other_transfer_rate),
      
      dropout_count =
        to_number(dropout_count),
      dropout_rate =
        to_number(dropout_rate),
      
      still_enrolled_count =
        to_number(still_enrolled_count),
      still_enrolled_rate =
        to_number(still_enrolled_rate)
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
      cohort_students,
      regular_hs_diploma_graduates_count,
      regular_hs_diploma_graduates_rate,
      met_uc_csu_grad_reqs_count,
      met_uc_csu_grad_reqs_rate,
      seal_of_biliteracy_count,
      seal_of_biliteracy_rate,
      golden_state_seal_merit_diploma_count,
      golden_state_seal_merit_diploma_rate,
      chspe_completer_count,
      chspe_completer_rate,
      graduates_meeting_local_requirements_exemption_count,
      graduates_meeting_local_requirements_exemption_rate,
      cpp_completer_count,
      cpp_completer_rate,
      adult_ed_hs_diploma_count,
      adult_ed_hs_diploma_rate,
      sped_certificate_count,
      sped_certificate_rate,
      ged_completer_count,
      ged_completer_rate,
      other_transfer_count,
      other_transfer_rate,
      dropout_count,
      dropout_rate,
      still_enrolled_count,
      still_enrolled_rate
    )
  
  cleaned
}

# -----------------------------------------------------------------------------
# 5. Clean all files
# -----------------------------------------------------------------------------

graduation_outcomes_district_long <- pmap_dfr(
  acgr_specs,
  clean_one_acgr
)

graduation_outcomes_district_total <- graduation_outcomes_district_long %>%
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
    reporting_category,
    cohort_students,
    graduation_count = regular_hs_diploma_graduates_count,
    graduation_rate = regular_hs_diploma_graduates_rate,
    met_uc_csu_grad_reqs_count,
    met_uc_csu_grad_reqs_rate,
    seal_of_biliteracy_count,
    seal_of_biliteracy_rate,
    golden_state_seal_merit_diploma_count,
    golden_state_seal_merit_diploma_rate,
    dropout_count,
    dropout_rate,
    still_enrolled_count,
    still_enrolled_rate
  )

# -----------------------------------------------------------------------------
# 6. QC checks
# -----------------------------------------------------------------------------

graduation_qc_summary <- bind_rows(
  graduation_outcomes_district_long %>%
    summarize(
      dataset = "graduation_outcomes_district_long",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds_category =
        sum(duplicated(paste(school_year, cds_code, reporting_category))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_cohort_students = sum(is.na(cohort_students)),
      missing_graduation_rate = sum(is.na(regular_hs_diploma_graduates_rate)),
      missing_dropout_rate = sum(is.na(dropout_rate))
    ),
  graduation_outcomes_district_total %>%
    summarize(
      dataset = "graduation_outcomes_district_total",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds_category =
        sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_district_name = sum(is.na(district_name)),
      missing_cohort_students = sum(is.na(cohort_students)),
      missing_graduation_rate = sum(is.na(graduation_rate)),
      missing_dropout_rate = sum(is.na(dropout_rate))
    )
)

graduation_year_qc <- bind_rows(
  graduation_outcomes_district_long %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "graduation_outcomes_district_long"),
  graduation_outcomes_district_total %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "graduation_outcomes_district_total")
) %>%
  select(dataset, school_year, rows) %>%
  arrange(dataset, school_year)

graduation_reporting_category_qc <- graduation_outcomes_district_long %>%
  count(reporting_category, name = "rows") %>%
  arrange(reporting_category)

cat("\n================ GRADUATION QC SUMMARY ================\n")
print(graduation_qc_summary, width = Inf)

cat("\n================ GRADUATION YEAR QC ================\n")
print(graduation_year_qc, n = Inf)

cat("\n================ GRADUATION REPORTING CATEGORY QC ================\n")
print(graduation_reporting_category_qc, n = Inf)

# -----------------------------------------------------------------------------
# 7. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(
  graduation_outcomes_district_long,
  file.path(clean_dir, "graduation_outcomes_district_long.csv")
)

write_csv(
  graduation_outcomes_district_total,
  file.path(clean_dir, "graduation_outcomes_district_total.csv")
)

write_csv(
  graduation_qc_summary,
  file.path(clean_dir, "graduation_qc_summary.csv")
)

write_csv(
  graduation_year_qc,
  file.path(clean_dir, "graduation_year_qc.csv")
)

write_csv(
  graduation_reporting_category_qc,
  file.path(clean_dir, "graduation_reporting_category_qc.csv")
)

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "graduation_outcomes_district_long.csv"), "\n")
cat(file.path(clean_dir, "graduation_outcomes_district_total.csv"), "\n")
cat(file.path(clean_dir, "graduation_qc_summary.csv"), "\n")
cat(file.path(clean_dir, "graduation_year_qc.csv"), "\n")
cat(file.path(clean_dir, "graduation_reporting_category_qc.csv"), "\n")