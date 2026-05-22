# =============================================================================
# 02_clean_lcff_funding.R
# Clean LCFF Summary and CALPADS UPC files
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/lcff_funding/
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/lcff_funding"
clean_dir <- "data/cleaned/lcff_funding"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. File specs based on actual workbook inspection
# -----------------------------------------------------------------------------

lcff_specs <- tribble(
  ~school_year, ~file_name,              ~sheet_name,                    ~skip_rows,
  "2019_20",    "lcffsummary1920.xlsx",  "LCFF Sum 19-20 AN R1",          7,
  "2020_21",    "lcffsummary2021.xlsx",  "LCFF Summary 20-21 ANR1",       6,
  "2021_22",    "lcffsummary2122.xlsx",  "LCFF Summary 21-22 AN R1",      6,
  "2022_23",    "lcffsummary2223.xlsx",  "LCFF Summary 22-23 ANR1",       6,
  "2023_24",    "lcffsummary2324.xlsx",  "LCFF Summary 23-24 AN R1",      5,
  "2024_25",    "lcffsummary2425.xlsx",  "LCFF Summary 24-25 Annual",     5
)

upc_specs <- tribble(
  ~school_year, ~file_name,          ~sheet_name,                    ~skip_rows,
  "2019_20",    "cupc1920-k12.xlsx", "LEA-Level CALPADS UPC Data",    1,
  "2020_21",    "cupc2021-k12.xlsx", "LEA-Level CALPADS UPC Data",    1,
  "2021_22",    "cupc2122-k12.xlsx", "LEA-Level CALPADS UPC Data",    1,
  "2022_23",    "cupc2223-k12.xlsx", "LEA-Level CALPADS UPC Data",    1,
  "2023_24",    "cupc2324-k12.xlsx", "LEA-Level CALPADS UPC Data",    1,
  "2024_25",    "cupc2425-k12.xlsx", "LEA-Level CALPADS UPC Data",    1
)

# Note:
# 2025-26 raw files are intentionally excluded for now because our outcome data
# only runs through 2024-25, and 2025-26 LCFF Summary is only P-1.

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
  parse_number(x)
}

to_percent <- function(x) {
  out <- parse_number(as.character(x))
  
  # If Excel stores percentages as decimals, convert to percent scale.
  # Example: 0.635 becomes 63.5.
  ifelse(!is.na(out) & out <= 1, out * 100, out)
}

add_id_fields <- function(df) {
  df %>%
    mutate(
      county_code = clean_id(county_code, width = 2),
      district_code = clean_id(district_code, width = 5),
      school_code = clean_id(school_code, width = 7),
      county_district_code_7 = paste0(county_code, district_code),
      cds_code = paste0(county_code, district_code, school_code),
      district_cds_code = paste0(county_code, district_code, "0000000")
    )
}

safe_select_col <- function(df, pattern) {
  names(df)[str_detect(names(df), pattern)][1]
}

# -----------------------------------------------------------------------------
# 4. Clean LCFF Summary files
# -----------------------------------------------------------------------------

clean_one_lcff <- function(school_year, file_name, sheet_name, skip_rows) {
  
  message("Cleaning LCFF Summary: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_excel(
    path = file_path,
    sheet = sheet_name,
    skip = skip_rows,
    col_types = "text",
    .name_repair = "minimal"
  ) %>%
    clean_names()
  
  # Original column names differ slightly across years, so rename by position.
  # 2019-20 has 12 columns; 2020-21 onward has 20 columns.
  if (ncol(raw) == 12) {
    
    names(raw) <- c(
      "county_code",
      "district_code",
      "school_code",
      "local_educational_agency",
      "charter_number",
      "total_funded_ada_or_alternative_education_grant_ada",
      "unduplicated_pupil_percentage",
      "lcff_base_grant_nss_allowance",
      "total_lcff_supplemental_grant",
      "total_lcff_concentration_grant",
      "county_operations_grant",
      "total_lcff_entitlement"
    )
    
  } else if (ncol(raw) >= 20) {
    
    names(raw)[1:20] <- c(
      "county_code",
      "district_code",
      "school_code",
      "local_educational_agency",
      "charter_number",
      "funded_tk_k_3_ada",
      "funded_4_6_ada",
      "funded_7_8_ada",
      "funded_9_12_ada",
      "total_funded_ada_or_alternative_education_grant_ada",
      "unduplicated_pupil_percentage",
      "lcff_base_grant_nss_allowance",
      "total_lcff_supplemental_grant",
      "total_lcff_concentration_grant",
      "county_operations_grant",
      "total_lcff_entitlement",
      "total_local_revenue_or_in_lieu_of_property_taxes",
      "education_protection_account_entitlement",
      "net_state_aid",
      "additional_state_aid_for_msa_guarantee"
    )
    
    raw <- raw %>%
      select(1:20)
    
  } else {
    stop("Unexpected LCFF Summary column count in ", file_name, ": ", ncol(raw))
  }
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      source_sheet = sheet_name,
      school_year = school_year
    ) %>%
    add_id_fields() %>%
    filter(
      !is.na(county_code),
      !is.na(district_code),
      !is.na(school_code),
      str_detect(county_code, "^[0-9]{2}$"),
      str_detect(district_code, "^[0-9]{5}$"),
      str_detect(school_code, "^[0-9]{7}$")
    ) %>%
    mutate(
      is_district_row = school_code == "0000000",
      is_charter_row = !is.na(charter_number) & charter_number != "",
      funded_tk_k_3_ada = to_number(if ("funded_tk_k_3_ada" %in% names(.)) funded_tk_k_3_ada else NA),
      funded_4_6_ada = to_number(if ("funded_4_6_ada" %in% names(.)) funded_4_6_ada else NA),
      funded_7_8_ada = to_number(if ("funded_7_8_ada" %in% names(.)) funded_7_8_ada else NA),
      funded_9_12_ada = to_number(if ("funded_9_12_ada" %in% names(.)) funded_9_12_ada else NA),
      total_funded_ada = to_number(total_funded_ada_or_alternative_education_grant_ada),
      unduplicated_pupil_percentage = to_percent(unduplicated_pupil_percentage),
      unduplicated_pupil_proportion = unduplicated_pupil_percentage / 100,
      lcff_base_grant_nss_allowance = to_number(lcff_base_grant_nss_allowance),
      total_lcff_supplemental_grant = to_number(total_lcff_supplemental_grant),
      total_lcff_concentration_grant = to_number(total_lcff_concentration_grant),
      county_operations_grant = to_number(county_operations_grant),
      total_lcff_entitlement = to_number(total_lcff_entitlement),
      total_local_revenue_or_in_lieu_of_property_taxes = to_number(
        if ("total_local_revenue_or_in_lieu_of_property_taxes" %in% names(.)) {
          total_local_revenue_or_in_lieu_of_property_taxes
        } else {
          NA
        }
      ),
      education_protection_account_entitlement = to_number(
        if ("education_protection_account_entitlement" %in% names(.)) {
          education_protection_account_entitlement
        } else {
          NA
        }
      ),
      net_state_aid = to_number(
        if ("net_state_aid" %in% names(.)) net_state_aid else NA
      ),
      additional_state_aid_for_msa_guarantee = to_number(
        if ("additional_state_aid_for_msa_guarantee" %in% names(.)) {
          additional_state_aid_for_msa_guarantee
        } else {
          NA
        }
      ),
      supplemental_concentration_grant =
        total_lcff_supplemental_grant + total_lcff_concentration_grant,
      supplemental_concentration_per_ada =
        supplemental_concentration_grant / total_funded_ada,
      total_lcff_entitlement_per_ada =
        total_lcff_entitlement / total_funded_ada
    ) %>%
    transmute(
      source_file,
      source_sheet,
      school_year,
      county_code,
      district_code,
      school_code,
      county_district_code_7,
      cds_code,
      district_cds_code,
      local_educational_agency,
      charter_number,
      is_district_row,
      is_charter_row,
      funded_tk_k_3_ada,
      funded_4_6_ada,
      funded_7_8_ada,
      funded_9_12_ada,
      total_funded_ada,
      unduplicated_pupil_percentage,
      unduplicated_pupil_proportion,
      lcff_base_grant_nss_allowance,
      total_lcff_supplemental_grant,
      total_lcff_concentration_grant,
      supplemental_concentration_grant,
      county_operations_grant,
      total_lcff_entitlement,
      total_local_revenue_or_in_lieu_of_property_taxes,
      education_protection_account_entitlement,
      net_state_aid,
      additional_state_aid_for_msa_guarantee,
      supplemental_concentration_per_ada,
      total_lcff_entitlement_per_ada
    )
  
  cleaned
}

lcff_summary_all <- pmap_dfr(lcff_specs, clean_one_lcff)

lcff_summary_district <- lcff_summary_all %>%
  filter(is_district_row)

# -----------------------------------------------------------------------------
# 5. Clean CALPADS UPC files
# -----------------------------------------------------------------------------

clean_one_upc <- function(school_year, file_name, sheet_name, skip_rows) {
  
  message("Cleaning CALPADS UPC: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_excel(
    path = file_path,
    sheet = sheet_name,
    skip = skip_rows,
    col_types = "text",
    .name_repair = "minimal"
  ) %>%
    clean_names()
  
  # Make year-specific missing columns explicit.
  needed_cols <- c(
    "academic_year",
    "county_code",
    "district_code",
    "school_code",
    "county_name",
    "district_name",
    "school_name",
    "district_type",
    "school_type",
    "charter_school_y_n",
    "charter_number",
    "charter_funding_type",
    "total_enrollment",
    "free_reduced_meal_program",
    "foster",
    "tribal_foster_youth",
    "homeless",
    "migrant_program",
    "direct_certification",
    "unduplicated_frpm_eligible_count",
    "english_learner_el",
    "calpads_unduplicated_pupil_count_upc",
    "calpads_fall_1_certification_status_y_n",
    "nslp_provision_status"
  )
  
  missing_cols <- setdiff(needed_cols, names(raw))
  
  if (length(missing_cols) > 0) {
    raw[missing_cols] <- NA_character_
  }
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      source_sheet = sheet_name,
      school_year = school_year
    ) %>%
    add_id_fields() %>%
    filter(
      !is.na(county_code),
      !is.na(district_code),
      !is.na(school_code),
      str_detect(county_code, "^[0-9]{2}$"),
      str_detect(district_code, "^[0-9]{5}$"),
      str_detect(school_code, "^[0-9]{7}$")
    ) %>%
    mutate(
      is_district_row = school_code == "0000000",
      is_charter_row = charter_school_y_n == "Y",
      total_enrollment = to_number(total_enrollment),
      free_reduced_meal_program = to_number(free_reduced_meal_program),
      foster = to_number(foster),
      tribal_foster_youth = to_number(tribal_foster_youth),
      homeless = to_number(homeless),
      migrant_program = to_number(migrant_program),
      direct_certification = to_number(direct_certification),
      unduplicated_frpm_eligible_count = to_number(unduplicated_frpm_eligible_count),
      english_learner_el = to_number(english_learner_el),
      calpads_unduplicated_pupil_count_upc = to_number(calpads_unduplicated_pupil_count_upc),
      upc_percentage = 100 * calpads_unduplicated_pupil_count_upc / total_enrollment,
      upc_proportion = calpads_unduplicated_pupil_count_upc / total_enrollment,
      frpm_percentage = 100 * unduplicated_frpm_eligible_count / total_enrollment,
      english_learner_percentage = 100 * english_learner_el / total_enrollment,
      foster_percentage = 100 * foster / total_enrollment,
      homeless_percentage = 100 * homeless / total_enrollment
    ) %>%
    transmute(
      source_file,
      source_sheet,
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
      school_name,
      district_type,
      school_type,
      charter_school_y_n,
      charter_number,
      charter_funding_type,
      is_district_row,
      is_charter_row,
      total_enrollment,
      free_reduced_meal_program,
      foster,
      tribal_foster_youth,
      homeless,
      migrant_program,
      direct_certification,
      unduplicated_frpm_eligible_count,
      english_learner_el,
      calpads_unduplicated_pupil_count_upc,
      upc_percentage,
      upc_proportion,
      frpm_percentage,
      english_learner_percentage,
      foster_percentage,
      homeless_percentage,
      calpads_fall_1_certification_status_y_n,
      nslp_provision_status
    )
  
  cleaned
}

calpads_upc_all <- pmap_dfr(upc_specs, clean_one_upc)

calpads_upc_district <- calpads_upc_all %>%
  filter(is_district_row)

# -----------------------------------------------------------------------------
# 6. QC summary
# -----------------------------------------------------------------------------

qc_summary <- bind_rows(
  lcff_summary_all %>%
    summarize(
      dataset = "lcff_summary_all_leas",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_total_funded_ada = sum(is.na(total_funded_ada)),
      missing_total_lcff_entitlement = sum(is.na(total_lcff_entitlement))
    ),
  lcff_summary_district %>%
    summarize(
      dataset = "lcff_summary_district_only",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_total_funded_ada = sum(is.na(total_funded_ada)),
      missing_total_lcff_entitlement = sum(is.na(total_lcff_entitlement))
    ),
  calpads_upc_all %>%
    summarize(
      dataset = "calpads_upc_all_leas",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_total_enrollment = sum(is.na(total_enrollment)),
      missing_upc = sum(is.na(calpads_unduplicated_pupil_count_upc))
    ),
  calpads_upc_district %>%
    summarize(
      dataset = "calpads_upc_district_only",
      rows = n(),
      school_years = n_distinct(school_year),
      unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
      duplicate_year_cds = sum(duplicated(paste(school_year, cds_code))),
      missing_cds_code = sum(is.na(cds_code)),
      missing_total_enrollment = sum(is.na(total_enrollment)),
      missing_upc = sum(is.na(calpads_unduplicated_pupil_count_upc))
    )
)

year_qc <- bind_rows(
  lcff_summary_all %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "lcff_summary_all_leas"),
  lcff_summary_district %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "lcff_summary_district_only"),
  calpads_upc_all %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "calpads_upc_all_leas"),
  calpads_upc_district %>%
    count(school_year, name = "rows") %>%
    mutate(dataset = "calpads_upc_district_only")
) %>%
  select(dataset, school_year, rows)

cat("\n================ LCFF FUNDING QC SUMMARY ================\n")
print(qc_summary, width = Inf)

cat("\n================ LCFF FUNDING YEAR QC ================\n")
print(year_qc, n = Inf)

# -----------------------------------------------------------------------------
# 7. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(
  lcff_summary_all,
  file.path(clean_dir, "lcff_summary_all_leas.csv")
)

write_csv(
  lcff_summary_district,
  file.path(clean_dir, "lcff_summary_district_only.csv")
)

write_csv(
  calpads_upc_all,
  file.path(clean_dir, "calpads_upc_all_leas.csv")
)

write_csv(
  calpads_upc_district,
  file.path(clean_dir, "calpads_upc_district_only.csv")
)

write_csv(
  qc_summary,
  file.path(clean_dir, "lcff_funding_qc_summary.csv")
)

write_csv(
  year_qc,
  file.path(clean_dir, "lcff_funding_year_qc.csv")
)

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "lcff_summary_all_leas.csv"), "\n")
cat(file.path(clean_dir, "lcff_summary_district_only.csv"), "\n")
cat(file.path(clean_dir, "calpads_upc_all_leas.csv"), "\n")
cat(file.path(clean_dir, "calpads_upc_district_only.csv"), "\n")
cat(file.path(clean_dir, "lcff_funding_qc_summary.csv"), "\n")
cat(file.path(clean_dir, "lcff_funding_year_qc.csv"), "\n")