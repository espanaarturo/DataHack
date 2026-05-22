# =============================================================================
# 07_build_district_year_panel.R
# Build final district-year analysis panel for LCFF DataHack project
#
# Raw files are NOT modified.
# This script reads cleaned datasets and writes final analysis-ready panel files.
# =============================================================================

library(tidyverse)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

final_dir <- "data/cleaned/final"
dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

# Cleaned input paths ----------------------------------------------------------

district_lookup_path <- "data/cleaned/directory/district_lookup.csv"

lcff_summary_path <- "data/cleaned/lcff_funding/lcff_summary_district_only.csv"
calpads_upc_path  <- "data/cleaned/lcff_funding/calpads_upc_district_only.csv"

finance_path <- "data/cleaned/finance_spending/current_expense_district.csv"

cumulative_enrollment_path <- "data/cleaned/student_need/cumulative_enrollment_district_total.csv"
homeless_path              <- "data/cleaned/student_need/homeless_enrollment_district_total.csv"

assessment_path <- "data/cleaned/outcomes_assessments/academic_indicator_district_total.csv"

graduation_path <- "data/cleaned/outcomes_graduation/graduation_outcomes_district_total.csv"

# -----------------------------------------------------------------------------
# 2. Read cleaned files
# -----------------------------------------------------------------------------

district_lookup <- read_csv(district_lookup_path, show_col_types = FALSE)

lcff_summary <- read_csv(lcff_summary_path, show_col_types = FALSE)

calpads_upc <- read_csv(calpads_upc_path, show_col_types = FALSE)

finance <- read_csv(finance_path, show_col_types = FALSE)

cumulative_enrollment <- read_csv(cumulative_enrollment_path, show_col_types = FALSE)

homeless <- read_csv(homeless_path, show_col_types = FALSE)

assessment <- read_csv(assessment_path, show_col_types = FALSE)

graduation <- read_csv(graduation_path, show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 3. Prepare each dataset for joining
# -----------------------------------------------------------------------------

# Directory lookup: one row per district
district_lookup_clean <- district_lookup %>%
  select(
    district_cds_code,
    directory_county_name = county_name,
    directory_district_name = district_name,
    district_type,
    grade_low,
    grade_high,
    assistance_status,
    district_city = city,
    district_zip = zip,
    district_region = region,
    district_latitude = latitude,
    district_longitude = longitude
  ) %>%
  distinct(district_cds_code, .keep_all = TRUE)

# LCFF summary: main funding variables
lcff_summary_clean <- lcff_summary %>%
  select(
    school_year,
    district_cds_code,
    county_code,
    district_code,
    county_district_code_7,
    local_educational_agency,
    total_funded_ada,
    unduplicated_pupil_percentage_lcff_summary = unduplicated_pupil_percentage,
    unduplicated_pupil_proportion_lcff_summary = unduplicated_pupil_proportion,
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

# CALPADS UPC: high-need variables for LCFF
calpads_upc_clean <- calpads_upc %>%
  select(
    school_year,
    district_cds_code,
    upc_county_name = county_name,
    upc_district_name = district_name,
    upc_district_type = district_type,
    upc_total_enrollment = total_enrollment,
    upc_free_reduced_meal_program = free_reduced_meal_program,
    upc_foster = foster,
    upc_tribal_foster_youth = tribal_foster_youth,
    upc_homeless = homeless,
    upc_migrant_program = migrant_program,
    upc_direct_certification = direct_certification,
    upc_unduplicated_frpm_eligible_count = unduplicated_frpm_eligible_count,
    upc_english_learner_el = english_learner_el,
    upc_unduplicated_pupil_count = calpads_unduplicated_pupil_count_upc,
    upc_percentage,
    upc_proportion,
    frpm_percentage,
    english_learner_percentage,
    foster_percentage,
    homeless_percentage_lcff_upc = homeless_percentage
  )

# Finance spending
finance_clean <- finance %>%
  select(
    school_year,
    district_cds_code,
    finance_district_name = district_name,
    finance_lea_type = lea_type,
    current_expense_total,
    current_expense_ada,
    current_expense_per_ada,
    current_expense_per_ada_calc
  )

# Cumulative enrollment
cumulative_enrollment_clean <- cumulative_enrollment %>%
  select(
    school_year,
    district_cds_code,
    enrollment_county_name = county_name,
    enrollment_district_name = district_name,
    district_cumulative_enrollment
  )

# Homeless enrollment
homeless_clean <- homeless %>%
  select(
    school_year,
    district_cds_code,
    homeless_county_name = county_name,
    homeless_district_name = district_name,
    district_cumulative_enrollment_hse,
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

# Assessment outcomes: pivot ELA and Math wide
assessment_wide <- assessment %>%
  filter(student_group == "ALL") %>%
  select(
    school_year,
    district_cds_code,
    subject,
    assessment_district_name = district_name,
    assessment_county_name = county_name,
    current_denominator,
    current_distance_from_standard,
    prior_denominator,
    prior_distance_from_standard,
    distance_change,
    status_level,
    change_level,
    color,
    participation_rate_enrolled,
    participation_rate_tested,
    participation_rate,
    participation_rate_loss_count
  ) %>%
  pivot_wider(
    names_from = subject,
    values_from = c(
      current_denominator,
      current_distance_from_standard,
      prior_denominator,
      prior_distance_from_standard,
      distance_change,
      status_level,
      change_level,
      color,
      participation_rate_enrolled,
      participation_rate_tested,
      participation_rate,
      participation_rate_loss_count
    ),
    names_glue = "{subject}_{.value}"
  )

# Graduation outcomes
graduation_clean <- graduation %>%
  select(
    school_year,
    district_cds_code,
    graduation_county_name = county_name,
    graduation_district_name = district_name,
    cohort_students,
    graduation_count,
    graduation_rate,
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
# 4. Build final panel
# -----------------------------------------------------------------------------
# Base = LCFF Summary district-only file, because the project is about LCFF funding.

district_year_panel <- lcff_summary_clean %>%
  left_join(
    calpads_upc_clean,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    finance_clean,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    cumulative_enrollment_clean,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    homeless_clean,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    assessment_wide,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    graduation_clean,
    by = c("school_year", "district_cds_code")
  ) %>%
  left_join(
    district_lookup_clean,
    by = "district_cds_code"
  ) %>%
  mutate(
    # Best available display names
    county_name = coalesce(
      upc_county_name,
      enrollment_county_name,
      homeless_county_name,
      assessment_county_name,
      graduation_county_name,
      directory_county_name
    ),
    district_name = coalesce(
      upc_district_name,
      local_educational_agency,
      enrollment_district_name,
      homeless_district_name,
      assessment_district_name,
      graduation_district_name,
      directory_district_name
    ),
    
    # Core analysis ratios
    supplemental_concentration_share =
      supplemental_concentration_grant / total_lcff_entitlement,
    
    base_grant_share =
      lcff_base_grant_nss_allowance / total_lcff_entitlement,
    
    current_expense_per_pupil =
      current_expense_per_ada,
    
    lcff_entitlement_per_pupil =
      total_lcff_entitlement_per_ada,
    
    supplemental_concentration_per_pupil =
      supplemental_concentration_per_ada,
    
    high_need_share =
      upc_percentage,
    
    high_need_proportion =
      upc_proportion,
    
    # Useful flags
    has_lcff_funding = !is.na(total_lcff_entitlement),
    has_upc = !is.na(upc_unduplicated_pupil_count),
    has_finance = !is.na(current_expense_per_ada),
    has_enrollment = !is.na(district_cumulative_enrollment),
    has_homeless = !is.na(homeless_student_enrollment),
    has_ela = !is.na(ela_current_distance_from_standard),
    has_math = !is.na(math_current_distance_from_standard),
    has_graduation = !is.na(graduation_rate)
  ) %>%
  select(
    school_year,
    district_cds_code,
    county_district_code_7,
    county_code,
    district_code,
    county_name,
    district_name,
    district_type,
    finance_lea_type,
    grade_low,
    grade_high,
    assistance_status,
    district_city,
    district_zip,
    district_region,
    district_latitude,
    district_longitude,
    
    # LCFF funding
    total_funded_ada,
    total_lcff_entitlement,
    lcff_base_grant_nss_allowance,
    total_lcff_supplemental_grant,
    total_lcff_concentration_grant,
    supplemental_concentration_grant,
    county_operations_grant,
    total_local_revenue_or_in_lieu_of_property_taxes,
    education_protection_account_entitlement,
    net_state_aid,
    additional_state_aid_for_msa_guarantee,
    total_lcff_entitlement_per_ada,
    supplemental_concentration_per_ada,
    supplemental_concentration_share,
    base_grant_share,
    
    # LCFF high-need / UPC
    upc_total_enrollment,
    upc_unduplicated_pupil_count,
    upc_percentage,
    upc_proportion,
    upc_unduplicated_frpm_eligible_count,
    upc_free_reduced_meal_program,
    upc_english_learner_el,
    upc_foster,
    upc_tribal_foster_youth,
    upc_homeless,
    upc_migrant_program,
    upc_direct_certification,
    frpm_percentage,
    english_learner_percentage,
    foster_percentage,
    homeless_percentage_lcff_upc,
    
    # Spending
    current_expense_total,
    current_expense_ada,
    current_expense_per_ada,
    current_expense_per_ada_calc,
    
    # Enrollment and homelessness
    district_cumulative_enrollment,
    district_cumulative_enrollment_hse,
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
    missing_unknown_percent,
    
    # ELA outcomes
    ela_current_denominator,
    ela_current_distance_from_standard,
    ela_prior_denominator,
    ela_prior_distance_from_standard,
    ela_distance_change,
    ela_status_level,
    ela_change_level,
    ela_color,
    ela_participation_rate_enrolled,
    ela_participation_rate_tested,
    ela_participation_rate,
    ela_participation_rate_loss_count,
    
    # Math outcomes
    math_current_denominator,
    math_current_distance_from_standard,
    math_prior_denominator,
    math_prior_distance_from_standard,
    math_distance_change,
    math_status_level,
    math_change_level,
    math_color,
    math_participation_rate_enrolled,
    math_participation_rate_tested,
    math_participation_rate,
    math_participation_rate_loss_count,
    
    # Graduation outcomes
    cohort_students,
    graduation_count,
    graduation_rate,
    met_uc_csu_grad_reqs_count,
    met_uc_csu_grad_reqs_rate,
    seal_of_biliteracy_count,
    seal_of_biliteracy_rate,
    golden_state_seal_merit_diploma_count,
    golden_state_seal_merit_diploma_rate,
    dropout_count,
    dropout_rate,
    still_enrolled_count,
    still_enrolled_rate,
    
    # Analysis aliases and flags
    high_need_share,
    high_need_proportion,
    current_expense_per_pupil,
    lcff_entitlement_per_pupil,
    supplemental_concentration_per_pupil,
    has_lcff_funding,
    has_upc,
    has_finance,
    has_enrollment,
    has_homeless,
    has_ela,
    has_math,
    has_graduation
  ) %>%
  arrange(school_year, district_cds_code)

# -----------------------------------------------------------------------------
# 5. QC checks
# -----------------------------------------------------------------------------

panel_qc_summary <- district_year_panel %>%
  summarize(
    dataset = "district_year_panel",
    rows = n(),
    school_years = n_distinct(school_year),
    unique_districts = n_distinct(district_cds_code),
    duplicate_year_district = sum(duplicated(paste(school_year, district_cds_code))),
    missing_district_cds_code = sum(is.na(district_cds_code)),
    missing_district_name = sum(is.na(district_name)),
    missing_total_lcff_entitlement = sum(is.na(total_lcff_entitlement)),
    missing_upc = sum(is.na(upc_unduplicated_pupil_count)),
    missing_current_expense_per_ada = sum(is.na(current_expense_per_ada)),
    missing_district_cumulative_enrollment = sum(is.na(district_cumulative_enrollment)),
    missing_homeless_student_enrollment = sum(is.na(homeless_student_enrollment)),
    missing_ela_distance = sum(is.na(ela_current_distance_from_standard)),
    missing_math_distance = sum(is.na(math_current_distance_from_standard)),
    missing_graduation_rate = sum(is.na(graduation_rate))
  )

panel_year_qc <- district_year_panel %>%
  group_by(school_year) %>%
  summarize(
    rows = n(),
    districts = n_distinct(district_cds_code),
    has_lcff_funding = sum(has_lcff_funding),
    has_upc = sum(has_upc),
    has_finance = sum(has_finance),
    has_enrollment = sum(has_enrollment),
    has_homeless = sum(has_homeless),
    has_ela = sum(has_ela),
    has_math = sum(has_math),
    has_graduation = sum(has_graduation),
    .groups = "drop"
  )

cat("\n================ FINAL PANEL QC SUMMARY ================\n")
print(panel_qc_summary, width = Inf)

cat("\n================ FINAL PANEL YEAR QC ================\n")
print(panel_year_qc, n = Inf, width = Inf)

# -----------------------------------------------------------------------------
# 6. Save final outputs
# -----------------------------------------------------------------------------

write_csv(
  district_year_panel,
  file.path(final_dir, "district_year_panel.csv")
)

write_csv(
  panel_qc_summary,
  file.path(final_dir, "district_year_panel_qc_summary.csv")
)

write_csv(
  panel_year_qc,
  file.path(final_dir, "district_year_panel_year_qc.csv")
)

cat("\nSaved final outputs:\n")
cat(file.path(final_dir, "district_year_panel.csv"), "\n")
cat(file.path(final_dir, "district_year_panel_qc_summary.csv"), "\n")
cat(file.path(final_dir, "district_year_panel_year_qc.csv"), "\n")