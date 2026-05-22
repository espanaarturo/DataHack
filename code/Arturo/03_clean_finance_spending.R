# =============================================================================
# 03_clean_finance_spending.R
# Clean Current Expense of Education / Current Cost of Education files
#
# Raw files are NOT modified.
# Cleaned outputs are written to data/cleaned/finance_spending/
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

raw_dir <- "data/raw/finance_spending"
clean_dir <- "data/cleaned/finance_spending"

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. File specs based on actual workbook inspection
# -----------------------------------------------------------------------------

finance_specs <- tribble(
  ~school_year, ~file_name,                 ~sheet_name, ~skip_rows,
  "2019_20",    "currentexpense1920.xlsx",  "CDS",       10,
  "2020_21",    "currentexpense2021.xlsx",  "District",  10,
  "2021_22",    "currentexpense2122.xlsx",  "District",  10,
  "2022_23",    "currentexpense2223.xlsx",  "District",  10,
  "2023_24",    "currentexpense2324.xlsx",  "District",  10,
  "2024_25",    "currentexpense2425.xlsx",  "District",  10
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
  parse_number(x)
}

# -----------------------------------------------------------------------------
# 4. Clean one finance file
# -----------------------------------------------------------------------------

clean_one_current_expense <- function(school_year, file_name, sheet_name, skip_rows) {
  
  message("Cleaning Current Expense file: ", file_name)
  
  file_path <- file.path(raw_dir, file_name)
  
  raw <- read_excel(
    path = file_path,
    sheet = sheet_name,
    skip = skip_rows,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  # 2021-22 has extra blank columns, so keep only the first 7 useful columns.
  raw <- raw %>%
    select(1:7)
  
  names(raw) <- c(
    "county_code",
    "district_code",
    "district_name",
    "edp_365",
    "current_expense_ada",
    "current_expense_per_ada",
    "lea_type"
  )
  
  cleaned <- raw %>%
    mutate(
      source_file = file_name,
      source_sheet = sheet_name,
      school_year = school_year,
      county_code = clean_id(county_code, width = 2),
      district_code = clean_id(district_code, width = 5),
      school_code = "0000000",
      county_district_code_7 = paste0(county_code, district_code),
      cds_code = paste0(county_code, district_code, school_code),
      district_cds_code = cds_code,
      district_name = str_squish(district_name),
      lea_type = str_squish(lea_type),
      lea_type = case_when(
        lea_type %in% c("High", "High School") ~ "High",
        lea_type %in% c("Common Admin", "Comm Admin") ~ "Common Admin",
        TRUE ~ lea_type
      ),
      current_expense_total = to_number(edp_365),
      current_expense_ada = to_number(current_expense_ada),
      current_expense_per_ada = to_number(current_expense_per_ada),
      current_expense_per_ada_calc = current_expense_total / current_expense_ada,
      current_expense_per_ada_diff =
        current_expense_per_ada - current_expense_per_ada_calc
    ) %>%
    filter(
      !is.na(county_code),
      !is.na(district_code),
      str_detect(county_code, "^[0-9]{2}$"),
      str_detect(district_code, "^[0-9]{5}$")
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
      district_name,
      lea_type,
      current_expense_total,
      current_expense_ada,
      current_expense_per_ada,
      current_expense_per_ada_calc,
      current_expense_per_ada_diff
    )
  
  cleaned
}

# -----------------------------------------------------------------------------
# 5. Clean all finance files
# -----------------------------------------------------------------------------

current_expense_district <- pmap_dfr(
  finance_specs,
  clean_one_current_expense
)

# -----------------------------------------------------------------------------
# 6. QC checks
# -----------------------------------------------------------------------------

finance_qc_summary <- current_expense_district %>%
  summarize(
    dataset = "current_expense_district",
    rows = n(),
    school_years = n_distinct(school_year),
    unique_cds_codes = n_distinct(cds_code, na.rm = TRUE),
    duplicate_year_cds = sum(duplicated(paste(school_year, cds_code))),
    missing_cds_code = sum(is.na(cds_code)),
    missing_district_name = sum(is.na(district_name)),
    missing_current_expense_total = sum(is.na(current_expense_total)),
    missing_current_expense_ada = sum(is.na(current_expense_ada)),
    missing_current_expense_per_ada = sum(is.na(current_expense_per_ada)),
    max_abs_per_ada_diff = max(abs(current_expense_per_ada_diff), na.rm = TRUE)
  )

finance_year_qc <- current_expense_district %>%
  count(school_year, name = "rows") %>%
  arrange(school_year)

finance_lea_type_qc <- current_expense_district %>%
  count(school_year, lea_type, name = "rows") %>%
  arrange(school_year, desc(rows))

cat("\n================ FINANCE SPENDING QC SUMMARY ================\n")
print(finance_qc_summary, width = Inf)

cat("\n================ FINANCE SPENDING YEAR QC ================\n")
print(finance_year_qc, n = Inf)

cat("\n================ FINANCE SPENDING LEA TYPE QC ================\n")
print(finance_lea_type_qc, n = Inf)

# -----------------------------------------------------------------------------
# 7. Save cleaned outputs
# -----------------------------------------------------------------------------

write_csv(
  current_expense_district,
  file.path(clean_dir, "current_expense_district.csv")
)

write_csv(
  finance_qc_summary,
  file.path(clean_dir, "finance_spending_qc_summary.csv")
)

write_csv(
  finance_year_qc,
  file.path(clean_dir, "finance_spending_year_qc.csv")
)

write_csv(
  finance_lea_type_qc,
  file.path(clean_dir, "finance_spending_lea_type_qc.csv")
)

cat("\nSaved cleaned outputs:\n")
cat(file.path(clean_dir, "current_expense_district.csv"), "\n")
cat(file.path(clean_dir, "finance_spending_qc_summary.csv"), "\n")
cat(file.path(clean_dir, "finance_spending_year_qc.csv"), "\n")
cat(file.path(clean_dir, "finance_spending_lea_type_qc.csv"), "\n")