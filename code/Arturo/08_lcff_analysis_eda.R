# =============================================================================
# 08_lcff_analysis_eda.R
# LCFF district-year analysis, EDA, plots, models, and priority districts
#
# Goal:
# Analyze whether LCFF funding is targeted toward high-need districts and whether
# higher targeted funding is associated with improved academic outcomes.
#
# Important fixes:
# - Removes invalid Inf per-ADA values caused by districts with 0 funded ADA.
# - Uses ADA-weighted funding averages instead of raw means.
# - Uses regular school districts for the main presentation analysis.
# - Saves all plots with white backgrounds for slide use.
# =============================================================================

# ----- 0. Packages -----------------------------------------------------------

library(tidyverse)
library(broom)
library(scales)

# ----- 1. Paths --------------------------------------------------------------

panel_path <- "data/cleaned/final/district_year_panel.csv"

fig_dir <- "outputs/figures"
table_dir <- "outputs/tables"

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ----- 2. Helper functions ---------------------------------------------------

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & is.finite(x) &
    !is.na(w) & is.finite(w) &
    w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  weighted.mean(x[ok], w[ok])
}

theme_lcff <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray25")
    )
}

save_lcff_plot <- function(filename, plot, width = 9, height = 6) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# ----- 3. Load data ----------------------------------------------------------

panel <- read_csv(panel_path, show_col_types = FALSE)

school_year_levels <- sort(unique(panel$school_year))

glimpse(panel)

# ----- 4. Prepare analysis variables ----------------------------------------

analysis <- panel %>%
  mutate(
    school_year_plot = factor(school_year, levels = school_year_levels),
    
    # Raw per-ADA variables from cleaned panel
    raw_lcff_per_ada = total_lcff_entitlement_per_ada,
    raw_supp_conc_per_ada = supplemental_concentration_per_ada,
    raw_spending_per_ada = current_expense_per_ada_calc,
    
    # Flags for invalid values
    zero_or_missing_ada = is.na(total_funded_ada) | total_funded_ada <= 0,
    invalid_lcff_per_ada = !is.na(raw_lcff_per_ada) & !is.finite(raw_lcff_per_ada),
    invalid_supp_conc_per_ada = !is.na(raw_supp_conc_per_ada) & !is.finite(raw_supp_conc_per_ada),
    invalid_spending_per_ada = !is.na(raw_spending_per_ada) & !is.finite(raw_spending_per_ada),
    
    # Clean analysis variables
    upc_pct = upc_percentage,
    
    lcff_per_ada = if_else(
      is.finite(raw_lcff_per_ada) &
        !is.na(total_funded_ada) &
        total_funded_ada > 0,
      raw_lcff_per_ada,
      NA_real_
    ),
    
    supp_conc_per_ada = if_else(
      is.finite(raw_supp_conc_per_ada) &
        !is.na(total_funded_ada) &
        total_funded_ada > 0,
      raw_supp_conc_per_ada,
      NA_real_
    ),
    
    spending_per_ada = if_else(
      is.finite(raw_spending_per_ada),
      raw_spending_per_ada,
      NA_real_
    ),
    
    upc_bin = case_when(
      is.na(upc_pct) ~ NA_character_,
      upc_pct < 25 ~ "0-25%",
      upc_pct < 40 ~ "25-40%",
      upc_pct < 55 ~ "40-55%",
      upc_pct < 70 ~ "55-70%",
      TRUE ~ "70%+"
    ),
    
    upc_bin = factor(
      upc_bin,
      levels = c("0-25%", "25-40%", "40-55%", "55-70%", "70%+")
    ),
    
    regular_district = str_detect(
      coalesce(district_type, ""),
      regex("Elementary|High|Unified", ignore_case = TRUE)
    )
  )

# Main presentation analysis sample:
# regular K-12 districts with positive funded ADA.
analysis_main <- analysis %>%
  filter(
    regular_district,
    !is.na(total_funded_ada),
    total_funded_ada > 0
  )

academic_panel <- analysis_main %>%
  filter(
    !is.na(ela_current_distance_from_standard),
    !is.na(math_current_distance_from_standard)
  )

grad_panel <- analysis_main %>%
  filter(!is.na(graduation_rate))

# ----- 5. QC tables ----------------------------------------------------------

invalid_per_ada_rows <- analysis %>%
  filter(
    zero_or_missing_ada |
      invalid_lcff_per_ada |
      invalid_supp_conc_per_ada |
      invalid_spending_per_ada
  ) %>%
  select(
    school_year,
    county_name,
    district_name,
    district_type,
    total_funded_ada,
    total_lcff_entitlement,
    raw_lcff_per_ada,
    raw_supp_conc_per_ada,
    raw_spending_per_ada,
    upc_percentage,
    upc_bin,
    zero_or_missing_ada,
    invalid_lcff_per_ada,
    invalid_supp_conc_per_ada,
    invalid_spending_per_ada
  ) %>%
  arrange(school_year, district_name)

write_csv(
  invalid_per_ada_rows,
  file.path(table_dir, "qc_invalid_per_ada_rows.csv")
)

qc_summary <- analysis %>%
  summarize(
    total_rows = n(),
    main_analysis_rows = sum(regular_district & !zero_or_missing_ada, na.rm = TRUE),
    zero_or_missing_ada = sum(zero_or_missing_ada, na.rm = TRUE),
    invalid_lcff_per_ada = sum(invalid_lcff_per_ada, na.rm = TRUE),
    invalid_supp_conc_per_ada = sum(invalid_supp_conc_per_ada, na.rm = TRUE),
    invalid_spending_per_ada = sum(invalid_spending_per_ada, na.rm = TRUE),
    regular_district_rows = sum(regular_district, na.rm = TRUE),
    non_regular_or_missing_type_rows = sum(!regular_district, na.rm = TRUE)
  )

write_csv(
  qc_summary,
  file.path(table_dir, "qc_summary.csv")
)

# ----- 6. Basic EDA tables ---------------------------------------------------

# Table 1: District counts by year
district_counts_by_year <- analysis %>%
  group_by(school_year) %>%
  summarize(
    n_districts_all = n_distinct(district_cds_code),
    n_regular_districts = n_distinct(district_cds_code[regular_district]),
    n_rows_all = n(),
    n_rows_main_analysis = sum(regular_district & !zero_or_missing_ada, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  district_counts_by_year,
  file.path(table_dir, "district_counts_by_year.csv")
)

# Table 2: Missingness by variable
missingness_by_variable <- analysis %>%
  summarize(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    pct_missing = n_missing / nrow(analysis)
  ) %>%
  arrange(desc(pct_missing))

write_csv(
  missingness_by_variable,
  file.path(table_dir, "missingness_by_variable.csv")
)

# Table 3: Average funding by year
average_funding_by_year <- analysis_main %>%
  group_by(school_year) %>%
  summarize(
    n_districts = n_distinct(district_cds_code),
    total_ada = sum(total_funded_ada, na.rm = TRUE),
    mean_lcff_per_ada = mean(lcff_per_ada, na.rm = TRUE),
    median_lcff_per_ada = median(lcff_per_ada, na.rm = TRUE),
    weighted_lcff_per_ada = weighted_mean_safe(lcff_per_ada, total_funded_ada),
    mean_supp_conc_per_ada = mean(supp_conc_per_ada, na.rm = TRUE),
    median_supp_conc_per_ada = median(supp_conc_per_ada, na.rm = TRUE),
    weighted_supp_conc_per_ada = weighted_mean_safe(supp_conc_per_ada, total_funded_ada),
    mean_spending_per_ada = mean(spending_per_ada, na.rm = TRUE),
    median_spending_per_ada = median(spending_per_ada, na.rm = TRUE),
    weighted_spending_per_ada = weighted_mean_safe(spending_per_ada, total_funded_ada),
    .groups = "drop"
  )

write_csv(
  average_funding_by_year,
  file.path(table_dir, "average_funding_by_year.csv")
)

# Table 4: Average UPC by year
average_upc_by_year <- analysis_main %>%
  group_by(school_year) %>%
  summarize(
    n_districts = n_distinct(district_cds_code),
    mean_upc_pct = mean(upc_pct, na.rm = TRUE),
    median_upc_pct = median(upc_pct, na.rm = TRUE),
    weighted_upc_pct = weighted_mean_safe(upc_pct, total_funded_ada),
    .groups = "drop"
  )

write_csv(
  average_upc_by_year,
  file.path(table_dir, "average_upc_by_year.csv")
)

# Table 5: Average outcomes by year
average_outcomes_by_year <- analysis_main %>%
  group_by(school_year) %>%
  summarize(
    n_districts = n_distinct(district_cds_code),
    mean_ela_distance = mean(ela_current_distance_from_standard, na.rm = TRUE),
    median_ela_distance = median(ela_current_distance_from_standard, na.rm = TRUE),
    mean_math_distance = mean(math_current_distance_from_standard, na.rm = TRUE),
    median_math_distance = median(math_current_distance_from_standard, na.rm = TRUE),
    mean_graduation_rate = mean(graduation_rate, na.rm = TRUE),
    median_graduation_rate = median(graduation_rate, na.rm = TRUE),
    mean_dropout_rate = mean(dropout_rate, na.rm = TRUE),
    median_dropout_rate = median(dropout_rate, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  average_outcomes_by_year,
  file.path(table_dir, "average_outcomes_by_year.csv")
)

# Table 6: Funding and outcomes by UPC bin
funding_outcomes_by_upc_bin <- analysis_main %>%
  group_by(upc_bin) %>%
  summarize(
    n_district_years = n(),
    n_districts = n_distinct(district_cds_code),
    total_ada = sum(total_funded_ada, na.rm = TRUE),
    mean_upc_pct = mean(upc_pct, na.rm = TRUE),
    median_upc_pct = median(upc_pct, na.rm = TRUE),
    weighted_upc_pct = weighted_mean_safe(upc_pct, total_funded_ada),
    mean_lcff_per_ada = mean(lcff_per_ada, na.rm = TRUE),
    median_lcff_per_ada = median(lcff_per_ada, na.rm = TRUE),
    weighted_lcff_per_ada = weighted_mean_safe(lcff_per_ada, total_funded_ada),
    mean_supp_conc_per_ada = mean(supp_conc_per_ada, na.rm = TRUE),
    median_supp_conc_per_ada = median(supp_conc_per_ada, na.rm = TRUE),
    weighted_supp_conc_per_ada = weighted_mean_safe(supp_conc_per_ada, total_funded_ada),
    mean_spending_per_ada = mean(spending_per_ada, na.rm = TRUE),
    median_spending_per_ada = median(spending_per_ada, na.rm = TRUE),
    weighted_spending_per_ada = weighted_mean_safe(spending_per_ada, total_funded_ada),
    mean_ela_distance = mean(ela_current_distance_from_standard, na.rm = TRUE),
    median_ela_distance = median(ela_current_distance_from_standard, na.rm = TRUE),
    mean_math_distance = mean(math_current_distance_from_standard, na.rm = TRUE),
    median_math_distance = median(math_current_distance_from_standard, na.rm = TRUE),
    mean_graduation_rate = mean(graduation_rate, na.rm = TRUE),
    median_graduation_rate = median(graduation_rate, na.rm = TRUE),
    mean_dropout_rate = mean(dropout_rate, na.rm = TRUE),
    median_dropout_rate = median(dropout_rate, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  funding_outcomes_by_upc_bin,
  file.path(table_dir, "funding_outcomes_by_upc_bin.csv")
)

# ----- 7. Core plots ---------------------------------------------------------

# Plot 1: LCFF funding over time by UPC bin
# Uses ADA-weighted averages to avoid distortion from tiny districts.
p1_funding_over_time <- analysis_main %>%
  filter(!is.na(upc_bin), !is.na(lcff_per_ada)) %>%
  group_by(school_year, school_year_plot, upc_bin) %>%
  summarize(
    weighted_lcff_per_ada = weighted_mean_safe(lcff_per_ada, total_funded_ada),
    median_lcff_per_ada = median(lcff_per_ada, na.rm = TRUE),
    n_districts = n_distinct(district_cds_code),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = school_year_plot,
    y = weighted_lcff_per_ada,
    color = upc_bin,
    group = upc_bin
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "LCFF Funding per ADA Over Time by Student Need",
    subtitle = "ADA-weighted averages among regular school districts",
    x = "School Year",
    y = "ADA-Weighted LCFF Entitlement per ADA",
    color = "UPC Bin"
  ) +
  theme_lcff() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_lcff_plot(
  "01_lcff_funding_over_time_by_upc_bin.png",
  p1_funding_over_time,
  width = 10,
  height = 6
)

# Plot 2: UPC vs supplemental/concentration funding per ADA
p2_targeted_funding_by_need <- analysis_main %>%
  filter(!is.na(upc_pct), !is.na(supp_conc_per_ada)) %>%
  ggplot(aes(
    x = upc_pct,
    y = supp_conc_per_ada
  )) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Targeted LCFF Funding Increases with Student Need",
    subtitle = "Supplemental and concentration funding per ADA rises as district UPC percentage increases",
    x = "Unduplicated Pupil Count Percentage",
    y = "Supplemental and Concentration Funding per ADA"
  ) +
  theme_lcff()

save_lcff_plot(
  "02_upc_vs_supp_conc_per_ada.png",
  p2_targeted_funding_by_need,
  width = 9,
  height = 6
)

# Plot 3A: 55% LCFF concentration threshold scatterplot
p3_threshold_55_scatter <- analysis_main %>%
  filter(!is.na(upc_pct), !is.na(supp_conc_per_ada)) %>%
  ggplot(aes(
    x = upc_pct,
    y = supp_conc_per_ada
  )) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE) +
  geom_vline(
    xintercept = 55,
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 57,
    y = Inf,
    label = "55% concentration threshold",
    vjust = 2,
    hjust = 0
  ) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "LCFF Targeted Funding Rises Around the 55% Need Threshold",
    subtitle = "Districts above 55% UPC qualify for concentration grant funding",
    x = "Unduplicated Pupil Count Percentage",
    y = "Supplemental and Concentration Funding per ADA"
  ) +
  theme_lcff()

save_lcff_plot(
  "03a_lcff_55_percent_threshold_scatter.png",
  p3_threshold_55_scatter,
  width = 9,
  height = 6
)

# Plot 3B: 55% LCFF concentration threshold binned plot
# Better for presentation because the threshold pattern is easier to see.
p3_threshold_55_binned <- analysis_main %>%
  filter(!is.na(upc_pct), !is.na(supp_conc_per_ada)) %>%
  mutate(
    upc_bin_5pt = pmin(floor(upc_pct / 5) * 5, 95),
    upc_midpoint = upc_bin_5pt + 2.5
  ) %>%
  group_by(upc_midpoint) %>%
  summarize(
    weighted_supp_conc_per_ada = weighted_mean_safe(
      supp_conc_per_ada,
      total_funded_ada
    ),
    median_supp_conc_per_ada = median(supp_conc_per_ada, na.rm = TRUE),
    n_districts = n_distinct(district_cds_code),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = upc_midpoint,
    y = weighted_supp_conc_per_ada
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = 55,
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 57,
    y = Inf,
    label = "55% threshold",
    vjust = 2,
    hjust = 0
  ) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "LCFF Targeted Funding Rises Sharply Around the 55% Threshold",
    subtitle = "Binned ADA-weighted supplemental and concentration funding per ADA",
    x = "Unduplicated Pupil Count Percentage",
    y = "Supplemental and Concentration Funding per ADA"
  ) +
  theme_lcff()

save_lcff_plot(
  "03b_lcff_55_percent_threshold_binned.png",
  p3_threshold_55_binned,
  width = 9,
  height = 6
)

# Plot 4A: ELA outcomes by UPC bin
p4a_ela_by_upc_bin <- academic_panel %>%
  filter(!is.na(upc_bin), !is.na(ela_current_distance_from_standard)) %>%
  ggplot(aes(
    x = upc_bin,
    y = ela_current_distance_from_standard
  )) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "ELA Achievement Declines as Student Need Rises",
    subtitle = "Lower values mean students are farther below the state standard",
    x = "UPC Bin",
    y = "ELA Distance from Standard"
  ) +
  theme_lcff()

save_lcff_plot(
  "04a_ela_distance_by_upc_bin.png",
  p4a_ela_by_upc_bin,
  width = 8,
  height = 6
)

# Plot 4B: Math outcomes by UPC bin
p4b_math_by_upc_bin <- academic_panel %>%
  filter(!is.na(upc_bin), !is.na(math_current_distance_from_standard)) %>%
  ggplot(aes(
    x = upc_bin,
    y = math_current_distance_from_standard
  )) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Math Achievement Declines as Student Need Rises",
    subtitle = "Lower values mean students are farther below the state standard",
    x = "UPC Bin",
    y = "Math Distance from Standard"
  ) +
  theme_lcff()

save_lcff_plot(
  "04b_math_distance_by_upc_bin.png",
  p4b_math_by_upc_bin,
  width = 8,
  height = 6
)

# Plot 5A: Supplemental/concentration funding vs ELA
p5a_funding_vs_ela <- academic_panel %>%
  filter(
    !is.na(supp_conc_per_ada),
    !is.na(ela_current_distance_from_standard),
    !is.na(upc_bin)
  ) %>%
  ggplot(aes(
    x = supp_conc_per_ada,
    y = ela_current_distance_from_standard,
    color = upc_bin
  )) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Targeted Funding and ELA Outcomes",
    subtitle = "Higher funding often reflects higher underlying student need, not a simple causal effect",
    x = "Supplemental and Concentration Funding per ADA",
    y = "ELA Distance from Standard",
    color = "UPC Bin"
  ) +
  theme_lcff()

save_lcff_plot(
  "05a_funding_vs_ela_distance.png",
  p5a_funding_vs_ela,
  width = 9,
  height = 6
)

# Plot 5B: Supplemental/concentration funding vs Math
p5b_funding_vs_math <- academic_panel %>%
  filter(
    !is.na(supp_conc_per_ada),
    !is.na(math_current_distance_from_standard),
    !is.na(upc_bin)
  ) %>%
  ggplot(aes(
    x = supp_conc_per_ada,
    y = math_current_distance_from_standard,
    color = upc_bin
  )) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Targeted Funding and Math Outcomes",
    subtitle = "Higher funding often reflects higher underlying student need, not a simple causal effect",
    x = "Supplemental and Concentration Funding per ADA",
    y = "Math Distance from Standard",
    color = "UPC Bin"
  ) +
  theme_lcff()

save_lcff_plot(
  "05b_funding_vs_math_distance.png",
  p5b_funding_vs_math,
  width = 9,
  height = 6
)

# Plot 5C: Supplemental/concentration funding vs graduation
p5c_funding_vs_grad <- grad_panel %>%
  filter(
    !is.na(supp_conc_per_ada),
    !is.na(graduation_rate),
    !is.na(upc_bin)
  ) %>%
  ggplot(aes(
    x = supp_conc_per_ada,
    y = graduation_rate,
    color = upc_bin
  )) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Targeted Funding and Graduation Rates",
    subtitle = "Association between supplemental/concentration funding and district graduation rates",
    x = "Supplemental and Concentration Funding per ADA",
    y = "Graduation Rate",
    color = "UPC Bin"
  ) +
  theme_lcff()

save_lcff_plot(
  "05c_funding_vs_graduation_rate.png",
  p5c_funding_vs_grad,
  width = 9,
  height = 6
)

# ----- 8. Optional map if latitude/longitude exist ---------------------------

if (all(c("district_latitude", "district_longitude") %in% names(analysis_main))) {
  
  p6_map_need_outcomes <- analysis_main %>%
    filter(
      !is.na(district_latitude),
      !is.na(district_longitude),
      !is.na(upc_bin),
      !is.na(total_funded_ada)
    ) %>%
    ggplot(aes(
      x = district_longitude,
      y = district_latitude,
      color = upc_bin,
      size = total_funded_ada
    )) +
    geom_point(alpha = 0.55) +
    scale_size_continuous(labels = comma) +
    labs(
      title = "Geography of High-Need Districts in California",
      subtitle = "Districts are sized by funded ADA and colored by UPC bin",
      x = "Longitude",
      y = "Latitude",
      color = "UPC Bin",
      size = "Funded ADA"
    ) +
    theme_lcff()
  
  save_lcff_plot(
    "06_map_high_need_districts.png",
    p6_map_need_outcomes,
    width = 8,
    height = 9
  )
}

# ----- 9. Regression models --------------------------------------------------

# Model A: Does student need predict targeted LCFF funding?
model_a_data <- analysis_main %>%
  filter(
    !is.na(supp_conc_per_ada),
    !is.na(upc_percentage),
    !is.na(english_learner_percentage),
    !is.na(foster_percentage),
    !is.na(homeless_percentage_lcff_upc),
    !is.na(school_year)
  )

model_a_funding_targeting <- lm(
  supp_conc_per_ada ~ upc_percentage +
    english_learner_percentage +
    foster_percentage +
    homeless_percentage_lcff_upc +
    factor(school_year),
  data = model_a_data
)

model_a_results <- tidy(model_a_funding_targeting, conf.int = TRUE)

write_csv(
  model_a_results,
  file.path(table_dir, "model_a_funding_targeting_results.csv")
)

# Model B1: ELA outcomes
model_b_ela_data <- academic_panel %>%
  filter(
    !is.na(ela_current_distance_from_standard),
    !is.na(supp_conc_per_ada),
    !is.na(upc_percentage),
    !is.na(spending_per_ada),
    !is.na(district_type),
    !is.na(school_year)
  )

model_b_ela <- lm(
  ela_current_distance_from_standard ~ supp_conc_per_ada +
    upc_percentage +
    spending_per_ada +
    factor(district_type) +
    factor(school_year),
  data = model_b_ela_data
)

model_b_ela_results <- tidy(model_b_ela, conf.int = TRUE)

write_csv(
  model_b_ela_results,
  file.path(table_dir, "model_b_ela_results.csv")
)

# Model B2: Math outcomes
model_b_math_data <- academic_panel %>%
  filter(
    !is.na(math_current_distance_from_standard),
    !is.na(supp_conc_per_ada),
    !is.na(upc_percentage),
    !is.na(spending_per_ada),
    !is.na(district_type),
    !is.na(school_year)
  )

model_b_math <- lm(
  math_current_distance_from_standard ~ supp_conc_per_ada +
    upc_percentage +
    spending_per_ada +
    factor(district_type) +
    factor(school_year),
  data = model_b_math_data
)

model_b_math_results <- tidy(model_b_math, conf.int = TRUE)

write_csv(
  model_b_math_results,
  file.path(table_dir, "model_b_math_results.csv")
)

# Model C: Graduation outcomes
model_c_graduation_data <- grad_panel %>%
  filter(
    !is.na(graduation_rate),
    !is.na(supp_conc_per_ada),
    !is.na(upc_percentage),
    !is.na(spending_per_ada),
    !is.na(district_type),
    !is.na(school_year)
  )

model_c_graduation <- lm(
  graduation_rate ~ supp_conc_per_ada +
    upc_percentage +
    spending_per_ada +
    factor(district_type) +
    factor(school_year),
  data = model_c_graduation_data
)

model_c_graduation_results <- tidy(model_c_graduation, conf.int = TRUE)

write_csv(
  model_c_graduation_results,
  file.path(table_dir, "model_c_graduation_results.csv")
)

model_sample_sizes <- tibble(
  model = c(
    "Model A: Funding Targeting",
    "Model B1: ELA Outcomes",
    "Model B2: Math Outcomes",
    "Model C: Graduation Outcomes"
  ),
  n_observations = c(
    nobs(model_a_funding_targeting),
    nobs(model_b_ela),
    nobs(model_b_math),
    nobs(model_c_graduation)
  )
)

write_csv(
  model_sample_sizes,
  file.path(table_dir, "model_sample_sizes.csv")
)

# Save model summaries as plain text
sink(file.path(table_dir, "model_summaries.txt"))

cat("MODEL A: Funding Targeting\n")
cat("Sample size:", nobs(model_a_funding_targeting), "\n\n")
print(summary(model_a_funding_targeting))

cat("\n\nMODEL B1: ELA Outcomes\n")
cat("Sample size:", nobs(model_b_ela), "\n\n")
print(summary(model_b_ela))

cat("\n\nMODEL B2: Math Outcomes\n")
cat("Sample size:", nobs(model_b_math), "\n\n")
print(summary(model_b_math))

cat("\n\nMODEL C: Graduation Outcomes\n")
cat("Sample size:", nobs(model_c_graduation), "\n\n")
print(summary(model_c_graduation))

sink()

# ----- 10. Priority districts / failure-case table ---------------------------

# Use 2024_25 if available, otherwise use the latest year in the main sample.
priority_year <- if ("2024_25" %in% analysis_main$school_year) {
  "2024_25"
} else {
  max(analysis_main$school_year, na.rm = TRUE)
}

priority_base <- analysis_main %>%
  filter(
    school_year == priority_year,
    !is.na(upc_percentage),
    !is.na(supp_conc_per_ada)
  )

priority_districts <- priority_base %>%
  mutate(
    high_need = upc_percentage >= quantile(upc_percentage, 0.75, na.rm = TRUE),
    high_funding = supp_conc_per_ada >= quantile(supp_conc_per_ada, 0.75, na.rm = TRUE),
    low_ela = ela_current_distance_from_standard <= quantile(
      ela_current_distance_from_standard,
      0.25,
      na.rm = TRUE
    ),
    low_math = math_current_distance_from_standard <= quantile(
      math_current_distance_from_standard,
      0.25,
      na.rm = TRUE
    ),
    priority_reason = case_when(
      low_ela & low_math ~ "Low ELA and low math",
      low_ela ~ "Low ELA",
      low_math ~ "Low math",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(high_need, high_funding, low_ela | low_math) %>%
  arrange(ela_current_distance_from_standard, math_current_distance_from_standard) %>%
  select(
    school_year,
    county_name,
    district_name,
    district_type,
    total_funded_ada,
    upc_percentage,
    lcff_per_ada,
    supp_conc_per_ada,
    spending_per_ada,
    ela_current_distance_from_standard,
    math_current_distance_from_standard,
    graduation_rate,
    dropout_rate,
    priority_reason
  )

write_csv(
  priority_districts,
  file.path(table_dir, "priority_districts.csv")
)

# ----- 11. Console recap -----------------------------------------------------

cat("\nLCFF analysis complete.\n")
cat("Figures saved to:", fig_dir, "\n")
cat("Tables saved to:", table_dir, "\n")
cat("Total rows in full panel:", nrow(analysis), "\n")
cat("Rows in main regular-district analysis sample:", nrow(analysis_main), "\n")
cat("Invalid LCFF per-ADA rows set to NA:", sum(analysis$invalid_lcff_per_ada, na.rm = TRUE), "\n")
cat("Rows with zero or missing ADA:", sum(analysis$zero_or_missing_ada, na.rm = TRUE), "\n")
cat("Priority district year used:", priority_year, "\n")
cat("Number of priority districts:", nrow(priority_districts), "\n")