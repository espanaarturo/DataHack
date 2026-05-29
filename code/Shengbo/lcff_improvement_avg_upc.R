# =============================================================================
# LCFF Funding vs. Student Academic Improvement  (2019–2025)
#
# X-axis : Supplemental & Concentration Funding per ADA
# Y-axis : Year-over-year change in Distance from Standard (improvement)
# Color  : UPC bin based on each district's AVERAGE UPC % across all years
#
# Why average UPC?
#   Avoids year-to-year shuffling between groups. Districts serving
#   structurally high-need populations stay in the same group across all
#   years — making regression lines and trend comparisons meaningful.
#
# Year coverage: 2018-19, 2022-23, 2023-24, 2024-25
#   2021-22 excluded — distance_change unavailable (no prior-year test,
#   COVID suspension). 2018-19 paired with 2019-20 LCFF (earliest available).
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)

data_dir   <- "/Users/lukalin/Desktop/Group1 LCFF Research/data/cleaned"
output_dir <- "/Users/lukalin/Desktop/Group1 LCFF Research/data/outputs"
dir.create(output_dir, showWarnings = FALSE)

# ── 1. Load raw files ─────────────────────────────────────────────────────────
acad <- read_csv(file.path(data_dir, "academic_indicator_district_total.csv"),
                 show_col_types = FALSE)
lcff <- read_csv(file.path(data_dir, "lcff_summary_district_only.csv"),
                 show_col_types = FALSE)
upc  <- read_csv(file.path(data_dir, "calpads_upc_district_only.csv"),
                 show_col_types = FALSE)

# ── 2. Average UPC per district across ALL available years ───────────────────
# This is the key methodological choice: each district gets ONE stable
# UPC value representing its structural high-need level, not a snapshot
upc_avg <- upc |>
  filter(is_district_row == TRUE) |>
  group_by(district_cds_code) |>
  summarise(
    upc_avg      = mean(upc_percentage, na.rm = TRUE),
    upc_sd       = sd(upc_percentage,   na.rm = TRUE),
    upc_n_years  = n(),
    district_type = first(district_type),
    .groups      = "drop"
  )

message("Districts with avg UPC computed: ", nrow(upc_avg))
message("Average UPC across all districts: ",
        round(mean(upc_avg$upc_avg), 1), "%")

# ── 3. Academic improvement data ─────────────────────────────────────────────
acad_sub <- acad |>
  filter(student_group == "ALL", !is.na(distance_change)) |>
  select(school_year,
         district_cds_code,
         district_name,
         subject,
         dfs        = current_distance_from_standard,
         dfs_change = distance_change,
         n_students = current_denominator)

# ── 4. Per-year LCFF funding ──────────────────────────────────────────────────
lcff_sub <- lcff |>
  filter(is_district_row == TRUE) |>
  select(funding_year  = school_year,
         district_cds_code,
         sc_per_ada    = supplemental_concentration_per_ada,
         total_per_ada = total_lcff_entitlement_per_ada)

# ── 5. Merge ──────────────────────────────────────────────────────────────────
# Recode 2018_19 acad to pair with 2019_20 LCFF (earliest LCFF year available)
df <- acad_sub |>
  mutate(funding_year = if_else(school_year == "2018_19",
                                "2019_20", school_year)) |>
  inner_join(lcff_sub,  by = c("funding_year", "district_cds_code")) |>
  left_join(upc_avg,    by = "district_cds_code") |>   # join on ID only — no year key
  filter(
    !is.na(sc_per_ada),
    !is.na(upc_avg),
    !is.na(dfs_change),
    n_students >= 50,
    !str_detect(coalesce(district_type, ""), "County Office")
  )

# ── 6. Bin on average UPC ─────────────────────────────────────────────────────
df <- df |>
  mutate(
    upc_bin = cut(
      upc_avg,
      breaks = c(-Inf, 25, 40, 55, 70, Inf),
      labels = c("0–25%  (Low Need)",
                 "25–40%",
                 "40–55%",
                 "55–70%",
                 "70%+  (High Need)"),
      right  = TRUE
    ),
    upc_bin = factor(upc_bin,
                     levels = c("0–25%  (Low Need)", "25–40%",
                                "40–55%", "55–70%", "70%+  (High Need)")),

    subject_label = if_else(subject == "ela", "ELA", "Math"),

    year_label = recode(school_year,
                        "2018_19" = "2018–19",
                        "2022_23" = "2022–23",
                        "2023_24" = "2023–24",
                        "2024_25" = "2024–25"),
    year_label = factor(year_label,
                        levels = c("2018–19", "2022–23",
                                   "2023–24", "2024–25"))
  )

message("Total observations: ", format(nrow(df), big.mark = ","))
message("UPC bin breakdown:")
print(table(df$upc_bin))

# ── 7. Shared aesthetics ──────────────────────────────────────────────────────
UPC_COLORS <- c(
  "0–25%  (Low Need)"  = "#2196F3",
  "25–40%"             = "#4CAF50",
  "40–55%"             = "#FF9800",
  "55–70%"             = "#9C27B0",
  "70%+  (High Need)"  = "#E91E63"
)

BASE <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9.5, color = "grey40",
                                    lineheight = 1.4,
                                    margin = margin(b = 10)),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9.5),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.4, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    plot.background  = element_rect(fill = "white",   color = NA),
    plot.caption     = element_text(size = 7.5, color = "grey55",
                                    hjust = 0, margin = margin(t = 8))
  )

CAPTION <- paste0(
  "Source: CDE Academic Indicator & LCFF Summary  |  n = ",
  format(nrow(df), big.mark = ","),
  " district-year-subject observations\n",
  "UPC bin = district's AVERAGE unduplicated pupil % across 2019–2025 ",
  "(stable structural need, not a yearly snapshot)  |  Districts ≥50 tested students\n",
  "2021–22 excluded (no prior-year DFS due to COVID)  |  ",
  "2018–19 academic paired with 2019–20 LCFF funding"
)

# =============================================================================
# CHART 1 — Core chart: Funding vs. Improvement, ELA & Math side-by-side
#           All years pooled | regression line + CI per group
# =============================================================================
p1 <- df |>
  ggplot(aes(x     = sc_per_ada,
             y     = dfs_change,
             color = upc_bin,
             fill  = upc_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.8) +
  geom_point(alpha = 0.20, size = 1.2, shape = 16) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 1.8, alpha = 0.12) +
  facet_wrap(~subject_label, ncol = 2) +
  scale_x_continuous(
    labels = dollar_format(accuracy = 1),
    breaks = c(0, 1000, 2000, 3000, 4000, 5000)
  ) +
  scale_y_continuous(
    labels = function(x) if_else(x > 0, paste0("+", x), as.character(x)),
    limits = c(-65, 65)
  ) +
  scale_color_manual(values = UPC_COLORS,
                     name   = "Avg. UPC % — Structural Need Level") +
  scale_fill_manual(values  = UPC_COLORS, guide = "none") +
  labs(
    title    = "LCFF Funding vs. Academic Improvement by Student Need Group  (2019–2025)",
    subtitle = paste0(
      "x = Supplemental & Concentration funding per ADA  |  ",
      "y = Year-over-year change in Distance from Standard\n",
      "Positive y = improvement  |  Dashed = no change  |  ",
      "Shaded band = 95% CI  |  Color = district's average UPC % across all years"
    ),
    x       = "Supplemental & Concentration Funding per ADA",
    y       = "Academic Improvement (Year-over-Year ΔDistance from Standard)",
    caption = CAPTION
  ) +
  BASE +
  guides(color = guide_legend(
    override.aes = list(alpha = 1, size = 3.5, linewidth = 2),
    nrow = 1
  ))

ggsave(file.path(output_dir, "lcff_imp_chart1_main.png"),
       p1, width = 15, height = 8, dpi = 160, bg = "white")
message("✅  Chart 1 saved")

# =============================================================================
# CHART 2 — One panel per year (2×2) × subject rows
#           Tracks whether the pattern shifted pre vs. post COVID recovery
# =============================================================================
p2 <- df |>
  ggplot(aes(x     = sc_per_ada,
             y     = dfs_change,
             color = upc_bin,
             fill  = upc_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.7) +
  geom_point(alpha = 0.18, size = 1.0, shape = 16) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 1.6, alpha = 0.12) +
  facet_grid(subject_label ~ year_label) +
  scale_x_continuous(
    labels = dollar_format(accuracy = 1),
    breaks = c(0, 2000, 4000)
  ) +
  scale_y_continuous(
    labels = function(x) if_else(x > 0, paste0("+", x), as.character(x)),
    limits = c(-60, 60)
  ) +
  scale_color_manual(values = UPC_COLORS,
                     name   = "Avg. UPC % Group") +
  scale_fill_manual(values  = UPC_COLORS, guide = "none") +
  labs(
    title    = "Funding vs. Improvement — by Year and Subject  (2019–2025)",
    subtitle = paste0(
      "Rows = subject  |  Columns = school year  |  ",
      "Each regression line uses avg UPC bin (stable group identity)\n",
      "Note: gap between 2018–19 and 2022–23 due to COVID assessment suspension"
    ),
    x       = "S&C Funding per ADA",
    y       = "Change in DFS (Improvement)",
    caption = CAPTION
  ) +
  BASE +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8.5)) +
  guides(color = guide_legend(
    override.aes = list(alpha = 1, linewidth = 2, size = 3),
    nrow = 1
  ))

ggsave(file.path(output_dir, "lcff_imp_chart2_by_year.png"),
       p2, width = 16, height = 11, dpi = 160, bg = "white")
message("✅  Chart 2 saved")

# =============================================================================
# CHART 3 — Within-group view: 5 rows (UPC groups) × 2 cols (subjects)
#           Free x-axis scale — each group occupies its own funding range
#           Answers: within the same need group, does more funding = more improvement?
# =============================================================================
p3 <- df |>
  ggplot(aes(x     = sc_per_ada,
             y     = dfs_change,
             color = upc_bin,
             fill  = upc_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey55", linewidth = 0.7) +
  geom_point(alpha = 0.20, size = 1.0, shape = 16) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 1.7, alpha = 0.14) +
  # Add regression equation annotation per panel
  facet_grid(upc_bin ~ subject_label, scales = "free_x") +
  scale_x_continuous(labels = dollar_format(accuracy = 1)) +
  scale_y_continuous(
    labels = function(x) if_else(x > 0, paste0("+", x), as.character(x)),
    limits = c(-60, 60)
  ) +
  scale_color_manual(values = UPC_COLORS, guide = "none") +
  scale_fill_manual(values  = UPC_COLORS, guide = "none") +
  labs(
    title    = "Within-Group: Does More S&C Funding Predict More Improvement?  (2019–2025)",
    subtitle = paste0(
      "Each row = one UPC need group (avg across years)  |  ",
      "x-axis free — each group occupies its own funding range\n",
      "Near-flat slope = funding level doesn't predict improvement within that group  |  ",
      "Groups stay fixed — no year-to-year reshuffling"
    ),
    x       = "S&C Funding per ADA",
    y       = "Change in DFS (Improvement)",
    caption = CAPTION
  ) +
  BASE +
  theme(
    strip.text.y = element_text(size = 8.5, angle = 0, hjust = 0),
    axis.text.x  = element_text(angle = 20, hjust = 1, size = 8)
  )

ggsave(file.path(output_dir, "lcff_imp_chart3_within_group.png"),
       p3, width = 12, height = 15, dpi = 160, bg = "white")
message("✅  Chart 3 saved")

# =============================================================================
# CHART 4 — Median improvement trend over time by UPC group
# =============================================================================
trend <- df |>
  group_by(year_label, subject_label, upc_bin) |>
  summarise(
    median_change = median(dfs_change, na.rm = TRUE),
    mean_change   = mean(dfs_change,   na.rm = TRUE),
    pct_improved  = mean(dfs_change > 0, na.rm = TRUE) * 100,
    n             = n(),
    .groups       = "drop"
  )

p4 <- trend |>
  ggplot(aes(x = year_label, y = median_change,
             color = upc_bin, group = upc_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  geom_point(aes(size = n),
             shape = 21, fill = "white", stroke = 2.0) +
  geom_text_repel(
    data        = trend |> filter(year_label == "2024–25"),
    aes(label   = sprintf("%+.1f", median_change)),
    nudge_x     = 0.3,
    size        = 3.2,
    fontface    = "bold",
    direction   = "y",
    show.legend = FALSE,
    segment.color = "grey70"
  ) +
  facet_wrap(~subject_label, ncol = 2) +
  scale_color_manual(values = UPC_COLORS,
                     name   = "Avg. UPC % Group") +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_y_continuous(
    labels = function(x) if_else(x > 0, paste0("+", x), as.character(x))
  ) +
  labs(
    title    = "Median Academic Improvement by Student Need Group Over Time  (2019–2025)",
    subtitle = paste0(
      "Median year-over-year DFS change  |  Point size = number of districts\n",
      "Groups based on each district's average UPC % — stable across all years"
    ),
    x       = NULL,
    y       = "Median Change in DFS",
    caption = CAPTION
  ) +
  BASE +
  guides(color = guide_legend(
    override.aes = list(linewidth = 2.5, size = 4, shape = 16),
    nrow = 1
  ))

ggsave(file.path(output_dir, "lcff_imp_chart4_trend.png"),
       p4, width = 14, height = 7, dpi = 160, bg = "white")
message("✅  Chart 4 saved")

# =============================================================================
# CHART 5 — Heatmap: % of district-years with positive DFS change
#           rows = UPC group | cols = funding band | facet = subject
#           Each cell = share of observations where improvement occurred
# =============================================================================
heat <- df |>
  mutate(
    funding_band = cut(
      sc_per_ada,
      breaks = c(-Inf, 500, 1000, 1500, 2500, 3500, Inf),
      labels = c("<$500", "$500–1K", "$1K–1.5K",
                 "$1.5K–2.5K", "$2.5K–3.5K", ">$3.5K"),
      right  = TRUE
    )
  ) |>
  filter(!is.na(funding_band)) |>
  group_by(upc_bin, funding_band, subject_label) |>
  summarise(
    pct_improved = mean(dfs_change > 0, na.rm = TRUE) * 100,
    n            = n(),
    .groups      = "drop"
  ) |>
  filter(n >= 5)

p5 <- heat |>
  ggplot(aes(x    = funding_band,
             y    = fct_rev(upc_bin),
             fill = pct_improved)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = paste0(round(pct_improved), "%\n(n=", n, ")"),
        color = if_else(abs(pct_improved - 50) > 18, "white", "grey20")),
    size = 3.1, fontface = "bold", lineheight = 1.1
  ) +
  facet_wrap(~subject_label, ncol = 2) +
  scale_fill_gradient2(
    low      = "#2166AC",
    mid      = "#F5F5F5",
    high     = "#B2182B",
    midpoint = 50,
    limits   = c(0, 100),
    name     = "% of district-years\nshowing improvement",
    guide    = guide_colorbar(barwidth  = 0.7,
                              barheight = 7,
                              title.position = "top")
  ) +
  scale_color_identity() +
  labs(
    title    = "Share of District-Years Showing Improvement — Funding Band × Need Group  (2019–2025)",
    subtitle = paste0(
      "Each cell = % of district-year observations where year-over-year DFS increased\n",
      "n = number of district-year observations in that cell  |  ",
      "Groups based on average UPC % (not yearly snapshot)"
    ),
    x       = "S&C Funding per ADA",
    y       = "Avg. UPC % Group",
    caption = CAPTION
  ) +
  BASE +
  theme(
    legend.position = "right",
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 9),
    panel.grid      = element_blank()
  )

ggsave(file.path(output_dir, "lcff_imp_chart5_heatmap.png"),
       p5, width = 13, height = 7.5, dpi = 160, bg = "white")
message("✅  Chart 5 saved")

# =============================================================================
# Summary: regression slopes per group (printed + saved)
# =============================================================================
slopes <- df |>
  group_by(upc_bin, subject_label) |>
  summarise(
    n_obs              = n(),
    n_districts        = n_distinct(district_cds_code),
    avg_upc            = mean(upc_avg,    na.rm = TRUE) |> round(1),
    avg_sc_per_ada     = mean(sc_per_ada, na.rm = TRUE) |> round(0),
    median_dfs_change  = median(dfs_change, na.rm = TRUE) |> round(2),
    pct_improved       = (mean(dfs_change > 0) * 100) |> round(1),
    slope_per_dollar   = coef(lm(dfs_change ~ sc_per_ada))[["sc_per_ada"]] |> round(6),
    r_squared          = summary(lm(dfs_change ~ sc_per_ada))$r.squared |> round(4),
    .groups            = "drop"
  )

message("\n── Regression slopes (DFS improvement per $1 in S&C funding) ──")
message("Positive slope = more funding associated with more improvement within group")
print(slopes, n = Inf)

write_csv(slopes, file.path(output_dir, "lcff_imp_regression_slopes.csv"))

message("\n✅  All done. Saved to: ", output_dir)
message("   lcff_imp_chart1_main.png          — core scatter, all years pooled")
message("   lcff_imp_chart2_by_year.png        — year × subject grid")
message("   lcff_imp_chart3_within_group.png   — within-group view")
message("   lcff_imp_chart4_trend.png          — median trend over time")
message("   lcff_imp_chart5_heatmap.png        — % improving heatmap")
message("   lcff_imp_regression_slopes.csv     — slope & R² per group")
