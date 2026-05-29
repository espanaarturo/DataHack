# =============================================================================
# California Cities — Cost of Living Map
# Dots on CA map colored by median gross rent (cost of living proxy)
# Data: US Census ACS 5-Year 2019-2023 via tidycensus
# =============================================================================

library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(scales)
library(ggplot2)

options(tigris_use_cache = TRUE)

output_dir <- "/Users/lukalin/Desktop/Group1 LCFF Research/data/outputs"
dir.create(output_dir, showWarnings = FALSE)

# =============================================================================
# ★★★  PASTE YOUR CENSUS API KEY HERE — run this line once, then restart R  ★★★
# census_api_key("PASTE_YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)
# =============================================================================

# ── 1. Pull city/place level data from ACS ───────────────────────────────────
message("Pulling ACS data for California places...")

places_raw <- get_acs(
  geography = "place",
  state     = "CA",
  variables = c(
    median_rent         = "B25064_001",   # Median gross rent
    median_income       = "B19013_001",   # Median household income
    median_home_value   = "B25077_001",   # Median home value
    rent_burden_pct     = "B25071_001",   # Median rent as % of income
    total_population    = "B01003_001"    # Population
  ),
  year      = 2023,
  survey    = "acs5",
  output    = "wide",
  geometry  = TRUE   # returns sf object with point/polygon geometry
)

# ── 2. Clean & derive metrics ─────────────────────────────────────────────────
places_clean <- places_raw |>
  rename(
    median_rent       = median_rentE,
    median_income     = median_incomeE,
    median_home_value = median_home_valueE,
    rent_burden_pct   = rent_burden_pctE,
    total_population  = total_populationE
  ) |>
  # Keep cities with meaningful population and valid rent
  filter(
    total_population >= 10000,
    !is.na(median_rent)
  ) |>
  mutate(
    city_name = str_remove(NAME, " city, California") |>
                str_remove(" town, California") |>
                str_remove(" CDP, California"),

    # Rent-to-income ratio (monthly)
    rent_to_income = median_rent / (median_income / 12),

    # COL tier based on median rent quartiles
    rent_quartile = ntile(median_rent, 4),
    col_tier = case_when(
      rent_quartile == 4 ~ "Highest COL",
      rent_quartile == 3 ~ "High COL",
      rent_quartile == 2 ~ "Moderate COL",
      rent_quartile == 1 ~ "Lower COL"
    ),
    col_tier = factor(col_tier,
                      levels = c("Lower COL", "Moderate COL",
                                 "High COL",  "Highest COL")),

    # Above/below state median flag (like reference image)
    state_median_rent = median(median_rent, na.rm = TRUE),
    vs_state = if_else(
      median_rent >= state_median_rent,
      "Higher than state median",
      "Lower than state median"
    ),
    vs_state = factor(vs_state,
                      levels = c("Lower than state median",
                                 "Higher than state median"))
  )

message("Cities included: ", nrow(places_clean))
message("State median rent: $", round(median(places_clean$median_rent, na.rm = TRUE)))

# ── 3. Get coordinates from geometry centroid ─────────────────────────────────
places_coords <- places_clean |>
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],
    lat = st_coordinates(st_centroid(geometry))[, 2]
  ) |>
  st_drop_geometry()

# ── 4. Download CA shapefile ──────────────────────────────────────────────────
message("Loading CA county boundaries...")
ca_counties <- counties(state = "CA", cb = TRUE, resolution = "5m", year = 2022) |>
  st_transform(4326)
ca_state <- ca_counties |> summarise(geometry = st_union(geometry))

# ── 5. MAP A — Two-color (replicates reference image style) ──────────────────
state_med <- round(median(places_coords$median_rent, na.rm = TRUE))

p_two <- ggplot() +
  geom_sf(data = ca_state,
          fill = "grey96", color = "grey55", linewidth = 0.45) +
  geom_sf(data = ca_counties,
          fill = NA, color = "grey82", linewidth = 0.12) +
  geom_point(
    data = places_coords,
    aes(x = lon, y = lat, color = vs_state, size = total_population),
    alpha = 0.75, shape = 16
  ) +
  scale_color_manual(
    values = c(
      "Lower than state median"  = "#5B9BD5",
      "Higher than state median" = "#C0382B"
    ),
    name = NULL
  ) +
  scale_size_continuous(
    range  = c(1.5, 7),
    labels = comma,
    name   = "Population",
    breaks = c(10000, 100000, 500000, 1000000)
  ) +
  coord_sf(
    xlim   = c(-124.6, -113.8),
    ylim   = c(32.4,    42.1),
    expand = FALSE
  ) +
  labs(
    title    = "California Cities by Cost of Living",
    subtitle = paste0(
      "Median gross rent vs. state median ($", comma(state_med), "/mo)  |  ",
      nrow(places_coords), " cities  |  Population ≥10,000  |  ACS 2019–2023"
    ),
    caption = "Source: US Census Bureau American Community Survey 5-Year Estimates (2019-2023)"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9.5, color = "grey40",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 8, color = "grey55",
                                    hjust = 0, margin = margin(t = 8)),
    legend.position  = c(0.82, 0.50),
    legend.text      = element_text(size = 9.5),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.spacing.y = unit(0.25, "cm"),
    legend.key.size  = unit(0.45, "cm"),
    plot.margin      = margin(12, 12, 12, 12),
    plot.background  = element_rect(fill = "white", color = NA)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1),
                         order = 1),
    size  = guide_legend(order = 2)
  )

ggsave(file.path(output_dir, "ca_col_cities_map_twotone.png"),
       p_two, width = 10, height = 12, dpi = 180, bg = "white")
message("✅ Saved: ca_col_cities_map_twotone.png")

# ── 6. MAP B — Four-color quartile gradient ───────────────────────────────────
q_breaks <- quantile(places_coords$median_rent, probs = c(0, .25, .5, .75, 1),
                     na.rm = TRUE) |> round()

p_four <- ggplot() +
  geom_sf(data = ca_state,
          fill = "grey96", color = "grey55", linewidth = 0.45) +
  geom_sf(data = ca_counties,
          fill = NA, color = "grey82", linewidth = 0.12) +
  geom_point(
    data = places_coords,
    aes(x = lon, y = lat, color = median_rent, size = total_population),
    alpha = 0.80, shape = 16
  ) +
  scale_color_gradientn(
    colors = c("#2166AC", "#74ADD1", "#FDAE61", "#D73027", "#8B0000"),
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),
    labels = dollar,
    name   = "Median gross rent\n(monthly)",
    guide  = guide_colorbar(
      barwidth  = 0.6,
      barheight = 8,
      title.position = "top"
    )
  ) +
  scale_size_continuous(
    range  = c(1.5, 7),
    labels = comma,
    name   = "Population",
    breaks = c(10000, 100000, 500000, 1000000)
  ) +
  coord_sf(
    xlim   = c(-124.6, -113.8),
    ylim   = c(32.4,    42.1),
    expand = FALSE
  ) +
  labs(
    title    = "California Cities — Cost of Living by Median Rent",
    subtitle = paste0(
      "ACS 2019–2023  |  ", nrow(places_coords),
      " cities  |  Population ≥10,000  |  Range: $",
      comma(q_breaks[1]), "–$", comma(q_breaks[5]), "/mo"
    ),
    caption = "Source: US Census Bureau American Community Survey 5-Year Estimates (2019-2023)"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 16,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 9.5, color = "grey40",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(size = 8, color = "grey55",
                                   hjust = 0, margin = margin(t = 8)),
    legend.position = c(0.84, 0.50),
    legend.text     = element_text(size = 9),
    legend.title    = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.45, "cm"),
    plot.margin     = margin(12, 12, 12, 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(output_dir, "ca_col_cities_map_gradient.png"),
       p_four, width = 10, height = 12, dpi = 180, bg = "white")
message("✅ Saved: ca_col_cities_map_gradient.png")

# ── 7. Save the underlying data too ──────────────────────────────────────────
places_coords |>
  select(city_name, lon, lat, total_population,
         median_rent, median_income, median_home_value,
         rent_burden_pct, rent_to_income,
         col_tier, vs_state) |>
  arrange(desc(median_rent)) |>
  write_csv(file.path(output_dir, "ca_col_cities_data.csv"))

message("✅ Saved: ca_col_cities_data.csv  (", nrow(places_coords), " cities)")
