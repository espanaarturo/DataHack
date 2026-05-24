rm(list = ls())

library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(purrr)
library(tibble)

year_files <- tibble::tribble(
  ~academic_year, ~file_stub,
  "2019-20", "hse1920",
  "2020-21", "hse2021",
  "2021-22", "hse2122",
  "2022-23", "hse2223",
  "2023-24", "hse2324",
  "2024-25", "hse2425"
)

download_year_file <- function(file_stub) {
  data_url <- paste0("https://www3.cde.ca.gov/demo-downloads/homeless/", file_stub, ".txt")
  data_path <- file.path("data", "raw","homeless student data", paste0(file_stub, ".txt"))
  
  if (!file.exists(data_path)) {
    download.file(data_url, destfile = data_path, mode = "wb")
  }
  
  data_path
}

extract_statewide_total <- function(academic_year, file_stub) {
  data_path <- download_year_file(file_stub)
  
  read_tsv(data_path, show_col_types = FALSE) %>%
    filter(
      `Aggregate Level` == "T",
      `County Code` == "00",
      `Reporting Category` == "TA",
      `Charter School` == "All",
      DASS == "All"
    ) %>%
    transmute(
      academic_year = academic_year,
      cumulative_enrollment = `Cumulative Enrollment`,
      homeless_students = `Homeless Student Enrollment`
    )
}

statewide_totals <- pmap_dfr(year_files, extract_statewide_total) %>%
  mutate(
    academic_year = factor(academic_year, levels = year_files$academic_year),
    homeless_share = homeless_students / cumulative_enrollment
  )

latest_year <- statewide_totals %>%
  filter(academic_year == "2024-25")

comparison_data <- tibble::tibble(
  group = c("All students", "Students experiencing homelessness"),
  students = c(
    latest_year$cumulative_enrollment,
    latest_year$homeless_students
  )
)

homeless_plot <- ggplot(
  statewide_totals,
  aes(x = academic_year, y = homeless_students, group = 1)
) +
  geom_line(color = "#c05a2b", linewidth = 1.2) +
  geom_point(color = "#c05a2b", size = 3.4) +
  geom_text(
    aes(label = comma(homeless_students)),
    vjust = -0.8,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  labs(
    title = "Student Homelessness Has Risen in California",
    subtitle = "Recent CDE data show a growing statewide need over time",
    x = NULL,
    y = "Homeless student enrollment",
    caption = paste(
      "Source: California Department of Education Homeless Enrollment Downloadable Data Files, 2019-20 to 2024-25.",
      "Policy note: homeless students generate LCFF funding indirectly through automatic FRPM eligibility,",
      "but homelessness is not its own separate LCFF targeted category."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/california_homeless_students_trend_2019_20_to_2024_25.png",
  plot = homeless_plot,
  width = 10,
  height = 6,
  dpi = 300
)

comparison_plot <- ggplot(
  comparison_data,
  aes(x = group, y = students, fill = group)
) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = comma(students)),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "All students" = "#9fb8ad",
      "Students experiencing homelessness" = "#c05a2b"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "California Student Homelessness Compared with Total Enrollment",
    subtitle = paste0(
      "In 2024-25, ",
      comma(latest_year$homeless_students),
      " out of ",
      comma(latest_year$cumulative_enrollment),
      " cumulatively enrolled students experienced homelessness (",
      percent(latest_year$homeless_share, accuracy = 0.1),
      ")"
    ),
    x = NULL,
    y = "Number of students",
    caption = "Source: California Department of Education Homeless Enrollment Downloadable Data File, 2024-25."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/california_homeless_vs_total_students_2024_25.png",
  plot = comparison_plot,
  width = 10,
  height = 6,
  dpi = 300
)


print(homeless_plot)
print(comparison_plot)
