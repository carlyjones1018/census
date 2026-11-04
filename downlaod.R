library(dplyr)
library(ggplot2)

table_id <- "B01001"
county_geoid <- "05000US12115"
under_18_variables <- paste0(
  "B01001", sprintf("%03d", c(3:6, 27:30))
)
young_adult_variables <- paste0(
  "B01001", sprintf("%03d", c(7:11, 31:35))
)
middle_age_variables <- paste0(
  "B01001", sprintf("%03d", c(12:15, 36:39))
)
senior_variables <- paste0(
  "B01001", sprintf("%03d", c(17:25, 41:49))
)
age_group_names <- c(
  "Under 18",
  "18–29: Young adult",
  "30–50: Middle age",
  "51+: Senior"
)
table_variables <- paste0("B01001", sprintf("%03d", 1:49))

tracts <- tigris::tracts(
  state = "FL",
  county = "Sarasota",
  year = 2024,
  cb = TRUE,
  class = "sf"
)

geography_ids <- c(county_geoid, paste0("14000US", tracts$GEOID))
geography_batches <- split(
  geography_ids,
  ceiling(seq_along(geography_ids) / 20)
)
acs_data <- lapply(geography_batches, \(batch) {
  data_url <- paste0(
    "https://api.censusreporter.org/1.0/data/show/acs2024_5yr",
    "?table_ids=", table_id,
    "&geo_ids=", paste(batch, collapse = ",")
  )
  jsonlite::fromJSON(data_url, simplifyVector = FALSE)$data
}) |>
  Reduce(f = c)

get_estimate <- \(geography, variable) {
  value <- acs_data[[geography]][[table_id]]$estimate[[variable]]
  if (is.null(value)) NA_real_ else as.numeric(value)
}

county_estimates <- vapply(
  table_variables,
  \(variable) get_estimate(county_geoid, variable),
  numeric(1)
)
if (anyNA(county_estimates)) {
  stop("The ACS response is missing one or more county age estimates.")
}

age_data <- data.frame(
  GEOID = "12115",
  NAME = "Sarasota County, Florida",
  variable = paste0("B01001_", sprintf("%03d", 1:49)),
  estimate = county_estimates
)
write.csv(age_data, "sarasota_age_data.csv", row.names = FALSE)

under_18_count <- sum(county_estimates[under_18_variables])
total_population <- county_estimates["B01001001"]
under_18_summary <- data.frame(
  county = "Sarasota County, Florida",
  year = 2024,
  under_18_population = under_18_count,
  total_population = total_population,
  percent_under_18 = under_18_count / total_population * 100
)
write.csv(under_18_summary, "sarasota_under_18_summary.csv", row.names = FALSE)

tract_age_groups <- bind_rows(lapply(seq_len(nrow(tracts)), \(i) {
  geography <- paste0("14000US", tracts$GEOID[i])
  estimates <- vapply(
    table_variables,
    \(variable) get_estimate(geography, variable),
    numeric(1)
  )
  if (anyNA(estimates)) {
    stop("The ACS response is missing age estimates for tract ", tracts$GEOID[i], ".")
  }

  age_group_population <- c(
    sum(estimates[under_18_variables]),
    sum(estimates[young_adult_variables]),
    sum(estimates[middle_age_variables]) +
      0.2 * sum(estimates[c("B01001016", "B01001040")]),
    sum(estimates[senior_variables]) +
      0.8 * sum(estimates[c("B01001016", "B01001040")])
  )

  data.frame(
    GEOID = tracts$GEOID[i],
    NAME = tracts$NAME[i],
    age_group = age_group_names,
    population = age_group_population,
    total_population = unname(estimates["B01001001"])
  )
})) |>
  mutate(age_group = factor(age_group, levels = age_group_names)) |>
  mutate(percent = 100 * population / total_population)

tract_age_map <- tracts |>
  left_join(tract_age_groups, by = c("GEOID", "NAME"))

write.csv(
  sf::st_drop_geometry(tract_age_map),
  "sarasota_age_groups_by_tract.csv",
  row.names = FALSE
)

age_group_map <- ggplot(tract_age_map) +
  geom_sf(aes(fill = percent), color = "white", linewidth = 0.1) +
  facet_wrap(~age_group, ncol = 2) +
  scale_fill_viridis_c(
    name = "Residents (%)",
    labels = \(x) paste0(x, "%")
  ) +
  labs(
    title = "Age Groups Across Sarasota County",
    subtitle = "Share of residents in each age group by census tract | 2024 ACS 5-year estimates",
    caption = "Source: U.S. Census Bureau ACS, via Census Reporter; tract boundaries: TIGER/Line"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "sarasota_age_groups_map.png",
  age_group_map,
  width = 11,
  height = 9,
  dpi = 300
)

florida_age_data <- read.csv(
  paste0(
    "https://www2.census.gov/programs-surveys/popest/datasets/",
    "2020-2024/state/asrh/sc-est2024-alldata6.csv"
  )
) |>
  filter(STATE == 12, SEX == 0, ORIGIN == 0, RACE %in% 1:6)

if (nrow(florida_age_data) == 0) {
  stop("The Census population estimates file did not contain Florida age data.")
}

estimate_years <- 2020:2024
florida_under_18_trend <- data.frame(
  year = estimate_years,
  under_18_population = vapply(estimate_years, \(year) {
    sum(
      florida_age_data[[paste0("POPESTIMATE", year)]][
        florida_age_data$AGE < 18
      ]
    )
  }, numeric(1)),
  total_population = vapply(estimate_years, \(year) {
    sum(florida_age_data[[paste0("POPESTIMATE", year)]])
  }, numeric(1))
) |>
  mutate(percent_under_18 = 100 * under_18_population / total_population)

write.csv(
  florida_under_18_trend,
  "florida_under_18_trend.csv",
  row.names = FALSE
)

florida_under_18_plot <- ggplot(
  florida_under_18_trend,
  aes(x = year, y = percent_under_18)
) +
  geom_line(color = "#287C8E", linewidth = 1) +
  geom_point(color = "#287C8E", size = 3) +
  geom_text(
    aes(label = sprintf("%.1f%%", percent_under_18)),
    vjust = -1,
    size = 4
  ) +
  scale_x_continuous(breaks = estimate_years) +
  scale_y_continuous(
    labels = \(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  labs(
    title = "Florida Population Under 18 Over Time",
    subtitle = "Share of Florida residents under age 18, 2020–2024",
    x = "Year",
    y = "Population under 18",
    caption = "Source: U.S. Census Bureau, Vintage 2024 Population Estimates"
  ) +
  theme_minimal()

ggsave(
  "florida_under_18_trend.png",
  florida_under_18_plot,
  width = 9,
  height = 5,
  dpi = 300
)
