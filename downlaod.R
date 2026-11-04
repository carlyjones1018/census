library(tidycensus)
library(dplyr)
library(ggplot2)

age_data <- get_acs(
  geography = "county",
  state = "FL",
  county = "Sarasota",
  table = "B01001",
  year = 2024,
  survey = "acs5"
)

write.csv(age_data, "sarasota_age_data.csv", row.names = FALSE)

under_18_variables <- c(
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030"
)

under_18_count <- sum(
  age_data$estimate[age_data$variable %in% under_18_variables]
)
total_population <- age_data$estimate[age_data$variable == "B01001_001"]

under_18_summary <- data.frame(
  county = "Sarasota County, Florida",
  year = 2024,
  under_18_population = under_18_count,
  total_population = total_population,
  percent_under_18 = under_18_count / total_population * 100
)

write.csv(under_18_summary, "sarasota_under_18_summary.csv", row.names = FALSE)

tract_age_data <- get_acs(
  geography = "tract",
  state = "FL",
  county = "Sarasota",
  table = "B01001",
  year = 2024,
  survey = "acs5",
  geometry = TRUE
)

tract_population <- tract_age_data |>
  sf::st_drop_geometry() |>
  group_by(GEOID, NAME) |>
  summarise(
    under_18_population = sum(estimate[variable %in% under_18_variables]),
    total_population = estimate[variable == "B01001_001"][1],
    .groups = "drop"
  ) |>
  mutate(percent_under_18 = 100 * under_18_population / total_population)

tract_map <- tract_age_data |>
  filter(variable == "B01001_001") |>
  select(GEOID, NAME, geometry) |>
  left_join(tract_population, by = c("GEOID", "NAME"))

write.csv(
  sf::st_drop_geometry(tract_map),
  "sarasota_under_18_by_tract.csv",
  row.names = FALSE
)

under_18_map <- ggplot(tract_map) +
  geom_sf(aes(fill = percent_under_18), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    name = "Under 18 (%)",
    labels = \(x) paste0(x, "%")
  ) +
  labs(
    title = "Population Under 18 in Sarasota County",
    subtitle = "Share of residents under age 18 by census tract | 2024 ACS 5-year estimates",
    caption = "Source: U.S. Census Bureau, American Community Survey"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "sarasota_under_18_map.png",
  under_18_map,
  width = 9,
  height = 7,
  dpi = 300
)
