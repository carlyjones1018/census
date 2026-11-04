library(dplyr)
library(ggplot2)

table_id <- "B01001"
county_geoid <- "05000US12115"
under_18_variables <- paste0(
  "B01001", sprintf("%03d", c(3:6, 27:30))
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

tract_population <- bind_rows(lapply(seq_len(nrow(tracts)), \(i) {
  geography <- paste0("14000US", tracts$GEOID[i])
  estimates <- vapply(
    table_variables,
    \(variable) get_estimate(geography, variable),
    numeric(1)
  )
  if (anyNA(estimates)) {
    stop("The ACS response is missing age estimates for tract ", tracts$GEOID[i], ".")
  }

  data.frame(
    GEOID = tracts$GEOID[i],
    NAME = tracts$NAME[i],
    under_18_population = sum(estimates[under_18_variables]),
    total_population = estimates["B01001001"]
  )
})) |>
  mutate(percent_under_18 = 100 * under_18_population / total_population)

tract_map <- tracts |>
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
    caption = "Source: U.S. Census Bureau ACS, via Census Reporter; tract boundaries: TIGER/Line"
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
