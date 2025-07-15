library(leaflet)
library(tidyverse)
library(tigris)
library(sf)
library(RColorBrewer)

# Simulated data for PA counties
set.seed(123)
my_data <- tibble(
  GEOID = sprintf("42%03d", 1:67),  # '42' = PA state FIPS, counties 001–067
  poverty_rate = runif(67, 5, 30)   # random poverty rate between 5% and 30%
)



options(tigris_use_cache = TRUE)

pa_counties <- counties(state = "PA", cb = TRUE, class = "sf")  # returns an sf object


map_data <- pa_counties %>%
  left_join(my_data, by = "GEOID")


pal <- colorNumeric(palette = "YlOrRd", domain = map_data$poverty_rate)

leaflet(map_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(poverty_rate),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste0(NAME, " County<br>",
                    "Poverty Rate: ", round(poverty_rate, 1), "%")
  ) %>%
  addLegend(pal = pal, values = ~poverty_rate, title = "Poverty Rate (%)")
