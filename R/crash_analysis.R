library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(leaflet)


# Load and clean crash data -----------------------------------------------


## need to download csv from https://opendata-nzta.opendata.arcgis.com/datasets/crash-analysis-system-cas-data-1/explore?location=-20.304565%2C0.000000%2C2.92

crash_csv <- data.table::fread(
  "Data/Crash_Analysis_System_(CAS)_data.csv",
  keepLeadingZeros = TRUE
)


summary(crash_csv)

vehicle_vars <- c(
  "bicycle", "bus", "carStationWagon", "moped", "motorcycle", "otherVehicleType", "schoolBus",
  "suv", "taxi", "train", "truck", "unknownVehicleType", "vanOrUtility", "vehicle"
)

outcome_vars <- c("fatalCount", "seriousInjuryCount", "minorInjuryCount")

na_to_0 <- function(x) {
  coalesce(x, 0)
}

crash_csv <- crash_csv %>%
  mutate(
    across(all_of(c(vehicle_vars, outcome_vars)), na_to_0),
    tlaId = stringr::str_pad(tlaId, width = 3, pad = "0"),
    severity = case_when(
      fatalCount > 0 ~ "High",
      seriousInjuryCount > 0 ~ "Medium",
      TRUE ~ "Low"
    )
  ) %>% 
  mutate(
    totalVehicle = bicycle + bus + carStationWagon + moped + motorcycle + otherVehicleType +
      schoolBus + suv + taxi + train + truck + unknownVehicleType + vanOrUtility
  )


# Load and clean population estimates -------------------------------------



## extracted from nzdotstat Subnational population estimates (TA, subdivision), by age and sex, at 30 June 2018-2023 (2023 boundaries)
## 2024-05-07: https://nzdotstat.stats.govt.nz/wbos/index.aspx?_ga=2.195249001.432160233.1714945581-1450402905.1713473188
## I have included all ages, and ages 15+ to represent the driving age population
erp_by_ta <- data.table::fread(
  "Data/erp_by_ta.csv", skip = 1
)

erp_by_ta <- erp_by_ta %>% 
  fill(
    c('Area', 'Age', 'Sex'), .direction = "down"
  ) %>% 
  filter(
    !is.na(`Year at 30 June`)
  ) %>% 
  mutate(
    tlaId = stringr::str_split_i(Area, pattern = ": ", i = 1),
    pop_group = ifelse(
      Age == "Total people, age", "est_res_pop", "driving_age_pop"
    )
  ) %>% 
  group_by(tlaId, Year = `Year at 30 June`, pop_group) %>% 
  summarise(
    count = sum(Figure)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c(tlaId, Year),
    names_from = pop_group,
    values_from = count
  )


# Aggregate crash data and combine with population ------------------------

crash_outcomes_by_year <- 
  crash_csv %>% 
  group_by(tlaId, tlaName, crashYear) %>% 
  summarise(
    across(all_of(outcome_vars), sum)
  ) %>%  
  pivot_longer(
    cols = all_of(outcome_vars),
    names_to = "outcome",
    values_to = "n"
  ) %>% 
  mutate(
    outcome = case_when(
      outcome == "minorInjuryCount" ~ "Minor injuries",
      outcome == "seriousInjuryCount" ~ "Serious injuries",
      outcome == "fatalCount" ~ "Fatalities"
    ),
    outcome = factor(outcome, levels = c("Minor injuries", "Serious injuries", "Fatalities"))
  )

crashes_per_capita <- 
  crash_outcomes_by_year %>% 
  inner_join(
    erp_by_ta,
    join_by(tlaId, crashYear == Year),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    outcome_per_cap = n / est_res_pop,
    outcome_per_dap = n / driving_age_pop
  ) %>% 
  filter(crashYear != 2023) #2023 data is not complete so skews picture


# Graphs and tables -------------------------------------------------------

fatalities_per_cap_2022 <-
  crashes_per_capita %>% 
  filter(crashYear == 2022, outcome == "Fatalities") %>% 
  arrange(desc(outcome_per_cap))

serious_injuries_per_cap_2022 <-
  crashes_per_capita %>% 
  filter(crashYear == 2022, outcome == "Serious injuries") %>% 
  arrange(desc(outcome_per_cap))

serious_or_fatal_per_cap <-
  crashes_per_capita %>% 
  filter(outcome %in% c("Fatalities", "Serious injuries")) %>% 
  group_by(tlaId, tlaName) %>% 
  summarise(
    average_per_cap = weighted.mean(x = outcome_per_cap, w = est_res_pop),
    average_per_dap = weighted.mean(x = outcome_per_dap, w = est_res_pop)
  ) %>%
  ungroup() %>% 
  arrange(desc(average_per_cap))

head(serious_or_fatal_per_cap)
tail(serious_or_fatal_per_cap)

ggplot(crash_csv) + 
  geom_histogram(aes(x=crashYear), binwidth = 1) +
  facet_wrap(~region, axes = "all")

ggplot(crashes_per_capita) +
  geom_col(aes(x=crashYear, y = outcome_per_cap, fill = outcome)) +
  facet_wrap(~tlaName)


# Interactive maps ----------------------------------------------

map_data_2022 <- crash_csv %>%
  filter(crashYear == 2022) %>% 
  mutate(
    Northing = as.numeric(Y),
    Easting = as.numeric(X)
  )

map_data_2022 <- st_as_sf(map_data_2022, coords = c("Easting", "Northing"), crs = 2193)
map_data_2022 <- st_transform(map_data_2022, 4326)

map_chathams <- map_data_2022[map_data_2022$tlaName == "Chatham Islands Territory",]
map_main <- map_data_2022[map_data_2022$tlaName != "Chatham Islands Territory",]
map_chathams <- st_shift_longitude(map_chathams) #moves Chathams from the Western side of the map to the East
map_data_2022 <- bind_rows(map_main, map_chathams)

map_data_2022$lng = st_coordinates(map_data_2022$geometry)[,1]
map_data_2022$lat = st_coordinates(map_data_2022$geometry)[,2]

map_data_2022 <-
  map_data_2022 %>% 
  select(lat, lng, severity) %>% 
  mutate(
    colour = case_when(
      severity == "High" ~ "red",
      severity == "Medium" ~ "orange",
      TRUE ~ "green"
    )
  )

leaflet(
  data = map_data_2022
) %>% 
  addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  addCircleMarkers(
    color = "black",
    weight = 2,
    fillColor = ~colour,
    fillOpacity = 0.7,
    radius = 5,
    label = ~paste("Severity: ", severity),
    lat = ~lat,
    lng = ~lng,
    clusterOptions = markerClusterOptions(
      maxClusterRadius = 75,
      disableClusteringAtZoom = 13
    )
  )
