library(tidyverse)
library(tidycensus)
library(tigris)
library(here)
library(sf)

# Load survey

PUMA_pop <- get_acs(geography = "public use microdata area",
                    state = "CA",
                    variables = "B01003_001")

PUMAs <- pumas(state = "CA") %>%
  rename(GEOID = GEOID10) %>%
  left_join(PUMA_pop) %>%
  st_drop_geometry() %>%
  mutate(PUMA = as.numeric(substr(GEOID, 3, 7))) %>%
  mutate(pop_dens_km2 = estimate * 1000000 / ALAND10) %>%
  select(PUMA, pop_dens_km2)
  

people <- here("Examples",
               "usa_00006.csv.gz") %>%
  read_csv() %>%
  filter(TRANTIME > 0) %>%
  left_join(PUMAs)  %>%
  mutate(mode = case_when(TRANWORK == 10 ~ "Car",
                          TRANWORK == 20 ~ "Motorcycle",
                          TRANWORK == 31 ~ "Transit",
                          TRANWORK == 34 ~ "Transit",
                          TRANWORK == 36 ~ "Transit",
                          TRANWORK == 37 ~ "Transit",
                          TRANWORK == 38 ~ "Taxi",
                          TRANWORK == 39 ~ "Ferry",
                          TRANWORK == 50 ~ "Bike",
                          TRANWORK == 60 ~ "Walk",
                          TRANWORK == 70 ~ "Other",
                          TRUE ~ "not_coded")) %>%
  select(pop_dens_km2, INCTOT, mode, TRANTIME)

write_csv(people, file = here("Examples",
                              "commuting.csv"))

