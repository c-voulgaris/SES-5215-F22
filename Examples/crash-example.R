library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)

MA_st_plane <- "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000.0001016002 +y_0=750000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +to_meter=0.3048006096012192 +no_defs"


crashes <- st_read("https://data.boston.gov/dataset/7b29c1b2-7ec2-4023-8292-c24f5d8f0905/resource/e4bfe397-6bfc-49c5-9367-c879fac7401d/download/tmp_jqtmwbg.csv",
                   options=c("X_POSSIBLE_NAMES=x_cord",
                             "Y_POSSIBLE_NAMES=y_cord")) %>%
  st_set_crs(MA_st_plane)
  
boston <- places(state = "MA") %>%
  filter(NAME == "Boston") 

tract_data <- get_acs(geography = "tract",
                  variables = c(pop = "B01003_001",
                                med_age = "B01002_001",
                                hh_below_pov = "B05010_002",
                                tot_hhs = "B05010_001"),
                  state = "MA",
                  county = "Suffolk",
                  geometry = TRUE,
                  output = "wide") %>%
  mutate(majority_poverty = hh_below_povE / tot_hhsE > 0.5) %>%
  st_centroid() %>%
  st_filter(boston) %>%
  st_drop_geometry() %>%
  select(GEOID, popE, med_ageE, majority_poverty, tot_hhsE)
  
tracts <- tracts(state = "MA", county = "Suffolk") %>%
  select(GEOID, ALAND) %>%
  right_join(tract_data) %>%
  filter(tot_hhsE > 0) %>%
  mutate(ppl_per_km2 = popE * 1000000 / ALAND) %>%
  st_transform(MA_st_plane) %>%
  mutate(n_crashes = lengths(st_intersects(., crashes))) %>%
  mutate(crashes_per_km2 = n_crashes * 1000000 / ALAND) %>%
  select(med_ageE, ppl_per_km2, majority_poverty, crashes_per_km2) 

ggplot(tracts) +
  geom_sf(fill = "pink") +
  geom_sf(data = boston, fill = NA, color = "red")
