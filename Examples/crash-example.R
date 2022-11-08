library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(ggspatial)
library(extrafont)
library(rstatix)

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
  annotation_map_tile(type = "cartolight", zoomin = -1) +
  geom_sf(fill = "orange", color = "gray", alpha = 0.5) +
  theme_void()

ggplot(tracts) +
  geom_histogram(aes(x = crashes_per_km2),
                 bins = 25,
                 fill = "orange", color = "gray", alpha = 0.5) +
  scale_x_continuous(name = "Crashes per square kilometer",
                     trans = "log",
                     breaks = breaks <- 10^seq(1, 5, by=0.5),
                     labels = formatC(breaks, format = "f", digits = 0)) +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

ggplot(tracts) +
  geom_histogram(aes(x = crashes_per_km2),
                 bins = 25,
                 fill = "orange", color = "gray", alpha = 0.5) +
  scale_x_continuous(name = "Crashes per square kilometer") +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

ggplot(tracts) +
  geom_histogram(aes(x = med_ageE),
                 bins = 25,
                 fill = "orange", color = "gray", alpha = 0.5) +
  scale_x_continuous(name = "Median age (years)") +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

ggplot(tracts) +
  geom_histogram(aes(x = ppl_per_km2),
                 bins = 25,
                 fill = "orange", color = "gray", alpha = 0.5) +
  scale_x_continuous(name = "Population per square kilometer") +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

ggplot(tracts) +
  geom_histogram(aes(x = ppl_per_km2),
                 bins = 25,
                 fill = "orange", color = "gray", alpha = 0.5) +
  scale_x_continuous(name = "Population per square kilometer",
                     trans = "log",
                     breaks = breaks <- 5*10^seq(1, 5, by=1),
                     labels = formatC(breaks, format = "f", digits = 0)) +
  scale_y_continuous(name = "Number of census tracts") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

ggplot(tracts) +
  annotation_map_tile(type = "cartolight", zoomin = -1) +
  geom_sf(alpha = 0.5,
          aes(fill = majority_poverty), color = "gray") +
  scale_fill_manual(name = "",
                    values = c("orange",
                               "green"),
                    labels = c("Minority poverty",
                               "Majority poverty")) +
  ggthemes::theme_map() +
  theme(text = element_text(family = "Cambria"),
        legend.background = element_rect(fill = NA))

ggplot(tracts) +
  geom_point(aes(x = med_ageE, y = crashes_per_km2)) +
  scale_y_continuous(trans = "log",
                     name = "Number of crashes per square kilometer",
                     breaks = breaks <- 5*10^seq(1, 4, by = 1)) +
  scale_x_continuous(name = "Median age within census tract") +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

cor.test(tracts$med_ageE, tracts$crashes_per_km2)
cor.test(tracts$med_ageE, log(tracts$crashes_per_km2))

ggplot(tracts) +
  geom_point(aes(x = ppl_per_km2, y = crashes_per_km2)) +
  scale_y_continuous(trans = "log",
                     name = "Number of crashes per square kilometer",
                     breaks = breaks <- 5*10^seq(1, 4, by = 1)) +
  scale_x_continuous(trans = "log",
                     name = "Population per square kilometer",
                     breaks = breaks <- 10^seq(1, 4, by = 1),
                     labels = prettyNum(breaks, big.mark = ",")) +
  theme_minimal() +
  theme(text = element_text(family = "Cambria"))

cor.test(log(tracts$ppl_per_km2), log(tracts$crashes_per_km2))

crashes_by_poverty <- tracts %>%
  st_drop_geometry() %>%
  group_by(majority_poverty) %>%
  get_summary_stats(crashes_per_km2, type = "mean_ci") %>%
  mutate(ci_low = mean - ci,
         ci_hi = mean + ci)

crashes_by_poverty

ggplot(crashes_by_poverty) +
  geom_col(aes(x = majority_poverty, y = mean),
           fill = "orange", alpha = 0.5) +
  geom_errorbar(aes(x = majority_poverty,
                    ymin = ci_low,
                    ymax = ci_hi),
                width = 0.2) +
  scale_x_discrete(name = "",
                   labels = c("Majority above\npoverty level",
                              "Majority below\npoverty level")) +
  scale_y_continuous(name = "Average crashes per square kilometer") +
  theme_minimal() +
    theme(text = element_text(family = "Cambria"))
  
comp_crashes_by_poverty <- tracts %>%
  t_test(crashes_per_km2 ~ majority_poverty, detailed = TRUE, conf.level = 0.95)
 
comp_crashes_by_poverty
