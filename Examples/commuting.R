library(tidyverse)
library(tidycensus)
library(tigris)
library(here)
library(sf)
library(RColorBrewer)
library(treemapify)


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
                          TRANWORK == 20 ~ "Other",
                          TRANWORK == 31 ~ "Transit",
                          TRANWORK == 34 ~ "Transit",
                          TRANWORK == 36 ~ "Transit",
                          TRANWORK == 37 ~ "Transit",
                          TRANWORK == 38 ~ "Other",
                          TRANWORK == 39 ~ "Other",
                          TRANWORK == 50 ~ "Bike",
                          TRANWORK == 60 ~ "Walk",
                          TRANWORK == 70 ~ "Other",
                          TRUE ~ "not_coded")) %>%
  select(pop_dens_km2, INCTOT, mode, TRANTIME)

write_csv(people, file = here("Examples",
                              "commuting.csv"))


summary(people$pop_dens_km2)
summary(people$INCTOT)
summary(people$TRANTIME)

sd(people$pop_dens_km2)
sd(people$INCTOT)
sd(people$TRANTIME)

ggplot(people) +
  geom_histogram(aes(x = TRANTIME), 
                 bins = 30,
                 fill = "darkgray",
                 color = "lightgray") +
  scale_x_continuous(name = "Typical commute time to work (hours)",
                     breaks = breaks <- seq(0, 150, by = 30),
                     labels = breaks / 60) +
  scale_y_continuous(name = "Number of survey respondents",
                     breaks = breaks <- seq(0, 125000, by = 25000),
                     labels = formatC(breaks, big.mark = ",",
                                      format = "f", digits = 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Barlow Light"))

ggplot(people) +
  geom_histogram(aes(x = INCTOT), 
                 bins = 30,
                 fill = "darkgray",
                 color = "lightgray") +
  scale_x_continuous(name = "Annual income", trans = "log",
                     breaks = breaks <- 10^seq(0, 6, by=1),
                     labels = paste0("$",
                                     formatC(breaks, format = "f",
                                             digits = 0,
                                             big.mark = ","))) +
  scale_y_continuous(name = "Number of survey respondents",
                     breaks = breaks <- seq(0, 150000, by = 25000),
                     labels = formatC(breaks, big.mark = ",",
                                      format = "f", digits = 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Barlow Light"))

ggplot(people) +
  geom_histogram(aes(x = pop_dens_km2), 
                 bins = 30,
                 fill = "darkgray",
                 color = "lightgray") +
  scale_x_continuous(name = "Residential density (people per square kilometer)", 
                     trans = "log",
                      breaks = breaks <- 10^seq(0, 4, by=1),
                      labels = formatC(breaks, format = "f",
                                              digits = 0,
                                              big.mark = ",")) +
  scale_y_continuous(name = "Number of survey respondents",
                     breaks = breaks <- seq(0, 100000, by = 25000),
                     labels = formatC(breaks, big.mark = ",",
                                      format = "f", digits = 0)) +
  theme_minimal() +
  theme(text = element_text(family = "Barlow Light"))

people %>%
  group_by(mode) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count)) %>%
ggplot() +
  geom_treemap(aes(area = share,
                   fill = mode)) +
  geom_treemap_text(aes(area = share,
                        label = 
                          paste0(mode, "\n",
                                 formatC(share*100, 
                                         format = "f",
                                         digits = 0),"%")),
                    family = "Barlow Light", color = "white") +
  scale_fill_grey(guide = "none") +
  theme_minimal() 

##
model_income <- lm(log(TRANTIME) ~ log(INCTOT),
                   data = people)
summary(model_income)

model_dens <- lm(log(TRANTIME) ~ log(pop_dens_km2),
                   data = people)
summary(model_dens)

model_mode <- lm(log(TRANTIME) ~ fct_infreq(mode),
                 data = people)
summary(model_mode)
