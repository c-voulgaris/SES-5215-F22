library(tidyverse)
library(tidycensus)
library(here)

# Load survey

people <- here("Examples",
               "usa_00006.csv.gz") %>%
  read_csv() %>%
  filter(TRANTIME > 0)

