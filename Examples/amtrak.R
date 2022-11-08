####

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)

### Load county-level datat

# List of county variable names from ACS
county_vars_acs <- c(med_income_ = "B06011_001",
                     population_ = "B01003_001")

# List of county variable names from decennial census
# (acs doesn't indicate urban/rural)
county_vars_dec <- c(urban_pop = "H002002",
                     total_pop = "H002001") 

# Download the data from ACS
counties_acs_data <- get_acs(geography = "county",
                             year = 2018,
                             variables = county_vars_acs,
                             output = "wide") %>%
  select(GEOID, med_income_E, population_E)

# Download urban/rural data from census
counties_dec_data <- get_decennial(geography = "county",
                                   year = 2010,
                                   variables = county_vars_dec,
                                   output = "wide") %>%
  # Calculate the percent of the population classified as being in 
  # urban areas
  mutate(pct_urban = urban_pop / total_pop) %>% 
  # Classify the county as urban if at least half of the population
  # is urban
  mutate(is_urban = pct_urban > 0.5) %>%
  select(GEOID, is_urban)

# Download county boundaries (also a useful variable indicating the
# land area of the county, excluding water).
counties <- counties(year = 2010) %>%
  rename(GEOID = GEOID10) %>%
  # ALAND10 is the land area in square meters
  select(GEOID, ALAND10) %>%
  left_join(counties_acs_data) %>%
  inner_join(counties_dec_data) %>%
  # calculate the population density in square km
  mutate(pop_dens = population_E * 1000000 / ALAND10) %>%
  select(GEOID, pop_dens, med_income_E, is_urban) %>%
  # Transform to WGS 84 CRS
  st_transform("WGS84")

### Load ridership data
## This is a list of stations, and it includes lat/lon coordinates,
## which makes things a little easier

## Note that ridership appears to be boardings + alightings
## You might also need to note that this seems to include Amtrack bus service.

https://www.bts.dot.gov/browse-statistical-products-and-data/state-transportation-statistics/amtrak-ridership
ridership_url <- "https://explore.dot.gov/vizql/w/STSamtrak/v/Table/vudcsv/sessions/FB09B191FA754F7E889985100222D981-1:0/views/14920515870136422130_17576232976373383193?summary=true"

ridership <- read_csv(ridership_url) 
%>%
  # Only keep the data from 1 year (I'm using 2018 here because there are some
  # issues with a couple missing coordinates for later years)
  filter(`Fiscal Year` == 2018) %>%
  # Convert to spatial data using the lat/long columns
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  # Specify that these are WGS 84 coordinates
  st_set_crs("WGS84") %>%
  # The ridership is in a column called "Value" rename to something mor useful
  rename(ridership = Value) %>%
  # Keep only the one column
  select(ridership) %>%
  # Join the data to the county containing the point
  st_join(counties) %>%
  # Drop the geometry column so we can collapse to the county level
  st_drop_geometry() %>%
  # List the county-level variables here
  group_by(GEOID, med_income_E, is_urban, pop_dens) %>%
  # Add up the total ridership within each county
  summarize(total_ridership = sum(ridership))

# write dataset to a csv
write_csv(ridership, "amtrak_riders_by_county.csv")
