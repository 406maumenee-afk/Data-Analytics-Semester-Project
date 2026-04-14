
# Data Cleaning Script 01 
# ECNS 460 Term Project - EV Charging Infrastructure Gaps
# Sawyer Bringle & Liam Maumenee


rm(list = ls())

# Load Libraries 
library(tidyverse)
library(janitor)    # for clean_names()


# PART 1: Clean EV registration for each state (3 states)


# Clean Colorado data (already aggregated by county) 

co_raw <- read_csv("/Raw Data/EVRegistrationCO.csv")

co_clean <- co_raw %>%
  clean_names() %>%
  # Remove vehicles registered out of state or with unknown county
  filter(county != "Out of State or Unknown") %>%
  mutate(county = str_remove(county, " County$") %>% str_trim()) %>%
  mutate(
    ev_share_of_original_registrations = parse_number(ev_share_of_original_registrations) / 100,
    ev_share_on_the_road = parse_number(ev_share_on_the_road) / 100,
    e_vs_on_the_road_growth_over_3_months = parse_number(e_vs_on_the_road_growth_over_3_months) / 100
  ) %>%
  mutate(state = "CO") %>%
  select(
    state, county,
    ev_registrations = ev_original_registrations,
    evs_on_road = e_vs_on_the_road,
    ev_share = ev_share_on_the_road
  )



# Connecticut data 

ct_raw <- read_csv("/Raw Data/EVRegistrationCT.csv")

ct_town_county <- tribble(
  ~town_pattern, ~county,
  "BETHEL|BRIDGEPORT|BROOKFIELD|DANBURY|DARIEN|EASTON|FAIRFIELD|GREENWICH|MONROE|NEW CANAAN|NEW FAIRFIELD|NEWTOWN|NORWALK|REDDING|RIDGEFIELD|SHELTON|SHERMAN|STAMFORD|STRATFORD|TRUMBULL|WESTON|WESTPORT|WILTON", "Fairfield",
  "AVON|BERLIN|BLOOMFIELD|BRISTOL|BURLINGTON|CANTON|EAST GRANBY|EAST HARTFORD|EAST WINDSOR|ENFIELD|FARMINGTON|GLASTONBURY|GRANBY|HARTFORD|HARTLAND|MANCHESTER|MARLBOROUGH|NEW BRITAIN|NEWINGTON|PLAINVILLE|ROCKY HILL|SIMSBURY|SOUTHINGTON|SOUTH WINDSOR|SUFFIELD|WEST HARTFORD|WETHERSFIELD|WINDSOR|WINDSOR LOCKS", "Hartford",
  "BARKHAMSTED|BETHLEHEM|BRIDGEWATER|CANAAN|COLEBROOK|CORNWALL|GOSHEN|HARWINTON|KENT|LITCHFIELD|MORRIS|NEW HARTFORD|NEW MILFORD|NORFOLK|NORTH CANAAN|PLYMOUTH|ROXBURY|SALISBURY|SHARON|THOMASTON|TORRINGTON|WARREN|WASHINGTON|WATERTOWN|WINCHESTER|WINSTED|WOODBURY", "Litchfield",
  "CHESTER|CLINTON|CROMWELL|DEEP RIVER|DURHAM|EAST HADDAM|EAST HAMPTON|ESSEX|HADDAM|KILLINGWORTH|MIDDLEFIELD|MIDDLETOWN|OLD SAYBROOK|PORTLAND|WESTBROOK", "Middlesex",
  "ANSONIA|BEACON FALLS|BETHANY|BRANFORD|CHESHIRE|DERBY|EAST HAVEN|GUILFORD|HAMDEN|MADISON|MERIDEN|MIDDLEBURY|MILFORD|NAUGATUCK|NEW HAVEN|NORTH BRANFORD|NORTH HAVEN|ORANGE|OXFORD|PROSPECT|SEYMOUR|SOUTHBURY|WALLINGFORD|WATERBURY|WEST HAVEN|WOLCOTT|WOODBRIDGE", "New Haven",
  "BOZRAH|COLCHESTER|EAST LYME|FRANKLIN|GRISWOLD|GROTON|LEBANON|LEDYARD|LISBON|LYME|MONTVILLE|NEW LONDON|NORTH STONINGTON|NORWICH|OLD LYME|PRESTON|SALEM|SPRAGUE|STONINGTON|VOLUNTOWN|WATERFORD", "New London",
  "ANDOVER|BOLTON|COLUMBIA|COVENTRY|ELLINGTON|HEBRON|MANSFIELD|SOMERS|STAFFORD|TOLLAND|UNION|VERNON|WILLINGTON", "Tolland",
  "ASHFORD|BROOKLYN|CANTERBURY|CHAPLIN|EASTFORD|HAMPTON|KILLINGLY|PLAINFIELD|POMFRET|PUTNAM|SCOTLAND|STERLING|THOMPSON|WINDHAM|WOODSTOCK", "Windham"
)

ct_clean <- ct_raw %>%
  clean_names() %>%
  # Keep only vehicles registered in CT
  filter(primary_customer_state == "CT") %>%
  # standardize city names to uppercase for matching against our crosswalk
  mutate(city = str_to_upper(str_trim(primary_customer_city))) %>%
  mutate(
    county = case_when(
      str_detect(city, ct_town_county$town_pattern[1]) ~ ct_town_county$county[1],
      str_detect(city, ct_town_county$town_pattern[2]) ~ ct_town_county$county[2],
      str_detect(city, ct_town_county$town_pattern[3]) ~ ct_town_county$county[3],
      str_detect(city, ct_town_county$town_pattern[4]) ~ ct_town_county$county[4],
      str_detect(city, ct_town_county$town_pattern[5]) ~ ct_town_county$county[5],
      str_detect(city, ct_town_county$town_pattern[6]) ~ ct_town_county$county[6],
      str_detect(city, ct_town_county$town_pattern[7]) ~ ct_town_county$county[7],
      str_detect(city, ct_town_county$town_pattern[8]) ~ ct_town_county$county[8],
      TRUE ~ NA_character_
    )
  ) %>%
  {
    unmatched <- sum(is.na(.$county))
    cat("CT towns not matched to a county:", unmatched, "out of", nrow(.), "\n")
    if (unmatched > 0) {
      cat("Unmatched cities:\n")
      print(filter(., is.na(county)) %>% count(city, sort = TRUE) %>% head(20))
    }
    .
  } %>%
  filter(!is.na(county)) %>%
  group_by(county) %>%
  summarize(
    ev_registrations = n(),
    bev_count = sum(type == "BEV", na.rm = TRUE),
    phev_count = sum(type == "PHEV", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(state = "CT", evs_on_road = ev_registrations, ev_share = NA_real_) %>%
  select(state, county, ev_registrations, evs_on_road, ev_share)

cat("Connecticut:", nrow(ct_clean), "counties loaded\n")


# Washington data
# WA data is also individual vehicle records but already has a county column. Just need to aggregate to county totals.

wa_raw <- read_csv("/Raw Data/EVRegistrationWA.csv")

wa_clean <- wa_raw %>%
  clean_names() %>%
  filter(state == "WA") %>%
  mutate(county = str_trim(county)) %>%
  filter(!is.na(county), county != "") %>%
  group_by(county) %>%
  summarize(
    ev_registrations = n(),
    bev_count = sum(str_detect(electric_vehicle_type, "Battery"), na.rm = TRUE),
    phev_count = sum(str_detect(electric_vehicle_type, "Plug-in"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(state = "WA", evs_on_road = ev_registrations, ev_share = NA_real_) %>%
  select(state, county, ev_registrations, evs_on_road, ev_share)



# Combine all three states into one dataset
ev_by_county <- bind_rows(co_clean, ct_clean, wa_clean)




# PART 2: Clean AFDC charging station data
# Start with full national dataset, filter to our 3 states, keep only

stations_raw <- read_csv("/Raw Data/alt_fuel_stations (Mar 11 2026) (1).csv")

stations_clean <- stations_raw %>%
  clean_names() %>%
  filter(fuel_type_code == "ELEC") %>%
  filter(state %in% c("CO", "CT", "WA")) %>%
  filter(status_code == "E") %>%
  select(station_name, city, state, zip, latitude, longitude,
         ev_level1_evse_num, ev_level2_evse_num, ev_dc_fast_count,
         ev_network, access_code, facility_type, open_date,
         ev_connector_types, ev_pricing) %>%
  filter(!is.na(latitude), !is.na(longitude))

print(count(stations_clean, state))



# PART 3: Assign stations to counties (spatial join)


library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

county_shapes <- counties(state = c("CO", "CT", "WA"), year = 2022, cb = TRUE) %>%
  st_transform(4326) %>%
  select(STATEFP, COUNTYFP, GEOID, NAME, geometry) %>%
  rename(county_name = NAME)

stations_sf <- stations_clean %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stations_with_county <- st_join(stations_sf, county_shapes, join = st_within) %>%
  st_drop_geometry()

#Fix CT: tigris uses planning regions, our EV data uses old county names
ct_station_region_to_county <- tribble(
  ~county_name, ~county_mapped,
  "Capitol", "Hartford",
  "Greater Bridgeport", "Fairfield",
  "Lower Connecticut River Valley", "Middlesex",
  "Naugatuck Valley", "New Haven",
  "Northeastern Connecticut", "Windham",
  "Northwest Hills", "Litchfield",
  "South Central Connecticut", "New Haven",
  "Southeastern Connecticut", "New London",
  "Western Connecticut", "Fairfield"
)

stations_with_county <- stations_with_county %>%
  left_join(ct_station_region_to_county, by = "county_name") %>%
  mutate(county_name = if_else(!is.na(county_mapped), county_mapped, county_name)) %>%
  select(-county_mapped)



stations_by_county <- stations_with_county %>%
  filter(!is.na(county_name)) %>%
  group_by(state, county_name) %>%
  summarize(
    total_stations = n(),
    total_ports = sum(coalesce(ev_level1_evse_num, 0) +
                        coalesce(ev_level2_evse_num, 0) +
                        coalesce(ev_dc_fast_count, 0), na.rm = TRUE),
    level2_ports = sum(coalesce(ev_level2_evse_num, 0), na.rm = TRUE),
    dc_fast_ports = sum(coalesce(ev_dc_fast_count, 0), na.rm = TRUE),
    public_stations = sum(access_code == "public", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(county = county_name)



# PART 4: Join EV stations and registrations by county


ev_stations_joined <- ev_by_county %>%
  left_join(stations_by_county, by = c("state", "county")) %>%
  mutate(
    total_stations = replace_na(total_stations, 0),
    total_ports = replace_na(total_ports, 0),
    level2_ports = replace_na(level2_ports, 0),
    dc_fast_ports = replace_na(dc_fast_ports, 0),
    public_stations = replace_na(public_stations, 0)
  ) %>%
  mutate(
    evs_per_station = if_else(total_stations > 0, evs_on_road / total_stations, NA_real_),
    evs_per_port = if_else(total_ports > 0, evs_on_road / total_ports, NA_real_),
    stations_per_1000_evs = if_else(evs_on_road > 0, (total_stations / evs_on_road) * 1000, NA_real_)
  )





# PART 5: Clean ACS census data for demographics and income levels 


reshape_census <- function(df, col_pattern) {
  headers <- names(df)
  est_cols <- headers[str_detect(headers, col_pattern)]
  geo_names <- str_remove(est_cols, "!!.*$") %>% str_trim()
  
  df_sub <- df %>% select(label = 1, all_of(est_cols))
  names(df_sub) <- c("label", geo_names)
  
  df_sub %>%
    pivot_longer(cols = -label, names_to = "geography", values_to = "value") %>%
    mutate(
      county_raw = str_extract(geography, "^[^,]+"),
      state_name = str_extract(geography, "(?<=, )\\w+$")
    )
}

# DP05: Population + Demographics from ACS
dp05_raw <- read_csv("/Raw Data/ACSDP5Y2023.DP05-2026-04-07T174614.csv")

population <- reshape_census(dp05_raw, "!!Estimate$") %>%
  filter(str_detect(label, "^\\s+Total population$")) %>%
  mutate(total_population = parse_number(as.character(value))) %>%
  select(geography, county_raw, state_name, total_population)

demographics <- reshape_census(dp05_raw, "!!Estimate$") %>%
  filter(
    str_detect(label, "^\\s+Median age \\(years\\)$") |
    str_detect(label, "^\\s+Hispanic or Latino \\(of any race\\)$") |
    str_detect(label, "^\\s+White alone$")
  ) %>%
  mutate(
    variable = case_when(
      str_detect(label, "Median age") ~ "median_age",
      str_detect(label, "Hispanic or Latino \\(of any race\\)") ~ "hispanic_pop",
      str_detect(label, "White alone") ~ "white_non_hispanic_pop"
    )
  ) %>%
  mutate(value = parse_number(as.character(value))) %>%
  select(geography, county_raw, state_name, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# S1901: Median household income 
s1901_raw <- read_csv("/Raw Data/ACSST5Y2023.S1901-2026-04-07T174729.csv")

median_income <- reshape_census(s1901_raw, "!!Households!!Estimate$") %>%
  filter(str_detect(label, "Median income")) %>%
  mutate(median_household_income = parse_number(as.character(value))) %>%
  select(geography, county_raw, state_name, median_household_income)

#  Combine all census variables
census_combined <- population %>%
  left_join(median_income, by = c("geography", "county_raw", "state_name")) %>%
  left_join(demographics, by = c("geography", "county_raw", "state_name")) %>%
  mutate(state = case_when(
    state_name == "Colorado" ~ "CO",
    state_name == "Connecticut" ~ "CT",
    state_name == "Washington" ~ "WA"
  )) %>%
  filter(!is.na(state))

# CT planning region → old county crosswalk
ct_region_to_county <- tribble(
  ~planning_region, ~county,
  "Capitol Planning Region", "Hartford",
  "Greater Bridgeport Planning Region", "Fairfield",
  "Lower Connecticut River Valley Planning Region", "Middlesex",
  "Naugatuck Valley Planning Region", "New Haven",
  "Northeastern Connecticut Planning Region", "Windham",
  "Northwest Hills Planning Region", "Litchfield",
  "South Central Connecticut Planning Region", "New Haven",
  "Southeastern Connecticut Planning Region", "New London",
  "Western Connecticut Planning Region", "Fairfield"
)

census_ct <- census_combined %>%
  filter(state == "CT") %>%
  left_join(ct_region_to_county, by = c("county_raw" = "planning_region")) %>%
  group_by(state, county) %>%
  summarize(
    total_population = sum(total_population, na.rm = TRUE),
    median_household_income = mean(median_household_income, na.rm = TRUE),
    median_age = mean(median_age, na.rm = TRUE),
    hispanic_pop = sum(hispanic_pop, na.rm = TRUE),
    white_non_hispanic_pop = sum(white_non_hispanic_pop, na.rm = TRUE),
    .groups = "drop"
  )

census_co_wa <- census_combined %>%
  filter(state != "CT") %>%
  mutate(county = str_remove(county_raw, " County$") %>% str_trim()) %>%
  select(state, county, total_population, median_household_income,
         median_age, hispanic_pop, white_non_hispanic_pop)

census_clean <- bind_rows(census_co_wa, census_ct) %>%
  distinct(state, county, .keep_all = TRUE)




# PART 6: Finally join EV, station, and census data into one dataset by county


ev_final <- ev_stations_joined %>%
  left_join(census_clean, by = c("state", "county")) %>%
  mutate(
    evs_per_1000_pop = if_else(total_population > 0,
                                (evs_on_road / total_population) * 1000, NA_real_),
    stations_per_100k_pop = if_else(total_population > 0,
                                     (total_stations / total_population) * 100000, NA_real_),
    pct_hispanic = if_else(total_population > 0,
                            (hispanic_pop / total_population) * 100, NA_real_),
    pct_white_non_hispanic = if_else(total_population > 0,
                                      (white_non_hispanic_pop / total_population) * 100, NA_real_),
    pct_nonwhite = 100 - pct_white_non_hispanic
  )






# PART 7: save data to csv and Rdata files

dir.create("clean_data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

save(ev_final, file = "clean_data/ev_final.RData")
write_csv(ev_final, "clean_data/ev_final.csv")
save(stations_with_county, file = "clean_data/stations_with_county.RData")
save(county_shapes, file = "clean_data/county_shapes.RData")


