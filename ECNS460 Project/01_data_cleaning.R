# Data Cleaning Script 01
# ECNS 460 Term Project - EV Charging Infrastructure Gaps
# Sawyer Bringle & Liam Maumenee
#
# This script pulls together the four raw inputs (CO/CT/WA EV registrations,
# the national AFDC charging-station file, and two ACS census tables) and
# produces a single county-level master dataset

rm(list = ls())

# Load Libraries
library(tidyverse)
library(janitor)    # for clean_names()


# PART 1: Clean EV registration for each state (3 states)
# CO is already aggregated to county level 
# CT and WA are individual vehicle records that we have to aggregate, 
# Additionally, CT registers vehicles by town rather than county 


# Colorado — already aggregated by county, so just clean column names and parse percents.

co_raw <- read_csv("Raw Data/EVRegistrationCO.csv")

co_clean <- co_raw |>
  clean_names() |>
  filter(county != "Out of State or Unknown") |>
  # Strip the trailing " County" so names other data
  mutate(county = str_remove(county, " County$") |> str_trim()) |>
  # Convert to decimals
  mutate(
    ev_share_of_original_registrations = parse_number(ev_share_of_original_registrations) / 100,
    ev_share_on_the_road = parse_number(ev_share_on_the_road) / 100,
    e_vs_on_the_road_growth_over_3_months = parse_number(e_vs_on_the_road_growth_over_3_months) / 100
  ) |>
  mutate(state = "CO") |>
  select(
    state, county,
    ev_registrations = ev_original_registrations,
    evs_on_road = e_vs_on_the_road,
    ev_share = ev_share_on_the_road
  )



# Connecticut data
# CT registers vehicles by town rather than by county. To get to county-level totals we build a town -> county crosswalk and join on it.
# Storing the crosswalk in long format lets us replace the case_when with a single left_join for simplicity

ct_raw <- read_csv("Raw Data/EVRegistrationCT.csv")

# One row per (county, vector of towns). unnest() expands this into a town -> county lookup with one row per town. 
# The towns are uppercase to match the standardized city column we build below.
ct_town_county <- tribble(
  ~county,        ~towns,
  "Fairfield",    c("BETHEL","BRIDGEPORT","BROOKFIELD","DANBURY","DARIEN","EASTON","FAIRFIELD","GREENWICH","MONROE","NEW CANAAN","NEW FAIRFIELD","NEWTOWN","NORWALK","REDDING","RIDGEFIELD","SHELTON","SHERMAN","STAMFORD","STRATFORD","TRUMBULL","WESTON","WESTPORT","WILTON"),
  "Hartford",     c("AVON","BERLIN","BLOOMFIELD","BRISTOL","BURLINGTON","CANTON","EAST GRANBY","EAST HARTFORD","EAST WINDSOR","ENFIELD","FARMINGTON","GLASTONBURY","GRANBY","HARTFORD","HARTLAND","MANCHESTER","MARLBOROUGH","NEW BRITAIN","NEWINGTON","PLAINVILLE","ROCKY HILL","SIMSBURY","SOUTHINGTON","SOUTH WINDSOR","SUFFIELD","WEST HARTFORD","WETHERSFIELD","WINDSOR","WINDSOR LOCKS"),
  "Litchfield",   c("BARKHAMSTED","BETHLEHEM","BRIDGEWATER","CANAAN","COLEBROOK","CORNWALL","GOSHEN","HARWINTON","KENT","LITCHFIELD","MORRIS","NEW HARTFORD","NEW MILFORD","NORFOLK","NORTH CANAAN","PLYMOUTH","ROXBURY","SALISBURY","SHARON","THOMASTON","TORRINGTON","WARREN","WASHINGTON","WATERTOWN","WINCHESTER","WINSTED","WOODBURY"),
  "Middlesex",    c("CHESTER","CLINTON","CROMWELL","DEEP RIVER","DURHAM","EAST HADDAM","EAST HAMPTON","ESSEX","HADDAM","KILLINGWORTH","MIDDLEFIELD","MIDDLETOWN","OLD SAYBROOK","PORTLAND","WESTBROOK"),
  "New Haven",    c("ANSONIA","BEACON FALLS","BETHANY","BRANFORD","CHESHIRE","DERBY","EAST HAVEN","GUILFORD","HAMDEN","MADISON","MERIDEN","MIDDLEBURY","MILFORD","NAUGATUCK","NEW HAVEN","NORTH BRANFORD","NORTH HAVEN","ORANGE","OXFORD","PROSPECT","SEYMOUR","SOUTHBURY","WALLINGFORD","WATERBURY","WEST HAVEN","WOLCOTT","WOODBRIDGE"),
  "New London",   c("BOZRAH","COLCHESTER","EAST LYME","FRANKLIN","GRISWOLD","GROTON","LEBANON","LEDYARD","LISBON","LYME","MONTVILLE","NEW LONDON","NORTH STONINGTON","NORWICH","OLD LYME","PRESTON","SALEM","SPRAGUE","STONINGTON","VOLUNTOWN","WATERFORD"),
  "Tolland",      c("ANDOVER","BOLTON","COLUMBIA","COVENTRY","ELLINGTON","HEBRON","MANSFIELD","SOMERS","STAFFORD","TOLLAND","UNION","VERNON","WILLINGTON"),
  "Windham",      c("ASHFORD","BROOKLYN","CANTERBURY","CHAPLIN","EASTFORD","HAMPTON","KILLINGLY","PLAINFIELD","POMFRET","PUTNAM","SCOTLAND","STERLING","THOMPSON","WINDHAM","WOODSTOCK")
) |>
  unnest(towns) |>
  rename(city = towns)

# Step 1: standardize columns + assign each row a county via the crosswalk
ct_with_county <- ct_raw |>
  clean_names() |>
  # Keep only vehicles registered in CT
  filter(primary_customer_state == "CT") |>
  # Standardize city names so they match the uppercase crosswalk
  mutate(city = str_to_upper(str_trim(primary_customer_city))) |>
  # Town -> county lookup (replaces the old 8-line regex case_when)
  left_join(ct_town_county, by = "city")

# Step 2: report any towns we couldn't match — useful for catching typos or new towns
unmatched <- sum(is.na(ct_with_county$county))
cat("CT towns not matched to a county:", unmatched, "out of", nrow(ct_with_county), "\n")
if (unmatched > 0) {
  cat("Unmatched cities:\n")
  print(ct_with_county |> filter(is.na(county)) |> count(city, sort = TRUE) |> head(20))
}

# Step 3: drop unmatched and aggregate individual vehicle records to county totals
ct_clean <- ct_with_county |>
  filter(!is.na(county)) |>
  group_by(county) |>
  summarize(
    ev_registrations = n(),
    bev_count = sum(type == "BEV", na.rm = TRUE),
    phev_count = sum(type == "PHEV", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(state = "CT", evs_on_road = ev_registrations, ev_share = NA_real_) |>
  select(state, county, ev_registrations, evs_on_road, ev_share)

cat("Connecticut:", nrow(ct_clean), "counties loaded\n")



# Washington has individual vehicle records that already include a county column,
# so aggregation is straightforward
wa_raw <- read_csv("Raw Data/EVRegistrationWA.csv")

wa_clean <- wa_raw |>
  clean_names() |>
  filter(state == "WA") |>
  mutate(county = str_trim(county)) |>
  filter(!is.na(county), county != "") |>
  # Aggregate individual records to county totals
  group_by(county) |>
  summarize(
    ev_registrations = n(),
    bev_count = sum(str_detect(electric_vehicle_type, "Battery"), na.rm = TRUE),
    phev_count = sum(str_detect(electric_vehicle_type, "Plug-in"), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(state = "WA", evs_on_road = ev_registrations, ev_share = NA_real_) |>
  select(state, county, ev_registrations, evs_on_road, ev_share)



# Stack all three states into a single tidy county-level frame
ev_by_county <- bind_rows(co_clean, ct_clean, wa_clean)




# PART 2: Clean AFDC charging station data
# Start from the national AFDC file and narrow down to operational electric
# stations in our three states with usable coordinates.

stations_raw <- read_csv("Raw Data/alt_fuel_stations.csv")

stations_clean <- stations_raw |>
  clean_names() |>
  filter(fuel_type_code == "ELEC") |>               # electric stations only
  filter(state %in% c("CO", "CT", "WA")) |>         # study states
  filter(status_code == "E") |>                     # "E" = currently open
  select(station_name, city, state, zip, latitude, longitude,
         ev_level1_evse_num, ev_level2_evse_num, ev_dc_fast_count,
         ev_network, access_code, facility_type, open_date,
         ev_connector_types, ev_pricing) |>
  # Drop stations without coordinates — can't spatial-join them to a county
  filter(!is.na(latitude), !is.na(longitude))

print(count(stations_clean, state))



# PART 3: Assign stations to counties (spatial join)
# AFDC gives lat/lon but no county. We pull census county polygons and
# do a point-in-polygon join to label each station with its county.

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# 2022 county boundaries, reprojected to WGS84 to match the AFDC coordinates.
county_shapes <- counties(state = c("CO", "CT", "WA"), year = 2022, cb = TRUE) |>
  st_transform(4326) |>
  select(STATEFP, COUNTYFP, GEOID, NAME, geometry) |>
  rename(county_name = NAME)

# Convert station table to an sf points object using its lat/lon columns
stations_sf <- stations_clean |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Point-in-polygon join: every station gets the county polygon it falls inside
stations_with_county <- st_join(stations_sf, county_shapes, join = st_within) |>
  st_drop_geometry()

# CT-specific fix: as of 2022 the Census Bureau replaced CT counties with nine
# "Planning Regions". Our EV data still uses the eight historical counties, so
# we map each planning-region polygon back to the historical county name.
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

stations_with_county <- stations_with_county |>
  left_join(ct_station_region_to_county, by = "county_name") |>
  # If we have a remapped name, use it; otherwise keep the original (CO/WA pass through)
  mutate(county_name = if_else(!is.na(county_mapped), county_mapped, county_name)) |>
  select(-county_mapped)



# Aggregate stations to county-level totals: total stations, total ports
# (Level 1 + Level 2 + DC Fast), and a separate count of public-access stations.
stations_by_county <- stations_with_county |>
  filter(!is.na(county_name)) |>
  group_by(state, county_name) |>
  summarize(
    total_stations = n(),
    total_ports = sum(coalesce(ev_level1_evse_num, 0) +
                        coalesce(ev_level2_evse_num, 0) +
                        coalesce(ev_dc_fast_count, 0), na.rm = TRUE),
    level2_ports = sum(coalesce(ev_level2_evse_num, 0), na.rm = TRUE),
    dc_fast_ports = sum(coalesce(ev_dc_fast_count, 0), na.rm = TRUE),
    public_stations = sum(access_code == "public", na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(county = county_name)



# PART 4: Join EV registrations and station counts by county
# Counties without any stations get 0s (not NAs) so downstream metrics work.
# Then derive the core ratios used throughout the analysis.


ev_stations_joined <- ev_by_county |>
  left_join(stations_by_county, by = c("state", "county")) |>
  # Counties with zero stations come through as NA from the join — replace with 0
  mutate(
    total_stations = replace_na(total_stations, 0),
    total_ports = replace_na(total_ports, 0),
    level2_ports = replace_na(level2_ports, 0),
    dc_fast_ports = replace_na(dc_fast_ports, 0),
    public_stations = replace_na(public_stations, 0)
  ) |>
  # Core infrastructure ratios. filter out NA when the denominator is zero 
  mutate(
    evs_per_station = if_else(total_stations > 0, evs_on_road / total_stations, NA_real_),
    evs_per_port = if_else(total_ports > 0, evs_on_road / total_ports, NA_real_),
    stations_per_1000_evs = if_else(evs_on_road > 0, (total_stations / evs_on_road) * 1000, NA_real_)
  )




# PART 5: Clean ACS census data for demographics and income
# Census tables come as wide CSVs
# We reshape them long, then pull the specific rows we need.


# Census exports have headers like "Hartford County, Connecticut!!Estimate".
# This grabs the matching value columns, pivots long, and splits the geography
# string into county_raw + state_name.
reshape_census <- function(df, col_pattern) {
  headers <- names(df)
  est_cols <- headers[str_detect(headers, col_pattern)]
  geo_names <- str_remove(est_cols, "!!.*$") |> str_trim()

  df_sub <- df |> select(label = 1, all_of(est_cols))
  names(df_sub) <- c("label", geo_names)

  df_sub |>
    pivot_longer(cols = -label, names_to = "geography", values_to = "value") |>
    mutate(
      county_raw = str_extract(geography, "^[^,]+"),
      state_name = str_extract(geography, "(?<=, )\\w+$")
    )
}

# DP05: Population + Demographics from ACS
dp05_raw <- read_csv("Raw Data/ACSDP5Y2023.DP05-2026-04-07T174614.csv")

# Pull total population per county
population <- reshape_census(dp05_raw, "!!Estimate$") |>
  filter(str_detect(label, "^\\s+Total population$")) |>
  mutate(total_population = parse_number(as.character(value))) |>
  select(geography, county_raw, state_name, total_population)

# Pull the three demographic variables we need (median age, Hispanic, White alone)
# and pivot wider so each is its own column
demographics <- reshape_census(dp05_raw, "!!Estimate$") |>
  filter(
    str_detect(label, "^\\s+Median age \\(years\\)$") |
    str_detect(label, "^\\s+Hispanic or Latino \\(of any race\\)$") |
    str_detect(label, "^\\s+White alone$")
  ) |>
  mutate(
    variable = case_when(
      str_detect(label, "Median age") ~ "median_age",
      str_detect(label, "Hispanic or Latino \\(of any race\\)") ~ "hispanic_pop",
      str_detect(label, "White alone") ~ "white_non_hispanic_pop"
    )
  ) |>
  mutate(value = parse_number(as.character(value))) |>
  select(geography, county_raw, state_name, variable, value) |>
  pivot_wider(names_from = variable, values_from = value)

# S1901: Median household income 
s1901_raw <- read_csv("Raw Data/ACSST5Y2023.S1901-2026-04-07T174729.csv")

median_income <- reshape_census(s1901_raw, "!!Households!!Estimate$") |>
  filter(str_detect(label, "Median income")) |>
  mutate(median_household_income = parse_number(as.character(value))) |>
  select(geography, county_raw, state_name, median_household_income)

# Stitch population + income + demographics into one census frame and add a
# 2-letter state abbreviation to match our EV/station data
census_combined <- population |>
  left_join(median_income, by = c("geography", "county_raw", "state_name")) |>
  left_join(demographics, by = c("geography", "county_raw", "state_name")) |>
  mutate(state = case_when(
    state_name == "Colorado" ~ "CO",
    state_name == "Connecticut" ~ "CT",
    state_name == "Washington" ~ "WA"
  )) |>
  filter(!is.na(state))

# CT planning region -> historical county crosswalk
# (Same fix as the station section above, but the ACS labels these as
# "X Planning Region" rather than just "X", so we need a separate crosswalk.)
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

# CT: collapse planning regions into historical counties
# Counts (population, race) sum; rates (income, age) are averaged
# This is a rough approximation. Ideally we'd weight by population, but the planning
# regions are similar enough in size that it's close enough for our analysis.
census_ct <- census_combined |>
  filter(state == "CT") |>
  left_join(ct_region_to_county, by = c("county_raw" = "planning_region")) |>
  group_by(state, county) |>
  summarize(
    total_population = sum(total_population, na.rm = TRUE),
    median_household_income = mean(median_household_income, na.rm = TRUE),
    median_age = mean(median_age, na.rm = TRUE),
    hispanic_pop = sum(hispanic_pop, na.rm = TRUE),
    white_non_hispanic_pop = sum(white_non_hispanic_pop, na.rm = TRUE),
    .groups = "drop"
  )

# CO & WA come through unchanged — strip the trailing " County" suffix to match
# the names we used in the EV registration data
census_co_wa <- census_combined |>
  filter(state != "CT") |>
  mutate(county = str_remove(county_raw, " County$") |> str_trim()) |>
  select(state, county, total_population, median_household_income,
         median_age, hispanic_pop, white_non_hispanic_pop)

# Stack CO/WA + the CT-collapsed frame back together
census_clean <- bind_rows(census_co_wa, census_ct) |>
  distinct(state, county, .keep_all = TRUE)




# PART 6: Master county-level dataset
# Join EV registrations, station counts, and census demographics into a single
# row-per-county frame, then compute the population-normalized metrics that
# drive the rest of the analysis (per-capita EV adoption, station density,
# racial/ethnic shares).


ev_final <- ev_stations_joined |>
  left_join(census_clean, by = c("state", "county")) |>
  mutate(
    # Per-capita adoption / density metrics
    evs_per_1000_pop = if_else(total_population > 0,
                                (evs_on_road / total_population) * 1000, NA_real_),
    stations_per_100k_pop = if_else(total_population > 0,
                                     (total_stations / total_population) * 100000, NA_real_),
    # Demographic shares
    pct_hispanic = if_else(total_population > 0,
                            (hispanic_pop / total_population) * 100, NA_real_),
    pct_white_non_hispanic = if_else(total_population > 0,
                                      (white_non_hispanic_pop / total_population) * 100, NA_real_),
    pct_nonwhite = 100 - pct_white_non_hispanic
  )






# Pre-build dashboard objects (added while working on dashboard)
# Anything the dashboard needs but shouldn't recompute on every render lives here:
# the polygon-level map data, the charging-desert pool + threshold, and the headline summary stats for value boxes
# Saving these as RData lets 05-dashboard.qmd just load() them and stay thin.

# CT planning region -> historical county name crosswalk for the polygon data
ct_shape_to_county <- c(
  "Capitol"                        = "Hartford",
  "Greater Bridgeport"             = "Fairfield",
  "Lower Connecticut River Valley" = "Middlesex",
  "Naugatuck Valley"               = "New Haven",
  "Northeastern Connecticut"       = "Windham",
  "Northwest Hills"                = "Litchfield",
  "South Central Connecticut"      = "New Haven",
  "Southeastern Connecticut"       = "New London",
  "Western Connecticut"            = "Fairfield"
)

# Build map-ready polygons: attach state abbreviation, remap CT planning regions
# to historical counties, then join the master ev_final frame on so each polygon
# carries the metrics that will be colored or labeled by on the dashboard.
county_map <- county_shapes |>
  mutate(
    state_abbr = case_when(
      STATEFP == "08" ~ "CO",
      STATEFP == "09" ~ "CT",
      STATEFP == "53" ~ "WA"
    ),
    county_clean = if_else(
      state_abbr == "CT" & county_name %in% names(ct_shape_to_county),
      ct_shape_to_county[county_name],
      county_name
    )
  ) |>
  left_join(ev_final, by = c("state_abbr" = "state", "county_clean" = "county"))

# Charging desert restriction: counties with >=50 EVs and >=1 station so the
# the top quartile of the charging desert pool is flagged

desert_pool <- ev_final |>
  filter(evs_on_road >= 50,
         total_stations > 0,
         !is.na(evs_per_station))

desert_threshold <- quantile(desert_pool$evs_per_station, 0.75, na.rm = TRUE)

desert_pool <- desert_pool |>
  mutate(is_desert = evs_per_station >= desert_threshold)

# Per-state summary table for the dashboard's "Summary by State" panel.
# Income and % nonwhite are population-weighted so a tiny rural county doesn't
# get the same say as a big metro one.
state_labels_full <- c("CO" = "Colorado", "CT" = "Connecticut", "WA" = "Washington")

state_summary <- ev_final |>
  group_by(state) |>
  summarize(
    Counties        = n(),
    total_evs       = sum(evs_on_road,         na.rm = TRUE),
    total_stations  = sum(total_stations,      na.rm = TRUE),
    total_pop       = sum(total_population,    na.rm = TRUE),
    wt_income       = weighted.mean(median_household_income,
                                    total_population, na.rm = TRUE),
    wt_nonwhite     = weighted.mean(pct_nonwhite,
                                    total_population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    State              = recode(state, !!!state_labels_full),
    `EVs / Station`    = round(total_evs / total_stations, 1),
    `Total EVs`        = format(total_evs,      big.mark = ","),
    `Total Stations`   = format(total_stations, big.mark = ","),
    `Total Population` = format(total_pop,      big.mark = ","),
    `Median Income`    = paste0("$", format(round(wt_income), big.mark = ",")),
    `% Nonwhite`       = paste0(round(wt_nonwhite, 1), "%")
  ) |>
  select(State, Counties, `Total EVs`, `Total Stations`, `EVs / Station`,
         `Total Population`, `Median Income`, `% Nonwhite`)

# One list of summary stats the dashboard's value boxes + summary table read from.
# Keeping all of them in one named list means 05-dashboard.qmd only has one
# object to load and reference for headline numbers.
dashboard_summary <- list(
  n_counties_total    = nrow(ev_final),
  total_evs           = sum(ev_final$evs_on_road,    na.rm = TRUE),
  total_stations_n    = sum(ev_final$total_stations, na.rm = TRUE),
  avg_evs_per_station = round(
    sum(ev_final$evs_on_road,    na.rm = TRUE) /
      sum(ev_final$total_stations, na.rm = TRUE),
    1
  ),
  desert_threshold    = unname(desert_threshold),
  n_deserts           = sum(desert_pool$is_desert),
  n_qualifying        = nrow(desert_pool),
  desert_state_counts = desert_pool |> filter(is_desert) |> count(state),
  state_summary       = state_summary
)


# PART 7: Save outputs

dir.create("clean_data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

save(ev_final, file = "clean_data/ev_final.RData")
write_csv(ev_final, "clean_data/ev_final.csv")
save(stations_with_county, file = "clean_data/stations_with_county.RData")
save(county_shapes, file = "clean_data/county_shapes.RData")
save(county_map, file = "clean_data/county_map.RData")
save(desert_pool, file = "clean_data/desert_pool.RData")
save(dashboard_summary, file = "clean_data/dashboard_summary.RData")


