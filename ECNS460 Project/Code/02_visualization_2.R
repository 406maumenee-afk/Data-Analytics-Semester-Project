# -------------------------------------------
# 02_visualization.R
# ECNS 460 Term Project
# Sawyer Bringle & Liam Maumenee
#
# This script produces figures exploring EV charging infrastructure gaps
# across Colorado, Connecticut, and Washington.
# -------------------------------------------

library(tidyverse)
library(scales)
library(sf)
library(tidytext)
library(ggrepel)

# Load Cleaned Data
ev_county = read.csv("clean_data/ev_final.csv")
load("clean_data/stations_with_county.RData")
load("clean_data/county_shapes.RData")
stations = read_csv("Stage 1/Raw Data/alt_fuel_stations (Mar 11 2026) (1).csv")

# Helper to create a consistent color theme
theme_ev = theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(color = "gray50", size = 8),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

state_colors = c("CO" = "#E69F00", "CT" = "#0072B2", "WA" = "#009E73")
state_labels = c("CO" = "Colorado", "CT" = "Connecticut", "WA" = "Washington")


# FIGURE 1: Top 10 Counties by EVs registrations faceted by state
# --------------------------------
# Purpose: Identify which counties EVs are located in

# Extract necessary data for plot
fig1_data = ev_county |>
  group_by(state) |>
  slice_max(evs_on_road, n = 10) |>
  ungroup() |>
  mutate(
    county_label = paste0(county, ", ", state),
    county_label = reorder_within(county_label, evs_on_road, state)
  )

# Create plot
fig1 = ggplot(fig1_data, aes(x = evs_on_road, y = county_label, fill = state)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(evs_on_road)), hjust = -0.1, size = 3) +
  facet_wrap(~ state, scales = "free", labeller = labeller(state = state_labels)) +
  scale_y_reordered() +
  scale_x_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = state_colors) +
  labs(
    title = "Figure 1: Top 10 Counties by EV Registrations",
    x = "EVs on the Road",
    y = NULL
  ) +
  theme_ev +
  theme(panel.grid.major.y = element_blank())

fig1

ggsave("output/figures/fig1_top_counties_ev.png", fig1,
       width = 14, height = 7, dpi = 300, bg = "white")


# FIGURE 2: Median Household Income vs. EV Adoption Rate
# -------------------------
# Purpose: Explore whether wealthier counties have higher EV adoption.

fig2 = ev_county |>
  filter(!is.na(median_household_income), !is.na(evs_per_1000_pop)) |>
  ggplot(aes(x = median_household_income, y = evs_per_1000_pop,
             color = state, size = total_population)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1),
              color = "gray30", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = comma_format(accuracy = 0.1)) +
  scale_color_manual(values = state_colors, labels = state_labels) +
  scale_size_continuous(range = c(1, 10), labels = comma_format(),
                        name = "Population") +
  labs(
    title = "Figure 2: Median Household Income vs. EV Adoption Rate",
    x = "Median Household Income",
    y = "EVs per 1,000 Residents",
    color = "State"
  ) +
  theme_ev

fig2

ggsave("output/figures/fig2_income_vs_ev_adoption.png", fig2,
       width = 10, height = 7, dpi = 300, bg = "white")



# FIGURE 3: Charging Stations vs. EVs
# ---------------------------------
# Purpose: View the relationship between charging stations and EVs on the
# road to help identify charging deserts

fig3 = ev_county |>
  filter(total_stations > 0, evs_on_road > 0) |>
  ggplot(aes(x = log(total_stations), y = log(evs_on_road), color = state)) +
  geom_point(aes(size = total_population), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1),
              color = "gray30", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = state_colors, labels = state_labels) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(
    title = "Figure 3: Charging Stations vs. EVs on the Road",
    x = "Number of Charging Stations (log Scale)",
    y = "EVs on the Road (Log Scale)",
    color = "State"
  ) +
  theme_ev

fig3

ggsave("output/figures/fig3_stations_vs_evs.png", fig3,
       width = 10, height = 7, dpi = 300, bg = "white")


# FIGURE 4: Maps of charging stations in each state
# ----------------------------------
# Purpose: Build a spatial visualization of each state and overlay
# pct nonwhite population as another layer to help identify which variables
# can be used for a predictive model.

# Clean prepare and combine datasets to create graphs
# -------------------------
# Filter out stations to only have open operational ev charging stations in 
# Co, CT, and WA
stations_filtered = stations |>
  filter(
    State %in% c("CO", "CT", "WA"),
    `Status Code` == "E",          # E = open/operational only
    !is.na(Latitude),
    !is.na(Longitude)
  )

stations_sf = st_as_sf(
  stations_filtered,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Filter shapefile to necessary states
# STATEFP codes: "08" = Colorado, "09" = Connecticut, "53" = Washington

ct_region_to_county = c(
  "Greater Bridgeport"            = "Fairfield",
  "Naugatuck Valley"              = "New Haven",
  "South Central Connecticut"     = "New Haven",
  "Lower Connecticut River Valley"= "Middlesex",
  "Capitol"                       = "Hartford",
  "Northwest Hills"               = "Litchfield",
  "Northeastern Connecticut"      = "Windham",
  "Western Connecticut"           = "Fairfield",
  "Southeastern Connecticut"      = "New London"
)

counties_3states = county_shapes |>
  filter(STATEFP %in% c("08", "09", "53")) |>
  mutate(
    state = case_when(
      STATEFP == "08" ~ "CO",
      STATEFP == "09" ~ "CT",
      STATEFP == "53" ~ "WA"
    ),
    county_name_clean = str_remove(county_name, " County$"),
    county_name_clean = if_else(
      state == "CT" & county_name_clean %in% names(ct_region_to_county),
      ct_region_to_county[county_name_clean],
      county_name_clean
    ),
    # force plain character — prevents vctrs join mismatch
    state             = as.character(state),
    county_name_clean = as.character(county_name_clean)
  )

stations_sf = st_transform(stations_sf, st_crs(counties_3states))


# Join EV county data to shapefile data

counties_joined = counties_3states |>
  left_join(
    ev_county,
    by = c("state" = "state", "county_name_clean" = "county")
  )

# make sure everything is in the same CRS
counties_joined = st_transform(counties_joined, crs = 4326)
stations_sf = st_transform(stations_sf,     crs = 4326)

# split data by state
counties_co = counties_joined |> filter(STATEFP == "08")
counties_ct = counties_joined |> filter(STATEFP == "09")
counties_wa = counties_joined |> filter(STATEFP == "53")

stations_co = stations_sf |> filter(State == "CO")
stations_ct = stations_sf |> filter(State == "CT")
stations_wa = stations_sf |> filter(State == "WA")

# shared fill scale limits so all 3 maps are comparable
fill_max = max(counties_joined$pct_nonwhite)
fill_min = min(counties_joined$pct_nonwhite)


# Create the plots
# ---------------------
# Colorado
p_co = ggplot() +
  geom_sf(
    data = counties_co,
    aes(fill = pct_nonwhite),
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = stations_co,
    color = "#F7E6CA",
    size = 0.4,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Percent Pop.\nNonWhite (%)",
    limits = c(fill_min, fill_max),
    na.value = "grey85"
  ) +
  labs(
    title = "Figure 4.1: Percent Nonwhite Population vs. Charging Access",
    subtitle = "Colorado"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey60"),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm")
  )
p_co

# Connecticut
p_ct = ggplot() +
  geom_sf(
    data = counties_ct,
    aes(fill = pct_nonwhite),
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = stations_ct,
    color = "#F7E6CA",
    size = 0.4,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Percent Pop.\nNonwhite (%)",
    limits = c(fill_min, fill_max),
    na.value = "grey85"
  ) +
  labs(
    title = "Figure 4.2: Percent Nonwhite Population vs. Charging Access",
    subtitle = "Connecticut"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey60"),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm")
  )
p_ct

# Washington 
p_wa = ggplot() +
  geom_sf(
    data = counties_wa,
    aes(fill = pct_nonwhite),
    color = "white",
    linewidth = 0.3
  ) +
  geom_sf(
    data = stations_wa,
    color = "#F7E6CA",
    size = 0.4,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Percent Pop.\nNonwhite (%)",
    limits = c(fill_min, fill_max),
    na.value = "grey85"
  ) +
  labs(
    title    = "Figure 4.3: Percent Nonwhite Population vs. Charging Access",
    subtitle = "Washington"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 8, color = "grey60"),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm")
  )
p_wa

# Save each graph seperately
ggsave("output/figures/fig4.1_map_colorado.png",     p_co, width = 10, height = 7, dpi = 300)
ggsave("output/figures/fig4.2_map_connecticut.png",  p_ct, width = 8,  height = 6, dpi = 300)
ggsave("output/figures/fig4.3_map_washington.png",   p_wa, width = 10, height = 7, dpi = 300)


# FIGURE 5: Top 15 Charging Desert Counties
# -------------------------------------------
# Purpose: Identify which counties are charging deserts

fig5 = ev_county |>
  filter(evs_on_road >= 50, total_stations > 0) |>
  slice_max(evs_per_station, n = 15) |>
  mutate(
    county_label = paste0(county, ", ", state),
    county_label = fct_reorder(county_label, evs_per_station)
  ) |>
  ggplot(aes(x = evs_per_station, y = county_label, fill = state)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(round(evs_per_station))),
            hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = state_colors, labels = state_labels) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Figure 5: Top 15 Charging Desert Counties",
    subtitle = "Number of EVs per Charging Station by County",
    x = "EVs per Charging Station",
    y = NULL,
    fill = "State",
  ) +
  theme_ev +
  theme(panel.grid.major.y = element_blank())

fig5

ggsave("output/figures/fig5_charging_deserts.png", fig5,
       width = 10, height = 7, dpi = 300, bg = "white")













