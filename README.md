# Stage 1: Project Plan

**Sawyer Bringle & Liam Maumenee** **ECNS 460 — Spring 2026**

------------------------------------------------------------------------

## Topic

**EV Charging Infrastructure Gaps Across the United States**

Electric vehicle adoption has grown rapidly across the United States, particularly following the passage of federal incentives like the Inflation Reduction Act. Yet this growth has raised concerns about the adequacy of charging infrastructure. The distribution of EV charging stations varies widely: some regions have dense networks, while others remain significantly underserved. Understanding where these gaps exist, and what demographic or geographic factors predict them, is critical for policymakers and private companies deciding where to invest in new infrastructure.

## Product Plan

We plan to build an interactive Shiny web application featuring a Leaflet-based map that allows users to explore EV charging station density relative to EV adoption at the county level across the United States. The map will highlight "charging deserts" — areas where EV registrations are growing but public charging infrastructure lags behind. Users will be able to select states or regions, view charger-to-EV ratios by county, and identify underserved areas.

**Goal:** Build an interactive map that identifies gaps between EV adoption and access to charging stations, enabling users to quickly spot counties where infrastructure investment is most needed.

**Audience:** Policymakers, EV charging companies, state transportation planners, and the general public.

## Challenge

Our challenge is to build the Shiny web application with Leaflet interactive mapping. Neither Shiny nor Leaflet have been covered in this course, and both team members will be learning these tools for the first time. This challenge is integrated directly into our product — the Shiny app is both our deliverable and our demonstration of a new tool.

## Datasets

### Dataset 1: Alternative Fuel Station Locations

-   **Source:** U.S. Department of Energy, Alternative Fuels Data Center (AFDC)
-   **URL:** <https://afdc.energy.gov/data_download>
-   **Description:** This dataset contains all electric vehicle charging stations in the United States. Each observation represents a single station.
-   **Key variables:** Station name, street address, city, state, ZIP code, latitude/longitude coordinates, fuel type code, EV connector types, EV network, access code (public/private), and open date.
-   **Spatial coverage:** All 50 U.S. states and Washington, D.C.
-   **Timespan:** Current snapshot of all active stations; historical records also available.

### Dataset 2: Electric Vehicle Registration Counts by State

-   **Source:** U.S. Department of Energy, Alternative Fuels Data Center (AFDC)
-   **URL:** <https://afdc.energy.gov/vehicle-registration>
-   **Description:** This dataset contains light-duty vehicle registration counts by state, derived by the National Laboratory of the Rockies with data from Experian Information Solutions.
-   **Key variables:** State, battery electric vehicle (BEV) count, plug-in hybrid electric vehicle (PHEV) count, hybrid electric vehicle (HEV) count, and total vehicle registrations.
-   **Spatial coverage:** All 50 U.S. states and Washington, D.C.
-   **Timespan:** Annual counts available for multiple years through 2023.
-   **Note:** This dataset provides state-level counts. We have reached out to Atlas EV Hub to request county-level registration data. If county-level data is unavailable, we plan to use state-level registrations combined with county-level demographics as a proxy for EV adoption patterns.

### Dataset 3: County-Level Demographics (American Community Survey)

-   **Source:** U.S. Census Bureau, American Community Survey (ACS) 5-Year Estimates
-   **URL:** <https://data.census.gov>
-   **Description:** This dataset provides county-level demographic and socioeconomic data from the ACS.
-   **Key variables:** Total population, median household income, population density, urbanization level, and other demographic indicators.
-   **Spatial coverage:** All U.S. counties.
-   **Timespan:** Most recent 5-year estimates (2019–2023).

## How the Datasets Relate

All three datasets can be linked geographically. We will aggregate the station-level data (Dataset 1) to the county level using latitude/longitude coordinates and join it with county-level demographics (Dataset 3) by county FIPS code. The state-level EV registration data (Dataset 2) will be joined by state. If we obtain county-level registration data from Atlas EV Hub, all three datasets will join at the county level via FIPS code, enabling a more granular analysis of the relationship between charging infrastructure, EV adoption, and local demographics.
