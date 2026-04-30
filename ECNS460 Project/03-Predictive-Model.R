# ------------------------------------
# 03_Predictive_Model.R
# ECNS 460 Term Project - EV Charging Infrastructure Gaps
# Sawyer Bringle & Liam Maumenee
#---------------------------------

# Load necessary packages
library(tidyverse)
library(lubridate)
library(tidymodels)

# Load Cleaned data
load("clean_data/stations_with_county.RData")
load("clean_data/ev_final.RData")

# Parse open_date and drop stations we can't date
stations_dated = stations_with_county |>
  filter(!is.na(county_name)) |>
  mutate(open_year = year(ymd(open_date))) |>
  filter(!is.na(open_year))

# Create data set with opening date data
station_features = stations_dated |>
  group_by(state, county = county_name) |>
  summarize(
    first_station_year = min(open_year),
    median_station_year = median(open_year),
    stations_by_2018 = sum(open_year <= 2018),
    stations_by_2020 = sum(open_year <= 2020),
    stations_by_2022 = sum(open_year <= 2022),
    stations_by_2024 = sum(open_year <= 2024),
    stations_total_dated = n(),
    pct_stations_post_2022 = mean(open_year >= 2023),
    new_stations_2023_2025 = sum(open_year >= 2023 & open_year <= 2025),
    .groups = "drop"
  ) |>
  mutate(
    # Relative growth in station count between two snapshots.
    station_growth_2020_2024 = if_else(
      stations_by_2020 > 0,
      (stations_by_2024 - stations_by_2020) / stations_by_2020,
      NA_real_
    ),
    early_adopter_county = first_station_year <= 2018
  )

# Joint newly created data set with original ev_final dataset
ev_final_with_history = ev_final |>
  left_join(station_features, by = c("state", "county")) |>
  mutate(
    across(
      c(stations_by_2018, stations_by_2020, stations_by_2022, stations_by_2024,
        stations_total_dated, new_stations_2023_2025),
      ~ replace_na(.x, 0L)
    ),
    early_adopter_county = replace_na(early_adopter_county, FALSE)
  )

# With new data set now create predictive model
# -----------------------------------------

# Prep model data
# Restrict counties to those that had at least one station in by 2020
# Drop any counties missing demographic variables
model_data = ev_final_with_history |>
  filter(
    stations_by_2020 >= 1,
    !is.na(median_household_income),
    !is.na(pct_nonwhite),
    !is.na(median_age),
    !is.na(total_population),
    !is.na(station_growth_2020_2024)
  ) |>
  mutate(log_population = log(total_population)) |>
  select(
    state, county,
    # outcome
    growth = station_growth_2020_2024,
    # predictors
    median_household_income, log_population, pct_nonwhite, median_age,
    evs_per_1000_pop, stations_by_2020, stations_per_100k_pop,
    early_adopter_county
  )
summary(model_data$growth)

# Train/test split
set.seed(123)
data_split = initial_split(model_data, prop = 0.75, strata = growth)
train_data = training(data_split)
test_data = testing(data_split)

# Create recipie
ev_recipe = recipe(growth ~ ., data = train_data) |>
  update_role(state, county, new_role = "id") |>
  step_mutate(early_adopter_county = as.integer(early_adopter_county)) |>
  step_normalize(all_numeric_predictors())

# 5 Fold CV
set.seed(456)
cv_folds = vfold_cv(train_data, v = 5, strata = growth)

# Create Lasso Regression
lasso_spec = linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

# Set Workflow
lasso_wf = workflow() |>
  add_recipe(ev_recipe) |>
  add_model(lasso_spec)

# Set Up Grid
lasso_grid = grid_regular(penalty(), levels = 30)

# Tune grid
lasso_tuned = tune_grid(
  lasso_wf,
  resamples = cv_folds,
  grid = lasso_grid,
  metrics = metric_set(rmse)
)


show_best(lasso_tuned, metric = "rmse")

# Plot CV results
autoplot(lasso_tuned, metric = "rmse")

# Select best model and finalize workflow
best_lasso = select_best(lasso_tuned, metric = "rmse")
final_lasso = finalize_workflow(lasso_wf, best_lasso)

# Evaluate on test set
lasso_fit = last_fit(final_lasso, split = data_split)
collect_metrics(lasso_fit)

# Look At coefficients
coefs = lasso_fit |> 
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0)

print(coefs)

# save block to load to dashboard
# score counties based on model 
final_lasso_fit <- fit(final_lasso, data = model_data)
scored <- predict(final_lasso_fit, new_data = model_data) |>
  bind_cols(model_data) |>
  rename(predicted_growth = .pred)

# pre-extract the final_lasso_fit values so the dashboard doesn't need to load tidymodels
predictive_model <- list(
  scored       = scored,
  coefs        = coefs |> arrange(desc(abs(estimate))),  # sorted for the dashboard
  test_metrics = collect_metrics(lasso_fit),
  test_preds   = collect_predictions(lasso_fit)
)
save(predictive_model, file = "clean_data/predictive_model.RData")


glimpse(predictive_model
        )



show_best(lasso_tuned, metric = "rmse", n = 3)
print(coefs)





























