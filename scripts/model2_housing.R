# FM housing ML models to predict sold prices: data cleaning and model development 

library(tidyverse)
library(janitor)
library(tidymodels)
library(kknn)
library(vetiver)
library(pins)

FM_housing <- read.csv("data/FM_Housing_2018_2022_clean.csv") |> 
  clean_names()

glimpse(FM_housing)

FM_housing |>
  filter(year_built >= 1800, year_built <= 2022) |>
  count(year_built) |>
  arrange(year_built) |>
  head(20)

#data pre processing 

house_model <-FM_housing |> 
  # Replace implausible construction years with NA.
  # EDA identified values <1800 and >2022 as invalid.
  mutate(
    year_built = if_else(
      year_built < 1800 | year_built > 2022,
      NA_integer_,
      year_built
    ),
    
    garage_stalls = if_else(
      garage_stalls == 21,
      NA_integer_,
      garage_stalls
    ) )|> 
  select(
  sold_price,
  total_sq_ft,
  year_built,
  total_bedrooms,
  total_bathrooms,
  garage_stalls,
  lot_size_sq_ft,
  city,
  high_school,
  style
)

# Split data into 80% training and 20% testing

housing_split <- initial_split(
  house_model,
  prop = 0.80,
  strata = sold_price
)

train_set <- training(housing_split)
test_set <- testing(housing_split)

dim(train_set)
dim(test_set)

# looking at missing values in train set
train_set |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing"
  ) |>
  mutate(
    percent_missing = missing / nrow(train_set) * 100
  ) |>
  arrange(desc(missing))

# preprocessing data with recipe for lm

housing_recipe <- recipe(
  sold_price ~ .,
  data = train_set
) |>
  step_impute_median(all_numeric_predictors())|>
  step_dummy(all_nominal_predictors())

# preprocessing for knn
knn_recipe <- recipe(
  sold_price ~ .,
  data = train_set
) |>
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())|>
  step_dummy(all_nominal_predictors()) 

#fitting regression 

# Multiple linear regression model

lm_model <- linear_reg() |>
  set_engine("lm")


# Combine preprocessing and model

lm_workflow <- workflow() |>
  add_recipe(housing_recipe) |>
  add_model(lm_model)


# Fit model using training data

lm_fit <- lm_workflow |>
  fit(data = train_set)

lm_coefficients <- lm_fit |>
  extract_fit_parsnip() |>
  tidy()

lm_coefficients

# Generate predictions on the test set

lm_results <- test_set |>
  select(sold_price) |>
  bind_cols(
    predict(lm_fit, new_data = test_set)
  )
head(lm_results)

# calculate RMSE, MAE,R^2
lm_metrics <- lm_results |>
  metrics(
    truth = sold_price,
    estimate = .pred
  )

lm_metrics

ggplot(lm_results, aes(
  x = sold_price,
  y = .pred
)) +
  geom_point(alpha = 0.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs. Predicted Sold Prices",
    x = "Actual Sold Price ($)",
    y = "Predicted Sold Price ($)"
  ) +
  theme_minimal()

# KNN regression model
#tuning 
knn_model <- nearest_neighbor(
  neighbors = tune()
) |>
  set_engine("kknn") |>
  set_mode("regression")

knn_model

#cross validation
set.seed(456)

knn_folds <- vfold_cv(
  train_set,
  v = 5,
  strata = sold_price
)

knn_folds

# KNN workflow

knn_workflow <- workflow() |>
  add_recipe(knn_recipe) |>
  add_model(knn_model)

knn_grid <- tibble(
  neighbors = c(3, 5, 7, 10, 15, 20, 30, 40, 50)
)

knn_tune <- knn_workflow |>
  tune_grid(
    resamples = knn_folds,
    grid = knn_grid,
    metrics = metric_set(rmse, mae, rsq)
  )
knn_tune |>
  collect_metrics()

knn_results <- knn_tune |>
  collect_metrics()

knn_results

knn_results |>
  filter(.metric == "rmse") |>
  arrange(mean)

knn_results |>
  filter(.metric == "mae") |>
  arrange(mean)

knn_results |>
  filter(.metric == "rsq") |>
  arrange(desc(mean))

#best knn model

best_knn <- select_best(
  knn_tune,
  metric = "rmse"
)

best_knn

final_knn_workflow <- knn_workflow |>
  finalize_workflow(best_knn)

final_knn_fit <- final_knn_workflow |>
  fit(data = train_set)

knn_results <- test_set |>
  select(sold_price) |>
  bind_cols(
    predict(final_knn_fit, new_data = test_set)
  )

knn_metrics <- knn_results |>
  metrics(
    truth = sold_price,
    estimate = .pred
  )

knn_metrics

ggplot(knn_results, aes(
  x = sold_price,
  y = .pred
)) +
  geom_point(alpha = 0.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "KNN: Actual vs. Predicted Sold Prices",
    subtitle = "K = 10",
    x = "Actual Sold Price ($)",
    y = "Predicted Sold Price ($)"
  ) +
  theme_minimal()

#knn is not the best at predicting the houses at higher prices likely due to the small number of 
#houses over million or more considering we are using k=10 neighbors. But it is very good about 80% 
#predicting houses below 500k

# Prepare model for deployment --------------------

v_knn <- vetiver_model(
  final_knn_fit,
  model_name = "fm_housing_knn"
)

v_knn

# Save model to local board ------------------------

model_board <- board_folder(
  path = "models",
  versioned = TRUE
)

vetiver_pin_write(
  model_board,
  v_knn
)

