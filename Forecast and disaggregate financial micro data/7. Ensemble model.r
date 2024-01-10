install.packages("modeltime.ensemble")

# Time Series Modeling and Machine Learning
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Time Series and Data Wrangling
library(timetk)
library(tidyverse)

store_1_1_tbl <- walmart_sales_weekly %>%
  filter(id == "1_1") %>%
  select(Date, Weekly_Sales)

store_1_1_tbl
store_1_1_tbl %>%
  plot_time_series(Date, Weekly_Sales, .smooth_period = "3 months", .interactive = FALSE)

store_1_1_tbl %>%
  plot_seasonal_diagnostics(
    Date, Weekly_Sales,
    .feature_set = c("week", "month.lbl"),
    .interactive = FALSE
  )

splits <- store_1_1_tbl %>%
  time_series_split(assess = "12 weeks", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Weekly_Sales, .interactive = FALSE)

recipe_spec <- recipe(Weekly_Sales ~ Date, store_1_1_tbl) %>%
  step_timeseries_signature(Date) %>%
  step_rm(matches("(iso$)|(xts$)|(day)|(hour)|(min)|(sec)|(am.pm)")) %>%
  step_mutate(Date_week = factor(Date_week, ordered = TRUE)) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(contains("index.num"), Date_year)

recipe_spec %>% prep() %>% juice()

model_fit_arima <- arima_reg(seasonal_period = 52) %>%
  set_engine("auto_arima") %>%
  fit(Weekly_Sales ~ Date, training(splits))

model_fit_arima

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

wflw_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(splits))

model_spec_xgboost <- boost_tree() %>%
  set_engine("xgboost")

set.seed(123)
wflw_fit_xgboost <- workflow() %>%
  add_model(model_spec_xgboost) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(training(splits))

model_spec_nnetar <- nnetar_reg(
  seasonal_period = 52,
  non_seasonal_ar = 4,
  seasonal_ar     = 1
) %>%
  set_engine("nnetar")

set.seed(123)
wflw_fit_nnetar <- workflow() %>%
  add_model(model_spec_nnetar) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

model_spec_prophet <- prophet_reg(
  seasonality_yearly = TRUE
) %>%
  set_engine("prophet") 

wflw_fit_prophet <- workflow() %>%
  add_model(model_spec_prophet) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

submodels_tbl <- modeltime_table(
  model_fit_arima,
  wflw_fit_glmnet,
  wflw_fit_xgboost,
  wflw_fit_nnetar,
  wflw_fit_prophet
)

submodels_tbl

submodels_tbl %>% 
  modeltime_accuracy(testing(splits)) %>%
  table_modeltime_accuracy(.interactive = FALSE)

submodels_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = store_1_1_tbl
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)

# Simple Average Ensemble
ensemble_fit_avg <- submodels_tbl %>%
  ensemble_average(type = "mean")

ensemble_fit_avg

# Simple Median Ensemble
ensemble_fit_med <- submodels_tbl %>%
  ensemble_average("median")

# Higher Loading on Better Models (Test RMSE)
ensemble_fit_wt <- submodels_tbl %>%
  ensemble_weighted(loadings = c(2, 4, 6, 1, 6))

ensemble_models_tbl <- modeltime_table(
  ensemble_fit_avg,
  ensemble_fit_med,
  ensemble_fit_wt
)

ensemble_models_tbl

ensemble_models_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  table_modeltime_accuracy(.interactive = FALSE)

ensemble_models_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = store_1_1_tbl
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)


###########################################################
# 1. 𝐴."Nerezidentų įsiskolinimai už prekes ir paslaugas"
###########################################################







###########################################################
# 2. 𝐴."Įmonės piniginės lėšos užsienio bankuose"
###########################################################







###########################################################
# 𝐵."Įsiskolinimai nerezidentams už prekes ir paslaugas"
###########################################################