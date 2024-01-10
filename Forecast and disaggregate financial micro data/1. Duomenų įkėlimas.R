library(x12)
library(readxl)
library(openxlsx)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(modeltime, modeltime.ensemble, modeltime.gluonts, ggthemes, tidymodels, tidyverse, 
               timetk, eurostat, lubridate, DataExplorer, tsibble, earth)


Ketvirtiniai <- openxlsx::read.xlsx("Duomenys/Laiko eilutės_2022-06-02_v1.xlsx", sheet = 1, colNames = TRUE, startRow = 2)
Menesiniai <- openxlsx::read.xlsx("Duomenys/Laiko eilutės_2022-06-02_v1.xlsx", sheet = 2, colNames = TRUE, startRow = 2)

library(DataExplorer)
DataExplorer::introduce(Ketvirtiniai)
DataExplorer::introduce(Menesiniai)

interactive = FALSE

##################
# Eurostat data
##################

NAMQ_10_GDP <- get_eurostat("namq_10_gdp")

NAMQ_10_GDP <- NAMQ_10_GDP %>% filter(
  unit=="CLV10_MNAC",
  s_adj=="SCA",
  na_item %in% c("B1GQ","B1G","P5G","p51G","P6","P61","P62","P7","P71","P72","D1","D11","D12","B2A3G"),
  geo %in% c("EE", "LV","LT")
)

list_of_items <- c("B1GQ","B1G","P5G","p51G","P6","P61","P62","P7","P71","P72","D1","D11","D12","B2A3G")

esNAMQ_10_GDP <- spread(NAMQ_10_GDP, geo, values)
names(esNAMQ_10_GDP) <- c("unit", "s_adj", "id", "date", "EE", "LT", "LV")

lt_data <- dplyr::select(esNAMQ_10_GDP, c("id", "date", "LT")) %>% 
  filter(id == "B1GQ") # Lithuanian GDP data (B1GQ)

###################
# /Eurostat data
###################
quarter <- tsibble::yearquarter("2015 Q1")
xx<-seq(quarter, length.out = 28, by = 1)

ISIP_isiskolinimai_k <- Ketvirtiniai |> dplyr::select(ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, MQ) |>
  dplyr::mutate(id = "ISIP_isiskolinimai", 
                date = seq(as.Date("2015-04-01"), as.Date("2022-01-01"), by = "1 quarter")) |>
  dplyr::rename(value = "ISIP..Įsiskolinimai.už.prekes.ir.paslaugas") |>
  dplyr::select(-c(MQ)) |> dplyr::relocate(id, date, value)

# Adjust data to only positive values (increase the whole vector by the minimum value in the time series data)
minimum_value <- min(ISIP_isiskolinimai_k$value)
ISIP_isiskolinimai_k$value <- ISIP_isiskolinimai_k$value - min(ISIP_isiskolinimai_k$value) + 1


#ISIP_isiskolinimai_m <- 
#UT_isiskolinimai_k <-
#UT_isiskolinimai_m <-
#UT_pinigai_k <-
#UT_pinigai_m <- 

# Split Data 80/20
splits <- initial_time_split(ISIP_isiskolinimai_k, prop = 0.9)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))

summary(model_fit_arima_no_boost)

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))

summary(model_fit_arima_boosted)

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))

model_fit_ets

# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

model_fit_prophet

# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

model_fit_lm

# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# Compile models into a Modeltime table
models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# Let's see how our models would have predicted the last year's Lithuanian economy
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    #actual_data = lt_data
    actual_data = ISIP_isiskolinimai_k
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  ) + ggthemes::theme_clean() + ggtitle("Įsipareigojimai, įsiskolinimai užsienio bendrovėms. Modelių palyginimas su realiais duomenimis", 
                                        paste("Lietuvos banko duomenys. Duomenys pakoreguoti, kad išvengtume neigiamų verčių: 0 yra ties", minimum_value))

# Let's see which models fit the data best
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )
# From the table we can see that 


# Let's refit out models to the full actual data
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = ISIP_isiskolinimai_k)

# Let's see what the models predict for the Lithuanian GDP looking 3 years into the future
refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = ISIP_isiskolinimai_k) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

###
# Let's make a model ensemble to forecast the dynamic of the Lithuanian economy

#splits <- time_series_split(lt_data, assess = "1 year", cumulative = TRUE)
splits <- time_series_split(ISIP_isiskolinimai_k, assess = "1 year", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = interactive)

# Let's create a feature engineering recipe that we'll use later on for meta-learning with Elastic Net
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(matches("(.iso$)|(.xts$)")) %>%
  step_normalize(matches("(index.num$)|(_year$)")) %>%
  step_dummy(all_nominal()) %>%
  step_fourier(date, K = 1, period = 12)

recipe_spec %>% prep() %>% juice()

# Train auto-arima
model_spec_arima <- arima_reg() %>%
  set_engine("auto_arima")

wflw_fit_arima <- workflow() %>%
  add_model(model_spec_arima) %>%
  add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
  fit(training(splits))

# Train the Facebook's Prophet model
model_spec_prophet <- prophet_reg() %>%
  set_engine("prophet")

wflw_fit_prophet <- workflow() %>%
  add_model(model_spec_prophet) %>%
  add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
  fit(training(splits))

# Train the Elastic Net Model
model_spec_glmnet <- linear_reg(
  mixture = 0.9,
  penalty = 4.36e-6
) %>%
  set_engine("glmnet")

wflw_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))


# Let's once again create a Modeltime table to put our models in
#lt_data_models <- modeltime_table(
#  wflw_fit_arima,
#  wflw_fit_prophet,
#  wflw_fit_glmnet
#)

ISIP_data_models <- modeltime_table(
  wflw_fit_arima,
  wflw_fit_prophet,
  wflw_fit_glmnet
)

#lt_data_models
ISIP_data_models

# Let's create an esemble of models
#ensemble_fit <- lt_data_models %>%
#  ensemble_average(type = "mean") # use the average
#ensemble_fit <- lt_data_models %>%
#  ensemble_average(type = "median") # use the median

ensemble_fit <- ISIP_data_models %>%
  ensemble_average(type = "mean") # use the average
ensemble_fit <- ISIP_data_models %>%
  ensemble_average(type = "median") # use the median


ensemble_fit

# Now it's time for the forecast on test data
# Calibration
calibration_tbl <- modeltime_table(
  ensemble_fit
) %>%
  modeltime_calibrate(testing(splits)) # specify the splits

# Forecast vs Test Set
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = ISIP_isiskolinimai_k
  ) %>%
  plot_modeltime_forecast(.interactive = interactive) + theme_clean() + 
  ggtitle("Įsipareigojimai. Įsiskloninimas užsienio įmonėms: Actual vs Ensemble Forecast", "Lietuvos banko duomenys ir skaičiavimai")

# Refit on Full Data and Forecast the Future

refit_tbl <- calibration_tbl %>%
  modeltime_refit(ISIP_isiskolinimai_k)

refit_tbl %>%
  modeltime_forecast(
    h = "1 year",
    actual_data = ISIP_isiskolinimai_k
  ) %>%
  plot_modeltime_forecast(.interactive = interactive) + theme_clean() + 
  ggtitle("Įsipareigojimai. Įsiskolinimas užsienio įmonėms: Forecast 1 year (4 quarters) into the future using combined model ensemble", 
          "Lietuvos banko duomenys ir skaičiavimai pasitelkiant modeltime.ensemble paketą, naudojant 3 efektyviausių modelių ansamblį (prognozių vidurkį) (Auto-Arima, Prophet ir glmnet (Elastic Net Model))")


##
# gluon-ts
# 
install_gluonts()

data <- m4_hourly %>%
  select(id, date, value) %>%
  group_by(id) %>%
  mutate(value = standardize_vec(value)) %>%
  ungroup()

data

laiko_eilute <- ts(ISIP_isiskolinimai_k$value, start = 2015, frequency = 4)

plot(decompose(laiko_eilute))
decomposed_data <- decompose(laiko_eilute)
random_ts <- ts(decomposed_data$random, start = c(2015, 3), frequency = 4)

hist(random_ts)

plot.ts(decomposed_data$random)


####
# 1. Austru modelis

# 2. Sigutes paklausti, i kuri kintamaji galetume remtis (Ketvitadieni po 10:00)

# 3. Nuosavas modelis - Prognozuoti trenda remiantis  (nuo 2015 iki 2021, validacijai panaudoti 2021 metu duomenis) ir aklai prideti sezoniskumo komponenta is decompose




