##########
# Y3 - Pinigai ir indėliai
# 120 - A. Įmonės piniginės lėšos užsienio bankuose
##########

###########
# Auto-ARDL
###########

dataset_15Q

model1_Y3 <- ARDL::auto_ardl(y3pi ~ s_exp + s_imp + g_exp + g_imp + # gs_exp + gs_imp + 
                               #gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis,# + #WarUkraine + 
                               #L_nfin_tot + D_nfin_tot + 
                             #IndProd_tot + 
                               Apyvarta_sum, 
                             data = dataset_15Q, max_order = 2)#, grid = TRUE)
summary(model1_Y3$best_model)
?modelsummary
modelsummary::modelsummary(model1_Y3$best_model, 
                           title = "Title", 
                           statistic = c("t = {statistic}",
                                         "se = {std.error}",
                                          "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", 
                           fmt = 3, 
                           shape = term + statistic ~ model)
model1_Y3$best_model$full_formula # Best model's formula
plot(model1_Y3$best_model)
acf(residuals(model1_Y3$best_model))

plot(model1_Y3$best_model$fitted.values)
#lines(dataset_15Q[2], colour = "red")

#############
# Manual ARDL
#############

model1_Y3_C1 <- dyn::dyn$lm(y3pi ~ stats::lag(y3pi, -1) + stats::lag(s_exp, -1) + stats::lag(s_exp, -2) + 
                              stats::lag(s_imp, -1) + stats::lag(s_imp, -2) + stats::lag(g_exp, -1) + stats::lag(g_exp, -2) + 
                              stats::lag(supplyChainCrysis, -1), 
                            data = dataset_15Q)
summary(model1_Y3_C1)
plot(model1_Y3_C1)
pred3 <- predict(model1_Y3_C1, 1)
plot(pred3)
pred3_df <- as.data.frame(pred3)
model1_Y3_C1
plot(residuals(model1_Y3_C1))
acf(model1_Y3_C1$residuals)
plot(model1_Y3_C1$fitted.values)
#dataset_15Q$model14c_fit <- ts(model14c$fitted.values, start = c(2016, 1), frequency = 4)
writexl::write_xlsx(pred3_df, "Prediction_ARDL_model1_Y3_C1.xlsx")

modelsummary::modelsummary(model1_Y3_C1, 
                           title = "Y3 Model1_C1", 
                           statistic = c("t = {statistic}",
                                         "se = {std.error}",
                                         "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", 
                           fmt = 3, 
                           shape = 
                             #term + 
                             statistic ~ 
                             model)


pii_men <- ts(Menesiniai$UT..Pinigai.ir.indėliai, start = c(2015, 1), frequency = 12)

plot(pii_men)

plot(tempdisagg::td(pred3 ~ pii_men, conversion = "first", to = "monthly"))
Y3_disagg <- tempdisagg::td(pred3 ~ pii_men, conversion = "last", to = "monthly")
Y3_disagg
plot(Y3_disagg$fitted.values)
plot(pred3)
plot(Y3_disagg$values)

Y3_men_df <- as.data.frame(Y3_disagg$values)

writexl::write_xlsx(Y3_men_df, "Y3_men_df_disagg.xlsx")

##########################################
# Model update 2023-02-08
##########################################

###########
# Auto ARDL
###########
library(ARDL)
model2_Y3 <- ARDL::auto_ardl(y3_120 ~ y2_110 + y1_150 +#s_exp + s_imp + g_exp + g_imp + # 
                              # gs_exp + gs_imp + 
                  #gfc2008 + crysisLT09 + 
                  #Cov19 + #Quarantine +
                  supplyChainCrysis + #WarUkraine + 
                  L_nfin_tot + 
                  D_nfin_tot +  
                  IndProd_tot, 
                # Apyvarta2_sum, 
                data = dataset_19Q, max_order = 2)

summary(model2_Y3$best_model)
model2_Y3$best_model$full_formula

#############
# Custom ARDL
#############
library(dyn)

model2_Y3_dyn <- dyn::dyn$lm(
  y3_120 ~ stats::lag(y3_120, -2) +
    stats::lag(supplyChainCrysis, -1) + 
    stats::lag(D_nfin_tot, -1) + 
    stats::lag(IndProd_tot, -2), # + 
    # stats::lag() + 
    # stats::lag() + 
    # stats::lag(),
  data = dataset_19Q
)
summary(model2_Y3_dyn)

plot(model2_Y3_dyn)
pred_y3 <- predict(model2_Y3_dyn, 1) # Predict one step ahead
plot(pred_y3)
print(pred_y3)
pred_y3_df <- as.data.frame(pred_y3)
plot(residuals(model2_Y3_dyn))
pred4 <- predict(model2_Y3_dyn, 2) # Try to predict 2 steps ahead (will not work, because of the model specification (time sensitive variables in the equation))
plot(pred4)
print(pred4)
acf(model2_Y3_dyn$residuals)
plot.ts(model2_Y3_dyn$fitted.values)
model2_Y3_fitted_values <- model2_Y3_dyn$fitted.values
model2_Y3_fitted_values_df <- as.data.frame(model2_Y3_fitted_values)
writexl::write_xlsx(pred_y3_df, "Prediction_ARDL_model2_Y3_dyn.xlsx") # Save the predicted values to Excel file
writexl::write_xlsx(model2_Y3_fitted_values_df, "Fitted_ARDL_model2_Y3_dyn.xlsx") # Save the fitted values to Excel file

pred_y3
pred_y3_df
plot.ts(pred_y3, col = "red")
lines(y3_120, col = "black")

###
# Omit outliers, build same ARDL
###
dataset_19Q
replace(dataset_19Q, dataset_19Q==-734919, 1)
####
library(ARDL)
model3_Y3 <- ARDL::auto_ardl(y3a_120 ~ y2_110 + y1_150 +#s_exp + s_imp + g_exp + g_imp + # 
                               # gs_exp + gs_imp + 
                               #gfc2008 + crysisLT09 + 
                               #Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               #L_nfin_tot + 
                               #D_nfin_tot +  
                               IndProd_tot,# +
                              #Apyvarta2_sum, 
                             data = dataset_19Q, max_order = 2)
summary(model3_Y3$best_model)

####
library(dyn)

model3_Y3_dyn <- dyn::dyn$lm(
  y3a_120 ~ stats::lag(y3_120, -2) +
    stats::lag(supplyChainCrysis, -1) + 
    #stats::lag(D_nfin_tot, -1) + 
    stats::lag(IndProd_tot, -2) + 
    stats::lag(y1_150, -1), # + 
  # stats::lag() + 
  # stats::lag(),
  data = dataset_19Q
)
summary(model3_Y3_dyn)
plot(model3_Y3_dyn)
pred_y3 <- predict(model3_Y3_dyn, 1) # Predict one step ahead
plot(pred_y3)
print(pred_y3)
pred_y3_df <- as.data.frame(pred_y3)
writexl::write_xlsx(pred_y3_df, "predicted_values_y3.xlsx")
fit_y3 <- as.data.frame(model3_Y3_dyn$fitted.values)
writexl::write_xlsx(fit_y3, "fitted_values_y3.xlsx")

modelsummary::modelsummary(model3_Y3_dyn, 
                           title = "Y3 Model3 dyn", 
                           statistic = c("t = {statistic}",
                                         "se = {std.error}",
                                         "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", 
                           fmt = 3, 
                           shape = term + statistic ~ model)




#################
# Disaggregation
#################

library(tempdisagg)

B0904 <- read_excel("Duomenys/Dataset_v4.3.xlsx", sheet = "B09-04")

y_monthly_seas <- decompose(ts(B0904$`120 - sąskaitos užsienio bankuose`, start = c(2016,1), frequency = 12))$seasonal

#y_monthly_seas <- ts(y_monthly_seas[time(y_monthly_seas) <= max(time(y))], start = c(2016,1), frequency = 12)

y_m_disaggregate <- tempdisagg::td(y3_120 ~ y_monthly_seas, conversion = "last", to = "monthly") # Does not work properly
y_m_disaggregate <- tempdisagg::td(y3_120 ~ y_monthly_seas, conversion = "last", to = 12)
plot(y_m_disaggregate)
y_m_disaggregate$values
y_m_disaggregate$fitted.values

plot(predict(y_m_disaggregate),
     sub = "A. Finansiniai reikalavimai nerezidentams",
     main = "Y3 - 120 Įmonės piniginės lėšos užsienio bankuose",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y3_120, start = c(2016, 1.67), frequency = 4), col="blue")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))
## Export disaggregated time series
Y3_disagg_export <- as.data.frame(predict(y_m_disaggregate))
class(Y3_disagg_export)


writexl::write_xlsx(Y3_disagg_export, "Y3 disagg export.xlsx")

###
# Disaggregated forecast
###
y_flows_forecasted <- readxl::read_excel("Duomenys/Dataset_v4.8.xlsx", sheet = "Plots2")
y2_110_forecast <- ts(y_flows_forecasted$`F-06_13xx+TUI-01_22xx+TUI-01_32xx`, start = c(2016, 1), frequency = 4) # 110 - Y2
y3_120_forecast <- ts(y_flows_forecasted$`srautas F-06 120`, start = c(2016, 1), frequency = 4)                  # 120 - Y3
y1_150_forecast <- ts(y_flows_forecasted$`F-06_23xx+TUI-01_12xx+TUI-01_42xx`, start = c(2016, 1), frequency = 4) # 150 - Y1


library(tempdisagg)

B0904 <- readxl::read_excel("Duomenys/Dataset_v4.8.xlsx", sheet = "B09-04")

y1_monthly_seas <- decompose(ts(B0904$`150 - Įsiskolinimai nerezidentams už prekes ir paslaugas`, start = c(2016,1), frequency = 12))$seasonal
y2_monthly_seas <- decompose(ts(B0904$`110 - nerezidentų įsiskolinimai už prekes ir paslaugas`, start = c(2016,1), frequency = 12))$seasonal
y3_monthly_seas <- decompose(ts(B0904$`120 - sąskaitos užsienio bankuose`, start = c(2016,1), frequency = 12))$seasonal

#y_monthly_seas <- ts(y_monthly_seas[time(y_monthly_seas) <= max(time(y))], start = c(2016,1), frequency = 12) # approx(?) same result as from line above (decompose()$seasonal )

y3_m_disaggregate <- tempdisagg::td(y3_120_forecast ~ y3_monthly_seas, conversion = "last", to = "monthly")

plot(predict(y3_m_disaggregate),
     sub = "A. Finansiniai reikalavimai nerezidentams",
     main = "Y3 - 120 Įmonės piniginės lėšos užsienio bankuose",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y3_120_forecast, start = c(2016, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))


y3_m_disagg_forecast_export <- as.data.frame(predict(y3_m_disaggregate))

y3_120_forecast

plot(y3_monthly_seas)

decompose(time_series)$seasonal
