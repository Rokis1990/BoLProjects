##################
# Y2 - UT.Nerezidentų įsiskolinimai už prekes ir paslaugas
# 110 - A. Finansiniai reikalavimai nerezidentams
##################
install.packages("modelsummary")
library(modelsummary)

########
# Auto-ARDL
########
dataset_15Q

model1_Y2 <- ARDL::auto_ardl(y2ti ~ #s_exp + s_imp + g_exp + g_imp + # gs_exp + gs_imp + 
                   #gfc2008 + crysisLT09 + 
                     Cov19 + #Quarantine +
                  supplyChainCrysis + #WarUkraine + 
                   L_nfin_tot + D_nfin_tot + IndProd_tot, # + Apyvarta_sum, 
                data = dataset_15Q, max_order = 2)#, grid = TRUE)
summary(model1_Y2$best_model)
?modelsummary
modelsummary::modelsummary(model1_Y2$best_model, title = "ARDL(Y2) model", statistic = c("t = {statistic}",
                                                                                "se = {std.error}",
                                                                                "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", fmt = 3, shape = term + statistic ~ model)
model1_Y2$best_model$full_formula # Best model's formula
plot(model1_Y2$best_model)
acf(residuals(model1_Y2$best_model))

plot(model1_Y2$best_model$fitted.values)
#lines(dataset_15Q[2], colour = "red")

########
# Custom ARDL
########

model1_Y2_C1 <- dyn::dyn$lm(y2ti ~ stats::lag(y2ti, -1) + #Cov19 + 
                              stats::lag(Cov19, -1) + stats::lag(Cov19, -2) + #supplyChainCrysis + 
                              stats::lag(supplyChainCrysis, -1) + stats::lag(supplyChainCrysis, -2) + #L_nfin_tot + 
                              #D_nfin_tot + 
                              stats::lag(D_nfin_tot, -1) + #IndProd_tot + 
                              stats::lag(IndProd_tot, -1), 
                        data = dataset_15Q)
summary(model1_Y2_C1)
plot(model1_Y2_C1)
pred2 <- predict(model1_Y2_C1, 1)
plot(pred2)
pred2_df <- as.data.frame(pred2)
plot(residuals(model1_Y2_C1))
plot(model1_Y2_C1$residuals)
lines(model1_Y2_C1$fitted.values)
lines(pred3)
#dataset_15Q$model14c_fit <- ts(model14c$fitted.values, start = c(2016, 1), frequency = 4)
writexl::write_xlsx(pred2_df, "Prediction_ARDL_model1_Y2_C1.xlsx")

modelsummary::modelsummary(model1_Y2_C1, 
                           title = "Y2 Model1_C1", 
                           statistic = c("t = {statistic}",
                                         "se = {std.error}",
                                         "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", 
                           fmt = 3, 
                           shape = term + statistic ~ model)

tempdisagg::td(pred3 ~ y_monthly_seas, conversion = "last", to = "monthly")

#Menesiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas

iup_men <- ts(Menesiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 1), frequency = 12)

plot(pii_men)

plot(tempdisagg::td(pred3 ~ iup_men, conversion = "last", to = "monthly"))
Y2_disagg <- tempdisagg::td(pred2 ~ iup_men, conversion = "last", to = "monthly")
Y2_disagg
plot(Y2_disagg$fitted.values)
plot(pred2)
plot(Y2_disagg$values)
plot(Y2_disagg$fitted.values)

Y2_men_df <- as.data.frame(Y2_disagg$values)

writexl::write_xlsx(Y2_men_df, "Y2_men_df_disagg.xlsx")

#######
# Read
#######

readxl::read_xlsx("Duomenys/Dataset_v1.xlsx")

##########################################################
# Y2 ARDL on updated data 2023-02-08
##########################################################

dataset_19Q
#########
# Auto ARDL
#########
model2_Y2 <- ARDL::auto_ardl(y2_110 ~ 
                  s_exp + s_imp + g_exp + g_imp + # gs_exp + gs_imp + 
                  #gfc2008 + crysisLT09 + 
                  #Cov19 + #Quarantine +
                  #supplyChainCrysis + #WarUkraine + 
                  L_nfin_tot, # + 
                  #D_nfin_tot + 
                  #IndProd_tot + 
                  #Apyvarta2_sum, 
                data = dataset_19Q, max_order = 2)
summary(model2_Y2$best_model)


#########
# Custom ARDL
#########

model2_Y2_dyn <- dyn::dyn$lm(y2_110 ~ 
                               stats::lag(y2_110, -1) +
                               #stats::lag(y2_110, -2) +
                               stats::lag(diff(s_exp), -2) +
                               stats::lag(diff(s_imp), -2) + 
                               #stats::lag(g_imp, -1) + 
                               stats::lag(diff(g_imp), -1), data = dataset_19Q)
summary(model2_Y2_dyn)
# As we're predicting t+1 period, the equation above cannot contain any variables (save for the dependent variable y1) that are in current time [t0]

plot(model2_Y2_dyn)
pred3 <- predict(model2_Y2_dyn, 1) # Predict one step ahead
plot(pred3)
print(pred3)
pred3_df <- as.data.frame(pred3)
plot(residuals(model2_Y2_dyn))
pred4 <- predict(model2_Y2_dyn, 2) # Try to predict 2 steps ahead (will not work, because of the model specification (time sensitive variables in the equation))
plot(pred4)
print(pred4)
acf(model2_Y2_dyn$residuals)
plot.ts(model2_Y2_dyn$fitted.values)
model2_Y2_fitted_values <- model2_Y2_dyn$fitted.values
plot.ts(model2_Y2_fitted_values)
model2_Y2_fitted_values_df <- as.data.frame(model2_Y2_fitted_values)
writexl::write_xlsx(pred3_df, "Prediction_ARDL_model2_Y2_dyn.xlsx") # Save the predicted values to Excel file
writexl::write_xlsx(model2_Y2_fitted_values_df, "Fitted_ARDL_model2_Y2_dyn.xlsx") # Save the fitted values to Excel file

#################
# Disaggregation
#################

library(tempdisagg)

B0904 <- read_excel("Duomenys/Dataset_v4.3.xlsx", sheet = "B09-04")

y_monthly_seas <- decompose(ts(B0904$`110 - nerezidentų įsiskolinimai už prekes ir paslaugas`, start = c(2016,1), frequency = 12))$seasonal

#y_monthly_seas <- ts(y_monthly_seas[time(y_monthly_seas) <= max(time(y))], start = c(2016,1), frequency = 12)
plot(y_monthly_seas)

y_m_disaggregate <- tempdisagg::td(y2_110 ~ y_monthly_seas, conversion = "last", to = 12) #"monthly")
class(y_m_disaggregate)
plot(y_m_disaggregate)
y_m_disaggregate$values
y_m_disaggregate$fitted.values

plot(predict(y_m_disaggregate),
     sub="A. Finansiniai reikalavimai nerezidentams",
     main = "Y2 - 110 Nerezidentų įsiskolinimai už prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y2_110, start = c(2016, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))

# Export disaggregated time series
Y2_disagg_export <- as.data.frame(predict(y_m_disaggregate))
class(Y2_disagg_export)


writexl::write_xlsx(Y2_disagg_export, "Y2 disagg export_v1.xlsx")

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

y2_m_disaggregate <- tempdisagg::td(y2_110_forecast ~ y2_monthly_seas, conversion = "last", to = "monthly")

plot(predict(y2_m_disaggregate),
     sub = "A. Finansiniai reikalavimai nerezidentams",
     main = "Y2 - 110 Nerezidentų įsiskolinimai už prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y2_110_forecast, start = c(2016, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))


y2_m_disagg_forecast_export <- as.data.frame(predict(y2_m_disaggregate))

y2_110_forecast

plot(y2_monthly_seas)




