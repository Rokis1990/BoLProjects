###########
# Auto-ARDL
# Y1 - B. Finansiniai įsipareigojimai nerezidentams
# 150 - Įsiskolinimai nerezidentams už prekes ir paslaugas
###########
model14 <- ARDL::auto_ardl(y1ii ~ s_exp + s_imp + g_exp + g_imp + # gs_exp + gs_imp + 
                             # gfc2008 + crysisLT09 + 
                             #   Cov19 + #Quarantine +
                             supplyChainCrysis, #+ #WarUkraine + 
                           #   L_nfin_tot + D_nfin_tot + IndProd_tot, # + Apyvarta_sum, 
                           data = dataset_15Q, max_order = 2)#, grid = TRUE)
summary(model14$best_model)
model14$best_model$full_formula # Best model's formula
plot(model14$best_model)
acf(residuals(model14$best_model))

#############
# Custom ARDL
#############

model14c <- dyn::dyn$lm(diff(y1ii) ~ stats::lag(y1ii, -1) + #s_exp + 
                          stats::lag(diff(s_exp), -1) + stats::lag(s_exp, -2) + 
                          #s_imp + 
                          stats::lag(diff(s_imp), -1) + #g_exp + 
                          #g_imp + 
                          stats::lag(diff(g_imp), -1) + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                          #Cov19 + 
                          stats::lag(Cov19, -1) + #Quarantine +
                          #supplyChainCrysis + 
                          stats::lag(supplyChainCrysis, -1) + #WarUkraine + 
                          #L_nfin_tot + 
                          stats::lag(diff(L_nfin_tot), -1) + #D_nfin_tot + 
                          stats::lag(diff(D_nfin_tot), -1) + 
                          #IndProd_tot + 
                          stats::lag(diff(IndProd_tot), -1) + #Apyvarta_sum + 
                          stats::lag(diff(Apyvarta_sum), -1), 
                        data = dataset_15Q)
summary(model14c)
plot(model14c)
pred_1 <- predict(model14c, 1)
plot(pred_1)
pred3_df <- as.data.frame(pred3)
plot(residuals(model14c))
pred4 <- predict(model14c, 2)
plot(pred4)
acf(model14c$residuals)
plot(model14c$fitted.values)
#dataset_15Q$model14c_fit <- ts(model14c$fitted.values, start = c(2016, 1), frequency = 4)
writexl::write_xlsx(pred3_df, "Prediction_ARDL_14c.xlsx")

modelsummary::modelsummary(model14c, 
                           title = "Y1 Model 14C", 
                           statistic = c("t = {statistic}",
                                         "se = {std.error}",
                                         "conf.int"),
                           estimate = "{estimate} [{conf.low}, {conf.high}]", 
                           fmt = 3, 
                           shape = term + statistic ~ model)


iiup_men <- decompose(ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 1), frequency = 12))$seasonal
plot(stl(dataset_15Q[, "y1ii"], s.window = 4))

plot(iiup_men)

plot(tempdisagg::td(pred_1 ~ iiup_men, conversion = "first", to = "monthly"))
Y1_disagg <- tempdisagg::td(pred_1 ~ iiup_men, conversion = "last", to = "monthly")
Y1_disagg
plot(Y1_disagg$fitted.values)
plot(pred_1)
plot(Y1_disagg$values)

Y1_men_df <- as.data.frame(Y1_disagg$values)

writexl::write_xlsx(Y1_men_df, "Y1_men_df_disagg.xlsx")


#######
# Read 
#######

#readxl::read_excel("Duomenys/Dataset_v1.xlsx")

#readxl::read_excel("Duomenys/Dataset_v1.xlsx")

tempdata <- openxlsx::read.xlsx("Duomenys/Dataset_v1.xlsx")
dataset_2 <- na.omit(tempdata)

tempdata <- ts(openxlsx::read.xlsx("Duomenys/Dataset_v1.xlsx", sheet = 2), start = c(2016, 1), frequency = 12)

y110_seas <- decompose(tempdata[,2])$seasonal
y120_seas <- decompose(tempdata[,3])$seasonal
y150_seas <- decompose(tempdata[,4])$seasonal

#y_m_disaggregate <- tempdisagg::td(y_q ~ y110_seas, conversion = "last", to = "monthly")

iiup_men <- decompose(ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 1), frequency = 12))$seasonal
plot(stl(dataset_15Q[, "y1ii"], s.window = 4))

plot(iiup_men)

plot(tempdisagg::td(pred_1 ~ y150_seas, conversion = "first", to = "monthly"))
Y1_disagg <- tempdisagg::td(pred_1 ~ y150_seas, conversion = "last", to = "monthly")
Y1_disagg
plot(Y1_disagg$fitted.values)
plot(pred_1)
plot(Y1_disagg$values)

Y1_men_df <- as.data.frame(Y1_disagg$values)

writexl::write_xlsx(Y1_men_df, "Y1_men_df_disagg_v2.xlsx")


#######################################
# Updated Y1 - 150 - flows
#######################################

dataset_19Q








