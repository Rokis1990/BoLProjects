# Let's install and load the required packages
# https://data.library.virginia.edu/diagnostic-plots/
install.packages("seas", repos='https://mran.microsoft.com/snapshot/2022-06-20/', dependencies = TRUE)
library(seas)

if (!require("pacman")) install.packages("pacman") # pacman has a neat way of loading required packages into the library
pacman::p_load(dynamac,
               forecast,
               tidyverse,
               tseries,
               urca,
               TSstudio,
               dLagM,
               dyn)

### Read the data
Ketvirtiniai <- openxlsx::read.xlsx("Duomenys/Laiko eilutės_2022-06-02_v1.xlsx", sheet = 1, colNames = TRUE, startRow = 2)
Menesiniai <- openxlsx::read.xlsx("Duomenys/Laiko eilutės_2022-06-02_v1.xlsx", sheet = 2, colNames = TRUE, startRow = 2)

Apyvarta <- openxlsx::read.xlsx("Duomenys/išoriniai makro rodikliai/Apyvarta.xlsx")
Apyvarta2 <- openxlsx::read.xlsx("Duomenys/išoriniai makro rodikliai/atnaujinimas/Apyvarta-pvm_apyvarta_evrk_menesiai_augimai.xlsx", sheet = "Sheet2")
Apyvarta_sum <- Apyvarta |> group_by(Metai_Mėnuo) |> summarise(sum(Apyvarta))
Apyvarta_sum <- ts(Apyvarta_sum$`sum(Apyvarta)`, start = c(2016, 1), frequency = 12)
Apyvarta2_sum <- ts(Apyvarta2$Grand.Total, start = c(2017, 1), frequency = 12)
Apyvarta_sum <- aggregate(Apyvarta_sum, nfrequency = 4) / 1000000 # in millions
Apyvarta2_sum <- aggregate(Apyvarta2_sum, nfrequency = 4) / 1000000 # In millions

# Read Current Account data
#CurrentAccount_trade <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/t-ketvirtinis-mokejimu-balansas_CurrentAccount_PrekesIrPaslaugos_DebetasKreditas.xlsx",
#                  sheet = 3)
CurrentAccount_trade <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/atnaujinimas/t-ketvirtinis-mokejimu-balansas (2).xlsx",
                                          sheet = 3) # Renewed 2023-02-06

s_exp <- ts(CurrentAccount_trade$Kreditas...9, start = 2004, frequency = 4)
s_imp <- ts(CurrentAccount_trade$Debetas...10, start = 2004, frequency = 4)
g_exp <- ts(CurrentAccount_trade$`1.A.a.1. Eksportas (kreditas)`, start = 2004, frequency = 4)
g_imp <- ts(CurrentAccount_trade$`1.A.a.2. Importas (debetas)`, start = 2004, frequency = 4)
gs_exp <- ts(CurrentAccount_trade$Kreditas...3, start = 2004, frequency = 4)
gs_imp <- ts(CurrentAccount_trade$Debetas...4, start = 2004, frequency = 4)

window(s_exp, )
plot(s_exp)
plot(s_imp)
plot(g_exp)
plot(g_imp)
plot(gs_exp)
plot(gs_imp)


# check for stationarity

par(mfrow=c(4,4))

pp.test(y) # Stationary
plot(s_exp)
pp.test(s_exp) # very Non-stationary
plot(diff(s_exp))
pp.test(diff(s_exp)) # stationary
plot(s_imp)
pp.test(s_imp) # Rather non-stationary
plot(diff(s_imp))
pp.test(diff(s_imp)) # stationary
plot(g_exp)
pp.test(g_exp) # rather non-stationary
plot(diff(g_exp))
pp.test(diff(g_exp)) # stationary
plot(g_imp)
pp.test(g_imp) # non-stationary
plot(diff(g_imp))
pp.test(diff(g_imp)) # stationary

pp.test(Apyvarta_sum) # Slightly non-stationary
plot(Apyvarta_sum)
plot(diff(Apyvarta_sum))
pp.test(diff(Apyvarta_sum)) # Stationary


# Not used in the models
plot(gs_exp)
pp.test(gs_exp) # rather non-stationary
plot(diff(gs_exp))
pp.test(diff(gs_exp)) # stationary
plot(gs_imp)
pp.test(gs_imp) # non-stationary
plot(diff(gs_imp))
pp.test(diff(gs_imp)) # stationary

# Read Industry production data
#IndustryProduction <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/data-table - 2022-07-05T160014-879.xlsx", sheet = 2)
IndustryProduction <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/atnaujinimas/data-table_B_TO_E Pramone Ketvirtiniai.xlsx", sheet = 2) # Updated dataset 2023-02-06

IndProd_tot <- ts(IndustryProduction$`B_TO_E Pramonė`, start = 1998, frequency = 4)

plot(IndProd_tot)
pp.test(IndProd_tot) # Non-stationary
plot(diff(IndProd_tot))
pp.test(diff(IndProd_tot)) # Stationary


# Read loan and deposit data
#LoansNonFinancialFirms <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/t-t-paskolos-ne-finansu-bendrovems-pagal-ekonomines-veiklos-rusis.xlsx",
#                                            sheet = 2)
LoansNonFinancialFirms <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/atnaujinimas/t-t-paskolos-ne-finansu-bendrovems-pagal-ekonomines-veiklos-rusis (1).xlsx",
                                            sheet = 2) # Update 2023-02-06
#DepositsNonFinancialFirms <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/t-lietuvos-ne-finansu-bendroviu-ir-namu-ukiu-indeliai-pagal-rusi-ir-trukme.xlsx",
#                                               sheet = 3)

# Monthly data - will convert to quarterly averages
DepositsNonFinancialFirms <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/atnaujinimas/t-lietuvos-ne-finansu-bendroviu-ir-namu-ukiu-indeliai-pagal-rusi-ir-trukme.xlsx",
                                               sheet = 3) # Update 2023-02-06

L_nfin_tot <- ts(LoansNonFinancialFirms$`Iš viso`, start = c(2011, 3), frequency = 4)
D_nfin_tot <- ts(DepositsNonFinancialFirms$`Iš viso`, start = c(2004, 4), frequency = 12) |>
  aggregate(nfrequency = 4, mean) # Quarterly averages from monthly deposit data

# Check stationarity
plot(L_nfin_tot)
pp.test(L_nfin_tot) # Non-stationary
plot(diff(L_nfin_tot))
pp.test(diff(L_nfin_tot)) # Stationary



plot(D_nfin_tot)
pp.test(D_nfin_tot) # Clearly very Non-stationary
plot(diff(D_nfin_tot))
pp.test(diff(D_nfin_tot)) # Stationary



# Read dummy variable data
dumvars <- readxl::read_excel("Duomenys/išoriniai makro rodikliai/Dumvars.xlsx")
gfc2008 <- ts(dumvars$GFC2008, start = 2004, frequency = 4)
crysisLT09 <- ts(dumvars$CrysisLT09, start = 2004, frequency = 4)
Cov19 <- ts(dumvars$COVID19, start = 2004, frequency = 4)
Quarantine <- ts(dumvars$CovQuarantine, start = 2004, frequency = 4)
supplyChainCrysis <- ts(dumvars$SupplyChainCrisis, start = 2004, frequency = 4)
WarUkraine <- ts(dumvars$WarUkraine, start = 2004, frequency = 4)
plot(gfc2008)
plot(crysisLT09)
plot(Cov19)
plot(Quarantine)
plot(supplyChainCrysis)
plot(WarUkraine)

### Define the dependent variables
y1ii <- ts(Ketvirtiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas,
        start = 2015, frequency = 4)
y2ti <- ts(Ketvirtiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas,
         start = 2015, frequency = 4)
y3pi <- ts(Ketvirtiniai$UT..Pinigai.ir.indėliai,
         start = 2015, frequency = 4)

par(mfrow = c(3, 1))
plot(y1ii)
pp.test(y1ii) # stationary
plot(y2ti)
pp.test(y2ti) # stationary
plot(y3pi)
pp.test(y3pi) # stationary

# Read y values from Excel file.
#y_flows <- readxl::read_excel("Duomenys/Dataset_v3.2.xlsx", sheet = "Plots") # from 2019
y_flows <- readxl::read_excel("Duomenys/Dataset_v4.2.xlsx", sheet = "Plots2") # from 2016

# Convert numeric vectors to time series
#y1_150 <- ts(y_flows$`F-06_23xx+TUI-01_12xx+TUI-01_42xx`, start = 2019, frequency = 4) # F-06 + TUI-01 sums of flows
#y2_110 <- ts(y_flows$`B-09-04 srautų suma (raw) 110`, start = 2019, frequency = 4) # F-06 + TUI-01 sums of flows
#y3_120 <- ts(y_flows$`srautas F-06 120`, start = 2019, frequency = 4) # F-06 + TUI-01 sums of flows

y1_150 <- ts(y_flows$`F-06_23xx+TUI-01_12xx+TUI-01_42xx`, start = 2016, frequency = 4) # F-06 + TUI-01 sums of flows
#y2_110 <- ts(y_flows$`B-09-04 srautų suma (raw) 110`, start = 2016, frequency = 4) # F-06 + TUI-01 sums of flows
y2_110 <- ts(y_flows$`F-06_13xx+TUI-01_22xx+TUI-01_32xx`, start = 2016, frequency = 4) # F-06 + TUI-01 sums of flows
y3_120 <- ts(y_flows$`srautas F-06 120`, start = 2016, frequency = 4) # F-06 + TUI-01 sums of flows

y3a_120 <- y_flows$`srautas F-06 120`
mean(y3a_120)
y3a_120 <- ts(replace(y3a_120, y3a_120==-734919, -20670.44), start = 2016, frequency = 4)


# Test for stationarity
par(mfrow = c(3, 1))
plot(y1_150)
pp.test(y1_150) # weak stationarity
plot(y2_110)
pp.test(y2_110) # non-stationary
plot(y3_120)
pp.test(y3_120) # weak stationarity


### Combine dataset
dataset <- cbind(y1ii, y2ti, y3pi, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, 
                 Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot, Apyvarta_sum)

dataset2 <- cbind(#y1ii, y2ti, y3pi, 
                  y1_150, y2_110, y3_120, y3a_120,
                  s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, 
                 Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot, Apyvarta2_sum)

# Omit rows that contain NA's (missing values)
dataset_15Q <- na.omit(dataset)
dataset_19Q <- na.omit(dataset2)

dates <- seq(as.Date("2015/1/1"), as.Date("2021/10/1"), by = "quarter")
dates2 <- seq(as.Date("2019/1/1"), as.Date("2022/10/1"), by = "quarter")

dataset_15Q$dates <- ts(dates, start = c(2015, 1), frequency = 4) # Coerces dataset to a list
dataset_19Q$dates <- ts(dates2, start = c(2019, 1), frequency = 4) # Coerces dataset to a list

seq(as.Date("2015/1/1"), as.Date("2022/12/30"), by = "month")

#Menesiniai$date <- 

# Analyze input data
summary(dataset_15Q)
hist(dataset_15Q[,1])
hist(dataset_15Q[,2])
hist(dataset_15Q[,3])
hist(dataset_15Q[,4])
hist(dataset_15Q[,5])
hist(dataset_15Q[,6])
hist(dataset_15Q[,7])
hist(dataset_15Q[,8])
hist(dataset_15Q[,9])



#####

# Estimate the ARDL model

model1 <- dynardl(y ~ s_exp + s_imp + g_exp + g_imp + gs_exp + gs_imp + 
                  gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
                  WarUkraine,
                  # lags = list("y" = 1, s_exp = 1, s_imp = 2),
                  # diffs = c("s_exp", "s_imp", "g_exp", "g_imp", "gs_exp", "gs_imp"), 
                  # data = dataset
                  ec = TRUE, simulate = FALSE)
summary(model1)
alias(model1$model)
#residuals(model1)
plot(model1$model)
?plot.lm
acf(residuals(model1$model))
pacf(residuals(model1$model))
dynardl.auto.correlated(model1)
plot(model1)

dLagM::forecast(model1$model) # not working

dataset1 <- tail(dataset, 29)

model2 <- dynardl(y ~ s_exp + s_imp + g_exp + g_imp + gs_exp + gs_imp + gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis + WarUkraine,
                  lags = list("y" = 1, "s_exp" = 1, "s_imp" = 2),
                  # diffs = c("s_exp", "s_imp", "g_exp", "g_imp", "gs_exp", "gs_imp"), 
                  data = data.frame(dataset),
                  ec = TRUE, simulate = FALSE)

summary(model2)
alias(model2$model)
plot(model2$model)
acf(residuals(model2$model))
pacf(residuals(model2$model))
dynardl.auto.correlated(model2)
forecast(model2)
dLagM::forecast(model2$model)

model3 <- dynardl(y ~ s_exp + s_imp + g_exp + g_imp + gs_exp + gs_imp + gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis + WarUkraine,
                  lags = list("y" = 1, "s_exp" = 1, "s_imp" = 2),
                  diffs = c("s_exp", "s_imp", "g_exp", "g_imp", "gs_exp", "gs_imp"), 
                  data = data.frame(dataset),
                  ec = TRUE, simulate = FALSE)

summary(model3)
alias(model3$model)
plot(model3$model)
acf(residuals(model3$model))
pacf(residuals(model3$model))
dynardl.auto.correlated(model3)
forecast(model3)
dLagM::forecast(model3$model)

model4 <- dynardl(y ~ s_exp + s_imp + g_exp + g_imp + gs_exp + gs_imp + gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis + WarUkraine,
                  lags = list("y" = 1,"y" = 4, "s_exp" = 1, "s_imp" = 2),
                  diffs = c("s_exp", "s_imp", "g_exp", "g_imp", "gs_exp", "gs_imp"), 
                  data = data.frame(dataset),
                  ec = TRUE, simulate = FALSE)

summary(model4)
alias(model4$model)
plot(model4$model)
acf(residuals(model4$model))
pacf(residuals(model4$model))
dynardl.auto.correlated(model4)
forecast(model4)

###########
# Create the full dataset
dataset <- cbind(y, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot)
library(dLagM)
model5 <- dynardl(y ~ s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + 
                    gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
                    WarUkraine + L_nfin_tot + D_nfin_tot,
                  # lags = list("y" = 1, s_exp = 1, s_imp = 2),
                  diffs = c("s_exp", "s_imp", "g_exp", "g_imp", #"gs_exp", "gs_imp", 
                            "L_nfin_tot", "D_nfin_tot"), 
                  # data = dataset
                  ec = TRUE, simulate = FALSE)

summary(model5)
plot(model5$model)
alias(model5$model)
acf(residuals(model5$model))
pacf(residuals(model5$model))
dynardl.auto.correlated(model5)
forecast(model5)
dLagM::forecast(model5$model)


##################################
# Forecasting only (conveniently) possible with ARDL model built with another package

library(dLagM)

??dLagM

dataset_15Q <- na.omit(dataset)

cormatrix <- cor(dataset_15Q)
writexl::write_xlsx(data.frame(cormatrix), "Output/correlationMatrix.xlsx")

model6 <- ardlDlm(c(y) ~ c(s_exp) + c(s_imp) + c(g_exp) + c(g_imp) + #gs_exp + gs_imp + 
          c(gfc2008) + c(crysisLT09) + c(Cov19) + c(Quarantine) + c(supplyChainCrysis) +
          c(WarUkraine) + c(L_nfin_tot) + c(D_nfin_tot), data = data.frame(dataset_15Q))

model6 <- ardlDlm(y ~ s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + 
                    gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
                    WarUkraine + L_nfin_tot + D_nfin_tot, data = data.frame(dataset_15Q))


summary(model6)
alias(model6$model)
plot(model6$model)
ardlBound(model6, y ~ s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + 
            gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
            WarUkraine + L_nfin_tot + D_nfin_tot)
ardlBoundOrders(model6)
acf(residuals(model6))
pacf(residuals(model6))
dLagM::forecast(model6, x = c(0.15, 0.45) , 
                h = 2 , interval = FALSE) # forecast produces NA's, x is arbitrarily taken from example with no real ground in anything


x.new <- matrix(c(0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2,
                  0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2,
                  0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2,
                  0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2,
                  0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2,
                  0.07 , 9.06 , 0.071 , 9.09, 0.08, 9.2), ncol = 3, 
               nrow = 12)
forecast(model6, x = x.new, h = 3)

?forecast

plot(decompose(y))
y_seasonal <- decompose(y)$seasonal
plot(y_seasonal)

predict.lm(model6)

###################
# Linear model - Try to reimplement the ARDL model

# Create the full dataset
dataset <- cbind(y, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot)
dataset_15Q <- na.omit(dataset)
model7 <- lm(y ~ #s_exp + s_imp + g_exp + g_imp + 
               gs_exp + gs_imp + 
   gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
   WarUkraine + L_nfin_tot + D_nfin_tot + IndProd_tot, data = dataset_15Q)

summary(model7)
alias(model7)
plot(model7)
acf(residuals(model7))
pacf(residuals(model7))
predict.lm(model7) |> plot()
prediction <- predict.lm(model7)

plot.ts(prediction)
plot.ts(y)

##########
# Time-series linear model

library(forecast)
model8 <- tslm(y ~ #s_exp + s_imp + g_exp + g_imp + 
                 gs_exp + gs_imp + 
                 #gfc2008 + crysisLT09 + 
                 Cov19 + Quarantine + supplyChainCrysis +
                 #WarUkraine + 
                 L_nfin_tot + D_nfin_tot + IndProd_tot, data = dataset_15Q)
summary(model8)
alias(model8)
plot(model8$model)
acf(residuals(model8))
pacf(residuals(model8))
predict.lm(model8)

fit <- ets(window(y, end = 2023))
forecast::forecast(y, model = fit) |> plot()

?forecast

###################
# Another package - Non-linear Autoregressive distributed lag model
library(nardl)
??nardl


###################
# Yet another package - Dynamic Simulation and Testing for Single-Equation ARDL Models
library(dynamac)
??dynamac


###################
# Correlation matrix
library(corrplot)

cormatrix <- cor(dataset_15Q)
writexl::write_xlsx(data.frame(cormatrix), "Output/correlationMatrix.xlsx")


####

# auto_ardl()
library(ARDL)
model9 <- auto_ardl(y ~ #s_exp + s_imp + g_exp + g_imp + 
                      gs_exp + gs_imp + 
                      gfc2008 + crysisLT09 + Cov19 + Quarantine + supplyChainCrysis +
                      WarUkraine + L_nfin_tot + D_nfin_tot + IndProd_tot, data = dataset_15Q, max_order = 5)

summary(model9)
summary(model9$best_model)
plot(model9$best_model)
alias(model9$best_model)
variable <- predict(model9$best_model)
plot.ts(y)
plot.ts(predict(model9$best_model))

# glm()

###############
# Model 10

# Create the full dataset
dataset <- cbind(y, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, 
                 Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot, Apyvarta_sum)
dataset_15Q <- na.omit(dataset)

model10 <- auto_ardl(y ~ #s_exp + s_imp + g_exp + g_imp + 
                       gs_exp + gs_imp + 
                       #gfc2008 + crysisLT09 + 
                       Cov19 + #Quarantine + 
                       supplyChainCrysis +
                       #WarUkraine + 
                       L_nfin_tot + D_nfin_tot + IndProd_tot + 
                       Apyvarta_sum, data = dataset_15Q, max_order = 5)
summary(model10)
summary(model10$best_order)
summary(model10$best_model)
plot(model10$best_model)
alias(model10$best_model)
acf(residuals(model10$best_model))
acf(model10$best_model$residuals)
plot(predict(model10$best_model))
cor(dataset_15Q)


#################
# Model 11
model11 <- ardlDlm(y ~ #s_exp + s_imp + g_exp + g_imp + 
                     gs_exp + gs_imp + 
                     #gfc2008 + crysisLT09 + 
                     Cov19 + #Quarantine + 
                     supplyChainCrysis +
                     #WarUkraine + 
                     L_nfin_tot + D_nfin_tot + IndProd_tot + 
                     Apyvarta_sum, data = as.data.frame(dataset_15Q))

summary(model11)
plot(model11$model)
acf(model11$model$residuals)
alias(model11$model)
pacf(residuals(model11))

#################
# Model 12
y <- ts(Ketvirtiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = 2015, frequency = 4)
dataset <- cbind(y, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, 
                 Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot, Apyvarta_sum)
dataset_15Q <- na.omit(dataset)

model12 <- dynardl(y ~ #s_exp + s_imp + g_exp + g_imp + 
          gs_exp + gs_imp + 
          #gfc2008 + crysisLT09 + 
          Cov19 + #Quarantine + 
          supplyChainCrysis +
          #WarUkraine + 
          L_nfin_tot + D_nfin_tot + IndProd_tot + 
          Apyvarta_sum,
         lags = list("y" = 1, s_exp = 1, s_imp = 2),
        diffs = c(#"s_exp", "s_imp", "g_exp", "g_imp",
                  "gs_exp", "gs_imp", "Cov19",
                  "L_nfin_tot", "D_nfin_tot",
                  "IndProd_tot",
                    "Apyvarta_sum"), 
        data = as.data.frame(dataset_15Q),
        ec = TRUE, simulate = FALSE)

summary(model12)
plot(model12$model)
acf(residuals(model12$model))
pacf(residuals(model12$model))







### 
# Weighted average of multiple forecasts

function(){
  
}
  

library(dyn)
library(ARDL)
library(dLagM)

#####
install.packages("dyn", repos='https://mran.microsoft.com/snapshot/2022-06-20/', dependencies = TRUE)
library(dyn)

y <- ts(Ketvirtiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = 2015, frequency = 4)
dataset <- cbind(y, s_exp, s_imp, g_exp, g_imp, gs_exp, gs_imp, 
                 gfc2008, crysisLT09, 
                 Cov19, Quarantine, supplyChainCrysis,
                 WarUkraine, L_nfin_tot, D_nfin_tot, IndProd_tot, Apyvarta_sum)
dataset_15Q <- na.omit(dataset)

model13 <- dyn::dyn$lm(y ~ #s_exp + s_imp + g_exp + g_imp + 
                         stats::lag(gs_exp, -1) + stats::lag(gs_imp, -1) + 
                         #gfc2008 + crysisLT09 + 
                         stats::lag(Cov19, -1) + #Quarantine + 
                         stats::lag(supplyChainCrysis, -1) +
                         #WarUkraine + 
                         stats::lag(L_nfin_tot, -1) + stats::lag(D_nfin_tot, -1) + stats::lag(IndProd_tot, -1)  + 
                         stats::lag(Apyvarta_sum, -1))

summary(model13)
acf(model13$residuals)
plot(model13)

predict(tail(model13$model))

predict(model13, tail(dataset_15Q, 1)) # forecast for time t = 1

model13$fitted.values - model13$model # residuals

ts.plot(model13$fitted.values)
lines(ts(model13$model$y), color = "red")

########################################
# Model 14
########################################
library(ARDL)
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


library(dyn)
# from the best auto_ardl suggested model, let's explicitly define the best model
model14a <- dyn::dyn$lm(y1ii ~ stats::lag(y1ii, -1) + #s_exp + 
                          stats::lag(s_exp, -1) + stats::lag(s_exp, -2) + 
                          #s_imp + 
                          stats::lag(s_imp, -1) + #g_exp + 
                          #g_imp + 
                          stats::lag(g_imp, -1) + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                          #Cov19 + 
                          stats::lag(Cov19, -1) + #Quarantine +
                          #supplyChainCrysis + 
                          stats::lag(supplyChainCrysis, -1) + #WarUkraine + 
                          #L_nfin_tot + 
                          stats::lag(L_nfin_tot, -1) + #D_nfin_tot + 
                          stats::lag(D_nfin_tot, -1) + 
                          #IndProd_tot + 
                          stats::lag(IndProd_tot, -1) + #Apyvarta_sum + 
                          stats::lag(Apyvarta_sum, -1), 
                        data = dataset_15Q)
summary(model14a)
model14b <- dyn::dyn$lm(y1ii ~ stats::lag(y1ii, -1) + #s_exp + 
                          stats::lag(s_exp, -1) + stats::lag(s_exp, -2) + 
                          #s_imp + 
                          stats::lag(s_imp, -1) + #g_exp + 
                          #g_imp + 
                          stats::lag(g_imp, -1) + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                          #Cov19 + 
                          stats::lag(Cov19, -1) + #Quarantine +
                          #supplyChainCrysis + 
                          stats::lag(supplyChainCrysis, -1) + #WarUkraine + 
                          #L_nfin_tot + 
                          stats::lag(L_nfin_tot, -1) + #D_nfin_tot + 
                          stats::lag(D_nfin_tot, -1) + 
                          #IndProd_tot + 
                          stats::lag(IndProd_tot, -1) + #Apyvarta_sum + 
                          stats::lag(Apyvarta_sum, -1), 
                        data = test_dataset)
summary(model14b)
plot(model14a)
plot(model14b)
acf(residuals(model14a))
acf(model14a$residuals)
acf(model14b$residuals)

pred1 <- predict(model14a, 1)
pred2 <- predict(model14b, 1)

plot(pred1)
plot(pred2)
plot(test_dataset[,1])
plot(dataset_15Q[,1])
lines(pred1, color = "blue")
lines(pred2, color = "red")

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
pred3 <- predict(model14c, 1)
plot(pred3)
pred3_df <- as.data.frame(pred3)
plot(residuals(model14c))
pred4 <- predict(model14c, 2)
plot(pred4)
acf(model14c$residuals)
plot(model14c$fitted.values)
#dataset_15Q$model14c_fit <- ts(model14c$fitted.values, start = c(2016, 1), frequency = 4)
writexl::write_xlsx(pred3_df, "Prediction_ARDL_14c.xlsx") # Save the predicted values to Excel file

#
model14d <- dyn::dyn$lm(diff(y1ii) ~ stats::lag(y1ii, -1) + #s_exp + 
                          stats::lag(diff(s_exp), -1) + stats::lag(s_exp, -2) + 
                          #s_imp + 
                          #stats::lag(diff(s_imp), -1) + #g_exp + 
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
summary(model14d)
plot(model14d)
pred5 <- predict(model14d, 1)
plot(pred3)
pred5_df <- as.data.frame(pred3)
plot(residuals(model14c))
acf(model14d$residuals)
plot(model14d$fitted.values)
#dataset_15Q$model14c_fit <- ts(model14c$fitted.values, start = c(2016, 1), frequency = 4)
writexl::write_xlsx(pred5_df, "Prediction_ARDL_14d.xlsx")



#

tmp_mdl <- dyn::dyn$lm(diff(y1ii) ~ stats::lag(diff(y1ii), -1), data = head(dataset_15Q, -1))
summary(tmp_mdl)

predict(tmp_mdl, 1)
tail(predict(tmp_mdl, 1), 2)[1]
tail(dataset_15Q[, "y1ii"], 2)[1]
tail(dataset_15Q[, "y1ii"], 1)


tail(dataset_15Q[, "y1ii"], 2)[1] + 
  tail(predict(tmp_mdl, 1), 2)[1]


tail(dataset_15Q, 2)
plot.ts(dataset_15Q[, "y1ii"])
plot.ts(diff(dataset_15Q[, "y1ii"]))
forecast::tsdisplay((dataset_15Q[, "y1ii"]))
forecast::tsdisplay(diff(dataset_15Q[, "y1ii"]))

plot(decompose(dataset_15Q[, "y1ii"]))
plot(stl(dataset_15Q[, "y1ii"], s.window = 4))

seasonal::seas(diff(dataset_15Q[, "y1ii"])) %>% plot()


####
# Crossvalidation? Model accuracy
####

test_dataset <- head(dataset_15Q, -1)

tmp_mdl2 <- dyn::dyn$lm(diff(y1ii) ~ stats::lag(diff(y1ii), -1)  + stats::lag(supplyChainCrysis, - 1), data = head(dataset_15Q, -1))
summary(tmp_mdl2)

####################
# Model 14 renewed
####################
library(ARDL)
(dataset_19Q)
model14e <- ARDL::auto_ardl(y1_150 ~ s_exp + 
                              # s_imp + 
                              # g_exp + 
                              # g_imp + # gs_exp + gs_imp + 
                              # gfc2008 + crysisLT09 + 
                              # Cov19 + 
                              # Quarantine +
                              # supplyChainCrysis + 
                              # WarUkraine, # + 
                              L_nfin_tot + 
                              D_nfin_tot + 
                              IndProd_tot + 
                              Apyvarta2_sum, 
                            data = dataset_19Q, max_order = 2)
summary(model14e$best_model) # R2 = 0.9883, p-value: 0.02344

# Redefine the model using multiple regression linear approximaiton funciton from the dyn package
model14e_dyn <- dyn::dyn$lm(y1_150 ~ stats::lag(y1_150, -1) + 
                              stats::lag(y1_150, -2) +
                              s_exp)

summary(model14e_dyn)


model14f <- ARDL::auto_ardl(y1_150 ~ s_exp + 
                               s_imp + 
                              # g_exp + 
                              # g_imp + # gs_exp + gs_imp + 
                              # gfc2008 + crysisLT09 + 
                              #   Cov19 + 
                              # Quarantine +
                              # supplyChainCrysis + 
                              # WarUkraine, # + 
                              L_nfin_tot + 
                              D_nfin_tot + 
                              IndProd_tot, 
                              #Apyvarta2_sum, 
                            data = dataset_19Q, max_order = 2)
summary(model14f$best_model) # R2 = 0.993, p-value: 0.01094             -  Risk of an overfit

# Redefine the model using another package's functions library(dyn)
model14f_dyn <- dyn::dyn$lm(y1_150 ~ stats::lag(y1_150, -1) + 
                              stats::lag(y1_150, -2) +
                              #s_exp +
                              #s_imp +
                              #L_nfin_tot + 
                              #IndProd_tot +
                              stats::lag(IndProd_tot, -1) +
                              stats::lag(IndProd_tot, -2))
# As we're predicting t+1 period, the equation above cannot contain any variables (save for the dependent variable y1) that are in current time [t0]
summary(model14f_dyn)
plot(model14f_dyn)
pred3 <- predict(model14f_dyn, 1) # Predict one step ahead
plot(pred3)
print(pred3)
pred3_df <- as.data.frame(pred3)
plot(residuals(model14f_dyn))
pred4 <- predict(model14f_dyn, 2) # Try to predict 2 steps ahead (will not work, because of the model specification (time sensitive variables in the equation))
plot(pred4)
acf(model14f_dyn$residuals)
plot.ts(model14f_dyn$fitted.values)
model14f_fitted_values <- model14f_dyn$fitted.values
model14f_fitted_values_df <- as.data.frame(model14f_fitted_values)
writexl::write_xlsx(pred3_df, "Prediction_ARDL_14f_Y1_dyn.xlsx") # Save the predicted values to Excel file
writexl::write_xlsx(model14f_fitted_values_df, "Fitted_ARDL_14f_Y1_dyn.xlsx") # Save the fitted values to Excel file

#################
# Disaggregation
#################

library(tempdisagg)

B0904 <- read_excel("Duomenys/Dataset_v4.3.xlsx", sheet = "B09-04")

y_monthly_seas <- decompose(ts(B0904$`150 - Įsiskolinimai nerezidentams už prekes ir paslaugas`, start = c(2016,1), frequency = 12))$seasonal

#y_monthly_seas <- ts(y_monthly_seas[time(y_monthly_seas) <= max(time(y))], start = c(2016,1), frequency = 12) # approx(?) same result as from line above (decompose()$seasonal )

y_m_disaggregate <- tempdisagg::td(y1_150 ~ y_monthly_seas, conversion = "last", to = "monthly")
plot(y_m_disaggregate)
y_m_disaggregate$values
y_m_disaggregate$fitted.values


plot(predict(y_m_disaggregate),
     sub = "B. Finansiniai įsipareigojimai nerezidentams",
     main = "Y1 - 150 Įsiskolinimai nerezidentams už prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y1_150, start = c(2016, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))

# Export disaggregated time series
Y1_disagg_export <- as.data.frame(predict(y_m_disaggregate))
class(Y1_disagg_export)


writexl::write_xlsx(Y1_disagg_export, "Y1 disagg export.xlsx")

###
# Disaggregated forecast
###

# Before this step, forecasted values have been manually copied to Dataset_v4.6.xlsx sheet named "Plots2"
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

y1_m_disaggregate <- tempdisagg::td(y1_150_forecast ~ y1_monthly_seas, conversion = "last", to = "monthly")

plot(predict(y1_m_disaggregate),
     sub = "B. Finansiniai įsipareigojimai nerezidentams",
     main = "Y1 - 150 Įsiskolinimai nerezidentams už prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(y1_150_forecast, start = c(2016, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))


y1_m_disagg_forecast_export <- as.data.frame(predict(y1_m_disaggregate))

y1_150_forecast

plot(y1_monthly_seas)







#predict(model14f_dyn, 1)

###########
# Model 15
###########

# Rescale some of the data

model15 <- dyn::dyn$lm(y1ii ~ stats::lag(y1ii, 1) + diff(s_exp) + stats::lag(diff(s_exp), 1) + stats::lag(diff(s_exp), 2) + 
                         diff(s_imp) + stats::lag(diff(s_imp), 1) + diff(g_exp) + 
                         diff(g_imp) + stats::lag(diff(g_imp), 1) + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                         Cov19 + stats::lag(Cov19, 1) + #Quarantine +
                         supplyChainCrysis + stats::lag(supplyChainCrysis, 1) + #WarUkraine + 
                         diff(L_nfin_tot) + stats::lag(diff(L_nfin_tot), 1) + diff(D_nfin_tot) + stats::lag(diff(D_nfin_tot), 1) + 
                         diff(IndProd_tot) + stats::lag(diff(IndProd_tot), 1) + diff(Apyvarta_sum) #+ stats::lag(diff(Apyvarta_sum), 1)
                        , 
                       data = dataset_15Q)

summary(model15)
?dyn

###########
# Model 16
###########


model16 <- dyn::dyn$lm(y1ii ~ y1ii + s_exp + s_exp + s_exp + 
                         s_imp + s_imp + g_exp + 
                         g_imp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                         Cov19 + Cov19 + #Quarantine +
                         supplyChainCrysis + supplyChainCrysis + #WarUkraine + 
                         L_nfin_tot + L_nfin_tot + D_nfin_tot + D_nfin_tot + 
                         IndProd_tot + IndProd_tot + Apyvarta_sum #+ stats::lag(diff(Apyvarta_sum), 1)
                       , 
                       data = dataset_15Q)
summary(model16)
model16$


###########
# UT Įsiskolinimai už prekes ir paslaugas
###########

Indeliai <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/Copy of Indėlių_saltiniai - Rokui.xlsx", sheet = 5, skip = 2)
Indeliai <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/Copy of Indėlių_saltiniai - Rokui_V1.xlsx", sheet = 5, skip = 2)
#readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/Copy of Indėlių_saltiniai - Rokui.xlsx", sheet = 2)

# Model 1

model1_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                           data = dataset_15Q, max_order = 5)
summary(model1_ui$best_model)
model1_ui$best_model$full_formula # Best model's formula
plot(model1_ui$best_model)
acf(residuals(model1_ui$best_model))


# Model 2

model2_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model2_ui$best_model)
model2_ui$best_model$full_formula # Best model's formula
plot(model2_ui$best_model)
acf(residuals(model2_ui$best_model))

# Model 3
model3_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model3_ui$best_model)
model3_ui$best_model$full_formula # Best model's formula
plot(model3_ui$best_model)
acf(residuals(model3_ui$best_model))


# Model 4
model4_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model4_ui$best_model)
model4_ui$best_model$full_formula # Best model's formula
plot(model4_ui$best_model)
acf(residuals(model4_ui$best_model))



# Model 5
model5_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model5_ui$best_model)
model5_ui$best_model$full_formula # Best model's formula
plot(model5_ui$best_model)
acf(residuals(model5_ui$best_model))


# Model 6
model6_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model6_ui$best_model)
model6_ui$best_model$full_formula # Best model's formula
plot(model6_ui$best_model)
acf(residuals(model6_ui$best_model))


# Model 7
model7_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model7_ui$best_model)
model7_ui$best_model$full_formula # Best model's formula
plot(model7_ui$best_model)
acf(residuals(model7_ui$best_model))



# Model 8
model8_ui <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp,# + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                             #Cov19 + #Quarantine +
                             #supplyChainCrysis, #+ #WarUkraine + 
                             #L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model8_ui$best_model)
model8_ui$best_model$full_formula # Best model's formula
plot(model8_ui$best_model)
acf(residuals(model8_ui$best_model))




###########
# UT Pinigai ir indėliai (Lietuvos turtas užsienyje)
###########

# Pagal tyrimus, įtaką šiam rodikliui turi šie kintamieji:
# 1. Paskolos (stock)
# 2. Indėliai (su 1 periodo lag'u)
# 3. Einamoji sąskaita:
# 3.1 Prekės
# 3.2 Paslaugos
# 3.3 Piriminės pajamos
# 3.4 Antrinės pajamos
# 4. Kapitalo sąskaitos balansas
# 5. Valdžios sektoriaus skola https://osp.stat.gov.lt/statistiniu-rodikliu-analize#/
# 6. ES parama (Violeta? Antrinės pajamos)


CVSkola <- readxl::read_xlsx("Duomenys/išoriniai makro rodikliai/Centrinės valdžios skola LSD.xlsx", skip = 2)


# Model 1

model1_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model1_pi$best_model)
model1_pi$best_model$full_formula # Best model's formula
plot(model1_pi$best_model)
acf(residuals(model1_pi$best_model))


# Model 2
model2_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model2_pi$best_model)
model2_pi$best_model$full_formula # Best model's formula
plot(model2_pi$best_model)
acf(residuals(model2_pi$best_model))


# Model 3
model3_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model3_pi$best_model)
model3_pi$best_model$full_formula # Best model's formula
plot(model3_pi$best_model)
acf(residuals(model3_pi$best_model))


# Model 4
model4_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(mode41_pi$best_model)
model4_pi$best_model$full_formula # Best model's formula
plot(model4_pi$best_model)
acf(residuals(model4_pi$best_model))


# Model 5
model5_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model5_pi$best_model)
model5_pi$best_model$full_formula # Best model's formula
plot(model5_pi$best_model)
acf(residuals(model5_pi$best_model))


# Model 6
model6_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model6_pi$best_model)
model6_pi$best_model$full_formula # Best model's formula
plot(model6_pi$best_model)
acf(residuals(model6_pi$best_model))


# Model 7
model7_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model7_pi$best_model)
model7_pi$best_model$full_formula # Best model's formula
plot(model7_pi$best_model)
acf(residuals(model7_pi$best_model))


# Model 8
model8_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model8_pi$best_model)
model8_pi$best_model$full_formula # Best model's formula
plot(model8_pi$best_model)
acf(residuals(model8_pi$best_model))


# Model 9
model9_pi <- ARDL::auto_ardl(y2ti ~ y1ii + y3pi + s_exp + s_imp + g_exp + g_imp + #gs_exp + gs_imp + gfc2008 + crysisLT09 + 
                               Cov19 + #Quarantine +
                               supplyChainCrysis + #WarUkraine + 
                               L_nfin_tot + D_nfin_tot + IndProd_tot + Apyvarta_sum, 
                             data = dataset_15Q, max_order = 5)
summary(model9_pi$best_model)
model9_pi$best_model$full_formula # Best model's formula
plot(model9_pi$best_model)
acf(residuals(model9_pi$best_model))

#############
# 
#############

