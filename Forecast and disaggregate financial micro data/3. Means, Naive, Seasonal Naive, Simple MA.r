# https://www.mssqltips.com/sqlservertip/6778/time-series-forecasting-methods-with-r-examples/

library(forecast)
y <- ts(c(4,8,12,14,22,27,14,17,17,10,12,16,22,15,11), start=2006)
autoplot(y, xlab = "Year", ylab = "Number of Newly Joined Members")

y <- ts(Ketvirtiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = 2015, frequency = 4)
autoplot(y, xlab = "Year", ylab = "Number of Newly Joined Members")

y <- ts(Ketvirtiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = 2015, frequency = 4)
autoplot(y, xlab = "Year", ylab = "Number of Newly Joined Members")

y <- ts(Ketvirtiniai$UT..Pinigai.ir.indėliai, start = 2015, frequency = 4)
autoplot(y, xlab = "Year", ylab = "Number of Newly Joined Members")


library(fpp2)
aelec <- window(elec, start=1980)
autoplot(aelec, xlab ="Year", ylab = "GWh")
library(fpp2)
library(forecast)
aelec <- window(elec, start=1980)
aelecComp <- decompose(aelec,type=c("additive"))
autoplot(aelecComp)

y_dec <- decompose(y, type = "additive")
autoplot(y_dec)

y_dec$x
y_dec$seasonal
y_dec$trend
y_dec$random
y_dec$figure


########################
# Forecst - Means method

library(forecast)
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))

# Use meanf() to forecast quarterly contributions in 2021
contributions.fc <- meanf(contributions.ts, h=4)

# Plot and summarize the forecasts
autoplot(contributions.fc,xlab = "Year", ylab = "Contributions")
summary(contributions.fc)


## 

y_f_means <- meanf(contributions.ts, h=4)
# Plot and summarize the forecasts
autoplot(y_f_means,xlab = "Year", ylab = "Contributions")
summary(y_f_means)

########################
# Forecast - Naive method (Baseline)
library(forecast)
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))

# Use naive() to forecast contributions in 2021
contributions.fc <- naive(contributions.ts, h=4)

# Plot and summarize the forecasts
autoplot(contributions.fc)
summary(contributions.fc)

##

y_f_naive <- naive(y, h = 4)
autoplot(y_f_naive)
summary(y_f_naive)

########################
# Forecast - Seasonal Naive method


# Use snaive() to forecast sales in 2021
y_f_snaive <- snaive(y, h=4)

# Plot and summarize the forecasts
autoplot(y_f_snaive)
summary(y_f_snaive)


#########################
# Forecast - The simple moving average method
library(smooth)
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))

# Use sma() to forecast number of Aaron's contributions in 2021
contributions.fc <- sma(contributions.ts, order=4, h=4,silent=FALSE)
# Print model summary
summary(contributions.fc)
# Print the forecasts
fc <- forecast(contributions.fc)
print(fc)

#
y_f_sma <- sma(y, order = 4, h = 4, silent = FALSE)

#########################
# Accuracy metrics - The Mean Absolute Deviation (MAD) / Mean Absolute Error


#########################
# Accuracy metrics - The Mean Absolute Percent Error (MAPE)


#########################
# Accuracy metrics - The Root Mean Squared Error (RMSE)


#########################
# Residuals

## Residual Assumptions:
# 1. Independence: the residuals associated with any two different observations are independent, i.e., the residuals are uncorrelated.
# 2. Unbiasedness: the mean value of the residuals is zero in any thin vertical rectangle in the residual plot. The forecasts are biased if the mean value differs from zero.
# 3. Homoscedasticity: the standard deviation of the errors is the same in any thin rectangle in the residual plot.
# 4. Normality: at any observation, the error component has a normal distribution.


checkresiduals(y_f_means) 
# ACF first lag
# residuals - Bimodal distribution



checkresiduals(y_f_naive)
checkresiduals(y_f_snaive)
checkresiduals(y_f_sma)
plot(residuals(y_f_sma))
acf(y_f_sma)
