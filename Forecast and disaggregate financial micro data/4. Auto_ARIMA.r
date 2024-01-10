#Create a Box Plot by Cycle

boxplot(AirPassengers~cycle(AirPassengers, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))

#From the above plot, you can see that the number of ticket sales goes higher in June, July, and August as compared to the other months of the years. 

#Build the ARIMA Model Using auto.arima() Function
mymodel <- auto.arima(AirPassengers)

mymodel


#Plot the Residuals
plot.ts(mymodel$residuals)


#Forecast the Values for the Next 10 Years
myforecast <- forecast(mymodel, level=c(95), h=10*12)

plot(myforecast)


#Validate the Model by Selecting Lag Values
Box.test(mymodel$resid, lag=5, type="Ljung-Box")

Box.test(mymodel$resid, lag=10, type="Ljung-Box")

Box.test(mymodel$resid, lag=15, type="Ljung-Box")

###############
boxplot(y ~ cycle(y, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))

y <- ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 1), frequency=12)
boxplot(y ~ cycle(y, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))

mymodel <- auto.arima(y)

mymodel

plot.ts(mymodel$residuals)

myforecast <- forecast(mymodel, level=c(95), h=10*12)

plot(myforecast)

Box.test(mymodel$resid, lag=5, type="Ljung-Box")

Box.test(mymodel$resid, lag=10, type="Ljung-Box")

Box.test(mymodel$resid, lag=15, type="Ljung-Box")

checkresiduals(mymodel)
