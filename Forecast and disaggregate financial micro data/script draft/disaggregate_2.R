library(readxl)
library(imputeTS)
library(gridExtra)
library(lsa)
library(tempdisagg)

K <- read_excel("Copy of Dataset_v3.xlsx", "Plots")
data19  <- read_excel("Copy of Dataset_v3.xlsx", "Sheet1")
nerezIsiskolPrekPasl<- ts(data19$`110 srautas - nerezidentų įsiskolinimai už prekes ir paslaugas`, start = 2015, frequency = 12)
plot(decompose(nerezIsiskolPrekPasl)$trend)
plot(decompose(nerezIsiskolPrekPasl)$seasonal)

seasonal110 <- decompose(nerezIsiskolPrekPasl)$seasonal
trend110 <- decompose(nerezIsiskolPrekPasl)$trend

RK <- K[nrow(K):1, ]

SR <- ts(RK$`F0613xx+TUI0122xx+TUI0132xx`, start = c(2019,1), frequency = 4)
SR

plot(SR)

#de_trend <- decompose(ts(RK$`F0613xx+TUI0122xx+TUI0132xx`, start = c(2019,1), frequency = 4))$trend
#de_trend <- ts(de_trend, start = c(2019, 1), frequency = 12)
#plot(de_trend)


disagg <- td(SR ~ seasonal110, to = "monthly", method = "fast")
disagg <- td(SR ~ seasonal110, conversion = "last", to = 12)

predict(disagg$fitted.values)

plot(predict(disagg),
     main="AAA",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(SR, start = c(2019, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))







#disagg <- tempdisagg::td(SR ~ de_trend, conversion = "last", to = 12)
#plot(predict(disagg),
     #main="AAA",
     #xlab="Laikas",
     #ylab="mln. Eur")
#lines(ts(SR, start = c(2019, 1.67), frequency = 4), col="red")
#legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       #lty = c(1,1),
       #lwd = c(1,1),
       #cex=0.5,
       #col = c("black", "red"))


#SR_monthly <- naive(SR)

#SR_monthly <- td(SR, method = "stl", aggregation = "sum", seasonality = 4, t0 = c(2019, 1), 
                 #ts.frequency = 4, start = c(2019, 1), end = c(2022, 12))

#library(forecast)
#quarterly_ts <- c(131498, 13059, 190664, -202159, -96710, -346929, 213830, 206731, 2654, 90562, 470687, 151839, 669193, -373381, -120027)
#quarterly_ts <- ts(quarterly_ts, frequency = 4)
#monthly_ts <- stl(quarterly_ts, s.window = "periodic")$time.series[, 1:12]
#monthly_ts <- rowMeans(monthly_ts)