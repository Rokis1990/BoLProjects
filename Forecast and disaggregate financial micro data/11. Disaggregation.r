install.packages('tempdisagg', repos='https://mran.microsoft.com/snapshot/2022-06-20/', dependencies = TRUE)
library(tempdisagg)

?td
#library(tempdisagg)
data(swisspharma)
m1 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")
predict(m1)

plot(sales.a)
view(sales.a)
plot(sales.q)
view(sales.q)

y_q <- y

y_monthly_seas <- decompose(ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015,1), frequency = 12))$seasonal

y_monthly_seas <- ts(y_monthly_seas[time(y_monthly_seas) <= max(time(y))], start = c(2015,1), frequency = 12)


view(tempdisagg::swisspharma)
view(tempdisagg::swisspharma)

y_m_disaggregate <- tempdisagg::td(y_q ~ y_monthly_seas, conversion = "last", to = "monthly")

plot(predict(y_m_disaggregate), type = "o")
lines(ts(y_q, start = c(2015, 1), frequency = 4), type = "b")
##
y_m_disaggregate <- tempdisagg::td(y_q ~ y_monthly_seas, conversion = "first", to = "monthly")

plot(predict(y_m_disaggregate))
lines(ts(Ketvirtiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 1), frequency = 4))

max(time(y_monthly_seas))
