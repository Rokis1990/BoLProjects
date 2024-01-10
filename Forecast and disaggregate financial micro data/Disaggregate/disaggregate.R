library(readxl)
library(imputeTS)
library(gridExtra)
library(lsa)
library(tempdisagg)


M <- read_excel("Laiko eilutės_2022-06-02_v1.1_be.xlsx", "Mėnesiniai")
KM <- read_excel("Laiko eilutės_2022-06-02_v1.1_be.xlsx", "Ketvirtiniai_menesiniai")
K <- read_excel("Laiko eilutės_2022-06-02_v1.1_be.xlsx", "Ketvirtiniai")

# mseas <- decompose(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)$seasonal
# error

# ISIP
mseas1 <- decompose(ts(M$`ISIP. Įsiskolinimai už prekes ir paslaugas`, start = c(2015,1), frequency = 12))$seasonal
#plot(mseas1)

mseas1 <- ts(mseas1, start = c(2015, 1), frequency = 12)
IK <- ts(K$`ISIP. Įsiskolinimai už prekes ir paslaugas`, start = c(2015, 1), frequency = 4)

y_m_disaggregate1 <- tempdisagg::td(IK ~ mseas1, conversion = "last", to = 12)
plot(predict(y_m_disaggregate1),
     main="ISIP isiskolinimai uz prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(IK, start = c(2015, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))

# Żpredict(y_m_disaggregate)

# plot(lag(predict(y_m_disaggregate), 2))
# lines(lag(IIK, 0), col='red')

##### UT isiskolinimai
mseas2 <- decompose(ts(M$`UT. Įsiskolinimai už prekes ir paslaugas`, start = c(2015,1), frequency = 12))$seasonal
#plot(mseas2)

mseas2 <- ts(mseas2, start = c(2015, 1), frequency = 12)
IIK <- ts(K$`UT. Įsiskolinimai už prekes ir paslaugas`, start = c(2015, 1), frequency = 4)

y_m_disaggregate2 <- tempdisagg::td(IIK ~ mseas2, conversion = "last", to = 12)
plot(predict(y_m_disaggregate2),
     main="UT isiskolinimai uz prekes ir paslaugas",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(IIK, start = c(2015, 1.67), frequency = 4), col="red")
legend("bottomleft", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))


##### UT Pinigai ir indeliai
mseas2 <- ts(mseas2, start = c(2015, 1), frequency = 12)
IIIK <- ts(K$`UT. Pinigai ir indėliai`, start = c(2015, 1), frequency = 4)

y_m_disaggregate3 <- tempdisagg::td(IIIK ~ mseas2, conversion = "last", to = 12)
plot(predict(y_m_disaggregate3),
     main="UT pinigai ir indeliai",
     xlab="Laikas",
     ylab="mln. Eur")
lines(ts(IIIK, start = c(2015, 1.67), frequency = 4), col="red")
legend("bottom", c("Menesiniai disagreguoti", "Ketvirtiniai"),
       lty = c(1,1),
       lwd = c(1,1),
       cex=0.5,
       col = c("black", "red"))


