####
# Imputation vs Disaggregation
####

install.packages("readxl")
install.packages("imputeTS")
install.packages("gridExtra")
library(readxl)
library(imputeTS)
library(gridExtra)

L <- read_excel("Duomenys/Copy of Laiko eilutės_2022-06-02_v1.1_bepirmos.xlsx")
#plot(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)

#plot(L$Date, L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
#lines(L$Date ,L$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="red")

#plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l")  
#lines(IP$Date, IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")

Menesiniai_names <- names(Menesiniai)
names(Menesiniai) <- c("Date", "Menuo", "ISIP..Įsiskolinimai.už.prekes.ir.paslaugas", 
                       "UT..Pinigai.ir.indėliai", "UT..Įsiskolinimai.už.prekes.ir.paslaugas")

ts.plot(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas)
ts.plot(Menesiniai$UT..Pinigai.ir.indėliai)
ts.plot(Menesiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas)
seasonplot(ts(Menesiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015,1), frequency = 12))
tsdisplay(ts(Menesiniai$UT..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015,1), frequency = 12))

plot(decompose(ts(Ketvirtiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015,1), frequency = 4)))
plot(decompose(ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015,1), frequency = 12)))

##### na_interpolation funkcija
IP <- L
IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_interpolation(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "linear")
IP$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(IP$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_interpolation(IP$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "linear")
IP$`UT. Pinigai ir indėliai`[is.na(IP$`UT. Pinigai ir indėliai`)] <- na_interpolation(IP$`UT. Pinigai ir indėliai`, option = "linear")


plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(IP$Date, IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(IP$Date, Menesiniai$`Orig_ISIP..Įsiskolinimai.už.prekes.ir.paslaugas`, col = "black")
legend("bottomleft", c("Ketvirciai ISIP", "na_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))



plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(IP$Date, IP$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(IP$Date, IP$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))



##### na_kalman funkcija
KM <- L
KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
KM$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KM$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KM$`UT. Įsiskolinimai už prekes ir paslaugas`)
KM$`UT. Pinigai ir indėliai`[is.na(KM$`UT. Pinigai ir indėliai`)] <- na_kalman(KM$`UT. Pinigai ir indėliai`)

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_kalman",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KM$Date, KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_kalman"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_kalman",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KM$Date, KM$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_kalman"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_kalman",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(KM$Date, KM$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_kalman"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

# model="StructTS"
KMS <- L
KMS$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KMS$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`,model="StructTS")
KMS$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KMS$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(L$`UT. Įsiskolinimai už prekes ir paslaugas`,model="StructTS")
KMS$`UT. Pinigai ir indėliai`[is.na(KMS$`UT. Pinigai ir indėliai`)] <- na_kalman(L$`UT. Pinigai ir indėliai`,model="StructTS")
# Default variantas (kaip KM) turetu buti su auto.arima, bet kazkodel yra su StructTS.

#plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
#     main="Ketvirciai ir na_kalman_StructTS",
#     xlab="Laikas",
#    ylab="mln. Eur")
#points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
#lines(KMS$Date, KMS$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
#legend("bottomleft", c("Ketvirciai (turimi)", "na_kalman_StructTS"),
#       lty = c(1,1),
#       lwd = c(2,1),
#       cex=0.5,
#       col = c("red", "blue"))

# model="auto.arima"
KMA <- L
KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`,model="auto.arima")
KMA$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KMA$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KMA$`UT. Įsiskolinimai už prekes ir paslaugas`,model="auto.arima")
KMA$`UT. Pinigai ir indėliai`[is.na(KMA$`UT. Pinigai ir indėliai`)] <- na_kalman(KMA$`UT. Pinigai ir indėliai`,model="auto.arima")
# Uzpildo nuliais arba buvusiom reiksmem.
# Nera neigiamu reiksmiu!

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_kalman_auto.arima",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KMA$Date, KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_ikalman_auto.arima"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_kalman_auto.arima",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KMA$Date, KMA$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_kalman_auto.arima"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_kalman_auto.arima",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(KMA$Date, KMA$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_kalman_auto.arima"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


##### na_locf funkcija
LCF <- L
LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_locf(LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
LCF$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(LCF$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_locf(LCF$`UT. Įsiskolinimai už prekes ir paslaugas`)
LCF$`UT. Pinigai ir indėliai`[is.na(LCF$`UT. Pinigai ir indėliai`)] <- na_locf(LCF$`UT. Pinigai ir indėliai`)

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(LCF$Date, LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_locf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(LCF$Date, LCF$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_locf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(LCF$Date, LCF$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_locf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


##### na_ma funkcija 
MA <- L
MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
MA$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MA$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MA$`UT. Įsiskolinimai už prekes ir paslaugas`)
MA$`UT. Pinigai ir indėliai`[is.na(MA$`UT. Pinigai ir indėliai`)] <- na_ma(MA$`UT. Pinigai ir indėliai`)

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_ma",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MA$Date, MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_ma"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MA$Date, MA$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_ma"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MA$Date, MA$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_ma"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))
# Linear
MAL <- L
MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, weighting = "linear")
MAL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MAL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAL$`UT. Įsiskolinimai už prekes ir paslaugas`, weighting = "linear")
MAL$`UT. Pinigai ir indėliai`[is.na(MAL$`UT. Pinigai ir indėliai`)] <- na_ma(MAL$`UT. Pinigai ir indėliai`, weighting = "linear")

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_ma_linear",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAL$Date, MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_ma_linear"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma_linear",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAL$Date, MAL$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_ma_linear"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma_linear",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MAL$Date, MAL$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_ma_linear"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))
# Simple
MAS <- L
MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`, weighting = "simple")
MAS$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MAS$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAS$`UT. Įsiskolinimai už prekes ir paslaugas`, weighting = "simple")
MAS$`UT. Pinigai ir indėliai`[is.na(MAS$`UT. Pinigai ir indėliai`)] <- na_ma(MAS$`UT. Pinigai ir indėliai`, weighting = "simple")

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_ma_simple",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAS$Date, MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_ma_simple"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma_simple",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAS$Date, MAS$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_ma_simple"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma_simple",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MAS$Date, MAS$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_ma_simple"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

##### na_mean funkcija
# mean
MN <- L
MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "mean")
MN$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MN$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MN$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "mean")
MN$`UT. Pinigai ir indėliai`[is.na(MN$`UT. Pinigai ir indėliai`)] <- na_mean(MN$`UT. Pinigai ir indėliai`, option = "mean")

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_mean",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MN$Date, MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_mean"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MN$Date, MN$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_mean"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MN$Date, MN$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_mean"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))
# median
MNM <- L
MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "median")
MNM$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MNM$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNM$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "median")
MNM$`UT. Pinigai ir indėliai`[is.na(MNM$`UT. Pinigai ir indėliai`)] <- na_mean(MNM$`UT. Pinigai ir indėliai`, option = "median")

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_mean_median",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MNM$Date, MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_mean_median"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean_median",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MNM$Date, MNM$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_mean_median"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean_median",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MNM$Date, MNM$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_mean_median"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

# mode
MND <- L
MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "mode")
MND$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MND$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MND$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "mode")
MND$`UT. Pinigai ir indėliai`[is.na(MND$`UT. Pinigai ir indėliai`)] <- na_mean(MND$`UT. Pinigai ir indėliai`, option = "mode")

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_mean_mode",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MND$Date, MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_mean_mode"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean_mode",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MND$Date, MND$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_mean_mode"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean_mode",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MND$Date, MND$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_mean_mode"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

# harmonic mean
#MNH <- L
#MNH$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MNH$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNH$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "harmonic")
#MNH$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MNH$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNH$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "harmonic")
#MNH$`UT. Pinigai ir indėliai`[is.na(MNH$`UT. Pinigai ir indėliai`)] <- na_mean(MNH$`UT. Pinigai ir indėliai`, option = "harmonic")

#netinka

##### na_random funkcija
RND <- L
RND$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(RND$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_random(RND$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
RND$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(RND$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_random(RND$`UT. Įsiskolinimai už prekes ir paslaugas`)
RND$`UT. Pinigai ir indėliai`[is.na(RND$`UT. Pinigai ir indėliai`)] <- na_random(RND$`UT. Pinigai ir indėliai`)

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_random",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(RND$Date, RND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_random"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_random",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(RND$Date, RND$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai UT isiskolinimai", "na_random"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_random",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(RND$Date, RND$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_random"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

##### na_seadec
# algorithm="interpolation" (default)
SIP <- L
SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SIP$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SIP$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SIP$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SIP$`UT. Pinigai ir indėliai`[is.na(SIP$`UT. Pinigai ir indėliai`)] <- na_seadec(SIP$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)
# ??? ERROR darant antra stulpeli.

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_seadec_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(SIP$Date, SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_iseadec_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seadec_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SIP$Date, SIP$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_seadec_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

# algorithm="locf"
SLCF <- L
SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "locf", find_frequency=TRUE)
SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "locf", find_frequency=TRUE)
SLCF$`UT. Pinigai ir indėliai`[is.na(SLCF$`UT. Pinigai ir indėliai`)] <- na_seadec(SLCF$`UT. Pinigai ir indėliai`, algorithm = "locf", find_frequency=TRUE)

# ??? ERROR darant antra stulpeli.

plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai ISIP ir na_seadec_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(SLCF$Date, SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")
legend("bottomleft", c("Ketvirciai ISIP", "na_seadec_locf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seadec_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SLCF$Date, SLCF$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_seadec_locf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))

# Dar yra pasirinkimai su mean, random, kalman, ma.

##### na_seasplit
# su algorithm="interpolation"
SPL <- L
SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SPL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPL$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPL$`UT. Pinigai ir indėliai`[is.na(SPL$`UT. Pinigai ir indėliai`)] <- na_seasplit(SPL$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)
#??? ERROR 


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seasplit_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SPL$Date, SPL$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_seasplit_interpolation"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))



# su algorithm="locf"
SPLL <- L
SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPLL$`UT. Pinigai ir indėliai`[is.na(SPLL$`UT. Pinigai ir indėliai`)] <- na_seasplit(SPLL$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)
#??? ERROR


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seasplit_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SPLL$Date, SPLL$`UT. Pinigai ir indėliai`, col="blue")
legend("bottomleft", c("Ketvirciai UT pinigai ir indeliai", "na_seasplit_ocf"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.5,
       col = c("red", "blue"))
                

