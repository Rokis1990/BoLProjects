install.packages("readxl")
install.packages("imputeTS")
install.packages("gridExtra")
install.packages("lsa")
library(readxl)
library(imputeTS)
library(gridExtra)
library(lsa)

L <- read_excel("Copy of Menes Laiko eilutės_2022-06-02_v1.2_bepirmos.xlsx")
#plot(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)

#plot(L$Date, L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
#lines(L$Date ,L$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="red")

#plot(L$Date[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],L$`ISIP. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`)],type="l")  
#lines(IP$Date, IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, col="blue")

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_interpolation", "Originalus ISIP"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(IP$Date, IP$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_interpolation", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(IP$Date, IP$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_interpolation", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse"))

sum(cosine(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, IP$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(IP$`UT. Įsiskolinimai už prekes ir paslaugas`, IP$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(IP$`UT. Pinigai ir indėliai`, IP$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_kalman", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_kalman",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KM$Date, KM$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_kalman", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_kalman",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(KM$Date, KM$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_kalman", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, KM$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(KM$`UT. Įsiskolinimai už prekes ir paslaugas`, KM$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(KM$`UT. Pinigai ir indėliai`, KM$`Orig_UT. Pinigai ir indėliai`))

cosine(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, KM$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)
cosine(KM$`UT. Įsiskolinimai už prekes ir paslaugas`, KM$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`)
cosine(KM$`UT. Pinigai ir indėliai`, KM$`Orig_UT. Pinigai ir indėliai`)


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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_ikalman_auto.arima", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_kalman_auto.arima",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(KMA$Date, KMA$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_kalman_auto.arima", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_kalman_auto.arima",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(KMA$Date, KMA$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_kalman_auto.arima", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`, KMA$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(KMA$`UT. Įsiskolinimai už prekes ir paslaugas`, KMA$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(KMA$`UT. Pinigai ir indėliai`, KMA$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_locf", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(LCF$Date, LCF$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_locf", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(LCF$Date, LCF$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_locf", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, LCF$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(LCF$`UT. Įsiskolinimai už prekes ir paslaugas`, LCF$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(LCF$`UT. Pinigai ir indėliai`, LCF$`Orig_UT. Pinigai ir indėliai`))


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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_ma", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MA$Date, MA$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_ma", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MA$Date, MA$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_ma", "originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue","chartreuse3"))

sum(cosine(MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MA$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MA$`UT. Įsiskolinimai už prekes ir paslaugas`, MA$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MA$`UT. Pinigai ir indėliai`, MA$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_ma_linear","Originalus ISIP isiskolinimai"),
       lty = c(1,1),
       lwd = c(2,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma_linear",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAL$Date, MAL$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_ma_linear","Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma_linear",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MAL$Date, MAL$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_ma_linear", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MAL$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MAL$`UT. Įsiskolinimai už prekes ir paslaugas`, MAL$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MAL$`UT. Pinigai ir indėliai`, MAL$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_ma_simple", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue","chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_ma_simple",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MAS$Date, MAS$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_ma_simple", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_ma_simple",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MAS$Date, MAS$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_ma_simple", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MAS$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MAS$`UT. Įsiskolinimai už prekes ir paslaugas`, MAS$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MAS$`UT. Pinigai ir indėliai`, MAS$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_mean", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MN$Date, MN$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_mean", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue","chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MN$Date, MN$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_mean", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MN$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MN$`UT. Įsiskolinimai už prekes ir paslaugas`, MN$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MN$`UT. Pinigai ir indėliai`, MN$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_mean_median", "Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean_median",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MNM$Date, MNM$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_mean_median", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean_median",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MNM$Date, MNM$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_mean_median", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MNM$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MNM$`UT. Įsiskolinimai už prekes ir paslaugas`, MNM$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MNM$`UT. Pinigai ir indėliai`, MNM$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai ISIP", "na_mean_mode","Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_mean_mode",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(MND$Date, MND$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_mean_mode", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_mean_mode",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(MND$Date, MND$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_mean_mode", "originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, MND$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(MND$`UT. Įsiskolinimai už prekes ir paslaugas`, MND$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(MND$`UT. Pinigai ir indėliai`, MND$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_random", " Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

plot(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT isiskolinimai ir na_random",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)],L$`UT. Įsiskolinimai už prekes ir paslaugas`[!is.na(L$`UT. Įsiskolinimai už prekes ir paslaugas`)], pch=20, col="red")
lines(RND$Date, RND$`UT. Įsiskolinimai už prekes ir paslaugas`, col="blue")
lines(L$Date, L$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("top", c("Ketvirciai UT isiskolinimai", "na_random", "Originalus UT isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_random",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(RND$Date, RND$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_random", "chartreuse3"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

sum(cosine(RND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, RND$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`),
cosine(RND$`UT. Įsiskolinimai už prekes ir paslaugas`, RND$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`),
cosine(RND$`UT. Pinigai ir indėliai`, RND$`Orig_UT. Pinigai ir indėliai`))

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_iseadec_interpolation", "originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seadec_interpolation",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SIP$Date, SIP$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_seadec_interpolation", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse"))

cosine(SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, SIP$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)
cosine(SIP$`UT. Įsiskolinimai už prekes ir paslaugas`, SIP$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`)
cosine(SIP$`UT. Pinigai ir indėliai`, SIP$`Orig_UT. Pinigai ir indėliai`)

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
lines(L$Date, L$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`, col="chartreuse3", lwd=1)
legend("bottomleft", c("Ketvirciai ISIP", "na_seadec_locf", " Originalus ISIP isiskolinimai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))


plot(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)], L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)],type="l", col="red", lty = "solid", lwd = 2,
     main="Ketvirciai UT pinigai ir indeliai ir na_seadec_locf",
     xlab="Laikas",
     ylab="mln. Eur")
points(L$Date[!is.na(L$`UT. Pinigai ir indėliai`)],L$`UT. Pinigai ir indėliai`[!is.na(L$`UT. Pinigai ir indėliai`)], pch=20, col="red")
lines(SLCF$Date, SLCF$`UT. Pinigai ir indėliai`, col="blue")
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_seadec_locf", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

cosine(SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, SLCF$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)
cosine(SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`, SLCF$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`)
cosine(SLCF$`UT. Pinigai ir indėliai`, SLCF$`Orig_UT. Pinigai ir indėliai`)

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
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_seasplit_interpolation", "Originalus UT pinigai ir indeliai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.5,
       col = c("red", "blue", " chartreuse3"))

cosine(SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, SPL$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)
cosine(SPL$`UT. Įsiskolinimai už prekes ir paslaugas`, SPL$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`)
cosine(SPL$`UT. Pinigai ir indėliai`, SPL$`Orig_UT. Pinigai ir indėliai`)

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
lines(L$Date, L$`Orig_UT. Pinigai ir indėliai`, col="chartreuse3", lwd=1)
legend("bottom", c("Ketvirciai UT pinigai ir indeliai", "na_seasplit_ocf", "Originalus UT pinigai ir ketvirciai"),
       lty = c(1,1,1),
       lwd = c(2,1,1),
       cex=0.4,
       col = c("red", "blue", "chartreuse3"))

cosine(SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, SPLL$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)
cosine(SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`, SPLL$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`)
cosine(SPLL$`UT. Pinigai ir indėliai`, SPLL$`Orig_UT. Pinigai ir indėliai`)
                