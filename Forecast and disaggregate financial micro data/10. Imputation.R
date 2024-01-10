install.packages("readxl")
install.packages("imputeTS")
library(readxl)
library(imputeTS)

L <- read_excel("Duomenys/Copy of Laiko eilutės_2022-06-02_v1.1_bepirmos.xlsx")

##### na_interpolation funkcija
IP <- L
plot(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_interpolation(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "linear")
plot(IP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
IP$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(IP$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_interpolation(IP$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "linear")
IP$`UT. Pinigai ir indėliai`[is.na(IP$`UT. Pinigai ir indėliai`)] <- na_interpolation(IP$`UT. Pinigai ir indėliai`, option = "linear")


##### na_kalman funkcija
KM <- L
KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
KM$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KM$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KM$`UT. Įsiskolinimai už prekes ir paslaugas`)
KM$`UT. Pinigai ir indėliai`[is.na(KM$`UT. Pinigai ir indėliai`)] <- na_kalman(KM$`UT. Pinigai ir indėliai`)

# model="StructTS"
KMS <- L
KMS$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KMS$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(L$`ISIP. Įsiskolinimai už prekes ir paslaugas`,model="StructTS")
KMS$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KMS$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(L$`UT. Įsiskolinimai už prekes ir paslaugas`,model="StructTS")
KMS$`UT. Pinigai ir indėliai`[is.na(KMS$`UT. Pinigai ir indėliai`)] <- na_kalman(L$`UT. Pinigai ir indėliai`,model="StructTS")
# Default variantas (kaip K) turetu buti su auto.arima, bet kazkodel yra su StructTS.

# model="auto.arima"
KMA <- L
KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KMA$`ISIP. Įsiskolinimai už prekes ir paslaugas`,model="auto.arima")
KMA$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(KMA$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_kalman(KMA$`UT. Įsiskolinimai už prekes ir paslaugas`,model="auto.arima")
KMA$`UT. Pinigai ir indėliai`[is.na(KMA$`UT. Pinigai ir indėliai`)] <- na_kalman(KMA$`UT. Pinigai ir indėliai`,model="auto.arima")
# Uzpildo nuliais arba buvusiom reiksmem.

# Nera neigiamu reiksmiu!


##### na_locf funkcija
LCF <- L
LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_locf(LCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
LCF$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(LCF$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_locf(LCF$`UT. Įsiskolinimai už prekes ir paslaugas`)
LCF$`UT. Pinigai ir indėliai`[is.na(LCF$`UT. Pinigai ir indėliai`)] <- na_locf(LCF$`UT. Pinigai ir indėliai`)


##### na_ma funkcija 
MA <- L
MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MA$`ISIP. Įsiskolinimai už prekes ir paslaugas`)
MA$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MA$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MA$`UT. Įsiskolinimai už prekes ir paslaugas`)
MA$`UT. Pinigai ir indėliai`[is.na(MA$`UT. Pinigai ir indėliai`)] <- na_ma(MA$`UT. Pinigai ir indėliai`)

# Linear
MAL <- L
MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, weighting = "linear")
MAL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MAL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAL$`UT. Įsiskolinimai už prekes ir paslaugas`, weighting = "linear")
MAL$`UT. Pinigai ir indėliai`[is.na(MAL$`UT. Pinigai ir indėliai`)] <- na_ma(MAL$`UT. Pinigai ir indėliai`, weighting = "linear")

# Simple
MAS <- L
MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAS$`ISIP. Įsiskolinimai už prekes ir paslaugas`, weighting = "simple")
MAS$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MAS$`UT. Įsiskolinimai už prekes ir paslaugas`)] <-na_ma(MAS$`UT. Įsiskolinimai už prekes ir paslaugas`, weighting = "simple")
MAS$`UT. Pinigai ir indėliai`[is.na(MAS$`UT. Pinigai ir indėliai`)] <- na_ma(MAS$`UT. Pinigai ir indėliai`, weighting = "simple")


##### na_mean funkcija
# mean
MN <- L
MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MN$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "mean")
MN$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MN$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MN$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "mean")
MN$`UT. Pinigai ir indėliai`[is.na(MN$`UT. Pinigai ir indėliai`)] <- na_mean(MN$`UT. Pinigai ir indėliai`, option = "mean")

# median
MNM <- L
MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNM$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "median")
MNM$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MNM$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MNM$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "median")
MNM$`UT. Pinigai ir indėliai`[is.na(MNM$`UT. Pinigai ir indėliai`)] <- na_mean(MNM$`UT. Pinigai ir indėliai`, option = "median")

# mode
MND <- L
MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MND$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option = "mode")
MND$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(MND$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_mean(MND$`UT. Įsiskolinimai už prekes ir paslaugas`, option = "mode")
MND$`UT. Pinigai ir indėliai`[is.na(MND$`UT. Pinigai ir indėliai`)] <- na_mean(MND$`UT. Pinigai ir indėliai`, option = "mode")

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

##### na_seadec
# algorithm="interpolation" (default)
SIP <- L
SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SIP$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SIP$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SIP$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SIP$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SIP$`UT. Pinigai ir indėliai`[is.na(SIP$`UT. Pinigai ir indėliai`)] <- na_seadec(SIP$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)

# ??? ERROR darant antra stulpeli.

# algorithm="locf"
SLCF <- L
SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SLCF$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "locf", find_frequency=TRUE)
SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seadec(SLCF$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "locf", find_frequency=TRUE)
SLCF$`UT. Pinigai ir indėliai`[is.na(SLCF$`UT. Pinigai ir indėliai`)] <- na_seadec(SLCF$`UT. Pinigai ir indėliai`, algorithm = "locf", find_frequency=TRUE)

# ??? ERROR darant antra stulpeli.

# Dar yra pasirinkimai su mean, random, kalman, ma.

##### na_seasplit
# su algorithm="interpolation"
SPL <- L
SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SPL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPL$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPL$`UT. Pinigai ir indėliai`[is.na(SPL$`UT. Pinigai ir indėliai`)] <- na_seasplit(SPL$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)
#??? ERROR 

# su algorithm="locf"
SPLL <- L
SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`[is.na(SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPLL$`ISIP. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`[is.na(SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`)] <- na_seasplit(SPLL$`UT. Įsiskolinimai už prekes ir paslaugas`, algorithm = "interpolation", find_frequency=TRUE)
SPLL$`UT. Pinigai ir indėliai`[is.na(SPLL$`UT. Pinigai ir indėliai`)] <- na_seasplit(SPLL$`UT. Pinigai ir indėliai`, algorithm = "interpolation", find_frequency=TRUE)
#??? ERROR


                