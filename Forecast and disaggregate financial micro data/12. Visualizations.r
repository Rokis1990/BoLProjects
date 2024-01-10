library(ggplot2)
library(reshape2)
meltdf <- melt(df,id="Year")
ggplot(meltdf,aes(x=Year,y=value,colour=variable,group=variable)) + geom_line()


library(ggplot2)
library(reshape2)
library(dplyr)

covid1 =(read.csv(file="EUCOVIDdeaths.csv",header=TRUE)[,-c(2)])

head(covid1)


covid_deaths <- melt(covid1,id.vars=c("Country"),value.name="value",
                     variable.name="Day")

head(covid_deaths)

covid_plot <- ggplot(data=covid_deaths, aes(x=Day, y=value, group = Country,
                                            colour = Country))
+ geom_line() +labs(y= "Deaths", x = "Day")
covid_plot + ggtitle("Daily Deaths for European countries in March,2020")+geom_point()

covid_plot

x3<-1:10
y3<-rnorm(10)
df3<-data.frame(x3,y3)
df3

x1<-1:10
y1<-rnorm(10)
df1<-data.frame(x1,y1)
df1

x2<-1:10
y2<-rnorm(10)
df2<-data.frame(x2,y2)
df2

library(ggplot2)
  
ggplot(df1,aes(x1,y1)) + geom_line(color="blue") + geom_line(aes(x2,y2),color="black") +
  geom_line(aes(x3,y3),color="green") + xlab("Time")+ylab("Rate") + 
  theme(legend.position="bottom", legend.title = element_text(colour="blue", size=10, face="bold"))

####################################
# 
####################################

dataset_15Q$date <- as.Date()

library(ggplot2)

####



ggplot(dataset_15Q,aes(y1ii,y2ti)) + geom_line(color="blue") + geom_line(aes(y3pi,s_exp),color="black") +
  geom_line(aes(s_imp,g_exp),color="green") + xlab("Time")+ylab("Rate") + 
  theme(legend.position="bottom", legend.title = element_text(colour="blue", size=10, face="bold"))

s_exp <- dataset_15Q[,4]
s_imp <- dataset_15Q[,5]
g_exp <- dataset_15Q[,6]

set.seed(1023172)                            # Create random example data
data <- round(data.frame(year = 2001:2025,
                         ts1 = 1:25 + rnorm(25),
                         ts2 = 30:6 + runif(25, 0, 10),
                         ts3 = rnorm(25, 5, 5)))
head(data)  

plot(data$year,                              # Draw first time series
     data$ts1,
     type = "l",
     col = 2,
     ylim = c(- 15, 40),
     xlab = "Year",
     ylab = "Values")
lines(data$year,                             # Draw second time series
      data$ts2,
      type = "l",
      col = 3)
lines(data$year,                             # Draw third time series
      data$ts3,
      type = "l",
      col = 4)
legend("topright",                           # Add legend to plot
       c("ts1", "ts2", "ts3"),
       lty = 1,
       col = 2:4)


install.packages("reshape2")                 # Install reshape2 package
library("reshape2")                          # Load reshape2 package

  
data_long <- melt(data, id.vars = "year")    # Reshaping data to long format
head(data_long)        


ggplot(data_long,                            # Draw ggplot2 time series plot
       aes(x = year,
           y = value,
           col = variable)) +
  geom_line()


time(myts)

library(zoo)
as.yearmon(time(myts))

dateT <- time(dataset_15Q[,1])
time(dataset_15Q[,1])
dataset_15Q <- as.data.frame(dataset_15Q)
dataset_15Q$date <- zoo::as.yearmon(dataset_15Q[,1])

dataset_15Q$date <- as.Date("2021-01-01") - 0:23

##########################################
# Vizualizacijos
##########################################
library(zoo)
library(ggthemes)
x <- "2016 Q1"
length_time <- (dataset_15Q)
dataset_15Q$date <- as.yearqtr(x) + seq(0, length = 24) / 4


ggplot(dataset_15Q,aes(date,y2ti)) + geom_line(color="blue") + geom_line(aes(date,s_exp),color="black") +
  geom_line(aes(date,g_exp),color="green") + xlab("Time")+ylab("Rate") + 
  theme(legend.position="bottom", legend.title = element_text(colour="blue", size=10, face="bold"))

ggplot(dataset_15Q, aes(date, y1ii)) + geom_line() + 
  geom_line()
  ggthemes::theme_solarized()

names(KMA) <- c("Date", "MQ", "Ketvirtis", "ISIP.Įsiskolinimai.už.prekes.ir.paslaugas",
                "UT.Įsiskolinimai.už.prekes.ir.paslaugas", "UT.Pinigai.ir.indėliai",
                "Orig_ISIP.Įsiskolinimai.už.prekes.ir.paslaugas", "Orig_UT.Pinigai.ir.indėliai",
                "Orig_UT.Įsiskolinimai.už.prekes.ir.paslaugas")
  
ggplot(KMA, aes(Date, `ISIP.Įsiskolinimai.už.prekes.ir.paslaugas`)) + geom_line() + 
  geom_line(`Orig_ISIP.Įsiskolinimai.už.prekes.ir.paslaugas`) +
  ggthemes::theme_solarized()

KMA2 <- KMA

names(KMA2) <- c("Date", "MQ", "Ketvirtis", "ISIP_ISISKOLINIMAI",
                 "UT_ISISKOLINIMAI", "UT_PINIGAI_INDELIAI",
                 "Orig_ISIP_ISISKOLINIMAI", "Orig_UT.PINIGAI_INDELIAI",
                 "Orig_UT_ISIP_ISISKOLINIMAI")

ggplot(KMA2, aes(Date, Orig_ISIP_ISISKOLINIMAI)) + geom_line(color = "blue") + 
  #geom_line(Orig_UT_ISIP_ISISKOLINIMAI) +
  ggthemes::theme_solarized()

  
Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas
ggplot(Menesiniai, aes(Date, ISIP..Įsiskolinimai.už.prekes.ir.paslaugas)) + geom_line(color="blue") + 
  geom_line(ISIP..Įsiskolinimai.už.prekes.ir.paslaugas) +
  ggthemes::theme_solarized()

names(Menesiniai) <- c("Data", "Menuo", "ISIP_ISISK_men", 
                        "UT_PII_men", "UT_ISIsK_men")


ggplot(data = Menesiniai, aes(Data, ISIP_ISISK_men)) + geom_line(color="blue") + 
  geom_line(UT_PII_men, color = "red") +
  ggthemes::theme_solarized()
ggplot(data = Menesiniai, aes(Data, UT_PII_men)) + geom_line(color="blue") + 
  #geom_line(x = Data, y = ISIP_ISISK_men, color = "red") +
  ggthemes::theme_solarized()


ggplot(data = Menesiniai, aes(x = Data, y = UT..Pinigai.ir.indėliai)) + 
  #geom_line(color = "red") +
  geom_line(aes(x = Data, y = UT..Įsiskolinimai.už.prekes.ir.paslaugas), color = "blue") + 
  geom_line(aes(x = Data, y = ISIP..Įsiskolinimai.už.prekes.ir.paslaugas))

#################################
library(imputeTS)
library(tidyverse)
data_to_impute <- readxl::read_xlsx("Duomenys/Copy of Laiko eilutės_2022-06-02_v1.2_bepirmos.xlsx", sheet = 1)


data_to_impute$`ISIP. Įsiskolinimai už prekes ir paslaugas` |> dplyr::mutate(ISIP_ISISK = na_interpolation(option ="linear"))

data_to_impute$`ISIP. Įsiskolinimai už prekes ir paslaugas`
data_to_impute$`Orig_UT. Įsiskolinimai už prekes ir paslaugas`
data_to_impute$`Orig_UT. Pinigai ir indėliai`
data_to_impute$`UT. Pinigai ir indėliai_imputedLinear`

data_to_impute$`ISIP. Įsiskolinimai už prekes ir paslaugas_imputedLinear` <- na_interpolation(data_to_impute$`ISIP. Įsiskolinimai už prekes ir paslaugas`, option="linear")
data_to_impute$`UT. Įsiskolinimai už prekes ir paslaugas_imputedLinear` <- na_interpolation(data_to_impute$`UT. Įsiskolinimai už prekes ir paslaugas`, option="linear")
data_to_impute$`UT. Pinigai ir indėliai_imputedLinear` <- na_interpolation(data_to_impute$`UT. Pinigai ir indėliai`, option="linear")
data_to_impute$`Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`

## 
ggplot(data = data_to_impute, aes(x = Date, y = `Orig_ISIP. Įsiskolinimai už prekes ir paslaugas`)) + geom_line(color = "red") +
  geom_line(aes(x = Date, y = `UT. Įsiskolinimai už prekes ir paslaugas_imputedLinear`)) + theme_gray()

ggplot(data = data_to_impute, aes(x = Date, y = `Orig_UT. Įsiskolinimai už prekes ir paslaugas`)) + geom_line(color = "red") +
  geom_line(aes(x = Date, y = `UT. Įsiskolinimai už prekes ir paslaugas_imputedLinear`)) + theme_gray()

ggplot(data = data_to_impute, aes(x = Date, y = `Orig_UT. Pinigai ir indėliai`)) + geom_line(color = "red") +
  geom_line(aes(x = Date, y = `UT. Pinigai ir indėliai_imputedLinear`)) + theme_gray()



