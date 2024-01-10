library(FKF)

FKF::fkf(y, )
FKF::fks(y)

library(padr)

class(Ketvirtiniai)
class(Ketvirtiniai$MQ)
class(Ketvirtiniai$Ketvirtis)
class(Ketvirtiniai$MQ)
class(Ketvirtiniai$MQ)

library(zoo)
yq <- as.yearqtr(Ketvirtiniai$MQ, format = "%Y-%m-%d")
yq

Ketvirtiniai |> dplyr::mutate(Date = as.Date(as.character(MQ),format="%Y%m%d"))

padr::pad(Ketvirtiniai)
z

Ketvirtiniai_menesiniai <- readxl::read_excel("Duomenys/Laiko eilutės_2022-06-02_v1.1.xlsx", sheet = 1)
