# Imputation compare with monthly

ts(Menesiniai$ISIP..Įsiskolinimai.už.prekes.ir.paslaugas, start = c(2015, 01), frequency = 12)

############
# Read F-06
############

## Set custom working directory

setwd("~/B-09-04 atsisakymas. Forecast/Duomenys/TUI, F06/F06")
#rm(file.list)
library(readxl)
file.list <- list.files(pattern='*.xlsx')
#file.list <- list.files(pattern='*.xlsx', recursive = TRUE)
df.list <- lapply(file.list, read_excel)

#setwd("~/B-09-04 atsisakymas. Forecast/Duomenys/TUI, F06/TUI01")


library(dplyr)
df <- bind_rows(df.list, .id = "id")

F06 <- df |> dplyr::filter(`ŠALIS` == "Viso") |> dplyr::group_by(METAI, PERIODAS, RODIKLIS) |>
  dplyr::summarise(sum(`REIKŠMĖ`))

F06 |> tidyr::gather(METAI, PERIODAS, RODIKLIS)
library(tidyr)
F06_exp <- F06 |> pivot_wider(names_from = RODIKLIS, values_from = `sum(REIKŠMĖ)`)
F06_exp

setwd("~/B-09-04 atsisakymas. Forecast/Duomenys/TUI, F06/F06")
writexl::write_xlsx(F06_exp, "F06_exp.xlsx")


##############
# Read TUI-01
##############
setwd("~/B-09-04 atsisakymas. Forecast/Duomenys/TUI, F06/TUI01")
library(readxl)
TUI_TUI_20184_20161_23020709353170 <- read_excel("TUI_TUI_20184_20161_23020709353170.xlsx")
View(TUI_TUI_20184_20161_23020709353170)

TUI01 <- TUI_TUI_20184_20161_23020709353170 |> dplyr::filter() |>
  dplyr::group_by(PER_METAI, PER_PERIODAS_PAV, ROD_KOD) |>
  dplyr::summarise(sum(D1))

TUI01_exp <- TUI01 |> tidyr::pivot_wider(names_from = ROD_KOD, values_from = `sum(D1)`)
TUI01_exp
setwd("~/B-09-04 atsisakymas. Forecast/Duomenys/TUI, F06/TUI01")
writexl::write_xlsx(TUI01_exp, "TUI01_exp.xlsx")


