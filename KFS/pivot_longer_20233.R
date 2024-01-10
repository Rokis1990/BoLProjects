# U6krauname reikalingus paketus
install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl)
# library(tidyverse)
# library(readxl)
# library(writexl)

# Atsisiunčiame KFS duomenis tiems laikotarpiams, kurie mums aktualūs
# http://vidinis.lb.lt/ISSIS/Bop/Comparison/2

# Atsisiunčiame dvi bylas už (šiuo atveju) 9 paskutinius laikotarpius:
# KFS_LE_T - visi KFS - BOP sarasas
# KFS kiti pokyciai sarasas

# Įkeliame parsisiųstas Excel byląs:
# KFS likučiai ir srautai
# KFS_LE_T_ALL_COMP_20224_4 <- read_excel(
# "MB_duomenys_KFSui/KFS_LE_T_ALL_COMP_20224_4.xlsx")
KFS_LE_T_ALL_COMP_20231 <- read_excel(
  "20233_KFS/MB duomenys KFS/KFS_LE_T_ALL_I9J9_COMP_20233_3_23121515490078.xlsx")
# KFS kiti pokyčiai
KFS_KA_ALL_COMP_20231 <- read_excel(
  "20233_KFS/MB duomenys KFS/KFS_KA_ALL_I9J9_COMP_20233_3_23121515502099.xlsx")

# Sujungiame KFS KA ir LE_T bylas į bendrą failą
#dataset_full <- rbind(KFS_KA_ALL_COMP_20224_4_a, KFS_LE_T_ALL_COMP_20224_4)
dataset_full <- rbind(KFS_KA_ALL_COMP_20231, KFS_LE_T_ALL_COMP_20231)

# Suskaičiuojme kiek eilučių turi pasikartojimų (COUNT stuleplis)
temp <- dataset_full %>% group_by_all() %>% summarise(COUNT = n())
sum(temp$COUNT) # Suskaičiuojame ir palyginame eilučių ir COUNT sumos skaičių

# Sukuriame naują lentelę su stulpeliu "kodas", kuriame talpiname rodiklio Daugiamatį Kodą
dataset <- KFS_LE_T_ALL_COMP_20231 |> dplyr::mutate(kodas = paste0(
  PERIODISKUMAS, ".", KOREGAVIMAS, ".", PRIESSALIS, ".", ATSISSEKTORIUS, ".",
  PRIESSEKTORIUS, ".", IRASOTIPAS, ".", APSKAITOSIRASOTIPAS, ".", RODIKLIOKODAS,
  ".", FUNKCINEKATEGORIJA, ".", PRIEMONE, ".", TERMINAS, ".", MATAVIMOVNT, ".",
  VALIUTA, ".", IVERTINIMAS, ".", SUDARYMOMETODAS))
dataset <- dataset_full |> dplyr::mutate(kodas = paste0(
  PERIODISKUMAS, ".", KOREGAVIMAS, ".", PRIESSALIS, ".", ATSISSEKTORIUS, ".",
  PRIESSEKTORIUS, ".", IRASOTIPAS, ".", APSKAITOSIRASOTIPAS, ".", RODIKLIOKODAS,
  ".", FUNKCINEKATEGORIJA, ".", PRIEMONE, ".", TERMINAS, ".", MATAVIMOVNT, ".",
  VALIUTA, ".", IVERTINIMAS, ".", SUDARYMOMETODAS))

# Stulpeliuose po datomis esančias rodiklio vertes suspaudžiame, ir konvertuojame 
# lentelę iš Plataus (wide) formato į ilgą (long) formatą (GAMINAME "DEŠRĄ")

#dataset_long <- tidyr::pivot_longer(dataset, cols = c("2020 4", "2021 1", "2021 2", 
#                                                      "2021 3", "2021 4", "2022 1", 
#                                                      "2022 2", "2022 3", "2022 4"), 
#                                    names_to = "Periodas", values_to = "verte")
#dataset_long <- tidyr::pivot_longer(dataset, cols = c("2023 2"), 
#                                    names_to = "Periodas", values_to = "verte")
dataset_long <- tidyr::pivot_longer(dataset, cols = c("2023 1", "2023 2", "2023 3"), 
                                    names_to = "Periodas", values_to = "verte")


# Pašaliname tarpus iš Periodo stulpelio verčių
dataset_long$Periodas <- gsub(" ", "", dataset_long$Periodas)

# Sukuriame naują stulpelį "Periodas_kodas", iš apjungtų Periodo ir daugiamačio kodo duomenų
dataset_long <- dataset_long |> dplyr::mutate(Periodas_kodas = paste0(Periodas, kodas)) |>
  dplyr::mutate(verte2 = verte)

# Pakeičiame lentelės stulpelių išdėstymo tvarką tiap, kad "verte" stulpelis 
# eitų po stulpelio "Periodas_kodas"
names(dataset_long)
col_order <- c("PERIODISKUMAS", "KOREGAVIMAS", "PRIESSALIS", "ATSISSEKTORIUS", 
               "PRIESSEKTORIUS", "IRASOTIPAS", "APSKAITOSIRASOTIPAS", 
               "RODIKLIOKODAS", "FUNKCINEKATEGORIJA", "PRIEMONE", "TERMINAS", 
               "MATAVIMOVNT", "VALIUTA", "IVERTINIMAS", "SUDARYMOMETODAS",
               "PAVADINIMAS", "kodas", "Periodas", "verte2", "Periodas_kodas",
               "verte")
dataset_long2 <- dataset_long[, col_order]

date <- Sys.Date() # Išsaugome šiandienos datą į tekstinė datos kintamąjį
# Išsaugome ilgo formato lentelę .xlsx formatu
writexl::write_xlsx(dataset_long2, paste0("20233_KFS/output/dataset_long_SR_", date, ".xlsx"))

