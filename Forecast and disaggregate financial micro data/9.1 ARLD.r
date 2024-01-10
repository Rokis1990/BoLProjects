#######
# ARDL
#######

names(dataset_15Q)

my_combs <- expand.grid(c(NA, "s_exp"), c(NA, "s_imp"), c(NA, "g_exp"), c(NA, "g_imp"), c(NA, "gfc2008"), c(NA, "CrysisLT09"), c(NA, "Cov19"), c(NA, "Quarantine"),
                      c(NA, "supplyChainCrysis"), c(NA, "WarUkraine"), c(NA, "L_nfin_tot"), c(NA, "D_nfin_tot"), c(NA, "IndProd_tot"), c(NA, "Apyvarta_sum"))
as.formula(paste0("Y", "~ ", paste(na.omit(as.character(unlist(tail(my_combs, 1)))), collapse="+ ")))

summary(model13)


expand.grid(1, "2", "3", c("a", "b"))

test_model <- lm(y1ii ~ s_exp + s_imp + g_exp + g_imp + #gfc2008 + crysisLT09 + 
                   #Cov19 + 
     Quarantine + #supplyChainCrysis + #WarUkraine + 
       L_nfin_tot + 
     D_nfin_tot + IndProd_tot + Apyvarta_sum, data = dataset_15Q)

summary(test_model)

test_model <- ARDL::auto_ardl(y1ii ~ s_exp + s_imp + g_exp + g_imp + #gfc2008 + crysisLT09 + 
                  #Cov19 + 
                  Quarantine + #supplyChainCrysis + #WarUkraine + 
                  L_nfin_tot + 
                  D_nfin_tot + IndProd_tot + Apyvarta_sum, data = dataset_15Q, max_order = 2)

summary(test_model$best_model)



