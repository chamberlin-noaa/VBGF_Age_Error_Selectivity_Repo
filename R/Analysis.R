rm(list = ls())
gc()

#Logistic ANOVAs
load("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R/workspace_logistic.RData")
all_flat_k_logistic <- aov(k_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+as.factor(spp)+as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_k_logistic)
par(mfrow = c(2, 2))
plot(all_flat_k_logistic)
par(mfrow = c(1, 1))

all_flat_L_inf_logistic <- aov(L_inf_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+as.factor(spp)+as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_L_inf_logistic)
par(mfrow = c(2, 2))
plot(all_flat_L_inf_logistic)
par(mfrow = c(1, 1))

all_flat_t_0_logistic <- aov(t_0_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+as.factor(spp)+as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_t_0_logistic)
par(mfrow = c(2, 2))
plot(all_flat_t_0_logistic)
par(mfrow = c(1, 1))

objects_to_keep <- c("all_flat_k_logistic", "all_flat_L_inf_logistic", "all_flat_t_0_logistic")
rm(list = setdiff(ls(), objects_to_keep))
gc()

#Dome ANOVAs
load("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R/workspace_dome.RData")

#remove some scenarios because ANOVA requires too much RAM
all_reduced <- subset(all_flat_dome, B1 != 400)
all_reduced <- subset(all_reduced, B2 != 0)
all_reduced <- subset(all_reduced, B3 != 12)
all_reduced <- subset(all_reduced, B4 != 13)

objects_to_keep <- c("all_reduced")
rm(list = setdiff(ls(), objects_to_keep))
gc()


all_flat_k_dome <- aov(k_RE ~ as.factor(CV_Age)*as.factor(B1)*as.factor(B2)*as.factor(B3)*as.factor(B4)+as.factor(spp)+as.factor(sample_size), data = all_reduced)
summary(all_flat_k_dome)
par(mfrow = c(2, 2))
plot(all_flat_k_dome)
par(mfrow = c(1, 1))

all_flat_L_inf_dome <- aov(L_inf_RE ~ as.factor(CV_Age)*as.factor(B1)*as.factor(B2)*as.factor(B3)*as.factor(B4)+as.factor(spp)+as.factor(sample_size), data = all_reduced)
summary(all_flat_L_inf_dome)
par(mfrow = c(2, 2))
plot(all_flat_L_inf_dome)
par(mfrow = c(1, 1))

all_flat_t_0_dome <- aov(t_0_RE ~ as.factor(CV_Age)*as.factor(B1)*as.factor(B2)*as.factor(B3)*as.factor(B4)+as.factor(spp)+as.factor(sample_size), data = all_reduced)
summary(all_flat_t_0_dome)
par(mfrow = c(2, 2))
plot(all_flat_t_0_dome)
par(mfrow = c(1, 1))

objects_to_keep <- c("all_flat_k_logistic", "all_flat_L_inf_logistic", "all_flat_t_0_logistic", "all_flat_k_dome", "all_flat_L_inf_dome", "all_flat_t_0_dome")
rm(list = setdiff(ls(), objects_to_keep))
gc()



#AIC Model selection
AIC_models <- function(df, param){
    formula1 <- as.formula(paste(param, "~ as.factor(CV_Age) + as.factor(sel_1) + as.factor(sel_2)"))
    formula2 <- as.formula(paste(param, "~ (as.factor(CV_Age) + as.factor(sel_1)) * as.factor(sel_2)"))
    formula3 <- as.formula(paste(param, "~ (as.factor(CV_Age) + as.factor(sel_2)) * as.factor(sel_1)"))
    formula4 <- as.formula(paste(param, "~ as.factor(CV_Age) * as.factor(sel_1) * as.factor(sel_2)"))
    
    formula5 <- as.formula(paste("log(", param, "+1.00001)~ as.factor(CV_Age) + as.factor(sel_1) + as.factor(sel_2)"))
    formula6 <- as.formula(paste("log(", param, "+1.00001)~ (as.factor(CV_Age) + as.factor(sel_1)) * as.factor(sel_2)"))
    formula7 <- as.formula(paste("log(", param, "+1.00001)~ (as.factor(CV_Age) + as.factor(sel_2)) * as.factor(sel_1)"))
    formula8 <- as.formula(paste("log(", param, "+1.00001)~ as.factor(CV_Age) * as.factor(sel_1) * as.factor(sel_2)"))
    
    mod1 <- aov(formula1, data = df)
    mod2 <- aov(formula2, data = df)
    mod3 <- aov(formula3, data = df)
    mod4 <- aov(formula4, data = df)
    
    mod5 <- aov(formula5, data = df)
    mod6 <- aov(formula6, data = df)
    mod7 <- aov(formula7, data = df)
    mod8 <- aov(formula8, data = df)
    
    AIC_results <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
    
    AIC_results$delta_AIC <- AIC_results$AIC - min(AIC_results$AIC)
    
    return(AIC_results)
  }
  
  AIC_models(subset(blue_flat, blue_flat$sample_size == 500), "k_RE")
  AIC_models(subset(blackgill_flat, blackgill_flat$sample_size == 500), "k_RE")
  AIC_models(subset(olive_flat, olive_flat$sample_size == 500), "k_RE")
  AIC_models(subset(calico_flat, calico_flat$sample_size == 500), "k_RE") #& calico_flat$sel_2 > 35))
  AIC_models(subset(all_flat, all_flat$sample_size == 500), "k_RE")
  
  AIC_models(subset(blue_flat, blue_flat$sample_size == 500), "L_inf_RE")
  AIC_models(subset(blackgill_flat, blackgill_flat$sample_size == 500), "L_inf_RE")
  AIC_models(subset(olive_flat, olive_flat$sample_size == 500), "L_inf_RE")
  AIC_models(subset(calico_flat, calico_flat$sample_size == 500), "L_inf_RE") #& calico_flat$sel_2 > 35))
  AIC_models(subset(all_flat, all_flat$sample_size == 500), "L_inf_RE")
  
  AIC_models(subset(blue_flat, blue_flat$sample_size == 500), "t_0_RE")
  AIC_models(subset(blackgill_flat, blackgill_flat$sample_size == 500), "t_0_RE")
  AIC_models(subset(olive_flat, olive_flat$sample_size == 500), "t_0_RE")
  AIC_models(subset(calico_flat, calico_flat$sample_size == 500), "t_0_RE") #& calico_flat$sel_2 > 35))
  AIC_models(subset(all_flat, all_flat$sample_size == 500), "t_0_RE")