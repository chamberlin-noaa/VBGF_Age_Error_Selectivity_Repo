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



#Best fit models
#think about trying a multispecies model

all_flat_k <- aov(k_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)*as.factor(spp)*as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_k)
par(mfrow = c(2, 2))
plot(all_flat_k)
par(mfrow = c(1, 1))

all_flat_L_inf <- aov(L_inf_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+as.factor(spp)+as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_L_inf)
qqnorm(residuals(all_flat_L_inf))
qqline(residuals(all_flat_L_inf))

all_flat_t_0 <- aov(t_0_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+as.factor(spp)+as.factor(sample_size), data = all_flat_logistic)
summary(all_flat_t_0)
qqnorm(residuals(all_flat_t_0))
qqline(residuals(all_flat_t_0))

#look at sample sizes TO DO!!!!!!!!!! This
all_flat_k_1 <- aov(k_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+spp+as.factor(sample_size), data = all_flat)
all_flat_k_2 <- aov(k_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2)+spp, data = subset(all_flat_logistic, all_flat_logistic$sample_size == 1000))
all_flat_k_3 <- aov(k_RE ~ as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(all_flat, all_flat$sample_size == 1000))

AIC(all_flat_k_1, all_flat_k_2, all_flat_k_3)
qqnorm(residuals(all_flat_k_2))
qqline(residuals(all_flat_k_2))





summary(aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(blackgill_flat, blackgill_flat$sample_size == 500)))
summary(aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(blue_flat, blue_flat$sample_size == 500)))
summary(aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(calico_flat, calico_flat$sample_size == 500)))
summary(aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(olive_flat, olive_flat$sample_size == 500)))


mod1 <- aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(calico_flat, calico_flat$sample_size == 500))
plot(mod1)

mod1 <- (aov(k_RE~as.factor(CV_Age)*as.factor(sel_1)*as.factor(sel_2), data = subset(calico_flat, calico_flat$sample_size == 500 & calico_flat$sel_2 > 35)))
plot(mod1)
