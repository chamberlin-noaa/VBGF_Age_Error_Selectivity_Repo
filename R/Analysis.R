rm(list = ls())
gc()

setwd("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R")
source("Functions.R")

library(broom)
library(dplyr)
library(ggplot2)
library(gratia)
library(mgcv)

#Logistic selectivity GAMs
load("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R/workspace_logistic.RData")
all_reduced <- subset(all_flat_logistic, sel_1 < 400)

gam_k_log <- gam(k_RE ~ s(CV_Age, k = 4) + te(sel_1, sel_2, k = c(5, 5)) + as.factor(spp) + as.factor(sample_size), data = all_reduced, method = "REML")
gam_L_inf_log <- gam(L_inf_RE ~ s(CV_Age, k = 4) + te(sel_1, sel_2, k = c(5, 5)) + as.factor(spp) + as.factor(sample_size), data = all_reduced, method = "REML")
gam_t_0_log <- gam(t_0_RE ~ s(CV_Age, k = 4) + te(sel_1, sel_2, k = c(5, 5)) + as.factor(spp) + as.factor(sample_size), data = all_reduced, method = "REML")

clean_k_log <- refit_clean_model(gam_k_log, all_reduced, "k_RE", 3)
clean_L_inf_log <- refit_clean_model(gam_L_inf_log, all_reduced, "L_inf_RE", 3)
clean_t_0_log <- refit_clean_model(gam_t_0_log, all_reduced, "t_0_RE", 3)

print(bind_rows(clean_k_log$summary, clean_L_inf_log$summary, clean_t_0_log$summary))

draw(clean_k_log$model, dist = 100)
draw(clean_L_inf_log$model, dist = 100)
draw(clean_t_0_log$model, dist = 100)

par(mfrow = c(2, 2))
gam.check(clean_k_log$model)
gam.check(clean_L_inf_log$model)
gam.check(clean_t_0_log$model)

combined_logistic <- bind_rows(
  extract_gam_results(clean_k_log$model, "k_RE"),
  extract_gam_results(clean_L_inf_log$model, "L_inf_RE"),
  extract_gam_results(clean_t_0_log$model, "t_0_RE")
) %>%
  mutate(Term = case_when(
    Term == "(Intercept)" ~ "Intercept (Blackgill, N=500)",
    Term == "as.factor(spp)blue" ~ "Species: Blue",
    Term == "as.factor(spp)calico" ~ "Species: Calico",
    Term == "as.factor(spp)olive" ~ "Species: Olive",
    Term == "as.factor(sample_size)1000" ~ "Sample Size: 1000",
    Term == "s(CV_Age)" ~ "Aging Error (s(CV_Age))",
    Term == "te(sel_1,sel_2)" ~ "Selectivity Interaction (te(sel_1, sel_2))",
    TRUE ~ Term
  ))

write.csv(combined_logistic, "G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/Logistic_VBGF_GAM_All_Models_Results.csv", row.names = FALSE)

#Size-at-Age BAM
bias_flat_logistic_reduced <- subset(bias_flat_logistic, sel_1 < 400) %>%
  group_by(spp) %>% mutate(relative_age = age / max(age)) %>% ungroup()

bam_size_log <- bam(relative_error ~ s(relative_age) + s(CV_Age, k = 4) + te(relative_age, sel_1) + te(relative_age, sel_2) + as.factor(spp) + as.factor(sample_size),
                    data = bias_flat_logistic_reduced, method = "fREML", discrete = TRUE)

clean_size_log <- refit_clean_model(bam_size_log, bias_flat_logistic_reduced, "size_at_age", 3)
print(clean_size_log$summary)

draw(clean_size_log$model, dist = 100)

plot_large_diagnostics(clean_size_log$model)

results_size_log <- extract_gam_results(clean_size_log$model, "Size_at_Age_RE") %>%
  mutate(Term = case_when(
    Term == "(Intercept)" ~ "Intercept (Blackgill, N=500)",
    Term == "as.factor(spp)blue" ~ "Species: Blue",
    Term == "as.factor(spp)calico" ~ "Species: Calico",
    Term == "as.factor(spp)olive" ~ "Species: Olive",
    Term == "as.factor(sample_size)1000" ~ "Sample Size: 1000",
    Term == "s(relative_age)" ~ "Relative Age (s(relative_age))",
    Term == "s(CV_Age)" ~ "Aging Error (s(CV_Age))",
    Term == "te(relative_age,sel_1)" ~ "Age x L50 Interaction (te(relative_age, sel_1))",
    Term == "te(relative_age,sel_2)" ~ "Age x Steepness Interaction (te(relative_age, sel_2))",
    TRUE ~ Term
  ))

write.csv(results_size_log, "G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/Logistic_Size_at_Age_BAM_Results.csv", row.names = FALSE)



#Dome selectivity GAMs
rm(list = ls())
gc()

setwd("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R")
source("Functions.R")

load("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R/workspace_dome.RData")
all_reduced_dome <- subset(all_flat_dome, B1 != 400 & B2 != 0 & B3 != 12 & B4 != 13)

gam_k_dome <- gam(k_RE ~ s(CV_Age, k = 4) + te(B1, B2, k = c(4, 4)) + te(B3, B4, k = c(4, 4)) + as.factor(spp) + as.factor(sample_size), data = all_reduced_dome, method = "REML")
gam_L_inf_dome <- gam(L_inf_RE ~ s(CV_Age, k = 4) + te(B1, B2, k = c(4, 4)) + te(B3, B4, k = c(4, 4)) + as.factor(spp) + as.factor(sample_size), data = all_reduced_dome, method = "REML")
gam_t_0_dome <- gam(t_0_RE ~ s(CV_Age, k = 4) + te(B1, B2, k = c(4, 4)) + te(B3, B4, k = c(4, 4)) + as.factor(spp) + as.factor(sample_size), data = all_reduced_dome, method = "REML")

clean_k_dome <- refit_clean_model(gam_k_dome, all_reduced_dome, "k_RE", 3)
clean_L_inf_dome <- refit_clean_model(gam_L_inf_dome, all_reduced_dome, "L_inf_RE", 3)
clean_t_0_dome <- refit_clean_model(gam_t_0_dome, all_reduced_dome, "t_0_RE", 3)

print(bind_rows(clean_k_dome$summary, clean_L_inf_dome$summary, clean_t_0_dome$summary))

draw(clean_k_dome$model, dist = 100)
draw(clean_L_inf_dome$model, dist = 100)
draw(clean_t_0_dome$model, dist = 100)

par(mfrow = c(2, 2))
gam.check(clean_k_dome$model)
gam.check(clean_L_inf_dome$model)
gam.check(clean_t_0_dome$model)

combined_dome <- bind_rows(
  extract_gam_results(clean_k_dome$model, "k_RE"),
  extract_gam_results(clean_L_inf_dome$model, "L_inf_RE"),
  extract_gam_results(clean_t_0_dome$model, "t_0_RE")
) %>%
  mutate(Term = case_when(
    Term == "(Intercept)" ~ "Intercept (Blackgill, N=500)",
    Term == "as.factor(spp)blue" ~ "Species: Blue",
    Term == "as.factor(spp)calico" ~ "Species: Calico",
    Term == "as.factor(spp)olive" ~ "Species: Olive",
    Term == "as.factor(sample_size)1000" ~ "Sample Size: 1000",
    Term == "s(CV_Age)" ~ "Aging Error (s(CV_Age))",
    Term == "te(B1,B2)" ~ "Ascending Selectivity (te(B1, B2))",
    Term == "te(B3,B4)" ~ "Descending Selectivity (te(B3, B4))",
    TRUE ~ Term
  ))

write.csv(combined_dome, "G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/Dome_VBGF_GAM_All_Models_Results.csv", row.names = FALSE)

#Size-at-Age BAM
bias_flat_dome <- bias_flat_dome %>% group_by(spp) %>% mutate(relative_age = age / max(age)) %>% ungroup()

bam_size_dome <- bam(relative_error ~ s(relative_age) + s(CV_Age, k = 4) + te(relative_age, B1, k = c(5, 4)) + te(relative_age, B2, k = c(5, 4)) + te(relative_age, B3, k = c(5, 4)) + te(relative_age, B4, k = c(5, 4)) + as.factor(spp) + as.factor(sample_size),
                     data = bias_flat_dome, method = "fREML", discrete = TRUE)

clean_size_dome <- refit_clean_model(bam_size_dome, bias_flat_dome, "size_at_age", 3)
print(clean_size_dome$summary)

draw(clean_size_dome$model, dist = 100)

plot_large_diagnostics(clean_size_dome$model)

results_size_dome <- extract_gam_results(clean_size_dome$model, "Size_at_Age_RE") %>%
  mutate(Term = case_when(
    Term == "(Intercept)" ~ "Intercept (Blackgill, N=500)",
    Term == "as.factor(spp)blue" ~ "Species: Blue",
    Term == "as.factor(spp)calico" ~ "Species: Calico",
    Term == "as.factor(spp)olive" ~ "Species: Olive",
    Term == "as.factor(sample_size)1000" ~ "Sample Size: 1000",
    Term == "s(relative_age)" ~ "Relative Age (s(relative_age))",
    Term == "s(CV_Age)" ~ "Aging Error (s(CV_Age))",
    Term == "te(relative_age,B1)" ~ "Age x Asc. Peak Interaction (te(relative_age, B1))",
    Term == "te(relative_age,B2)" ~ "Age x Asc. Slope Interaction (te(relative_age, B2))",
    Term == "te(relative_age,B3)" ~ "Age x Desc. Slope Interaction (te(relative_age, B3))",
    Term == "te(relative_age,B4)" ~ "Age x Desc. Peak Interaction (te(relative_age, B4))",
    TRUE ~ Term
  ))

write.csv(results_size_dome, "G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/Dome_Size_at_Age_BAM_Results.csv", row.names = FALSE)
