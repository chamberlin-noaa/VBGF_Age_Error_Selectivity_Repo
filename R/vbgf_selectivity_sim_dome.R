# To Do
# cleanup code, cleanup comments

rm(list = ls())
gc()

setwd("G:/My Drive/Research/VBGF_Age_Error_Selectivity_Repo/R")
source("Functions.R")

library(dplyr)
library(tidyr)
library(future.apply)

n_iter <- 100
batch_size <- 100

# dome shaped
# setup scenarios for each species
{
  # Shared life history params
  L_inf <- 500
  t_0 <- 1
  CV_L <- 0.1
  shape <- 2  #1=logistic, 2=dome
  sel_1 <- NA
  sel_2 <- NA
  B1 <- seq(0, 400, 100)
  B2 <- seq(-4, 0, 1)
  B3 <- seq(8, 12, 1)
  B4 <- seq(9, 13, 1)
  sig_r <- 0.6
  CV_Age <- seq(0, 0.20, 0.05)
  sample_size <- c(500, 1000)
  
  column_names <- c("max_age", "M", "L_inf", "k", "t_0", "CV_L", "shape","sel_1", 
                    "sel_2", "B1", "B2", "B3", "B4", "sig_r", "CV_Age", "sample_size")
  
  # Create a data frames with all possible combinations
  # blackgill
  max_age <- 100
  M <- 0.075
  k <- 0.05
  combinations_dome <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                   sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_dome <- as.matrix(combinations_dome)
  colnames(combinations_matrix_dome) <- column_names
  blackgill_scenario_dome <- combinations_matrix_dome
  
  # blue
  max_age <- 50
  M <- 0.15
  k <- 0.09
  combinations_dome <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                   sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_dome <- as.matrix(combinations_dome)
  colnames(combinations_matrix_dome) <- column_names
  blue_scenario_dome <- combinations_matrix_dome
  
  # olive
  max_age <- 30
  M <- 0.25
  k <- 0.152
  combinations_dome <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                   sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_dome <- as.matrix(combinations_dome)
  colnames(combinations_matrix_dome) <- column_names
  olive_scenario_dome <- combinations_matrix_dome
  
  # calico
  max_age <- 10
  M <- 0.5
  k <- 0.303
  combinations_dome <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                   sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_dome <- as.matrix(combinations_dome)
  colnames(combinations_matrix_dome) <- column_names
  calico_scenario_dome <- combinations_matrix_dome
}


plan(multisession, workers = 6)

# blackgill
set.seed(9265)
blackgill_output <- dome_batch(
  scenario_matrix = blackgill_scenario_dome,
  species_name = "Blackgill",
  n_iter = n_iter,
  batch_size = batch_size
)
blackgill_results_df_dome <- blackgill_output$results_df
blackgill_flat_dome <- blackgill_output$flat_df
blackgill_flat_dome$spp <- "blackgill"


# blue Rockfish
set.seed(9265)
blue_output <- dome_batch(
  scenario_matrix = blue_scenario_dome,
  species_name = "Blue Rockfish",
  n_iter = n_iter,
  batch_size = batch_size
)
blue_results_df_dome <- blue_output$results_df
blue_flat_dome <- blue_output$flat_df
blue_flat_dome$spp <- "blue"


# olive Rockfish
set.seed(9265)
olive_output <- dome_batch(
  scenario_matrix = olive_scenario_dome,
  species_name = "Olive Rockfish",
  n_iter = n_iter,
  batch_size = batch_size
)
olive_results_df_dome <- olive_output$results_df
olive_flat_dome <- olive_output$flat_df
olive_flat_dome$spp <- "olive"


# calico Rockfish
set.seed(9265)
calico_output <- dome_batch(
  scenario_matrix = calico_scenario_dome,
  species_name = "Calico Rockfish",
  n_iter = n_iter,
  batch_size = batch_size
)
calico_results_df_dome <- calico_output$results_df
calico_flat_dome <- calico_output$flat_df
calico_flat_dome$spp <- "calico"


all_flat_dome <- rbind(
  blackgill_flat_dome, 
  blue_flat_dome, 
  olive_flat_dome, 
  calico_flat_dome
)

save.image("workspace_dome.RData")