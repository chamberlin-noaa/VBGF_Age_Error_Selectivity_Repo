# To Do
# cleanup code, cleanup comments

rm(list = ls())
gc()

setwd("C:/Users/Derek.Chamberlin/Work/Research/VBGF_Age_Error_Selectivity_Repo/R")
source("Functions.R")

library(dplyr)
library(tidyr)
library(future.apply)

n_iter <- 100

# setup scenarios for each species
{
  # Shared life history params
  L_inf <- 500
  t_0 <- 1
  CV_L <- 0.1
  shape <- 1 #1=logistic, 2=dome
  sel_1 <- seq(0, 400, 50) # all varied params must be same length for surface plot
  sel_2 <- seq(0.01, 0.21, 0.025) # seq(1, 101, 10) #cope set to ~100
  B1 <- NA
  B2 <- NA
  B3 <- NA
  B4 <- NA
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
  combinations_logistic <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                       sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_logistic <- as.matrix(combinations_logistic)
  colnames(combinations_matrix_logistic) <- column_names
  blackgill_scenario_logistic <- combinations_matrix_logistic

  # blue
  max_age <- 50
  M <- 0.15
  k <- 0.09
  combinations_logistic <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                       sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_logistic <- as.matrix(combinations_logistic)
  colnames(combinations_matrix_logistic) <- column_names
  blue_scenario_logistic <- combinations_matrix_logistic

  # olive
  max_age <- 30
  M <- 0.25
  k <- 0.152
  combinations_logistic <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                       sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_logistic <- as.matrix(combinations_logistic)
  colnames(combinations_matrix_logistic) <- column_names
  olive_scenario_logistic <- combinations_matrix_logistic

  # calico
  max_age <- 10
  M <- 0.5
  k <- 0.303
  combinations_logistic <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
                                       sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  combinations_matrix_logistic <- as.matrix(combinations_logistic)
  colnames(combinations_matrix_logistic) <- column_names
  calico_scenario_logistic <- combinations_matrix_logistic
}

# Set up parallel processing (use all available cores)
plan(multisession)

set.seed(9265)
blackgill_results_logistic <- future_apply(blackgill_scenario_logistic, 1, run_OM, n_iter = n_iter)
blackgill_flat_logistic <- flatten_results(blackgill_results_logistic, blackgill_scenario_logistic)
blackgill_mean_vbgf_re_logistic <- mean_vbgf_re(blackgill_results_logistic, n_iter)
blackgill_results_df_logistic <- data.frame(
  max_age = blackgill_scenario_logistic[, "max_age"],
  M = blackgill_scenario_logistic[, "M"],
  L_inf = blackgill_scenario_logistic[, "L_inf"],
  k = blackgill_scenario_logistic[, "k"],
  t_0 = blackgill_scenario_logistic[, "t_0"],
  CV_L = blackgill_scenario_logistic[, "CV_L"],
  shape = blackgill_scenario_logistic[, "shape"],
  sel_1 = blackgill_scenario_logistic[, "sel_1"],
  sel_2 = blackgill_scenario_logistic[, "sel_2"],
  B1 = blackgill_scenario_logistic[, "B1"],
  B2 = blackgill_scenario_logistic[, "B2"],
  B3 = blackgill_scenario_logistic[, "B3"],
  B4 = blackgill_scenario_logistic[, "B4"],
  sig_r = blackgill_scenario_logistic[, "sig_r"],
  CV_Age = blackgill_scenario_logistic[, "CV_Age"],
  sample_size = blackgill_scenario_logistic[, "sample_size"],
  mean_re_L_inf = blackgill_mean_vbgf_re_logistic[, 1],
  mean_re_k = blackgill_mean_vbgf_re_logistic[, 2],
  mean_re_t_0 = blackgill_mean_vbgf_re_logistic[, 3],
  mean_re_CV_L = blackgill_mean_vbgf_re_logistic[, 4]
)


set.seed(9265)
blue_results_logistic <- future_apply(blue_scenario_logistic, 1, run_OM, n_iter = n_iter)
blue_flat_logistic <- flatten_results(blue_results_logistic, blue_scenario_logistic)
blue_mean_vbgf_re_logistic <- mean_vbgf_re(blue_results_logistic, n_iter)
blue_results_df_logistic <- data.frame(
  max_age = blue_scenario_logistic[, "max_age"],
  M = blue_scenario_logistic[, "M"],
  L_inf = blue_scenario_logistic[, "L_inf"],
  k = blue_scenario_logistic[, "k"],
  t_0 = blue_scenario_logistic[, "t_0"],
  CV_L = blue_scenario_logistic[, "CV_L"],
  shape = blue_scenario_logistic[, "shape"],
  sel_1 = blue_scenario_logistic[, "sel_1"],
  sel_2 = blue_scenario_logistic[, "sel_2"],
  B1 = blue_scenario_logistic[, "B1"],
  B2 = blue_scenario_logistic[, "B2"],
  B3 = blue_scenario_logistic[, "B3"],
  B4 = blue_scenario_logistic[, "B4"],
  sig_r = blue_scenario_logistic[, "sig_r"],
  CV_Age = blue_scenario_logistic[, "CV_Age"],
  sample_size = blue_scenario_logistic[, "sample_size"],
  mean_re_L_inf = blue_mean_vbgf_re_logistic[, 1],
  mean_re_k = blue_mean_vbgf_re_logistic[, 2],
  mean_re_t_0 = blue_mean_vbgf_re_logistic[, 3],
  mean_re_CV_L = blue_mean_vbgf_re_logistic[, 4]
)


set.seed(9265)
olive_results_logistic <- future_apply(olive_scenario_logistic, 1, run_OM, n_iter = n_iter)
olive_flat_logistic <- flatten_results(olive_results_logistic, olive_scenario_logistic)
olive_mean_vbgf_re_logistic <- mean_vbgf_re(olive_results_logistic, n_iter)
olive_results_df_logistic <- data.frame(
  max_age = olive_scenario_logistic[, "max_age"],
  M = olive_scenario_logistic[, "M"],
  L_inf = olive_scenario_logistic[, "L_inf"],
  k = olive_scenario_logistic[, "k"],
  t_0 = olive_scenario_logistic[, "t_0"],
  CV_L = olive_scenario_logistic[, "CV_L"],
  shape = olive_scenario_logistic[, "shape"],
  sel_1 = olive_scenario_logistic[, "sel_1"],
  sel_2 = olive_scenario_logistic[, "sel_2"],
  B1 = olive_scenario_logistic[, "B1"],
  B2 = olive_scenario_logistic[, "B2"],
  B3 = olive_scenario_logistic[, "B3"],
  B4 = olive_scenario_logistic[, "B4"],
  sig_r = olive_scenario_logistic[, "sig_r"],
  CV_Age = olive_scenario_logistic[, "CV_Age"],
  sample_size = olive_scenario_logistic[, "sample_size"],
  mean_re_L_inf = olive_mean_vbgf_re_logistic[, 1],
  mean_re_k = olive_mean_vbgf_re_logistic[, 2],
  mean_re_t_0 = olive_mean_vbgf_re_logistic[, 3],
  mean_re_CV_L = olive_mean_vbgf_re_logistic[, 4]
)


set.seed(9265)
calico_results_logistic <- future_apply(calico_scenario_logistic, 1, run_OM, n_iter = n_iter)
calico_flat_logistic <- flatten_results(calico_results_logistic, calico_scenario_logistic)
calico_mean_vbgf_re_logistic <- mean_vbgf_re(calico_results_logistic, n_iter)
calico_results_df_logistic <- data.frame(
  max_age = calico_scenario_logistic[, "max_age"],
  M = calico_scenario_logistic[, "M"],
  L_inf = calico_scenario_logistic[, "L_inf"],
  k = calico_scenario_logistic[, "k"],
  t_0 = calico_scenario_logistic[, "t_0"],
  CV_L = calico_scenario_logistic[, "CV_L"],
  shape = calico_scenario_logistic[, "shape"],
  sel_1 = calico_scenario_logistic[, "sel_1"],
  sel_2 = calico_scenario_logistic[, "sel_2"],
  B1 = calico_scenario_logistic[, "B1"],
  B2 = calico_scenario_logistic[, "B2"],
  B3 = calico_scenario_logistic[, "B3"],
  B4 = calico_scenario_logistic[, "B4"],
  sig_r = calico_scenario_logistic[, "sig_r"],
  CV_Age = calico_scenario_logistic[, "CV_Age"],
  sample_size = calico_scenario_logistic[, "sample_size"],
  mean_re_L_inf = calico_mean_vbgf_re_logistic[, 1],
  mean_re_k = calico_mean_vbgf_re_logistic[, 2],
  mean_re_t_0 = calico_mean_vbgf_re_logistic[, 3],
  mean_re_CV_L = calico_mean_vbgf_re_logistic[, 4]
)

blackgill_flat_logistic$spp <- "blackgill"
blue_flat_logistic$spp <- "blue"
olive_flat_logistic$spp <- "olive"
calico_flat_logistic$spp <- "calico"
all_flat_logistic <- rbind(blackgill_flat_logistic, blue_flat_logistic, olive_flat_logistic, calico_flat_logistic)

save.image("workspace_logistic.RData")

#Dome Shaped
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


# Set up parallel processing (use all available cores)
plan(multisession)

set.seed(9265)
blackgill_results_dome <- future_apply(blackgill_scenario_dome, 1, run_OM, n_iter = n_iter)
blackgill_flat_dome <- flatten_results(blackgill_results_dome, blackgill_scenario_dome)
blackgill_mean_vbgf_re_dome <- mean_vbgf_re(blackgill_results_dome, n_iter)
blackgill_results_df_dome <- data.frame(
  max_age = blackgill_scenario_dome[, "max_age"],
  M = blackgill_scenario_dome[, "M"],
  L_inf = blackgill_scenario_dome[, "L_inf"],
  k = blackgill_scenario_dome[, "k"],
  t_0 = blackgill_scenario_dome[, "t_0"],
  CV_L = blackgill_scenario_dome[, "CV_L"],
  shape = blackgill_scenario_dome[, "shape"],
  sel_1 = blackgill_scenario_dome[, "sel_1"],
  sel_2 = blackgill_scenario_dome[, "sel_2"],
  B1 = blackgill_scenario_dome[, "B1"],
  B2 = blackgill_scenario_dome[, "B2"],
  B3 = blackgill_scenario_dome[, "B3"],
  B4 = blackgill_scenario_dome[, "B4"],
  sig_r = blackgill_scenario_dome[, "sig_r"],
  CV_Age = blackgill_scenario_dome[, "CV_Age"],
  sample_size = blackgill_scenario_dome[, "sample_size"],
  mean_re_L_inf = blackgill_mean_vbgf_re_dome[, 1],
  mean_re_k = blackgill_mean_vbgf_re_dome[, 2],
  mean_re_t_0 = blackgill_mean_vbgf_re_dome[, 3],
  mean_re_CV_L = blackgill_mean_vbgf_re_dome[, 4]
)


set.seed(9265)
blue_results_dome <- future_apply(blue_scenario_dome, 1, run_OM, n_iter = n_iter)
blue_flat_dome <- flatten_results(blue_results_dome, blue_scenario_dome)
blue_mean_vbgf_re_dome <- mean_vbgf_re(blue_results_dome, n_iter)
blue_results_df_dome <- data.frame(
  max_age = blue_scenario_dome[, "max_age"],
  M = blue_scenario_dome[, "M"],
  L_inf = blue_scenario_dome[, "L_inf"],
  k = blue_scenario_dome[, "k"],
  t_0 = blue_scenario_dome[, "t_0"],
  CV_L = blue_scenario_dome[, "CV_L"],
  shape = blue_scenario_dome[, "shape"],
  sel_1 = blue_scenario_dome[, "sel_1"],
  sel_2 = blue_scenario_dome[, "sel_2"],
  B1 = blue_scenario_dome[, "B1"],
  B2 = blue_scenario_dome[, "B2"],
  B3 = blue_scenario_dome[, "B3"],
  B4 = blue_scenario_dome[, "B4"],
  sig_r = blue_scenario_dome[, "sig_r"],
  CV_Age = blue_scenario_dome[, "CV_Age"],
  sample_size = blue_scenario_dome[, "sample_size"],
  mean_re_L_inf = blue_mean_vbgf_re_dome[, 1],
  mean_re_k = blue_mean_vbgf_re_dome[, 2],
  mean_re_t_0 = blue_mean_vbgf_re_dome[, 3],
  mean_re_CV_L = blue_mean_vbgf_re_dome[, 4]
)


set.seed(9265)
olive_results_dome <- future_apply(olive_scenario_dome, 1, run_OM, n_iter = n_iter)
olive_flat_dome <- flatten_results(olive_results_dome, olive_scenario_dome)
olive_mean_vbgf_re_dome <- mean_vbgf_re(olive_results_dome, n_iter)
olive_results_df_dome <- data.frame(
  max_age = olive_scenario_dome[, "max_age"],
  M = olive_scenario_dome[, "M"],
  L_inf = olive_scenario_dome[, "L_inf"],
  k = olive_scenario_dome[, "k"],
  t_0 = olive_scenario_dome[, "t_0"],
  CV_L = olive_scenario_dome[, "CV_L"],
  shape = olive_scenario_dome[, "shape"],
  sel_1 = olive_scenario_dome[, "sel_1"],
  sel_2 = olive_scenario_dome[, "sel_2"],
  B1 = olive_scenario_dome[, "B1"],
  B2 = olive_scenario_dome[, "B2"],
  B3 = olive_scenario_dome[, "B3"],
  B4 = olive_scenario_dome[, "B4"],
  sig_r = olive_scenario_dome[, "sig_r"],
  CV_Age = olive_scenario_dome[, "CV_Age"],
  sample_size = olive_scenario_dome[, "sample_size"],
  mean_re_L_inf = olive_mean_vbgf_re_dome[, 1],
  mean_re_k = olive_mean_vbgf_re_dome[, 2],
  mean_re_t_0 = olive_mean_vbgf_re_dome[, 3],
  mean_re_CV_L = olive_mean_vbgf_re_dome[, 4]
)


set.seed(9265)
calico_results_dome <- future_apply(calico_scenario_dome, 1, run_OM, n_iter = n_iter)
calico_flat_dome <- flatten_results(calico_results_dome, calico_scenario_dome)
calico_mean_vbgf_re_dome <- mean_vbgf_re(calico_results_dome, n_iter)
calico_results_df_dome <- data.frame(
  max_age = calico_scenario_dome[, "max_age"],
  M = calico_scenario_dome[, "M"],
  L_inf = calico_scenario_dome[, "L_inf"],
  k = calico_scenario_dome[, "k"],
  t_0 = calico_scenario_dome[, "t_0"],
  CV_L = calico_scenario_dome[, "CV_L"],
  shape = calico_scenario_dome[, "shape"],
  sel_1 = calico_scenario_dome[, "sel_1"],
  sel_2 = calico_scenario_dome[, "sel_2"],
  B1 = calico_scenario_dome[, "B1"],
  B2 = calico_scenario_dome[, "B2"],
  B3 = calico_scenario_dome[, "B3"],
  B4 = calico_scenario_dome[, "B4"],
  sig_r = calico_scenario_dome[, "sig_r"],
  CV_Age = calico_scenario_dome[, "CV_Age"],
  sample_size = calico_scenario_dome[, "sample_size"],
  mean_re_L_inf = calico_mean_vbgf_re_dome[, 1],
  mean_re_k = calico_mean_vbgf_re_dome[, 2],
  mean_re_t_0 = calico_mean_vbgf_re_dome[, 3],
  mean_re_CV_L = calico_mean_vbgf_re_dome[, 4]
)

blackgill_flat_dome$spp <- "blackgill"
blue_flat_dome$spp <- "blue"
olive_flat_dome$spp <- "olive"
calico_flat_dome$spp <- "calico"
all_flat_dome <- rbind(blackgill_flat_dome, blue_flat_dome, olive_flat_dome, calico_flat_dome)

save.image("workspace_all_scenarios.RData")






# plot individual scenario/iteration
plot(blackgill_results[[243]][[1]][[5]], blackgill_results[[243]][[1]][[3]], ylim = c(0, 1000)) # sampled true age and mean length
points(blackgill_results[[243]][[1]][[6]], blackgill_results[[243]][[1]][[3]], col = "red") # observed age and mean length
points(blackgill_results[[243]][[1]][[6]], blackgill_results[[243]][[1]][[4]], col = "green") # observed age and observed length
points(blackgill_results[[243]][[1]][[5]], blackgill_results[[243]][[1]][[4]], col = "blue") # sampled true age and observed length


# plot individual scenario/iteration
plot(calico_results[[130]][[1]][[5]], calico_results[[130]][[1]][[3]], xlim = c(0, 15), ylim = c(0, 1000)) # sampled true age and mean length
points(calico_results[[130]][[1]][[6]], calico_results[[130]][[1]][[3]], col = "red") # observed age and mean length
points(calico_results[[130]][[1]][[6]], calico_results[[130]][[1]][[4]], col = "green") # observed age and observed length
points(calico_results[[130]][[1]][[5]], calico_results[[130]][[1]][[4]], col = "blue") # sampled true age and observed length

plot(calico_results[[126]][[1]][[5]], calico_results[[126]][[1]][[3]], xlim = c(0, 15), ylim = c(0, 1000)) # sampled true age and mean length
points(calico_results[[126]][[1]][[6]], calico_results[[126]][[1]][[3]], col = "red") # observed age and mean length
points(calico_results[[126]][[1]][[6]], calico_results[[126]][[1]][[4]], col = "green") # observed age and observed length
points(calico_results[[126]][[1]][[5]], calico_results[[126]][[1]][[4]], col = "blue") # sampled true age and observed length




max_age <- 100 # max age
M <- 0.075 # instantaneous natural mortality of adults
L_inf <- 500 # asymtotic mean length
k <- 0.05 # von B growth coeeficient
t_0 <- 0.05 # theoretical age of 0 length could be - if L_inf(1-exp(-k*(t-t_0)))
CV_L <- 0.1 # variation in growth (comes in later)
shape <- 1
sel_1 <- 250 # Selectivity is length based with sel50% at length 70
sel_2 <- 10 # determines how steep the logistic curve is
B1 <- NA
B2 <- NA
B3 <- NA
B4 <- NA
sig_r <- 0.6
sample_size <- 500
CV_Age <- 0

seed <- 235
set.seed(seed)

results <- OM(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, 
              sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)

plot(results[[1]])

plot(results[[4]], results[[2]], ylim = c(0, 1000)) # sampled true age and mean length
points(results[[4]], results[[3]], col = "red") # add in observed length

plot(results[[4]], results[[5]])

plot(results[[4]], results[[2]], ylim = c(0, 1000)) # sampled true age and mean length
points(results[[5]], results[[2]], col = "red") # observed age and mean length
points(results[[5]], results[[3]], col = "green") # observed age and observed length
points(results[[4]], results[[3]], col = "blue") # sampled true age and observed length
