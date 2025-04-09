#To Do
#cleanup code, cleanup comments
#make sure selectivity is applied correctly
#maybe make CV_L age related because young ages should have lower CV_L
#should I add in age 0, cope didn't

rm(list = ls())
gc()

setwd("C:/Users/Derek.Chamberlin/Work/Research/VBGF_Age_Error_Selectivity_Repo/R")
source("Functions.R")

#setup scenarios for each species
{
  #Shared life history params
  L_inf <- 500
  t_0 <- 1
  CV_L <- 0.1
  sel_1 <- seq(0, 300, 30) #all varied params must be same length for surface plot
  sel_2 <- seq(10, 110, 10) #cope set to ~100
  sig_r <- 0.6
  CV_Age <- seq(0, 0.15, 0.015)
  sample_size <- c(100,500)
  
  #Create a data frames with all possible combinations
  #blackgill
  max_age <- 100
  M <- 0.075
  k <- 0.05
  combinations <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)
  combinations_matrix <- as.matrix(combinations)
  colnames(combinations_matrix) <- c("max_age", "M", "L_inf", "k", "t_0", "CV_L", "sel_1", "sel_2", "sig_r", "CV_Age", "sample_size")
  blackgill_scenario <- combinations_matrix
  
  #blue
  max_age <- 50
  M <- 0.15
  k <- 0.09
  combinations <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)
  combinations_matrix <- as.matrix(combinations)
  colnames(combinations_matrix) <- c("max_age", "M", "L_inf", "k", "t_0", "CV_L", "sel_1", "sel_2", "sig_r", "CV_Age", "sample_size")
  blue_scenario <- combinations_matrix
  
  #olive
  max_age <- 30
  M <- 0.25
  k <- 0.152
  combinations <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)
  combinations_matrix <- as.matrix(combinations)
  colnames(combinations_matrix) <- c("max_age", "M", "L_inf", "k", "t_0", "CV_L", "sel_1", "sel_2", "sig_r", "CV_Age", "sample_size")
  olive_scenario <- combinations_matrix
  
  #calico
  max_age <- 10
  M <- 0.5
  k <- 0.303
  combinations <- expand.grid(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)
  combinations_matrix <- as.matrix(combinations)
  colnames(combinations_matrix) <- c("max_age", "M", "L_inf", "k", "t_0", "CV_L", "sel_1", "sel_2", "sig_r", "CV_Age", "sample_size")
  calico_scenario <- combinations_matrix
}

n_iter <- 100

set.seed(51)
blackgill_results <- apply(blackgill_scenario, 1, run_OM, n_iter = n_iter)
blackgill_flat <- flatten_results(blackgill_results, blackgill_scenario)
blackgill_mean_vbgf_re <- mean_vbgf_re(blackgill_results, n_iter)
blackgill_results_df <- data.frame(
  max_age = blackgill_scenario[, 1],
  M = blackgill_scenario[, 2],
  L_inf = blackgill_scenario[, 3],
  k = blackgill_scenario[, 4],
  t_0 = blackgill_scenario[, 5],
  CV_L = blackgill_scenario[, 6],
  sel_1 = blackgill_scenario[, 7],
  sel_2 = blackgill_scenario[, 8],
  sig_r = blackgill_scenario[, 9],
  CV_Age = blackgill_scenario[, 10],
  sample_size = blackgill_scenario[, 11],
  mean_re_L_inf = blackgill_mean_vbgf_re[, 1],
  mean_re_k = blackgill_mean_vbgf_re[, 2],
  mean_re_t_0 = blackgill_mean_vbgf_re[, 3],
  mean_re_CV_L = blackgill_mean_vbgf_re[, 4]
)


set.seed(51)
blue_results <- apply(blue_scenario, 1, run_OM, n_iter = n_iter)
blue_flat <- flatten_results(blue_results, blue_scenario)
blue_mean_vbgf_re <- mean_vbgf_re(blue_results, n_iter)
blue_results_df <- data.frame(
  max_age = blue_scenario[, 1],
  M = blue_scenario[, 2],
  L_inf = blue_scenario[, 3],
  k = blue_scenario[, 4],
  t_0 = blue_scenario[, 5],
  CV_L = blue_scenario[, 6],
  sel_1 = blue_scenario[, 7],
  sel_2 = blue_scenario[, 8],
  sig_r = blue_scenario[, 9],
  CV_Age = blue_scenario[, 10],
  sample_size = blue_scenario[, 11],
  mean_re_L_inf = blue_mean_vbgf_re[, 1],
  mean_re_k = blue_mean_vbgf_re[, 2],
  mean_re_t_0 = blue_mean_vbgf_re[, 3],
  mean_re_CV_L = blue_mean_vbgf_re[, 4]
)


set.seed(51)
olive_results <- apply(olive_scenario, 1, run_OM, n_iter = n_iter)
olive_flat <- flatten_results(olive_results, olive_scenario)
olive_mean_vbgf_re <- mean_vbgf_re(olive_results, n_iter)
olive_results_df <- data.frame(
  max_age = olive_scenario[, 1],
  M = olive_scenario[, 2],
  L_inf = olive_scenario[, 3],
  k = olive_scenario[, 4],
  t_0 = olive_scenario[, 5],
  CV_L = olive_scenario[, 6],
  sel_1 = olive_scenario[, 7],
  sel_2 = olive_scenario[, 8],
  sig_r = olive_scenario[, 9],
  CV_Age = olive_scenario[, 10],
  sample_size = olive_scenario[, 11],
  mean_re_L_inf = olive_mean_vbgf_re[, 1],
  mean_re_k = olive_mean_vbgf_re[, 2],
  mean_re_t_0 = olive_mean_vbgf_re[, 3],
  mean_re_CV_L = olive_mean_vbgf_re[, 4]
)


set.seed(51)
calico_results <- apply(calico_scenario, 1, run_OM, n_iter = n_iter)
calico_flat <- flatten_results(calico_results, calico_scenario)
calico_mean_vbgf_re <- mean_vbgf_re(calico_results, n_iter)
calico_results_df <- data.frame(
  max_age = calico_scenario[, 1],
  M = calico_scenario[, 2],
  L_inf = calico_scenario[, 3],
  k = calico_scenario[, 4],
  t_0 = calico_scenario[, 5],
  CV_L = calico_scenario[, 6],
  sel_1 = calico_scenario[, 7],
  sel_2 = calico_scenario[, 8],
  sig_r = calico_scenario[, 9],
  CV_Age = calico_scenario[, 10],
  sample_size = calico_scenario[, 11],
  mean_re_L_inf = calico_mean_vbgf_re[, 1],
  mean_re_k = calico_mean_vbgf_re[, 2],
  mean_re_t_0 = calico_mean_vbgf_re[, 3],
  mean_re_CV_L = calico_mean_vbgf_re[, 4]
)

save.image("workspace.RData")






#plot individual scenario/iteration
plot(blackgill_results[[243]][[1]][[4]],blackgill_results[[243]][[1]][[2]], ylim = c(0,1000)) #sampled true age and mean length
points(blackgill_results[[243]][[1]][[5]],blackgill_results[[243]][[1]][[2]], col = "red") #observed age and mean length
points(blackgill_results[[243]][[1]][[5]],blackgill_results[[243]][[1]][[3]], col = "green") #observed age and observed length
points(blackgill_results[[243]][[1]][[4]],blackgill_results[[243]][[1]][[3]], col = "blue") #sampled true age and observed length


#plot individual scenario/iteration
plot(calico_results[[110]][[1]][[4]],calico_results[[110]][[1]][[2]], xlim = c(0,15), ylim = c(0,1000)) #sampled true age and mean length
points(calico_results[[110]][[1]][[5]],calico_results[[110]][[1]][[2]], col = "red") #observed age and mean length
points(calico_results[[110]][[1]][[5]],calico_results[[110]][[1]][[3]], col = "green") #observed age and observed length
points(calico_results[[110]][[1]][[4]],calico_results[[110]][[1]][[3]], col = "blue") #sampled true age and observed length






max_age<-100 #max age
M<-0.075 # instantaneous natural mortality of adults 
L_inf<-500 #asymtotic mean length
k<-0.05 #von B growth coeeficient
t_0<- 0.05 #theoretical age of 0 length could be - if L_inf(1-exp(-k*(t-t_0)))
CV_L<-0.1 # variation in growth (comes in later)
sel_1<-250#Selectivity is length based with sel50% at length 70
sel_2<-25 #determines how steep the logistic curve is
sig_r<-0.6
sample_size <- 500
CV_Age = 0

seed<-235
set.seed(seed)

results <- OM(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)

plot(results[[1]])

plot(results[[4]],results[[2]], ylim = c(0,1000)) #sampled true age and mean length
points(results[[4]],results[[3]], col = "red") #add in observed length

plot(results[[4]], results[[5]])

plot(results[[4]],results[[2]], ylim = c(0,1000)) #sampled true age and mean length
points(results[[5]],results[[2]], col = "red") #observed age and mean length
points(results[[5]],results[[3]], col = "green") #observed age and observed length
points(results[[4]],results[[3]], col = "blue") #sampled true age and observed length
