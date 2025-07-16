logistic_selectivity_function <- function(mean_len, sel_1, sel_2) {
  return(1 / (1 + exp(-sel_2 * (mean_len - sel_1))))
}

dome_selectivity_function <- function(size, B1, B2, B3, B4) {
  max_size <- max(size)
  peak2 <- B1 + 1 + ((0.99 * max_size - B1 - 1) / (1 + exp(-B2)))
  j1 <- (1 + exp(-20 * ((size - B1) / (1 + abs(size - B1)))))^-1
  j2 <- (1 + exp(-20 * ((size - peak2) / (1 + abs(size - peak2)))))^-1
  asc <- exp(-(size - B1)^2 / exp(B3))
  dsc <- exp(-(size - peak2)^2 / exp(B4))
  sel <- asc * (1 - j1) + j1 * ((1 - j2) + j2 * dsc)
  return(sel / max(sel))
}

OM <- function(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size){
  age<-1:max_age # age vector
  mean_len<-L_inf*(1-exp(-k*(age+t_0))) #mean length using the von B
  
  Ma<-M*(L_inf/mean_len) #length dependent age specific mortality
  surv<-exp(-Ma) # convert instantaneous M to survival to tidy up survivorship calculations
  lxo<-c(1,cumprod(surv)[1:(max_age-1)]) #survivorship
  lxo[max_age]=lxo[max_age]/(1-surv[max_age]) #accounting for the plus group
  
  size <- 1:1000 #range of plausible lengths, L_inf is 500
  max_size <- max(size)
  
  if (shape == 2) {
    sel <- dome_selectivity_function(size, B1, B2, B3, B4)
  } else if (shape == 1) {
    sel <- logistic_selectivity_function(size, sel_1, sel_2)
    
  } else {
    stop("Invalid shape specified. Please choose 'dome' or 'logistic'.")
  }
  
  sel <- sel / max(sel)
  
  perr<-rnorm((max_age),0,sig_r) #recruitment anomalies
  
  nt<-lxo*exp(perr[1:max_age]-0.5*sig_r*sig_r) #numbers at age accounting for mortality and process variation
  
  #old sampling design that introduced CV_L after sel
  {
  #vnt<-nt*sel #vulnerable numbers accounting for selectivity
  #vnt_probs <- vnt/sum(vnt) #make sure vulnerable probability sums to 1
  
  #sampled_true_ages <- sample(age, size = sample_size, replace = TRUE, prob = vnt_probs) #true age individuals sampled from vulnerable population
  
  #mean_len_samp<-L_inf*(1-exp(-k*(sampled_true_ages+t_0))) #mean length of sampled individuals
  
  #obs_len <- mean_len_samp + rnorm(mean_len_samp,0,mean_len_samp*CV_L) #observed length adding in growth process error
  }
    
  #updated sampling design to avoid introducing CV_L after selectivity
  {
  nt_prob <- nt/sum(nt) #proportion at age
  pop_ages <- sample(age, size = 1000000, replace = TRUE, prob = nt_prob) #sample from those proportions
  pop_len_mean <- L_inf*(1-exp(-k*(pop_ages+t_0))) #calculate mean length for each individual
  pop_len_obs <- round(pop_len_mean + rnorm(pop_len_mean,0,pop_len_mean*CV_L)) #add in growth variability
  
  fake_fish <- data.frame(
    length = pop_len_obs,
    age = pop_ages
  )
  
  # count number of individuals for each length and age combo
  len_age_matrix <- fake_fish %>%
    filter(length >= 1, length <= 1000) %>%
    count(length, age) %>%
    complete(length = 1:1000, age = 1:max_age, fill = list(n = 0)) %>%
    pivot_wider(names_from = age, values_from = n) %>%
    arrange(length)
  
  len_age_matrix <- as.matrix(len_age_matrix[,-1])
  
  #apply selectivity
  len_age_sel <- (len_age_matrix * sel)/sum(len_age_matrix * sel)
  
  #calculate join length, age probability
  joint_probs_df <- expand.grid(
    length = 1:1000,
    age = age
  ) %>%
    mutate(prob = as.vector(len_age_sel))
  
  #sample from join probability
  sampled_fish <- joint_probs_df %>%
    slice_sample(n = sample_size, weight_by = prob, replace = TRUE)
  }
  
  AE_mat<-diag(max_age)
  
  bias = 0 #Can add in ability to test the effects of bias in addition to imprecision
  
  #defining ageing error matrix
  for (i in 1:nrow(AE_mat)) {
    for(j in 1:nrow(AE_mat)){
      if (j %in% 1:(nrow(AE_mat)-1)){        #integrate from age+0.5 to age-0.5
        AE_mat[i,j]<-pnorm(age[j]+0.5, mean = age[i]-bias, sd = (age[i]*CV_Age))-pnorm(age[j]-0.5, mean = age[i]-bias, sd = (age[i]*CV_Age))
      }else if (j==nrow(AE_mat)){    # if you are in plus group integrate from age-0.5 to infinity
        AE_mat[i,j]<-1-pnorm(age[j]-0.5, mean = age[i]-bias, sd = (age[i]*CV_Age))
      }
    }
  }
  
  obs_age <- vector(length = length(sampled_fish$age))
  
  #adding ageing error to true ages
  for (i in 1:length(sampled_fish$age)) {
    obs_age[i]<- sample(age, size = 1, prob = AE_mat[sampled_fish$age[i],])
  }
  
  
  
  


  
  #add VBGF calcs here
  nll <- function(theta){
    vblinf <- exp(theta[1])
    vbk <- exp(theta[2])
    vbto <- exp(theta[3])
    vbcv <- exp(theta[4])
    plengths <- vblinf*(1-exp(-vbk*(obs_age+vbto)))
    nll <- -sum(dnorm(sampled_fish$length,plengths,vbcv*plengths,log=TRUE))
    return(nll)
  }
  theta <- c(log(L_inf),log(k),log(0.5),log(CV_L))
  fit <- optim(theta,nll,hessian=TRUE)
  #se.fit<-sqrt(diag(solve(fit$hessian)))
  
  report <- function(theta){
    vblinf <- exp(theta[1])
    vbk <- exp(theta[2])
    vbto <- exp(theta[3])
    vbcv <- exp(theta[4])
    plengths <- vblinf*(1-exp(-vbk*(obs_age+vbto)))
    nll <- -sum(dnorm(sampled_fish$length,plengths,vbcv*plengths,log=TRUE))
    return(list(vblinf=vblinf,vbk=vbk,vbto=vbto,vbcv=vbcv,obs_age=obs_age,obs_len=sampled_fish$length,plengths=plengths))
  }
  vbgf_params <- report(fit$par)
  
  vbgf_params_RE <- vector(length = 4)
  vbgf_params_RE[1] <- (vbgf_params$vblinf-L_inf)/L_inf
  vbgf_params_RE[2] <- (vbgf_params$vbk-k)/k
  vbgf_params_RE[3] <- (vbgf_params$vbto-t_0)/t_0
  vbgf_params_RE[4] <- (vbgf_params$vbcv-CV_L)/CV_L
  
  #return(list(vnt, mean_len_samp, obs_len, sampled_true_ages, obs_age, vbgf_params, vbgf_params_RE))
  return(list(vbgf_params_RE, sampled_fish$age, sampled_fish$length, obs_age, vbgf_params))
}


run_OM <- function(row, n_iter) {
  max_age <- row[1]
  M <- row[2]
  L_inf <- row[3]
  k <- row[4]
  t_0 <- row[5]
  CV_L <- row[6]
  shape <- row[7]
  sel_1 <- row[8]
  sel_2 <- row[9]
  B1 <- row[10]
  B2 <- row[11]
  B3 <- row[12]
  B4 <- row[13]
  sig_r <- row[14]
  CV_Age <- row[15]
  sample_size <- row[16]
  
  results <- list()
  k_re_estimates <- vector(length = n_iter) 
  
  for (i in 1:n_iter) {
    results[[i]] <- OM(max_age, M, L_inf, k, t_0, CV_L, shape, sel_1, sel_2, B1, B2, B3, B4, sig_r, CV_Age, sample_size)
  }
  
  return(results)
}



mean_vbgf_re <- function(results, n_iter) {
  vbgf_means <- matrix(nrow = length(results), ncol = 4)
  
  for (i in 1:length(results)) {
    L_inf_values <- sapply(results[[i]], function(res) res[[1]][1]) #change to res[[7]][1] if reporting other metrics
    k_values <- sapply(results[[i]], function(res) res[[1]][2])
    t_0_values <- sapply(results[[i]], function(res) res[[1]][3])
    CV_L_values <- sapply(results[[i]], function(res) res[[1]][4])
    
    vbgf_means[i,1] <- mean(L_inf_values)
    vbgf_means[i,2] <- mean(k_values)
    vbgf_means[i,3] <- mean(t_0_values)
    vbgf_means[i,4] <- mean(CV_L_values)
  }
  
  return(vbgf_means)
}

flatten_results <- function(spp_results, spp_scenario){
  flat <- matrix(nrow = length(spp_results)*n_iter, ncol = 20)
  colnames(flat) <- c(
    "L_inf_RE", "k_RE", "t_0_RE", "CV_L_RE",
    "max_age", "M", "L_inf", "k", "t_0",
    "CV_L", "shape", "sel_1", "sel_2", 
    "B1", "B2", "B3", "B4", "sig_r",
    "CV_Age", "sample_size"
  )
  for (i in 1:length(spp_results)) {
    for (j in 1:n_iter) {
      flat[((i-1)*n_iter)+j,1:4] <- spp_results[[i]][[j]][[1]][1:4]
      flat[((i-1)*n_iter)+j,5:20] <- spp_scenario[i,]
    }
  }
  return(as.data.frame(flat))
}

dome_batch <- function(scenario_matrix, species_name, n_iter, batch_size) {
  n_scenarios <- nrow(scenario_matrix)
  n_batches <- ceiling(n_scenarios / batch_size)
  
  results_list <- list()
  flat_list <- list()
  
  for (i in 1:n_batches) {
    
    cat(sprintf("Processing %s: Batch %d of %d...\n", species_name, i, n_batches))
    
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, n_scenarios)
    
    current_scenarios <- scenario_matrix[start_row:end_row, ]
    
    batch_results <- future_apply(current_scenarios, 1, run_OM, n_iter = n_iter)
    
    flat_results <- flatten_results(batch_results, current_scenarios)
    mean_re <- mean_vbgf_re(batch_results, n_iter)
    
    results_df <- data.frame(
      max_age = current_scenarios[, "max_age"],
      M = current_scenarios[, "M"],
      L_inf = current_scenarios[, "L_inf"],
      k = current_scenarios[, "k"],
      t_0 = current_scenarios[, "t_0"],
      CV_L = current_scenarios[, "CV_L"],
      shape = current_scenarios[, "shape"],
      sel_1 = current_scenarios[, "sel_1"],
      sel_2 = current_scenarios[, "sel_2"],
      B1 = current_scenarios[, "B1"],
      B2 = current_scenarios[, "B2"],
      B3 = current_scenarios[, "B3"],
      B4 = current_scenarios[, "B4"],
      sig_r = current_scenarios[, "sig_r"],
      CV_Age = current_scenarios[, "CV_Age"],
      sample_size = current_scenarios[, "sample_size"],
      mean_re_L_inf = mean_re[, 1],
      mean_re_k = mean_re[, 2],
      mean_re_t_0 = mean_re[, 3],
      mean_re_CV_L = mean_re[, 4]
    )
    
    results_list[[i]] <- results_df
    flat_list[[i]] <- flat_results
    
    #remove batch to prevent memory issues
    rm(batch_results, flat_results, mean_re, results_df, current_scenarios)
    gc()
  }

  final_results_df <- do.call(rbind, results_list)
  final_flat_df <- do.call(rbind, flat_list)
  
  cat(sprintf("Finished processing for %s.\n\n", species_name))
  
  return(list(results_df = final_results_df, flat_df = final_flat_df))
}