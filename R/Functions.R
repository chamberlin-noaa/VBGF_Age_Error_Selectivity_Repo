
OM <- function(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size){
  age<-1:max_age # age vector
  mean_len<-L_inf*(1-exp(-k*(age+t_0))) #mean length using the von B
  
  Ma<-M*(L_inf/mean_len) #length dependent age specific mortality
  surv<-exp(-Ma) # convert instantaneous M to survival to tidy up survivorship calculations
  lxo<-c(1,cumprod(surv)[1:(max_age-1)]) #survivorship
  lxo[max_age]=lxo[max_age]/(1-surv[max_age]) #accounting for the plus group
  
  sel<-plogis(mean_len,sel_1,sel_2) # logistic selectivity 
  sel<-sel/max(sel) #make sure the max selectivity is 1 
  
  perr<-rnorm((max_age),0,sig_r) #recruitment anomalies
  
  nt<-lxo*exp(perr[1:max_age]-0.5*sig_r*sig_r) #umbers at age accounting for mortality and process variation
  vnt<-nt*sel #vulnerable numbers accounting for selectivity
  
  sampled_true_ages <- sample(age, size = sample_size, replace = TRUE, prob = vnt) #true age individuals sampled from vulnerable population
  
  mean_len_samp<-L_inf*(1-exp(-k*(sampled_true_ages+t_0))) #mean length of sampled individuals
  
  obs_len <- mean_len_samp + rnorm(mean_len_samp,0,mean_len_samp*CV_L) #observed length adding in growth process error
  
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
  
  obs_age <- vector(length = length(sampled_true_ages))
  
  #adding ageing error to true ages
  for (i in 1:length(sampled_true_ages)) {
    obs_age[i]<- sample(age, size = 1, prob = AE_mat[sampled_true_ages[i],])
  }
  
  
  #add VBGF calcs here
  nll <- function(theta){
    vblinf <- exp(theta[1])
    vbk <- exp(theta[2])
    vbto <- exp(theta[3])
    vbcv <- exp(theta[4])
    plengths <- vblinf*(1-exp(-vbk*(obs_age+vbto)))
    nll <- -sum(dnorm(obs_len,plengths,vbcv*plengths,log=TRUE))
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
    nll <- -sum(dnorm(obs_len,plengths,vbcv*plengths,log=TRUE))
    return(list(vblinf=vblinf,vbk=vbk,vbto=vbto,vbcv=vbcv,obs_age=obs_age,obs_len=obs_len,plengths=plengths))
  }
  vbgf_params <- report(fit$par)
  
  vbgf_params_RE <- vector(length = 4)
  vbgf_params_RE[1] <- (vbgf_params$vblinf-L_inf)/L_inf
  vbgf_params_RE[2] <- (vbgf_params$vbk-k)/k
  vbgf_params_RE[3] <- (vbgf_params$vbto-t_0)/t_0
  vbgf_params_RE[4] <- (vbgf_params$vbcv-CV_L)/CV_L
  
  #return(list(vnt, mean_len_samp, obs_len, sampled_true_ages, obs_age, vbgf_params, vbgf_params_RE))
  return(list(vbgf_params_RE))
}


run_OM <- function(row, n_iter) {
  max_age <- row[1]
  M <- row[2]
  L_inf <- row[3]
  k <- row[4]
  t_0 <- row[5]
  CV_L <- row[6]
  sel_1 <- row[7]
  sel_2 <- row[8]
  sig_r <- row[9]
  CV_Age <- row[10]
  sample_size <- row[11]
  
  results <- list()
  k_re_estimates <- vector(length = n_iter) 
  
  for (i in 1:n_iter) {
    results[[i]] <- OM(max_age, M, L_inf, k, t_0, CV_L, sel_1, sel_2, sig_r, CV_Age, sample_size)
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
  flat <- matrix(nrow = length(calico_results)*n_iter, ncol = 15)
  colnames(flat) <- c(
    "L_inf_RE", "k_RE", "t_0_RE", "CV_L_RE",
    "max_age", "M", "L_inf", "k", "t_0",
    "CV_L", "sel_1", "sel_2", "sig_r",
    "CV_Age", "sample_size"
  )
  for (i in 1:length(spp_results)) {
    for (j in 1:n_iter) {
      flat[((i-1)*100)+j,1:4] <- spp_results[[i]][[j]][[1]]
      flat[((i-1)*100)+j,5:15] <- spp_scenario[i,]
    }
  }
  return(flat)
}