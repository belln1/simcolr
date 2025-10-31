# library(psych) # describe()
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(DescTools) # Rev() reverse order of rows and columns in a matrix
# library(gtools) # cut quantiles
# library(ggplot2)
# library(ggfortify) # time series plots
# library(forecast)
# library(patchwork) # combine multiple plots with +
# library(kableExtra)
# library(abind)
# library(stargazer)



# Set Basic Parameter Distributions
sigma_all <- seq(0.1, 0.35, 0.05)
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0
gamma <- c(0.7, 0.8, 0.9)
theta <- c(0, 0.5, 1)
struc <- c(0, 1)


# General Functions --------------------------------------------------------------

# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# rep.col<-function(x,n){
#   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
# }

# Simulation Functions  --------------------------------------------------------------

# combine_parms_model12 <- function(n_firms_in, sigma_in){
#   n_sim <- length(n_firms_in) * length(sigma_in)
#   parms <- tibble(
#     n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
#     sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
#   )
# }
#
# combine_parms_model3 <- function(n_firms_in, sigma_in, gamma_in, theta_in, struc_in){
#   n_sim <- length(n_firms_in) * length(sigma_in) * length(gamma_in) * length(theta_in) * length(struc_in)
#   parms <- tibble(
#     n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
#     sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
#     theta = rep(theta_in, times=length(n_firms_in)*length(sigma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in))),
#     gamma = rep(gamma_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in))),
#     structured = rep(struc_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in)*length(struc_in)))
#   )
# }

# get_delta <- function(r){1/(1+r)}
#
# random_steps <- function(n){
#     x <- rbinom(n, 1, 0.5)
#     (2*x - 1)/500
# }
#
# get_r <- function(){
#   r_start <- runif(1, r_min, r_max)
# }
#
# get_walk_r <- function(){
#   r <- get_r()
#   steps <- random_steps(allperiods-1)
#   cumsum(c(r, steps))
# }
#
# get_deltas_walk_r <- function(walk_r){
#   d <- get_delta(walk_r)
#   deltas <- pnorm(d)
# }



###################
# # sigma = Prob(at least once found during cartel duration) = 1 - ((1-p_t)^duration)
# # p_t = Prob(found in time t) = 1 - ((1-sigma)^(1/duration)) # formula for sigma, solved for p_t
#
# # Model 1 and 2: ICC depending on number of firms (cite Stigler 1964)
# get_ICC_model1 <- function(n) {
#   1-1/n
# }
#
# # Model 3: ICC with fines and leniency (Bos/Davies/Harrington/Ormosi(2018)
#
# get_sigma_all_t <- function(sigma_t){
#   1 - (1-sigma_t)^200
# }
# get_ICC_entry_model3 <- function(n, sigma_t, gamma, theta){
#   sigma <- get_sigma_all_t(sigma_t)
#   1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
# }
# get_ICC_exit_model3 <- function(n, sigma_t, gamma, theta){
#   sigma <- get_sigma_all_t(sigma_t)
#   1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
# }
#
# increase_sigma <- function(sigma, n_times_caught) {
#   return(ifelse(n_times_caught > 0, sigma * (1+1/2^n_times_caught), sigma))
# }

get_sample <- function(firms_in_cartel, detection){
  firms_in_cartel * detection
}

get_undetected <- function(firms_in_cartel, detection){
  firms_in_cartel * (1 - detection)
}

get_detection <- function(periods, sigma){
    x <- matrix(as.numeric(runif(periods) <= sigma), nrow = periods)
}

# get_detection_ind <- function(periods, sigma){
#   x <- as.numeric(runif(periods) <= sigma)
# }

# In Model 2, detection depends on n_firms and sigma
# get_detection_model2_ind <- function(periods, sigma, d_nfirms){
#   x <- as.numeric(runif(periods) <= sigma*d_nfirms) # revised version, more intuitive formular
# }

# Does an industry want to be in a cartel?
# Reduce(function, vector) applies function on the first two elements of vector, then on the result of that and the third element
# accumulate=TRUE returns all results, accumulate=FALSE returns only last result
# get_in_cartel <- function(ind, ICC_entry, ICC_exit) {
#   ind_entry <- ifelse(ind > ICC_entry, 1, 0) # ifelse keeps matrix, if_else makes large vector
#   ind_exit <- ifelse(ind < ICC_exit, -1, 0)
#   v <- ind_entry + ind_exit
#   w <- Reduce(function(x,y) ifelse(y==0, x, y), v, accumulate=TRUE) # fill 0-values with last non-0-value (makes large vector)
#   in_cartel <- if_else(w == -1, 0, 1) # change -1 to 0 (not in cartel)
# }

# We have a cartel if enough firms want to be in a cartel. firm_share = 1 means complete cartels. vector with 0 and 1 in all times
# Version 1, 2 (firm level)
check_firm_share <- function(firms, firm_share){
  as.numeric(rowSums(firms) >= ncol(firms)*firm_share)
}


# INDUSTRYLEVEL - MODELs I AND II
# simulate_industry_M1_M2 <- function(i, parms, model, min_share) {
#   sigma <- rep(parms$sigma_start, allperiods)
#   n_times_caught <- rep(0, allperiods)
#   ICC_entry <- get_ICC_model1(parms$n_firms)
#   ICC_exit <- ICC_entry
#
#   walk_r <- get_walk_r()
#   all_ind_delta <- get_deltas_walk_r(walk_r)
#
#   # Who wants to be in cartel?
#   in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
#
#   if (model==2){
#     detection <- get_detection_model2_ind(allperiods, sigma, parms$d_nfirms)
#   }else {
#     detection <- get_detection_ind(allperiods, sigma)
#   }
#
#   for (j in 1:allperiods)
#   {
#     # If detected & in cartel
#     if(sum(in_cartel[j] * detection[j])>0){
#       # Increase number of times caught for actual period
#       n_times_caught[j] <- n_times_caught[j] + in_cartel[j] * detection[j]
#       # Periods in cartel before i get detected (if there are periods before)
#       if(j>1){
#         v <- detection[1:j]
#         v <- Rev(v, margin=1)
#         c <- in_cartel[1:j]
#         c <- Rev(c, margin=1)
#         v[cumall(c>0)]=1
#         v <- v*c
#         detection[1:j] = Rev(v, margin = 1)
#       }
#       if(j<allperiods){
#         # Increase number of times caught
#         range <- (j+1):allperiods
#         n_times_caught[range] <- n_times_caught[range] + rep.row(in_cartel[j] * detection[j], allperiods-j)
#       }
#       # No cartel in next period (if there are next periods)
#       if(j<allperiods){
#         in_cartel[j+1] = 0
#       }
#     }
#   }
#   return(list(cartels = in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_cartels = sigma, deltas = all_ind_delta, walk_r = walk_r))
# }
#
# # INDUSTRYLEVEL - MODEL III
# simulate_industry_M3 <- function(i, parms, min_share) {
#   gamma = parms$gamma
#   theta = parms$theta
#   sigma <- rep(parms$sigma_start, allperiods)
#   n_times_caught <- rep(0, allperiods)
#   ICC_entry <- get_ICC_entry_model3(parms$n_firms, sigma, gamma, theta)
#   ICC_exit <- get_ICC_exit_model3(parms$n_firms, sigma, gamma, theta)
#   walk_r <- get_walk_r()
#   all_ind_delta <- get_deltas_walk_r(walk_r)
#
#   # Who wants to be in cartel?
#   in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
#   detection <- get_detection_ind(allperiods, sigma)
#
#   for (j in 1:allperiods)
#   {
#     # If detected & in cartel
#     if(sum(in_cartel[j] * detection[j])>0){
#       # Increase number of times caught for actual period
#       n_times_caught[j] <- n_times_caught[j] + in_cartel[j] * detection[j]
#
#       # Periods in cartel before i get detected (if there are periods before)
#       if(j>1){
#         v <- detection[1:j]
#         v <- Rev(v, margin=1)
#         c <- in_cartel[1:j]
#         c <- Rev(c, margin=1)
#         v[cumall(c>0)]=1
#         v <- v*c
#         detection[1:j] = Rev(v, margin = 1)
#       }
#       if(j<allperiods){
#         # Increase number of times caught
#         range <- (j+1):allperiods
#         n_times_caught[range] <- n_times_caught[range] + rep.row(in_cartel[j] * detection[j], allperiods-j)
#       }
#       # Change sigma if there are more than one periods left, only for detected cartels
#       if (parms$structured & (allperiods-j)>1) {
#         # Row-wise matrix multiplication with vector
#         change <- n_times_caught[range] * (in_cartel[j] * detection[j])
#         sigma[range] = increase_sigma(sigma[range], change)
#         ICC_entry[range] <- get_ICC_entry_model3(parms$n_firms, sigma[range], gamma, theta)
#         ICC_exit[range] <- get_ICC_exit_model3(parms$n_firms, sigma[range], gamma, theta)
#         in_cartel[range] <- get_in_cartel(all_ind_delta[range], ICC_entry[range], ICC_exit[range])
#         detection[range] <- get_detection_ind(allperiods-j, sigma[range])
#       }
#       # No cartel in next period (if there are next periods)
#       if(j<allperiods){
#         in_cartel[j+1] = 0
#       }
#     }
#   }
#   return(list(cartels = in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_cartels = sigma, deltas = all_ind_delta, walk_r = walk_r))
# }

