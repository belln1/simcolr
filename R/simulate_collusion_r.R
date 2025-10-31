#rm(list = ls())
#source(file = "analysis/scripts/functions_simulation.R")
# debugSource(file = "R/distributions.R")
# debugSource(file = "R/cartel_duration.R")

combine_parms_model12 <- function(n_firms_in, sigma_in){
  n_sim <- length(n_firms_in) * length(sigma_in)
  parms <- tibble(
    n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
    sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
  )
}

combine_parms_model3 <- function(n_firms_in, sigma_in, gamma_in, theta_in, struc_in){
  n_sim <- length(n_firms_in) * length(sigma_in) * length(gamma_in) * length(theta_in) * length(struc_in)
  parms <- tibble(
    n_firms = rep(n_firms_in, each=n_sim/length(n_firms_in)),
    sigma_start = rep(sigma_in, times=length(n_firms_in), each=n_sim/(length(n_firms_in)*length(sigma_in))),
    theta = rep(theta_in, times=length(n_firms_in)*length(sigma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in))),
    gamma = rep(gamma_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in))),
    structured = rep(struc_in, times=length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in), each=n_sim/(length(n_firms_in)*length(sigma_in)*length(theta_in)*length(gamma_in)*length(struc_in)))
  )
}

combine_parms <- function(model, n_firms) {
  if (model==1) return(combine_parms_model12(n_firms, sigma_t))
  else if(model==2) {
    parms <- combine_parms_model12(n_firms, sigma_t)
    parms$d_nfirms <- sort(1/parms$n_firms, decreasing = FALSE)
    return(parms)
  }
  else return(combine_parms_model3(n_firms, sigma_t, gamma, theta, struc))
}

rep_row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

get_delta <- function(r){1/(1+r)}

random_steps <- function(n){
  x <- rbinom(n, 1, 0.5)
  (2*x - 1)/500
}

get_r <- function(r_min, r_max){
  r_start <- runif(1, r_min, r_max)
}

get_walk_r <- function(periods, r_min, r_max){
  r <- get_r(r_min, r_max)
  steps <- random_steps(periods-1)
  cumsum(c(r, steps))
}

get_deltas_walk_r <- function(walk_r){
  d <- get_delta(walk_r)
  deltas <- pnorm(d)
}

# sigma = Prob(at least once found during cartel duration) = 1 - ((1-p_t)^duration)
# p_t = Prob(found in time t) = 1 - ((1-sigma)^(1/duration)) # formula for sigma, solved for p_t
get_sigma_all_t <- function(sigma_t){
  1 - (1-sigma_t)^200
}

# Model 1 and 2: ICC depending on number of firms (cite Stigler 1964)
get_ICC_model1 <- function(n) {
  1-1/n
}

# Model 3: ICC with fines and leniency (Bos/Davies/Harrington/Ormosi(2018)
get_ICC_entry_model3 <- function(n, sigma_t, gamma, theta){
  sigma <- get_sigma_all_t(sigma_t)
  1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
}
get_ICC_exit_model3 <- function(n, sigma_t, gamma, theta){
  sigma <- get_sigma_all_t(sigma_t)
  1-((1-sigma)/(n+sigma*gamma-theta*sigma*gamma-sigma))
}
increase_sigma <- function(sigma, n_times_caught) {
  return(ifelse(n_times_caught > 0, sigma * (1+1/2^n_times_caught), sigma))
}

get_detection_ind <- function(periods, sigma){
  x <- as.numeric(runif(periods) <= sigma)
}

# In Model 2, detection depends on n_firms and sigma
get_detection_model2_ind <- function(periods, sigma, d_nfirms){
  x <- as.numeric(runif(periods) <= sigma*d_nfirms) # revised version, more intuitive formular
}

# Does an industry want to be in a cartel?
# Reduce(function, vector) applies function on the first two elements of vector, then on the result of that and the third element
# accumulate=TRUE returns all results, accumulate=FALSE returns only last result
get_in_cartel <- function(ind, ICC_entry, ICC_exit) {
  ind_entry <- ifelse(ind > ICC_entry, 1, 0) # ifelse keeps matrix, if_else makes large vector
  ind_exit <- ifelse(ind < ICC_exit, -1, 0)
  v <- ind_entry + ind_exit
  w <- Reduce(function(x,y) ifelse(y==0, x, y), v, accumulate=TRUE) # fill 0-values with last non-0-value (makes large vector)
  in_cartel <- if_else(w == -1, 0, 1) # change -1 to 0 (not in cartel)
}

simulate_industry_M1_M2 <- function(i, parms, model, r_min, r_max, periods) {
  sigma <- rep(parms$sigma_start,periods)
  n_times_caught <- rep(0,periods)
  ICC_entry <- get_ICC_model1(parms$n_firms)
  ICC_exit <- ICC_entry

  walk_r <- get_walk_r(periods, r_min, r_max)
  all_ind_delta <- get_deltas_walk_r(walk_r)

  # Who wants to be in cartel?
  in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)

  if (model==2){
    detection <- get_detection_model2_ind(periods, sigma, parms$d_nfirms)
  }else {
    detection <- get_detection_ind(periods, sigma)
  }

  for (j in 1:periods)
  {
    # If detected & in cartel
    if(sum(in_cartel[j] * detection[j])>0){
      # Increase number of times caught for actual period
      n_times_caught[j] <- n_times_caught[j] + in_cartel[j] * detection[j]
      # Periods in cartel before i get detected (if there are periods before)
      if(j>1){
        v <- detection[1:j]
        v <- Rev(v, margin=1)
        c <- in_cartel[1:j]
        c <- Rev(c, margin=1)
        v[cumall(c>0)]=1
        v <- v*c
        detection[1:j] = Rev(v, margin = 1)
      }
      if(j<periods){
        # Increase number of times caught
        range <- (j+1):periods
        n_times_caught[range] <- n_times_caught[range] + rep_row(in_cartel[j] * detection[j],periods-j)
      }
      # No cartel in next period (if there are next periods)
      if(j<periods){
        in_cartel[j+1] = 0
      }
    }
  }
  return(list(cartels = in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_cartels = sigma, deltas = all_ind_delta, walk_r = walk_r))
}

# INDUSTRYLEVEL - MODEL III
simulate_industry_M3 <- function(i, parms, r_min, r_max, periods) {
  gamma = parms$gamma
  theta = parms$theta
  sigma <- rep(parms$sigma_start,periods)
  n_times_caught <- rep(0,periods)
  ICC_entry <- get_ICC_entry_model3(parms$n_firms, sigma, gamma, theta)
  ICC_exit <- get_ICC_exit_model3(parms$n_firms, sigma, gamma, theta)
  walk_r <- get_walk_r(periods, r_min, r_max)
  all_ind_delta <- get_deltas_walk_r(walk_r)

  # Who wants to be in cartel?
  in_cartel <- get_in_cartel(all_ind_delta, ICC_entry, ICC_exit)
  detection <- get_detection_ind(periods, sigma)

  for (j in 1:periods)
  {
    # If detected & in cartel
    if(sum(in_cartel[j] * detection[j])>0){
      # Increase number of times caught for actual period
      n_times_caught[j] <- n_times_caught[j] + in_cartel[j] * detection[j]

      # Periods in cartel before i get detected (if there are periods before)
      if(j>1){
        v <- detection[1:j]
        v <- Rev(v, margin=1)
        c <- in_cartel[1:j]
        c <- Rev(c, margin=1)
        v[cumall(c>0)]=1
        v <- v*c
        detection[1:j] = Rev(v, margin = 1)
      }
      if(j<periods){
        # Increase number of times caught
        range <- (j+1):periods
        n_times_caught[range] <- n_times_caught[range] + rep_row(in_cartel[j] * detection[j],periods-j)
      }
      # Change sigma if there are more than one periods left, only for detected cartels
      if (parms$structured & (periods-j)>1) {
        # Row-wise matrix multiplication with vector
        change <- n_times_caught[range] * (in_cartel[j] * detection[j])
        sigma[range] = increase_sigma(sigma[range], change)
        ICC_entry[range] <- get_ICC_entry_model3(parms$n_firms, sigma[range], gamma, theta)
        ICC_exit[range] <- get_ICC_exit_model3(parms$n_firms, sigma[range], gamma, theta)
        in_cartel[range] <- get_in_cartel(all_ind_delta[range], ICC_entry[range], ICC_exit[range])
        detection[range] <- get_detection_ind(periods-j, sigma[range])
      }
      # No cartel in next period (if there are next periods)
      if(j<periods){
        in_cartel[j+1] = 0
      }
    }
  }
  return(list(cartels = in_cartel, detection = detection, ICC_entry = ICC_entry, ICC_exit = ICC_exit, sigma_cartels = sigma, deltas = all_ind_delta, walk_r = walk_r))
}

#' Simulate Collusion based on stochastic interest rates
#'
#' Simulation of industries with collusive and non-collusive periods over time, detected and undetected, based on Stigler(1964) and inspired by Bos/Davies/Harrington/Ormosi(2018) and Harrington/Chang(2009)
#' @param model Modelname (1, 2, 3)
#' @param periods Number of time periods
#' @param n_industries Number of industries
#'
#' @return List with detected cartels, undetected cartels, all cartels, discount factor over time, interest rate over time
#' @import dplyr
#' @examples
#' sim_list <- sim_col_r();
#' sim_list <- sim_col_r(model = "2", periods = 1000, n_ind = 20);
#' sim_list <- sim_col_r(model = "3", periods = 1000, n_ind = 30, n_firms_max=50);
#' @export
sim_col_r <- function(model=1, periods=1000, n_industries=30,
                      r_min=0.001, r_max=0.3, min_share=1, n_firms_min=2,
                      n_firms_max=20){
  n_firms <- n_firms_min:n_firms_max
  parms <- combine_parms(model, n_firms)

  delta_all <- array(0,dim = c(periods, n_industries, nrow(parms)))
  r_all <- array(0,dim = c(periods, n_industries, nrow(parms)))
  cartels_detected <- array(0,dim = c(periods, n_industries, nrow(parms)))
  cartels_undetected <- array(0,dim = c(periods, n_industries, nrow(parms)))
  cartels_population <- array(0,dim = c(periods, n_industries, nrow(parms)))

  for (k in 1:nrow(parms)) {
    # Array: dim = rows=periods, columns=industries, matrices=parameters
    allcartels_det <- matrix(0, nrow =periods, ncol = n_industries)
    allcartels_undet <- matrix(0, nrow =periods, ncol = n_industries)
    allcartels_pop <- matrix(0, nrow =periods, ncol = n_industries)
    delta_inds <- matrix(0, nrow =periods, ncol = n_industries)
    r_inds <- matrix(0, nrow =periods, ncol = n_industries)

    for (i in 1:n_industries) {
      if (model==3) {
        sim_list <- simulate_industry_M3(i, parms[k,], r_min, r_max, periods)
      } else {
        sim_list <- simulate_industry_M1_M2(i, parms[k,], model=model, r_min, r_max, periods)
      }
      cartels_det <- get_sample(sim_list$cartels, sim_list$detection)
      cartels_undet <- get_undetected(sim_list$cartels, sim_list$detection)
      cartels_pop <- sim_list$cartels

      allcartels_det[, i] <- cartels_det
      allcartels_undet[, i] <- cartels_undet
      allcartels_pop[, i] <- cartels_pop
      delta_inds[, i] <- sim_list$deltas
      r_inds[, i] <- sim_list$walk_r
    }
    delta_all[,, k] <- delta_inds
    r_all[,, k] <- r_inds
    cartels_detected[,, k] <- allcartels_det
    cartels_undetected[,, k] <- allcartels_undet
    cartels_population[,, k] <- allcartels_pop
  }
  return(list(cartels_detected = cartels_detected, cartels_undetected = cartels_undetected, cartels_population = cartels_population, deltas = delta_all, interest_r = r_all, parms=parms))
}
