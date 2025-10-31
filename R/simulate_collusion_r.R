rm(list = ls())
#source(file = "analysis/scripts/functions_simulation.R")
debugSource(file = "R/distributions.R")
debugSource(file = "R/cartel_duration.R")


sim_col_r <- function(model=1, periods=100, n_industries=30,
                      r_min=0.001, r_max=0.3, min_share=1, n_firms_min=2,
                      n_firms_max=20){

}

modelname <- 1
#modelname <- 2
#modelname <- 3



# Set Basic Parameters  --------------------------------------------------------------

# Set seed for reproducibility
sim_seed <- 123
set.seed(sim_seed)

allperiods <- 1000
r_min <- 0.001
r_max <- 0.3
min_share <- 1 # minimum percentage of firms needed to form a cartel - 1 for complete cartels
#n_industries <- 300
n_industries <- 30

n_min <- 2
#n_max <- 100 # max number of firms
n_max <- 20 # max number of firms
n_firms_in <- n_min:n_max
sigma_all <- seq(0.1, 0.35, 0.05)
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0


# ------------------------------------------------------------------------------------

# Build dataframe with all possible parameter combinations from above

if (modelname==3) {
  gamma_in <- c(0.7, 0.8, 0.9)
  theta_in <- c(0, 0.5, 1)
  struc_in <- c(0, 1)
  parms <- combine_parms_model3(n_firms_in, sigma_t, gamma_in, theta_in, struc_in)
} else {
  parms <- combine_parms_model12(n_firms_in, sigma_t)
}
if (modelname==2) {
  parms$d_nfirms <- sort(1/parms$n_firms, decreasing = FALSE)
}
#write.table(parms, file = paste("analysis/data/", directory, "/parms.csv", sep = ""), row.names = FALSE, sep = ";")

delta_all <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
r_all <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

for (k in 1:nrow(parms)) {
  # code in comments can be used for firmlevel
  # firms_detected <- data.frame(matrix(ncol = 1000, nrow = 0))
  # firms_undetected <- data.frame(matrix(ncol = 1000, nrow = 0))
  # firms_population <- data.frame(matrix(ncol = 1000, nrow = 0))

  # Array: dim = rows=periods, columns=industries, matrices=parameters
  allcartels_det <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_undet <- matrix(0, nrow = allperiods, ncol = n_industries)
  allcartels_pop <- matrix(0, nrow = allperiods, ncol = n_industries)
  delta_inds <- matrix(0, nrow = allperiods, ncol = n_industries)
  r_inds <- matrix(0, nrow = allperiods, ncol = n_industries)

  for (i in 1:n_industries) {
    if (modelname==3) {
      sim_list <- simulate_industry_M3(i, parms[k,], min_share)
    } else {
      sim_list <- simulate_industry_M1_M2(i, parms[k,], model=modelname, min_share)
    }
    # firms_det <- get_sample(sim_list$firms, sim_list$detection)
    # firms_undet <- get_undetected(sim_list$firms, sim_list$detection)
    # firms_pop <- sim_list$firms
    # cartels_det <- ifelse(rowSums(firms_det)>0, 1, 0)
    # cartels_undet <- ifelse(rowSums(firms_undet)>0, 1, 0)
    # cartels_pop <- ifelse(rowSums(firms_pop)>0, 1, 0)

    cartels_det <- get_sample(sim_list$cartels, sim_list$detection)
    cartels_undet <- get_undetected(sim_list$cartels, sim_list$detection)
    cartels_pop <- sim_list$cartels

    allcartels_det[, i] <- cartels_det
    allcartels_undet[, i] <- cartels_undet
    allcartels_pop[, i] <- cartels_pop
    delta_inds[, i] <- sim_list$deltas
    r_inds[, i] <- sim_list$walk_r
  }
  # saveRDS(allcartels_det, file = paste("analysis/data/", directory, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  # saveRDS(allcartels_undet, file = paste("analysis/data/", directory, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  # saveRDS(allcartels_pop, file = paste("analysis/data/", directory, "/cartels/cartels_population_", k, ".rds", sep = ""))
  delta_all[,, k] <- delta_inds
  r_all[,, k] <- r_inds
  cartels_detected[,, k] <- allcartels_det
  cartels_undetected[,, k] <- allcartels_undet
  cartels_population[,, k] <- allcartels_pop
}

# Read all cartel files into one large array
# cartels_detected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
# cartels_undetected <- array(0,dim = c(allperiods, n_industries, nrow(parms)))
# cartels_population <- array(0,dim = c(allperiods, n_industries, nrow(parms)))

# For every parm combination, read in the matrix with 300 industries
# for (k in 1:nrow(parms)) {
#k <- 1
#for (k in 1:5000) {
  # cartels_det <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_detected_", k, ".rds", sep = ""))
  # cartels_undet <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_undetected_", k, ".rds", sep = ""))
  # cartels_pop <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_population_", k, ".rds", sep = ""))

  # Save each matrix
  # cartels_detected[,, k] <- cartels_det
  # cartels_undetected[,, k] <- cartels_undet
  # cartels_population[,, k] <- cartels_pop
# }
# Save large array
# saveRDS(cartels_detected, file = paste("analysis/data/", directory, "/cartels/cartels_detected.rds", sep = ""))
# saveRDS(cartels_undetected, file = paste("analysis/data/", directory, "/cartels/cartels_undetected.rds", sep = ""))
# saveRDS(cartels_population, file = paste("analysis/data/", directory, "/cartels/cartels_population.rds", sep = ""))
# saveRDS(delta_all, file = paste("analysis/data/", directory, "/delta_all.rds", sep = ""))
# saveRDS(r_all, file = paste("analysis/data/", directory, "/r_all.rds", sep = ""))


######################################################################
# Calculate cartel durations
# parms <- read.table(file = paste("analysis/data/", directory, "/parms.csv", sep = ""), header = TRUE, sep = ";")
# cartels_undetected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_undetected.rds", sep = ""))
# cartels_detected <- readRDS(file = paste("analysis/data/", directory, "/cartels/cartels_detected.rds", sep = ""))
# delta_all <- readRDS(file = paste("analysis/data/", directory, "/delta_all.rds", sep = ""))
# r_all <- readRDS(file = paste("analysis/data/", directory, "/r_all.rds", sep = ""))

cartels_duration <- combine_durations(cartels_detected, cartels_undetected, parms, model=modelname)
cartels_duration <- data.frame(cartels_duration)

# data1 <- add_r_sumstats(cartels_duration, r_all)

data1 <- calculate_mean_r(cartels_duration, r_all)
data1 <- calculate_var_r(data1, r_all)
data1$var_r <- ifelse(is.na(data1$var_r), 0, data1$var_r)
data1 <- calculate_mean_delta(data1, delta_all)

# Add industries with 0 cartels for Heckman Selection Correction
#data_all <- add_non_collusive_industries(parms, n_industries, cartels_duration)

# Add nonlinear variables for Lasso CV
data <- add_nonlinears(data1, model=modelname)
cartels_duration <- data
describe(cartels_duration)

#write.table(data, file = paste("analysis/data/", directory, "/cartels_duration.csv", sep = ""), row.names = FALSE, sep = ";")
