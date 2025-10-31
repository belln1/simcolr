#rm(list = ls())
library(simcolr)
library(assertthat)

# Set Seed for Reproducibility
sim_seed <- 123
set.seed(sim_seed)

# Model 1: Baseline Model, ICC: delta >= 1 - 1/n (Stigler 1964)
sim_list <- sim_col_r()
cartels_duration <- get_cartel_duration(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=1)
cartels_duration <- data.frame(cartels_duration)
describe(cartels_duration)

# assert_that(sum(cartels_duration$leniency) == 0)
# assert_that(nrow(cartels_duration) == 254)

################################################################################################

# Model 2: sigma > theta, all firms apply for leniency when cartel collapses, no inactive undetected cartels
sim_list <- sim_col_r(model = "2", periods = 100, n_ind = 100, theta = 0.035)
cartels_duration <- get_cartel_duration(sim_list$cartels, sim_list$detection, sim_list$leniency)
# assert_that(nrow(filter(cartels_duration, in_sample==0 & end < 10)) == 0)
# assert_that(nrow(cartels_duration) == 1)

################################################################################################

# Model 3: Probability of CA succes (s) and therefore sigma depend on CA caseload
sim_list <- sim_col_r(model = "3", periods = 10, n_ind = 10)
cartels_duration <- get_cartel_duration(sim_list$cartels, sim_list$detection, sim_list$leniency)
input_ind <- sim_list$input_ind
cartels_duration <- left_join(cartels_duration, input_ind, by="industry")
# assert_that(nrow(cartels_duration) == 1)



######################################################################
# Calculate cartel durations

# cartels_duration <- get_cartel_duration(sim_list$cartels_detected, sim_list$cartels_undetected, parms, model=1)
# cartels_duration <- data.frame(cartels_duration)

# data1 <- calculate_mean_r(cartels_duration, r_all)
# data1 <- calculate_var_r(data1, r_all)
# data1$var_r <- ifelse(is.na(data1$var_r), 0, data1$var_r)
# data1 <- calculate_mean_delta(data1, delta_all)

# Add nonlinear variables for Lasso CV
data <- add_nonlinears(data1, model=model)
cartels_duration <- data
describe(cartels_duration)

