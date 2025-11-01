rm(list = ls())
library(simcolr)
library(assertthat)

# Model 1: Baseline Model, ICC: delta >= 1 - 1/n (Stigler 1964), Probability of Detection is sigma
set.seed(123) # Set Seed for Reproducibility
sim_list <- sim_col_r()
cartels_duration <- get_durations(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=1)
cartels_duration <- data.frame(cartels_duration)
cartels_duration <- add_nonlinears(cartels_duration, model=1)
describe(cartels_duration)

assert_that(nrow(cartels_duration) == 2708)

################################################################################################

# Model 2: ICC: delta >= 1 - 1/n, Probability of Detection depends on sigma and number of firms, Pr(Detection) = sigma / (n_firms_max + n_firms_min - n_firms)
set.seed(123)
sim_list <- sim_col_r(model = "2", periods = 1000, n_ind = 20);
cartels_duration <- get_durations(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=2)
cartels_duration <- data.frame(cartels_duration)
cartels_duration <- add_nonlinears(cartels_duration, model=2)
describe(cartels_duration)

assert_that(nrow(cartels_duration) == 1309)

################################################################################################

# Model 3: ICC as in Bos et al (2018), Probability of Detection is sigma
set.seed(123)
sim_list <- sim_col_r(model = "3", periods = 1000, n_ind = 30, n_firms_max=10);
cartels_duration <- get_durations(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=3)
cartels_duration <- data.frame(cartels_duration)
cartels_duration <- add_nonlinears(cartels_duration, model=3)
describe(cartels_duration)

assert_that(nrow(cartels_duration) == 35148)


