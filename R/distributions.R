
# Set Basic Parameter Distributions
sigma_all <- seq(0.1, 0.35, 0.05)
sigma_t <- 1 - (1-sigma_all)^(1/200) # 200 is an approximation of mean duration, in simulation run with sigma=0
gamma <- c(0.7, 0.8, 0.9)
theta <- c(0, 0.5, 1)
struc <- c(0, 1)
