# simcolr
## Program for Simulating Collusion based on Stochastic Interest Rates
Simulation of industries with collusive and non-collusive periods over time, detected and undetected, based on stochastic interest rates and with ICC from Stigler (1964) and inspired by Harrington/Chang (2009) and Bos et al (2018). This simulation is described in detail in the paper "Simulating Collusion: Challenging Conventional Estimation Methods" by Bellert and Günster (2025).

## Requirements
Install R: https://cran.rstudio.com/

Install RStudio: https://posit.co/downloads/

In your RStudio console:

`library(devtools)`

`install_github("belln1/simcolr");`

`install.packages(c("dplyr", "DescTools"))`


## Getting Started
Type `?simcolr` and run the described examples.

Run R Markdown with code examples for simulation, summary statistics, plots and estimates:  
examples/examples.Rmd

Run R file simulating different Models 1, 2, 3:  
examples/test_simulate_collusion.R


## Questions and comments 
bell@zhaw.ch
