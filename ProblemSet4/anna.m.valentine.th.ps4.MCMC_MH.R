############################################
##  file: anna.m.valentine.th.ps4.MCMC_MH.R
##   R Problem Set #4 : using MCMC metropolis hasting on airplane fuel problem
## 
########################################################
##  authors: Anna Valentine 02/01/2025
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

## Ran in R Studio: Version 2024.12.0+467 (2024.12.0+467)
#######################################################

## let's set up some parameters we know about the problem: 
## same as AirplaneFuel_BMC.R Parameters
# Set parameters
tank_capacity   <- 182         # total fuel tank capacity in liters
sensor_reading  <- 34          # sensor reading in liters
sensor_std      <- 20          # sensor measurement error (std dev in liters)


set.seed(123)  # for reproducibility
### I checked seed affect on my plots by commenting out and using: 
# set.seed(334)
# set.seed(441)

### below is code for when my plots are being finicky (can comment in or out):
while (!is.null(dev.list())) dev.off()

# Define the unnormalized log posterior function
logp <- function(fuel) {
  
  ## immeadiately throw away guesses that are not within my prior: 
  #if (fuel < 0 || fuel > tank_capacity) return(-Inf)  # Reject invalid values
  
  ## estimate the residuals
  resid <- sensor_reading - fuel  # Deviation of observed sensor reading from fuel estimate
  
  ## now I want the log of the liklihood function (this part kind of confuses me)
  #log.likelihood <- -N/2 * log(2 * pi) - N/2 * log(sensor_std^2) - sum(resid^2) / (2 * sensor_std^2)
  log.likelihood <- -0.5 * log(2 * pi * sensor_std^2) - (resid^2) / (2 * sensor_std^2)
  
  # Uniform prior for fuel from 0 to 182 liters, this is different then my AirplaneFuel_BMC.R
  # because we need to be able to "loop" through in Metropolis hastings
  if (fuel < 0 || fuel > tank_capacity) {
    log.prior <- -Inf  # log(-inf) = 0
  } else {
    log.prior <- 0  # Uniform prior within bounds (log(0)=1)
  }
 
  ## now we can get our posterior
  log.posterior <- log.likelihood + log.prior
  return(log.posterior)
}

# MCMC Metropolis-Hastings implementation (deriving from Workflow_example.R)
# my step size is defined in the function definition
mcmc_metropolis_hastings <- function(NI, step_size) {
  
  #### seup some variables to keep track of: 
  
  
  ## define a new state for fuel
  fuel <- runif(1, 0, tank_capacity) ## this is essentially our prior
  lp <- logp(fuel)
  chain <- numeric(NI) ## place to store chains
  accepts <- 0
  lp.max = lp ## max of log posterior
  fuel_best = fuel
  
  ## now we have to walk through the metropolis hastings part: 
  for (i in 1:NI) {
    
    # propose a new fuel guess
    fuel_new <- rnorm(1, mean = fuel, sd = step_size)
    
    ### I feel like I could have done this a different way, but the only way I can
    ### get it to work is by re-stating my prior bounds, I know this might be an error?
    if (fuel_new < 0 || fuel_new > tank_capacity) next  
    lp_new <- logp(fuel_new)
    
    ## evaluate the new unnormalized psoterior of parameter values
    lq = lp_new - lp
    
    ## draw a uniformly distributed random number from [0, 1]
    lr = log(runif(1))
    
    if (lr < lq) {
      fuel = fuel_new
      lp = lp_new
      accepts = accepts+1 #acceptance ratio
    
      # check if current state is the best! 
      if (lp > lp.max){
        fuel_best <- fuel_best
        lp_max <- lp
    }
    }

    chain[i] <- fuel ### now it is added to the chain
  }
  
  ### calculating the acceptance rate
  list(chain = chain, accept_rate = accepts / NI)
}

# Here are my parameters to run MCMC Metropolis Hastings
NI <- 1e6
step_size <- 5 ### a good idea to see if this affects my convergence
mcmc_result <- mcmc_metropolis_hastings(NI, step_size)
chain <- mcmc_result$chain
accept_rate <- mcmc_result$accept_rate

# Assess mode convergence by throwing away the first 3rd of data (BURN IN!)
burn_in <- floor(0.3 * NI)
post_burnin_chain <- chain[(burn_in + 1):NI]
mcmc_mode <- density(post_burnin_chain)$x[which.max(density(post_burnin_chain)$y)]

# Plot convergence
#pdf("Fuel_Level_MCMC_Convergence2.pdf", width = 10, height = 6)
plot(post_burnin_chain, type='l', col="red", xlab="Iterations", ylab="Fuel Level (L)", main="MCMC-Metropolis Hastings Fuel Level")
abline(h=mcmc_mode, col="blue", lwd=2, lty=2)
#dev.off()

# Print results
cat("Initial MCMC:\n")
cat("  Acceptance rate:", accept_rate, "\n")
cat("  Estimated fuel level mode:", mcmc_mode, "\n")

### okay now I want to do a positive control

# Set a high sensor reading for positive control! (but not out of capacity)
sensor_reading <- 100  

# Run the MCMC Metropolis-Hastings algorithm with the positive control sensor reading
mcmc_result_pos <- mcmc_metropolis_hastings(NI, step_size)
chain_pos <- mcmc_result_pos$chain
accept_rate_pos <- mcmc_result_pos$accept_rate

# do the burn-in period again! 
burn_in_pos <- floor(0.3 * NI)
post_burnin_chain_pos <- chain_pos[(burn_in_pos + 1):NI]
density_est <- density(post_burnin_chain_pos)
### compute the mode
mcmc_mode_pos <- density_est$x[which.max(density_est$y)]

## okay but what is the absolute error: 
pos_error = abs(mcmc_mode_pos - sensor_reading)

### and plot it: 
# Plot the convergence of the positive control chain
plot(post_burnin_chain_pos, type = 'l', col = "green", xlab = "Iterations", ylab = "Fuel Level (L)",
     main = "MCMC Fuel Level (Positive Control @ 100L)")
abline(h = mcmc_mode_pos, col = "blue", lwd = 2, lty = 2)
cat("Positive Control:\n")
cat("  Acceptance rate:", accept_rate_pos, "\n")
cat("  Estimated fuel level mode:", mcmc_mode_pos, "\n")
cat("  Absolute Error:", pos_error, "\n")


### Compute Convergence Using Existing MCMC Chains ###

# Function to compute 95% confidence interval width
compute_ci_width <- function(chain) {
 
  if (length(chain) > 0) { # make sure there is in fact a chain to computeL 
    
    # Compute 95% confidence interval
    ci_low <- quantile(chain, 0.025)
    ci_high <- quantile(chain, 0.975)
    
    # Compute CI width
    ci_width <- ci_high - ci_low
    return(ci_width)
  } else {
    return(NA)  # Return NA for error code
  }
    
}

# Compute CI width for main MCMC run
ci_width_main <- compute_ci_width(post_burnin_chain)
cat("CI width (Main MCMC):", ci_width_main)

# Compute CI width for positive control MCMC
ci_width_pos <- compute_ci_width(post_burnin_chain_pos)
cat("CI width (Positive Control):", ci_width_pos)

