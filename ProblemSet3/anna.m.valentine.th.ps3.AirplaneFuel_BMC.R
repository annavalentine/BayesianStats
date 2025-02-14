############################################
##  file: anna.m.valentine.th.ps3.AirplaneFuel_BMC.R
##   R Problem Set #3 : Practicing with Analytical Methods, Bayes Methods, Grid
##   - applying bayes monte carlo to our airplane problem
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
# Set parameters
tank_capacity   <- 182         # total fuel tank capacity in liters
sensor_reading  <- 34          # sensor reading in liters
sensor_std      <- 20          # sensor measurement error (std dev in liters)
num_samples     <- 1e6     # number of Monte Carlo samples

set.seed(123)  # for reproducibility

#### first thing, I need to plot the PDF for usable fuel in the tank, just using
### the info of the fuel gauge: i.e. a Gaussian surrounding gauge readings
### next: what's my proper (uniform) prior? plot. 
fuel_samples <- runif(num_samples, min = 0, max = tank_capacity)

# this is kinda my likelihood, but I'm using dnorm to incorporate my uniform prior: 
likelihood <- dnorm(fuel_samples, mean = sensor_reading, sd = sensor_std)

hist(fuel_samples, 
     breaks = 100,                  # Adjust number of bins as needed
     main = "Histogram of Fuel Samples",
     xlab = "Fuel (liters)",
     col = "lightblue",
     border = "white")

## plot my posterior: 
posterior <- likelihood

# Now compute the normalized posterior.
posterior_norm <- posterior / sum(posterior)

posterior_samples <- sample(fuel_samples, size = num_samples, replace = TRUE, prob = posterior_norm)


hist(posterior_samples, 
     breaks = 100,      ## bin size 
     main = "Posterior",
     xlab = "Fuel (liters)",
     col = "lightblue",
     border = "white")


#########################################################
### let's look at our positive control so we can look at convergence: 
compute_posterior_control <- function(true_fuel_level, fuel_samples, sensor_std, num_samples, seed = NULL, tolerance = .1) {
  # This is how I tested some different seeds 
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ## some variables to keep track of!
  iter <- 0
  control_modes <- c()
  control_error <- Inf
  
  ### while my error is not below the tolerance: 
  while (control_error > tolerance) {
    iter <- iter + 1
    
    # Simulated noisy sensor reading (using sensor std)
    sensor_reading_control <- true_fuel_level + rnorm(1, 0, sensor_std)
    
    # Compute likelihood for pos control 
    likelihood_control <- dnorm(fuel_samples, mean = sensor_reading_control, sd = sensor_std)
    posterior_control <- likelihood_control / sum(likelihood_control)
    
    # Sample from positive control
    posterior_control_samples <- sample(fuel_samples, size = num_samples, replace = TRUE, prob = posterior_control)
    
    # Compute median of positive control
    control_mode <- median(posterior_control_samples)
    control_modes <- c(control_modes, control_mode)
    
    # Compute mean
    avg_control_mode <- mean(control_modes)
    
    # Compute absolute error for validation
    control_error <- abs(avg_control_mode - true_fuel_level)
    
  }
  
  # Print results
  cat("Number of iterations:", iter, "\n")
  cat("Final Control Mode:", avg_control_mode, "\n")
  cat("Final Control Error:", control_error, "\n")
  
  ## retuern my control mode error, and mean
  return(list(average_control_mode = avg_control_mode, control_error = control_error, iterations = iter))
}

## test out our positive control (how many iterations to converge?)
true_fuel_level = 100 # liters
compute_posterior_control(true_fuel_level, fuel_samples, sensor_std, num_samples, seed = NULL, tolerance = 1)

###### finally: how long will the plane fly for? 
# -------------------------------
# Flight Parameters
# -------------------------------
flight_duration <- (100 / 60 ) + .5         # in hours
fuel_rate_mean  <- 18          # liters per hour (mean consumption)
required_fuel   <- flight_duration * fuel_rate_mean
required_fuel_hi <- flight_duration * (fuel_rate_mean+2)   # 2L std
required_fuel_lo <- flight_duration * (fuel_rate_mean-2) #2L std
# -------------------------------

# Compute the probability that fuel is sufficient for the flight
## we can do this by taking the mean of posterior samples: 
fuel_prob <- mean(posterior_samples >= required_fuel)
fuel_prob_lo <- mean(posterior_samples >= required_fuel_lo)
fuel_prob_hi <- mean(posterior_samples >= required_fuel_hi)
cat("Probability of having enough fuel for the flight:", fuel_prob, "\n")

# get the CDF function for plotting: 
cdf_function <- ecdf(posterior_samples)

# Plot the CDF
plot(cdf_function,
     main = "Posterior CDF of Usable Fuel",
     xlab = "Usable Fuel (liters)",
     ylab = "Cumulative Probability",
     col = "purple", lwd = 2)

# Add vertical lines at required fuel thresholds
abline(v = required_fuel, col = "orange", lty = 2, lwd = 2) #required
abline(v = required_fuel_hi, col = "green", lty = 2, lwd = 2) #hi
abline(v = required_fuel_lo, col = "red", lty = 2, lwd = 2) #lo

# Annotate the plot:
text(required_fuel, cdf_function(required_fuel)-0.25,
     labels = paste("P(fuel >= ", required_fuel, ") = ", round(fuel_prob, 5), sep = ""),
     pos = 4, col = "orange", cex=.5)
text(required_fuel, cdf_function(required_fuel_hi)-0.45,
     labels = paste("P(fuel >= ", round(required_fuel_hi, 2), ") = ", round(fuel_prob_hi, 5), sep = ""),
     pos = 4,  col = "green", cex=.5)
text(required_fuel, cdf_function(required_fuel_lo),
     labels = paste("P(fuel >= ", round(required_fuel_lo, 2), ") = ", round(fuel_prob_lo, 5), sep = ""),
     pos = 4,  col = "red", cex=.5)





