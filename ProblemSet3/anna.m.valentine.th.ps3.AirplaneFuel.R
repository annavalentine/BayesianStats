############################################
##  file: anna.m.valentine.th.ps3.AirplaneFuel.R
##   R Problem Set #3 : Practicing with Analytical Methods, Bayes Methods, Grid
##   - our analytical solution
## 
########################################################
##  authors: Anna Valentine 02/01/2025
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

## Ran in R Studio: Version 2024.12.0+467 (2024.12.0+467)
#######################################################


# Set parameters
tank_capacity   <- 182         # total fuel tank capacity in liters
sensor_reading  <- 34          # sensor reading in liters
sensor_std      <- 20          # sensor measurement error (std dev in liters)
num_samples     <- 100000      # number of Monte Carlo samples
set.seed(123)  # for reproducibility

# Define a sequence of fuel values (x-axis) from 0 to tank_capacity
x <- seq(-50, tank_capacity+50, length.out = 1000)
dx <- x[2] - x[1]  #spacing between values, for normalization

# Likelihood: theoretical Gaussian (normal) density based on the sensor reading
likelihood_theoretical <- dnorm(x, mean = sensor_reading, sd = sensor_std)

# Compute CDF values
cdf_values <- pnorm(x, mean = sensor_reading, sd = sensor_std)

# Plot the CDF
plot(x, cdf_values, type = "l", lwd = 2, col = "blue",
     xlab = "Fuel Reading", ylab = "CDF",
     main = "Cumulative Distribution Function")

# Add a vertical line at x = 0
abline(v = 0, col = "red", lty = 2)

# Add probability annotation
probability_negative_fuel <- pnorm(0, mean = sensor_reading, sd = sensor_std)
text(0, probability_negative_fuel, labels = paste0("P(X < 0) = ", round(probability_negative_fuel, 4)), pos = 4, col = "red")
# Prior: uniform density over the fuel range (0 to tank_capacity)

# This density is constant and equals 1/tank_capacity over [0, tank_capacity]
prior_theoretical <- ifelse(x >= 0 & x <= tank_capacity, 1/tank_capacity, 0)

# Plot the likelihood density first
plot(x, likelihood_theoretical, type = "l", col = "blue", lwd = 2,
     xlim = c(-50, tank_capacity+50),
     xlab = "Usable Fuel (liters)", ylab = "Density",
     main = "Likelihood and Prior Densities")

# Overlay the prior density on the same plot
lines(x, prior_theoretical, col = "red", lwd = 2)

# Add a legend to distinguish the two curves
legend("topright", legend = c("Guage Reading", "Uniform Prior"),
       col = c("blue", "red"), lwd = 2, cex = 0.5)

#### okay cool, now let's plot my posterior: 
# The unnormalized posterior is the product of the likelihood and the prior.
un_posterior <- likelihood_theoretical * prior_theoretical

# Compute the normalization constant using the trapezoidal approximation:
norm_const <- sum(un_posterior) * dx

# Now compute the normalized posterior.
posterior_theoretical <- un_posterior / norm_const

# Overlay the posterior density on the plot
lines(x, posterior_theoretical, col = "purple", lwd = 2)
legend("topleft", legend = c("Posterior"), col = c("purple"), lwd = 2, cex = 0.5)


### and now I again, want to know if I will have negative fuel: 
# Compute the posterior CDF by integrating the posterior density
posterior_cdf <- cumsum(posterior_theoretical) * dx

# Normalize to ensure the CDF goes from 0 to 1
posterior_cdf1 <- posterior_cdf / max(posterior_cdf)

# Plot the posterior CDF
plot(x, posterior_cdf1, type = "l", col = "purple", lwd = 2,
     xlab = "Usable Fuel (liters)", ylab = "Posterior CDF",
     main = "Posterior Cumulative Distribution Function")

# Add a vertical line at x = 0
abline(v = 0, col = "red", lty = 2)

# Compute the probability of negative fuel (P(X < 0))
prob_posterior_negative <- posterior_cdf1[which.min(abs(x - 0))]

# Add probability annotation
text(0, prob_posterior_negative, 
     labels = paste0("P(X < 0) = ", round(prob_posterior_negative, 4)), 
     pos = 4, col = "red")


### next: what's my proper (uniform) prior? plot. 
prior <- runif(num_samples, min = 0, max = tank_capacity)
likelihood <- rnorm(num_samples, mean = sensor_reading, sd = sensor_std)

# calculate posterior
posterior <- prior * likelihood
posterior_norm <- posterior / sum(posterior)

hist(posterior_norm, 
     breaks = 100,                  # Adjust number of bins as needed
     main = "Histogram of posterior",
     xlab = "Fuel (liters)",
     col = "lightblue",
     border = "white")


