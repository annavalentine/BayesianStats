############################################
##  file: anna.m.valentine.th.Task4a.ps#2.R
##   R Problem Set #2 : Practicing with Monte Carlo
##   - Estimate mean and p95 of guassian using Monte Carlo Methods 
## 
########################################################
##  authors: Anna Valentine 01/24/2025
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

## Ran in R Studio: Version 2024.12.0+467 (2024.12.0+467)
#######################################################

### most function 

# okay first things first, let's set the mean and the STD
num_samples <- 10^3   # number of random draws (for our initial look)
mu <- 0               # mean of normal distribution to draw from
sigma <- 1            # standard deviation of normal distribution
set.seed(123)       # setting a seed 

# and now, let's generate a random guassian: 
# reference: 
samples <- rnorm(num_samples, mean = mu, sd = sigma)

## we can look at this using a historgram:
hist(samples, breaks = 50, main = "Gaussian Distribution seed=123", xlab = "Value", col = "lightgreen")

## woah! looks pretty terrible, so we must need more samples (this is also a function of binning)

####### finding mean and 95th percentile ###########
test_mean = mean(samples)  # find the mean
#print(test_mean)

test_p95 <- quantile(samples, probs = 0.95)

#####################################################
# now I'm ready to actually do the problem, so I'll make a function I can call with different 
# seeds (i), and samples sizes (j), and I can put this all in a nested for loop. I'm sure there
# might be a slicker way to do this, but then I can put it all in a matrix at the end to make
# plotting easier. 
#

## turning this into a function: 
compute_sample= function(i_val, j_val, seed, mu, sigma){
    
  sample_mean <- matrix(NA, nrow = length(i_val), ncol = length(j_val))
  sample_p95 <- matrix(NA, nrow = length(i_val), ncol = length(j_val))
  
  for (i in i_val){
    for (j in j_val){
      new_seed = seed+i
      set.seed(new_seed)
      samples <- rnorm(j, mean = mu, sd = sigma)
      sample_mean[i, j] = mean(samples)
      sample_p95[i, j] = quantile(samples, probs = 0.95)
    }
  } 
  return(list(sample_mean = sample_mean, sample_p95 = sample_p95))
}

## calling my function, we set all the other parameters at the top:
i_val <- 1:10
j_val <- 1:10000
seed = 123

results_test <- compute_sample(i_val, j_val, seed, mu, sigma)
sample_mean_test = results_test$sample_mean

# Plot the sample_mean matrix
matplot(t(sample_mean_test), type = "l", lty = 1, col = rainbow(length(i_val)),
        xlab = "Number of Samples", ylab = "Sample mean", 
        main = "Random Guassian Sample Means n=1e5")
legend("topright", legend = paste("seed =", 123 +i_values), col = rainbow(length(i_val)), lty = 1, cex=0.3)

## to quntify our uncertainty, let's look at the largest sample size: 
# Find the column corresponding to j = 100000
n = 100
i_val <- 1:10
j_val <- 1:n
seed = 123

results_bigJ <- compute_sample(i_val, j_val, seed, mu, sigma)
sample_mean_bigJ = results_bigJ$sample_mean
sample_p95_bigJ = results_bigJ$sample_p95

# Find the last column in the sample_mean matrix
last_column_mean <- sample_mean_bigJ[, n-1]
last_column_p95 <- sample_p95_bigJ[, n-1]

# Calculate the minimum and maximum for uncertainty in n_samples = 10000
min_value_mean <- min(last_column_mean)
max_value_mean <- max(last_column_mean)
uncertainty_mean <- (max_value_mean - min_value_mean) /2.  ### our uncertainty
avg_value_mean <- mean(last_column_mean) ### our estiamted mean

min_value_p95 <- min(last_column_p95)
max_value_p95 <- max(last_column_p95)
avg_value_p95 <- mean(last_column_p95) #### our estiamted p95 value
uncertainty_p95 <- (max_value_p95 - min_value_p95) / 2 ### our uncertainty

