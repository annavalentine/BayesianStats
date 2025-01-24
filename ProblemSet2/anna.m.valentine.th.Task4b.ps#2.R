############################################
##  file: anna.m.valentine.th.Task4b.ps#2
##   R Problem Set #2 : Practicing with Monte Carlo
##   - Estimate Pi using Monte Carlo Methods 
## 
########################################################
##  authors: Anna Valentine 01/24/2025
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)

## Ran in R Studio: Version 2024.12.0+467 (2024.12.0+467)
#######################################################

# okay first things first, let's set the mean and the STD
num_samples <- 10^3   # number of random draws
mu <- 0               # mean of normal distribution to draw from
sigma <- 1            # standard deviation of normal distribution

# and now, let's generate a guassian: 
# reference: 
# https://numpy.org/doc/2.1/reference/random/generated/numpy.random.normal.html
# first set a seed: 
set.seed(123)
n = 10000
#### create a grid from -1 to 1
x = runif(n, min=-1, max=1)
y = runif(n, min=-1, max=1)

### set up vectors to sort them out: 
x_in = c()
y_in = c()
x_out = c()
y_out = c()
### calculate which are in a circle: (x^2 + y^2 = 1)
for (i in seq_along(x)){ #since x and y are same length, we can use this: 
  distance = (x[i])^2 + (y[i])^2
  #print(distance)
  if (distance < 1){
    x_in = c(x_in, x[i])
    y_in = c(y_in, y[i])
  }else{
    x_out = c(x_out, x[i])
    y_out = c(y_out, y[i])
  }
}

#### estimating pi here: 
pi_est = 4*length(x_in) / length(x)
print(pi_est)

#### plot them on a grid: 
### add a legend !
title <- paste("Scatterplot Visual n=", n)
plot(x, y, main = title, xlab = "X-axis", ylab = "Y-axis", pch = 19, col = "blue")
points(x_in, y_in, col="red", pch=19)
legend("topright", legend = c("points out", "points in"), col = c("blue", "red"), pch = 19, cex=0.5)

#### okay so now I will make a function, that computes pi for diff seeds and sample sizes:
i_val <- 1:10  # number of seeds to iterate over
j_val <- 1:1000  # number of samples 
seed = 123

compute_pi= function(i_val, j_val, seed){
  
  pi_est <- matrix(NA, nrow = length(i_val), ncol = length(j_val))
  
  for (i in i_val){
    for (j in j_val){
      new_seed = seed+i
      set.seed(new_seed)
      #### create a grid from -1 to 1
      x = runif(j, min=-1, max=1)
      y = runif(j, min=-1, max=1)
      
      ### set up vectors to sort them out: 
      x_in = c()
      y_in = c()
      x_out = c()
      y_out = c()
      ### calculate which are in a circle: (x^2 + y^2 = 1)
      for (k in seq_along(x)){ #since x and y are same length, we can use this: 
        distance = (x[k])^2 + (y[k])^2
        #print(distance)
        if (distance < 1){
          x_in = c(x_in, x[k])
          y_in = c(y_in, y[k])
        }else{
          x_out = c(x_out, x[k])
          y_out = c(y_out, y[k])
        }
      }
      
      #### estimating pi here: 
      pi = 4*length(x_in) / length(x)
      pi_est[i, j] = pi
    }
  } 
  return(pi_est)
}

pi = compute_pi(i_val, j_val, seed)

###### and plot them: 
# Plot the sample_mean matrix
matplot(t(pi), type = "l", lty = 1, col = rainbow(length(i_val)),
        xlab = "Number of Samples", ylab = "Pi Estimate", 
        main = "Estimating Pi w/ Monte Carlo", ylim=c(2, 4))
legend("topright", legend = paste("seed =", 123 +i_values), col = rainbow(length(i_val)), lty = 1, cex=0.3)



#### I need a function to test with just num_samples and 10 different seeds:
#### okay so now I will make a function, that computes pi for diff seeds and sample sizes:
i_val <- 1:10  # number of seeds to iterate over
seed = 123

compute_pi_n= function(i_val, n, seed){
  
  pi_est_n <- matrix(NA, nrow = length(i_val), ncol = 1)
  
  for (i in i_val){
      new_seed = seed+i
      set.seed(new_seed)
      #### create a grid from -1 to 1
      x = runif(n, min=-1, max=1)
      y = runif(n, min=-1, max=1)
      
      ### set up vectors to sort them out: 
      x_in = c()
      y_in = c()
      x_out = c()
      y_out = c()
      ### calculate which are in a circle: (x^2 + y^2 = 1)
      for (k in seq_along(x)){ #since x and y are same length, we can use this: 
        distance = (x[k])^2 + (y[k])^2
        #print(distance)
        if (distance < 1){
          x_in = c(x_in, x[k])
          y_in = c(y_in, y[k])
        }else{
          x_out = c(x_out, x[k])
          y_out = c(y_out, y[k])
        }
      }
      
      #### estimating pi here: 
      pi = 4*length(x_in) / length(x)
      pi_est[i] = pi
    }
  return(pi_est)
}

#### now we can run and just look for the right number of samples: 
pi_n = compute_pi_n(i_val, 10000, seed)

min_value <- min(pi_n)
max_value <- max(pi_n)
mean_value <- mean(pi_n)

range = max_value - min_value / 2 
