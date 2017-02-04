set.seed(31) # set seed for reproducability
n <- 40 # 40 samples
simulations <- 1000 # 1000 simulations
lambda <- 0.2 # set lambda to 0.2
simulated_exponentials <- replicate(simulations, rexp(n, lambda)) # simulate
means_exponentials <- apply(simulated_exponentials, 2, mean) # calculate mean of exponentials

# distrribution mean
 analytical_mean <- mean(means_exponentials)
 analytical_mean
#[1] 4.993867
 # analytical mean
 theory_mean <- 1/lambda
 theory_mean
#[1] 5

 # distrribution mean
 analytical_mean <- mean(means_exponentials)
 analytical_mean
#[1] 4.993867
 # analytical mean
 theory_mean <- 1/lambda
 theory_mean
#[1] 5
########Q2: Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
 # standard deviation of distribution
 standard_deviation_dist <- sd(means_exponentials)
 standard_deviation_dist
#[1] 0.7931608
 # standard deviation from analytical expression
 standard_deviation_theory <- (1/lambda)/sqrt(n)
 standard_deviation_theory
#[1] 0.7905694
 # variance of distribution
 variance_dist <- standard_deviation_dist^2
 variance_dist
#[1] 0.6291041
 # variance from analytical expression
 variance_theory <- ((1/lambda)*(1/sqrt(n)))^2
 variance_theory
#[1] 0.625
