# Monte Carlo for 90% CI of the mean, unknown variance
set.seed(123)

B <- 1000      # number of repetitions
n <- 100       # sample size
mu <- 5        # true mean
sigma <- 2     # true sd (used only to generate data)

contains_mu <- logical(B)

for (b in 1:B) {
  # draw sample
  x <- rnorm(n, mean = mu, sd = sigma)
  xbar <- mean(x)
  s <- sd(x)
  
  # critical value from t distribution
  tcrit <- qt(0.95, df = n - 1)
  
  # CI endpoints
  half_len <- tcrit * s / sqrt(n)
  lower <- xbar - half_len
  upper <- xbar + half_len
  
  # check if true mean lies in CI
  contains_mu[b] <- (lower <= mu) && (mu <= upper)
}

prop_covered <- mean(contains_mu)
cat("Proportion of 90% CIs containing mu = 5:", round(prop_covered, 3), "\n")
