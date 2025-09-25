# Monte Carlo CLT demo for Exp(1)
set.seed(123)

B <- 1000   # number of repetitions
n <- 50     # sample size

# Exponential(1) has mean = 1, sd = 1
mu <- 1
sigma <- 1

# Collect the statistics
Z <- numeric(B)

for (b in 1:B) {
  x <- rexp(n, rate = 1)             # draw n samples from Exp(1)
  xbar <- mean(x)                    # sample mean
  Z[b] <- (xbar - mu) / (sigma / sqrt(n))  # standardized statistic
}

# Histogram of the simulated Z
hist(Z, probability = TRUE,
     main = "Standardized sample mean",
     xlab = "Z", col = "lightblue", border = "white")

# Overlay standard normal density
curve(dnorm(x), from = -4, to = 4, add = TRUE, lwd = 2, col = "black")
