## Speed of convergence: Uniform(50,100) vs Pareto(alpha = 2.01, xm = 1)
set.seed(123)

## Parameters
B <- 1000
n_vals <- c(10, 100, 1000)

## X ~ Uniform(50, 100)
a <- 50
b <- 100
mu_X <- (a + b) / 2                   # = 75
sigma2_X <- (b - a)^2 / 12            # = 2500/12
sigma_X <- sqrt(sigma2_X)

## Y ~ Pareto(alpha, xm = 1), pdf f(y) = alpha * y^(-alpha-1), y >= 1
alpha <- 2.01
mu_Y <- alpha / (alpha - 1)           # = 2.01 / 1.01
sigma2_Y <- alpha / ((alpha - 1)^2 * (alpha - 2))  # finite since alpha > 2
sigma_Y <- sqrt(sigma2_Y)

## 1) Plot densities (pmf/pdf)
par(mfrow = c(1, 2))
curve(dunif(x, a, b), from = a, to = b, n = 1000,
      xlab = "x", ylab = "Density", main = "Uniform(50, 100)")
curve(alpha * x^(-alpha - 1), from = 1, to = 20, n = 2000,
      xlab = "y", ylab = "Density", main = "Pareto(α = 2.01, x_m = 1)")

## 2) Monte Carlo: standardized means, histograms with N(0,1) overlay
par(mfrow = c(2, 3))
xlim_std <- c(-6, 6)
n_bins   <- 40

## --- n = 10 ---
n <- 10
Zx <- numeric(B)
for (b_iter in 1:B) {
  x <- runif(n, a, b)
  Zx[b_iter] <- (mean(x) - mu_X) / (sigma_X / sqrt(n))
}
hist(Zx, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Uniform[50,100], n = 10", xlab = "Z_X",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)



## --- n = 100 ---
n <- 100
Zx <- numeric(B)
for (b_iter in 1:B) {
  x <- runif(n, a, b)
  Zx[b_iter] <- (mean(x) - mu_X) / (sigma_X / sqrt(n))
}
hist(Zx, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Uniform[50,100], n = 100", xlab = "Z_X",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)

## --- n = 1000 ---
n <- 1000
Zx <- numeric(B)
for (b_iter in 1:B) {
  x <- runif(n, a, b)
  Zx[b_iter] <- (mean(x) - mu_X) / (sigma_X / sqrt(n))
}
hist(Zx, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Uniform[50,100], n = 1000", xlab = "Z_X",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)

Zy <- numeric(B)
for (b_iter in 1:B) {
  u <- runif(n)
  y <- u^(-1/alpha)   # inverse-CDF for Pareto with xm = 1
  Zy[b_iter] <- (mean(y) - mu_Y) / (sigma_Y / sqrt(n))
}
hist(Zy, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Pareto(2.01), n = 10", xlab = "Z_Y",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)

Zy <- numeric(B)
for (b_iter in 1:B) {
  u <- runif(n)
  y <- u^(-1/alpha)
  Zy[b_iter] <- (mean(y) - mu_Y) / (sigma_Y / sqrt(n))
}
hist(Zy, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Pareto(2.01), n = 100", xlab = "Z_Y",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)



Zy <- numeric(B)
for (b_iter in 1:B) {
  u <- runif(n)
  y <- u^(-1/alpha)
  Zy[b_iter] <- (mean(y) - mu_Y) / (sigma_Y / sqrt(n))
}
hist(Zy, probability = TRUE, breaks = n_bins, xlim = xlim_std,
     main = "Pareto(2.01), n = 1000", xlab = "Z_Y",
     col = "lightblue", border = "white")
curve(dnorm(x), add = TRUE, lwd = 2)
abline(v = 0, lty = 2)
