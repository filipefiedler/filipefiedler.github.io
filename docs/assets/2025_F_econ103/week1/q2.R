# Monte Carlo for sample variance of Chi-squablack(50)
set.seed(123)

B <- 1000         # number of repetitions
df <- 50          # degrees of freedom
true_var <- 2 * df  # = 100

# Fixed x-axis range for comparison
xlim_range <- c(20, 150)
n_bins <- 30

# Case n = 10
n <- 10
vars <- numeric(B)
for (b in 1:B) {
  x <- rchisq(n, df = df)
  vars[b] <- var(x)   # sample variance
}
cat("n = 10, difference =", mean(vars) - true_var, "\n")
par(mfrow = c(1, 3)) # Make histograms appear side by side
hist(vars, probability = TRUE, breaks = n_bins,
     main = "Sample variance, n = 10",
     xlab = "Sample variance", col = "lightblue", border = "white",
     xlim = xlim_range)
abline(v = true_var, col = "black", lwd = 2)

# Case n = 100
n <- 100
vars <- numeric(B)
for (b in 1:B) {
  x <- rchisq(n, df = df)
  vars[b] <- var(x)
}
cat("n = 100, difference =", mean(vars) - true_var, "\n")
hist(vars, probability = TRUE, breaks = n_bins,
     main = "Sample variance, n = 100",
     xlab = "Sample variance", col = "lightblue", border = "white",
     xlim = xlim_range)
abline(v = true_var, col = "black", lwd = 2)

# Case n = 1000
n <- 1000
vars <- numeric(B)
for (b in 1:B) {
  x <- rchisq(n, df = df)
  vars[b] <- var(x)
}
cat("n = 1000, difference =", mean(vars) - true_var, "\n")
hist(vars, probability = TRUE, breaks = n_bins,
     main = "Sample variance, n = 1000",
     xlab = "Sample variance", col = "lightblue", border = "white",
     xlim = xlim_range)
abline(v = true_var, col = "black", lwd = 2)
