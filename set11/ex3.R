library(MASS)
library(mvtnorm)

R <- 10000
miu <- c(1, 2, 3)
sigma <- matrix(c(2, 2, 0, 2, 4, 1, 0, 1, 8), ncol = 3)

samples <- mvrnorm(R, miu, sigma)
X <- samples[, 1] + 2 * samples[, 2] - 3 * samples[, 3]

hist(X, breaks = 50, main = "Y1 + 2*Y2 - 3*Y3", col = "blue", border = "white", probability = TRUE)
curve(dnorm(x, mean(X), sd = sd(X)), from = min(X), to = max(X), col = "red", lwd = 2, add = TRUE)