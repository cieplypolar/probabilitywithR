library(MASS)
library(mvtnorm)

R <- 1000
is <- c(-1, 0, 1)
ds <- c(1, 3, 5)
par(mfrow = c(1, 2))

for (d in ds) {
  for (i in is) {
    miu <- c(1, 2)
    sigma <- matrix(c(1, i, i, d), ncol = 2)
    samples <- mvrnorm(R, miu, sigma)
    X <- samples[, 1]
    Y <- samples[, 2]
    hist(X, breaks = 50, main = paste("Marginal distribution of X for i =", i, "d=", d),
         col = "blue", border = "white", probability = TRUE)
    curve(dnorm(x, 1, 1), col = "red", lwd = 2, add = TRUE)
    hist(Y, breaks = 50, main = paste("Marginal distribution of Y for i =", i, "d=", d),
         col = "purple", border = "white", probability = TRUE)
    curve(dnorm(x, 2, sqrt(d)), col = "pink", lwd = 2, add = TRUE)
  }
}