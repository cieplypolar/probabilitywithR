library(MASS)
library(mvtnorm)

R <- 1000
is <- c(-2, -1, 0, 1, 2)
par(mfrow = c(2, 3))

for (i in is) {
  miu <- c(1, 2)
  sigma <- matrix(c(1, i, i, 4), ncol = 2)
  samples <- mvrnorm(R, miu, sigma)
  plot(samples, main = paste("Scatter plot for i =", i),
       pch = 20, col = "blue")
}