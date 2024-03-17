# ex 1
chi2 = function(n) {
  repeats = 10000
  v = rnorm(n * repeats)
  v = v ^ 2
  m = matrix(v, n, repeats)
  y = colSums(m)
  
  hist(y, probability = TRUE, main = paste("Sum of", n, "N(0, 1)"))
  
  interval = seq(min(y), max(y), by = 0.1)
  d = dchisq(interval,df=n)
  
  lines(interval, d, col = "BLUE")
}

# chi2(50)

# ex 2
poissonJoin = function(rate1, rate2) {
  repeats = 10000
  
  p1 = rpois(repeats, rate1)
  p2 = rpois(repeats, rate2)
  p = p1 + p2
  
  hist(p, probability = TRUE, main = "Sum of 2 Poiss", breaks = seq(min(p) - 1, max(p) + 1, by = 1))
  
  interval = seq(min(p), max(p), by = 1)
  d = dpois(interval, rate1 + rate2)
  
  points(interval, d, pch = 23, col = "BLUE", bg = "BLUE")
}

# poissonJoin(1, 2)

# ex 3
library(Matrix)
wigner = function(n) {
  repeats = 1000
  E = c()
  for (i in 1:repeats) {
    m = matrix(rnorm(n ^ 2, 0, 1), n)
    m = forceSymmetric(m)
    m = m / sqrt(n) # as harvard papers states matrices should be normalized
    e = eigen(m)$values
    E = append(E, e)
  }
  
  interval = seq(-2, 2, by = 0.02)
  wignersemicircle = (function(x)(sqrt(4 - x^2)/(2 * pi)))(interval)
  
  hist(E, probability = TRUE, main = "Wigner's semicircle law", 
       plot = TRUE, xlim = c(min(-2, min(E)) - 1, max(2, max(E)) + 1),
       ylim = c(0, max(wignersemicircle)))
  
  lines(interval, wignersemicircle, col = "BLUE", lwd = 2)
}

# for (i in c(10, 100, 500)) {
#  wigner(i)
# }



