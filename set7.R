#set 7

# ex 1
ex1 = function(mean = 7, sd = 11, N = 5, R = 10000) {
  samples = replicate(R, 
    { 
      v = rnorm(N, mean, sd)
      v_mean = mean(v)
      v_copy = v
      s = sqrt(mean((v_copy - v_mean) ^ 2))
      return (N * (v_mean - mean) / s)
    }
  )
  dens = density(samples)
  plot(dens, main = "from samples", col = "red")
  x_min = min(samples)
  x_max = max(samples)
  seq = seq(x_min, x_max, by = 0.05)  
  dens_norm = dnorm(seq)
  dens_t = dt(seq, N - 1)
  lines(seq, dens_norm, col = "blue")
  lines(seq, dens_t, col = "green")
}

# ex1()

# ex 2
ex2 = function(mean =  500, v = c(501, 494, 497, 500, 488, 494, 497, 491, 493), alpha = 0.01) {
  t.test(x = v, alternative = "less", mu = mean, conf.level = 1 - alpha)
}

# ex2()
# 	One Sample t-test
# data:  v
# t = -3.5857, df = 8, p-value = 0.003565
# alternative hypothesis: true mean is less than 500
# 99 percent confidence interval:
#   -Inf 499.0389
# sample estimates:
#   mean of x 
# 495 
# p-value < alpha -> odrzucamy hipoteze na rzecz alternatywnej

# ex 4
ex4 = function(sd1 = 7.2, sd2 = 4.5, N = 25, alpha = 0.05) {
  k = qchisq(alpha, N - 1)
  z = (N * sd2 ^ 2) / sd1 ^ 2
  if (z <= k) {
    print("sd < 7.2")
  } else {
    print("nie ma podstaw do odrzucenia")
  }
  
}

# ex4()

# ex 5
ex5 = function(ones = 46, N = 200) {
  p0 = 1 / 6
  p = ones / N
  z = (p - p0) / sqrt(p * (1 - p))
  pval = 1 - pnorm(z)
  print(paste("p-value:", pval))
}

# ex5() 
# nie ma podstaw do odrzucenia, bo duze p-value - kostka symetryczna

# ex 6
ex6 = function(dices = c(46, 29, 27, 39, 29, 30), N = 200) {
  chisq.test(dices)
}

# ex6()
# nie ma podstaw do odrzucenia, bo duze p-value - kostka symetryczna





