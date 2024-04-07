# ex 2
ex2 = function(M = 1000, n = 100, miu = 8, sigma = 5, alpha = 0.05) {
  res = c()  
  for (i in 1:M) {
    x = rnorm(n, miu, sigma)  
    x_mean = mean(x)
    s_temp = x
    s_temp = (s_temp - x_mean) ^ 2
    s = sqrt(mean(s_temp))
    
    l = x_mean - (s/sqrt(n - 1)) * qt((1 - alpha/2), n - 1)
    r = x_mean + (s/sqrt(n - 1)) * qt((1 - alpha/2), n - 1)
    print(paste("Przedzial ufnosci: (", l, ",", r, ")"))
    if (miu >= l && miu <= r) {
      res[i] = 1
    } else {
      res[i] = 0
    }
  }
  suc = sum(res)
  print(paste("Prawdziwa wartość miu należy do: ", suc * 100 / M, "% przedziałów"))
}

# ex2()

# ex 3
ex3 = function(x_mean = 743, sigma = 5, alpha = 0.05, n = 16) {
  print("Part 1:")
  
  l = x_mean - (sigma / sqrt(n)) * qnorm(1 - alpha/2)
  r = x_mean + (sigma / sqrt(n)) * qnorm(1 - alpha/2)
  
  print(paste("Przedział ufności 95% to: (", l, ",", r, ")"))
  
  print("Part 2:")
  
  m = 1
  beta = 0.01
  repeat {
    l = x_mean - (sigma / sqrt(m)) * qnorm(1 - beta/2)
    r = x_mean + (sigma / sqrt(m)) * qnorm(1 - beta/2)
    if (abs(l - r) <= 1) {
      print(paste("Przedział ufności 99% długości co najwyżej 1ml to: (", l, ",", r, ")"))
      print(paste("Wymagane n:", m))
      break
    }
    m = m + 1
  }
  
  
}

# ex3()

# ex 4
ex4 = function(p = 0.7, alpha = 0.1) {
  n = 30
  repeat {
    l = p - (sqrt(p * (1 - p) / sqrt(n))) * qnorm(1 - alpha/2)
    r = p + (sqrt(p * (1 - p) / sqrt(n))) * qnorm(1 - alpha/2)
    if (abs(l - r) <= 0.02) {
      print(paste("Przedział ufności 90% długości co najwyżej 2% to: (", l, ",", r, ")"))
      print(paste("Wymagane n:", n))
      break
    }
    n = n + 1
    print(n)
    print(abs(l - r))
  }
}

# ex4()

# ex 5 
ex5 = function(theta = 111, n = 500, alpha = 0.05, M = 1000) {
  res = c()
  for (i in 1:M) {
    v = runif(500, 0, theta)
    xn = max(v)
    l = xn
    r = xn / (alpha ^ (1/n))
    if (theta >= l && theta <= r) {
      res[i] = 1
    } else {
      res[i] = 0
    }
  }
  
  suc = sum(res)
  print(paste("Przedział ufności na poziomie", suc * 100 / M, "%"))
}

# ex5()

# ex 6
ex6 = function(miu = 7, sigma = 3, n = 1000, M = 10000) {
  res = c()
  for (i in 1:M) {
    v = rnorm(n, miu, sigma)
    v_mean = mean(v)
    z = sqrt(n) * (v_mean - miu) / sigma
    res[i] = 2 * pnorm(-abs(z))
  }
  dens = density(res)
  print(res)
  plot(dens, main = "p-value")
}

ex6()


