# ex 3
ex3 = function(alpha = 10, n = 5000) {
  v = runif(n, 0, alpha)
  r = 2 * mean(v)
  s = ((n + 1) / n) * max(v)
  t = (n + 1) * min(v)
  
  print(paste("Alpha:", alpha))
  print(paste("R:", r))
  print(paste("S:", s))
  print(paste("T:", t))
  
  repeats = 5000
  r = c()
  s = c()
  t = c()
  
  for (i in 1:repeats) {
    v = runif(n, 0, alpha)
    r[i] = 2 * mean(v)
    s[i] = ((n + 1) / n) * max(v)
    t[i] = (n + 1) * min(v)
  }
  
  print(paste("R variance:", var(r)))
  print(paste("S variance:", var(s)))
  print(paste("T variance:", var(t)))
}

# ex3()

# ex 5
ex5 = function(rate = 8, n = 5000) {
  repeats = 5000
  s = c()
  t = c()
  r = c()
  
  for (i in 1:repeats) {
    v = rexp(n, rate)
    s[i] = mean(v)
    t[i] = (1 / log(2)) * median(v)
    r[i] = n * min(v)
  }
  scp = s
  tcp = t
  rcp = r
  
  expected = 1 / rate
  
  sbias = scp - expected
  tbias = tcp - expected
  rbias = rcp - expected
  print(paste("Rate:", rate))
  print(paste("S bias:", mean(sbias)))
  print(paste("T bias:", mean(tbias)))
  print(paste("R bias:", mean(rbias)))
  print(paste("S variance:", var(s)))
  print(paste("T variance:", var(t)))
  print(paste("R variance:", var(r)))
  print(paste("S MSE:", var(sbias)))
  print(paste("T MSE:", var(tbias)))
  print(paste("R MSE:", var(rbias)))
}

# ex5()


