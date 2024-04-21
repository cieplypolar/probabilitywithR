n = 10
repeat {
  res = 0
  for (k in 10:n) {
    res = res +  (factorial(n) / (factorial(k) * factorial(n - k)) * (1 / 2 ^ n))
  }
  print(paste("n:", n, "pvalue:", res))
  n = n + 1
  if (res > 1) {
    break
  }
}