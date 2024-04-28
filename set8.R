set.seed(1882)
# ex 1
ex1 = function(N = 100, R = 5000, by = 0.05, alpha = 0.05) {
  p_seq = seq(by, 1, by)
  for (p in p_seq) {
    res = replicate(R, {
      geo = rgeom(N, p)
      t = table(geo)
      sample = as.numeric(t)
      prob = dexp(x = as.numeric(names(t)), rate = p)
      t = chisq.test(sample, p = prob, rescale.p = TRUE)
      return (t$p.value >= alpha)
    })
    print(paste("Chisq test p:", p, "rejected hypothesis rate:", 1 - sum(res) / R))
    
    res = replicate(R, {
      geo = rgeom(N, p)
      exp = rexp(N, p)
      t = ks.test(geo, exp)
      return (t$p.value >= alpha)
    })
    print(paste("KS test p:", p, "rejected hypothesis rate:", 1 - sum(res) / R))
  }
}

# ex1()
# conclusions chisq test has local minimum, ks test above some small p begins to reject 100%

# ex 2
ex2 = function(sizes = c(15, 30, 50), s_range = c(0,5), sd = 1, R = 1000) {
  # cmp N(0, 1) to N(s, 1) with t-test and ks-test
  for (s in s_range[1]:s_range[2]) {
    for (c in sizes) {
      res = replicate(R, {
        norm  = rnorm(c, 0, sd)
        norms = rnorm(c, s, sd)
        tt  = t.test(norm, norms)
        kst = ks.test(norm, norms)
        return (tt$p.value <= kst$p.value)
      })
      ttbetter  = sum(res)
      print(paste("s:", s, "sample size:", c, 
                  "t-test was better in", ttbetter, 
                  "KS-test was better in", R - ttbetter, "per", R))
    }    
  } 
}

# ex2()
# conclusions t-test is better 

# ex 3
library(nortest)
ex3 = function(R = 1000, N = 30) {
  print("Kolmogorov-Smirnov")
  mean_p = mean(replicate(R, { return (lillie.test(runif(N))$p.value)}))
  print(paste("Light tail - uni. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (lillie.test(rcauchy(N))$p.value)}))
  print(paste("Heavy tail - Cauchy. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (lillie.test(rlnorm(N))$p.value)}))
  print(paste("Asymmetric - lognormal. Mean p-value:", mean_p))
  print("")
  
  print("Anderson-Darling")
  mean_p = mean(replicate(R, { return (ad.test(runif(N))$p.value)}))
  print(paste("Light tail - uni. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (ad.test(rcauchy(N))$p.value)}))
  print(paste("Heavy tail - Cauchy. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (ad.test(rlnorm(N))$p.value)}))
  print(paste("Asymmetric - lognormal. Mean p-value:", mean_p))
  print("")
  
  print("Pearson")
  mean_p = mean(replicate(R, { return (pearson.test(runif(N))$p.value)}))
  print(paste("Light tail - uni. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (pearson.test(rcauchy(N))$p.value)}))
  print(paste("Heavy tail - Cauchy. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (pearson.test(rlnorm(N))$p.value)}))
  print(paste("Asymmetric - lognormal. Mean p-value:", mean_p))
  print("")
  
  print("Cramer-von Mises")
  mean_p = mean(replicate(R, { return (cvm.test(runif(N))$p.value)}))
  print(paste("Light tail - uni. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (cvm.test(rcauchy(N))$p.value)}))
  print(paste("Heavy tail - Cauchy. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (cvm.test(rlnorm(N))$p.value)}))
  print(paste("Asymmetric - lognormal. Mean p-value:", mean_p))
  print("")
  
  print("Shapiro-Francia")
  mean_p = mean(replicate(R, { return (sf.test(runif(N))$p.value)}))
  print(paste("Light tail - uni. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (sf.test(rcauchy(N))$p.value)}))
  print(paste("Heavy tail - Cauchy. Mean p-value:", mean_p))
  mean_p = mean(replicate(R, { return (sf.test(rlnorm(N))$p.value)}))
  print(paste("Asymmetric - lognormal. Mean p-value:", mean_p))
  print("")
}

# before running
# light tails - high pvalue, test can fail
# heavy tails - less fails than "light tails"
# asymmetric - should not fail

# ex3()

# ex 4
ex4 = function(s1 = c(84, 72, 57, 46, 63, 76, 99, 91), 
               s2 = c(81, 69, 74, 61, 56, 87, 69, 65, 66, 44, 62, 69),
               R  = 10000) {
  t = t.test(s1, s2)
  u = wilcox.test(s1, s2)
  print(t)
  print("")
  print(u)
  print("")
  
  s1_copy = s1
  s2_copy = s2
  
  s = c()
  append(s, s1_copy)
  append(s, s2_copy)
  
  print("Permutation Test")
  res = replicate(R, {
    temp = mean(s1_copy) - mean(s2_copy)
    l1 = length(s1_copy)
    l2 = length(s2_copy)
    perm = sample(0:1, l1 + l2, replace = TRUE)
    s1_new = c()
    s2_new = c()
    for (i in 1:l1) {
      if (perm[i] == 0) {
        append(s1_new, s[i])
      } else {
        append(s2_new, s[i])
      }
    }
    
    for (i in 1:l2) {
      if (perm[l1 + i] == 0) {
        append(s1_new, s[l1 + i])
      } else {
        append(s2_new, s[l1 + i])
      }
    }
    
    s = c()
    append(s, s1_new)
    append(s, s2_new)
    s1_copy = s1_new
    s2_copy = s2_new
    return (temp)
  })
  
  dens = density(res)
  plot(dens, main = "permutation test statistics distribution", col = "green")
  print(dens)
  print(paste("initial statistic:", mean(s1) - mean(s2)))
  print("reading from plot, p-value is very high, so we accept null hypothesis (means are equal)")
}

# ex4()












