# zad 1

k = 3
repeats = 10000
p = 0.5

result_vec <- c()
for (i in 1:repeats ) {
  pos = 1
  while (TRUE) {
    if (pos == (k + 2) * 2 - 1) {
      result_vec[i] <- 0
      break
    }
    if (sample(c(0, 1), size = 1, replace = TRUE, prob = c(1 - p, p)) == 0) {
      if (pos >= k + 2) {
        result_vec[i] <- 1
      }
      else {
        result_vec[i] <- 0
      }
      break
    } else {
      pos <- pos + 1
    }
  }
}

sucs <- sum(result_vec)
print(paste("Simulated probability: ", sucs/repeats))

# zad 2
n = 10
repeats = 10000
v <- runif(n * repeats)
m <- matrix(v, n, repeats)
y <- colSums(m)
y <- y / c(n)
y <- y - c(0.5)
y <- y / c(sqrt((1/12)/n))
hist(y, probability = TRUE)
hist(rnorm(repeats), probability = TRUE)

#zad 4
TrafN = function(n)  {
  if (n > 6) return(0)
  repeats = 10000
  sucs = 0
  for (i in 1:repeats) {
    winner = sample(1:49, 6, replace = FALSE)
    my = sample(1:49, 6, replace = FALSE)
    inter = intersect(winner, my)
    if (length(inter) == n) {
      sucs = sucs + 1
    }
  }
  return (sucs/repeats)
}

Bankructwo = function(w3, w4, w5, w6, z, b, p) {
  sucs = 0
  for (i in 1:b) {
    balance = 0
    winner = sample(1:49, 6, replace = FALSE)
    for (j in 1:z) {
      balance = balance - p
      mycoupon = sample(1:49, 6, replace = FALSE)
      n = length(intersect(winner, mycoupon))
      prize = 0
      if (n == 6) prize = w6
      else if (n == 5) prize = w5
      else if (n == 4) prize = w4
      else if (n == 3) prize = w3
      balance = balance + prize
    }
    if (balance > 0) {
      sucs = sucs + 1
    }
  }
  return (sucs / b)
}

# Bankructwo(24, 300, 10000, 1000000, 3000, 100, 3)

# zad 5
# E[X] 
repeats = 10000
sum = 0
rate = 1
p = 0.6
q = 0.8
w = 0.1



for (i in 1:repeats) {
  T = 0
  x0 = rnorm(1, 10, 1)
  T = T + x0
  road <- c()
  color = sample(c(0, 1), 1, TRUE, c(1 - p, p))
  if (color == 1) {
    road[1] <- 0
  } else {
    road[1] <- rexp(1, rate)
  }
  
  for (j in 2:10) {
    if (road[j - 1] > 0) {
      color = sample(c(0, 1), 1, TRUE, c(1 - w, w))
      if (color == 1) {
        road[j] <- 0
      } else {
        road[j] <- rexp(1, rate)
      }
    } else {
      color = sample(c(0, 1), 1, TRUE, c(1 - q, q))
      if (color == 1) {
        road[j] <- 0
      } else {
        road[j] <- rexp(1, rate)
      }
    }
  }
  
  T = T + sum(road)
  sum = sum + T
}

print(paste("Mean: ", sum / repeats))

# P(T > 30)
repeats = 10000
sucs = 0
rate = 1
p = 0.6
q = 0.8
w = 0.1



for (i in 1:repeats) {
  T = 0
  x0 = rnorm(1, 10, 1)
  T = T + x0
  road <- c()
  color = sample(c(0, 1), 1, TRUE, c(1 - p, p))
  if (color == 1) {
    road[1] <- 0
  } else {
    road[1] <- rexp(1, rate)
  }
  
  for (j in 2:10) {
    if (road[j - 1] > 0) {
      color = sample(c(0, 1), 1, TRUE, c(1 - w, w))
      if (color == 1) {
        road[j] <- 0
      } else {
        road[j] <- rexp(1, rate)
      }
    } else {
      color = sample(c(0, 1), 1, TRUE, c(1 - q, q))
      if (color == 1) {
        road[j] <- 0
      } else {
        road[j] <- rexp(1, rate)
      }
    }
  }
  
  T = T + sum(road)
  if (T > 30) {
    sucs = sucs + 1
  }
}

print(paste("Bound of P(T > 30): ", sucs / repeats))

# zad 6

# states
dead = 1
sick = 2
lsick = 3
states = c(dead, sick, lsick)

sickp = c(0.3, 0.6, 0.1)
lsickp = c(0.1, 0.3, 0.6)

costs = c(0, 40000, 5000)

repeats = 100000
# starting with lsick
years = 0
cost = 0
for (i in 1:repeats) {
  mchain <- c(lsick)
  y = 1
  while(TRUE) {
    if (mchain[y] == dead) {
      break
    }
    
    newstate = switch(mchain[y], -1, sample(states, 1, prob = sickp), sample(states, 1, prob = lsickp))
    cost = cost + costs[mchain[y]]
    y = y + 1
    mchain[y] <- newstate
  }
  years = years + y - 1 # because of indeces in R 
}

print(paste("Slightly sick patient will be in hospital: ", years / repeats, " and it will cost ", cost / repeats))

# sick patient 
# starting with lsick
years = 0
cost = 0
for (i in 1:repeats) {
  mchain <- c(sick)
  y = 1
  while(TRUE) {
    if (mchain[y] == dead) {
      break
    }
    
    newstate = switch(mchain[y], -1, sample(states, 1, prob = sickp), sample(states, 1, prob = lsickp))
    cost = cost + costs[mchain[y]]
    y = y + 1
    mchain[y] <- newstate
  }
  years = years + y - 1 # because of indeces in R 
}

print(paste("Sick patient will be in hospital: ", years / repeats, " and it will cost ", cost / repeats))

# zad 8
# Zadanie 4. Dwójka przyjaciół chodzi do szkoły razem. Każdy z nich niezależnie przybywa
# na umówione skrzyżowanie o godzinie będącej zmienną losową z rozkładu jednostajnego
# na przedziale 7 : 00 − 7 : 20 i czeka na kompana 5 minut. Po tym czasie jeśli przyjaciel
# nie przyszedł osoba idzie sama do szkoły. Jakie jest prawdopodobieństwo, że przyjaciele
# pójdą razem? Powinno wyjsc 7/16

repeats = 100000 
sucs = 0

for (i in 1:repeats) {
  x1 = runif(1, 0, 20)
  x2 = runif(1, 0, 20)
  x = abs(x1 - x2)
  if (x <= 5) {
    sucs = sucs + 1
  }
}
print(paste("They meet with P = ", sucs / repeats))

# Expected k-th particle decay time, each particle ~ Exp(1), should end up with rate^-1(H_n - H_(n - k)
repeats = 100000
time = 0
rate = 1
n = 100
k = 60
for (i in 1:repeats) {
  t = 0
  particles = n
  j = 0
  while (j < k) {
    v = rexp(particles, rate)
    firsttime = min(v)
    t = t + firsttime
    j = j + 1
    particles = particles - 1
  }
  time = time + t
}

print(paste("Mean time of k-th particle decay: ", time / repeats))


















