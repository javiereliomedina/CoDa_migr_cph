# hongite - kongite

library(compositions)
library(tidyverse)

file <- "C:/Users/FU53VP/OneDrive - Aalborg Universitet/Skrivebord/prueba/hongite.csv"
dat <- read.csv(file)

h <- dat[1:25,]
k <- dat[26:50, ]

h_alr <- alr(h[ , -1])
k_alr <- alr(k[ , -1])

# Maximum likelihood estimates (table 7.7 - Aitchison 1986)

N1 <- length(h$GP)
N2 <- length(k$GP)

m1 <- mean(h_alr) %>% matrix(ncol = 1)
m2 <- mean(k_alr) %>% matrix(ncol = 1)

E1 <- cov(h_alr)
E2 <- cov(k_alr)
 
# S1
DD <- h_alr - t(m1)
S1 <- matrix(NA, 4, 4)

for(i in 1:4) { 
  for(j in 1:4) {
    S1[i, j] = (1/N1) * sum(DD[ , i] * t(DD[ , j]))
  }
}

# S2 
DD <- k_alr - t(m2)
S2 <- matrix(NA, 4, 4)
for(i in 1:4) { 
  for(j in 1:4) {
    S2[i, j] = (1/N2) * sum(DD[ , i] * t(DD[ , j]))
  }
}

# Sp
Sp <- (1 / (N1 + N2)) * (N1*S1 + N2*S2)

# mc = (N1 + N2)^-1 * (N1*m1 + N2*m2)
mc <- (N1 + N2)^-1*(N1*m1 + N2*m2)

# Sc = Sp + (N1 + N2)-2 N1 N2 (m 1 -m2)(m1 - m2)'
Sc <- Sp + ((N1 + N2)^-2 * N1 * N2 * (m1 - m2) %*% t(m1 - m2))

# mh = (N1*S1h^-1 + N2*S2h^-1)^-1 x (N1*S1h^-1*m1 + N2*S2h^-1*m2)
S1h = matrix(S1, nrow = 4)
S2h = matrix(S2, nrow = 4)


# mh  = solve(N1 * solve(S1h) + N2 * solve(S2h)) %*% ((N1 * solve(S1h) %*% m1) + (N2 * solve(S2h) %*% m2))
# mh
# S1h <- S1 + (m1 - mh) * t(m1 - mh)
# S1h
# S2h <- S2 + (m2 - mh) * t(m2 - mh)
# S2h

x <- 1
repeat {
  print(x)
  mh  = solve(N1 * solve(S1h) + N2 * solve(S2h)) %*% ((N1 * solve(S1h) %*% m1) + (N2 * solve(S2h) %*% m2))
  S1h <- S1 + (m1 - mh) %*% t(m1 - mh)
  S2h <- S2 + (m2 - mh) %*% t(m2 - mh)
  mh
  x = x+1
  if (x == 25){
    break
  }
}

S1h <- matrix(S1h, nrow = 4)
S2h <- matrix(S2h, nrow = 4)
mh

# Level 1: Hypothesis (H0: µ1 = µ2 and E1 = E2)
value <- (N1*log(det(Sc)/det(S1))) + (N2*log(det(Sc)/det(S2)))
value
d <- 4
df <- 0.5 * d * ( d + 3)
df
p_value <- pchisq(value, df = df, lower.tail = FALSE)
p_value 

# level 2 - H0: E1 = E2
value <- (N1*log(det(Sp)/det(S1))) + (N2*log(det(Sp)/det(S2)))
value
d <- 4
df <- 0.5 * d * ( d + 1)
df
p_value <- pchisq(value, df = df, lower.tail = FALSE)
p_value 

# level 2 - H0: µ1 = µ2
value <- (N1*log(det(S1h)/det(S1))) + (N2*log(det(S2h)/det(S2)))
value
d <- 4
df <- d
df
p_value <- pchisq(value, df = df, lower.tail = FALSE)
p_value




