y <- c(44, 21, 60, 19, 5, 16)
N <- c(1578, 481, 1121, 1142, 231, 857)
smoke <- c(1,1,1,0,0,0)
strat <- c(1,2,3,1,2,3)

X <- data.frame(y, N, smoke, strat)

logfit <- glm(cbind(y,N-y)~smoke+strat, data = X, family = binomial(link = "log"))

loglog <- glm(cbind(y,N-y)~smoke+strat, data = X, family = binomial(link = "cloglog"))

prob <- glm(cbind(y,N-y)~smoke+strat, data = X, family = binomial(link = "probit"))
