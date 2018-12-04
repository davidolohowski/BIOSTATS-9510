library(tidyverse)
library(xtable)

renal <- read_csv("renal.csv")

fit <- glm(obs ~., data = renal, family = binomial(link = "logit"))

sigmahat <- diag((renal$obs-fitted(fit))^2)

X <- as.matrix(cbind(intercept = rep(1,172), renal[,-1]))

Jhat <- t(X)%*%sigmahat%*%X

#xtable(Jhat, caption = "Estimated Score Vector Outer Products", digits = 5)

sigmaR <- vcov(fit)%*%Jhat%*%vcov(fit)

#xtable(sigmaR, caption = "Robust Estimate of the covariance", digits = 5)

seR <- sqrt(diag(sigmaR))

lower <- fit$coefficients - 1.96*seR

upper <- fit$coefficients + 1.96*seR

wald <- fit$coefficients^2/diag(sigmaR)

p <- pchisq(wald, 1, lower.tail = F)

te <- bind_cols(SE = seR, Lower = lower, Upper = upper, `Wald X^2` = wald, `p <=` = p)

#xtable(te, caption = "Robust Test statistics", digits = 5)

sigmahat0 <- diag((renal$obs-mean(fitted(fit)))^2)

Jhat0 <- t(X)%*%sigmahat0%*%X

#xtable(Jhat0, caption = "Estimated Score Vector Outer Products under the Null", digits = 5)

I0 <- mean(fitted(fit))*(1-mean(fitted(fit)))*t(X)%*%X

sigmaR0 <- solve(I0)%*%Jhat0%*%solve(I0)

#xtable(sigmaR0, caption = "Robust Estimate of the covariance under the Null", digits = 5)

Ualpha <- renal$obs - mean(fitted(fit))

Ubeta <- Ualpha*X

U0 <- colSums(Ubeta)

X2 <- t(U0)%*%solve(Jhat0)%*%U0




















