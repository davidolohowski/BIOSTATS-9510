a <- 78
b <- 683
c <- 262
d <- 1542

lOR <- log(a*d/b/c)
OR <- exp(lOR)

Vlor <- 1/a + 1/b + 1/c + 1/d

ci <- c(exp(lOR - 1.96*sqrt(Vlor)), exp(lOR + 1.96*sqrt(Vlor)))


a <- c(18, 12, 27, 7, 14)
b <- c(162, 26, 121, 21, 353)
c <- c(25, 123, 104, 3, 7)
d <- c(252, 431, 475, 25, 359)
N <- a+b+c+d

ORmh <- sum(a*d/N)/sum(b*c/N)

S1 <- sum(a*d/N)
S2 <- sum(b*c/N)
S3 <- sum((a+d)*a*d/N^2)
S4 <- sum((b+c)*b*c/N^2)
S5 <- sum(((a+d)*b*c + (b+c)*a*d)/N^2)

Vlormh <- S3/2/S1^2 + S5/2/S1/S2 + S4/2/S2^2

cimh <- c(exp(log(ORmh) - 1.96*sqrt(Vlormh)), exp(log(ORmh) + 1.96*sqrt(Vlormh)))














