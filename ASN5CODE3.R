library(tidyverse)
library(survival)
library(xtable)

bresmack <- read_csv("bresmack.csv") %>%
  mutate(obese = ifelse(obese == 9, 0, obese),
         conjest = ifelse(dose == 0, 0, 1),
         dose = relevel(as.factor(ifelse(dose == 9, NA, 
                       ifelse(dose >=3, "high", 
                              ifelse(dose == 2, "middle", 
                                     ifelse(dose == 1, "low", "none"))))), ref = "none"),
         dur = ifelse(dur == 99, NA, dur))
#(a)

clog_unadj <- clogit(case ~ estrogen + strata(caseset), data = bresmack)

clog_adj.add <- clogit(case ~ estrogen + gbdx + hyper + obese + nonest + strata(caseset), data = bresmack)

summary(clog_unadj)

summary(clog_adj.add)

#(b)

conju_un <- clogit(case ~ dose + strata(caseset), data = bresmack)

conju_adj <- clogit(case ~ dose + gbdx + hyper + obese + nonest + strata(caseset), data = bresmack)

summary(conju_un)

summary(conju_adj)

#(c)
bresmack <- bresmack %>%
  mutate(dose = relevel(dose, ref = "low"))

conju_low.un <- clogit(case ~ dose + strata(caseset), data = bresmack)

conju_low.adj <- clogit(case ~ dose + gbdx + hyper + obese + nonest + strata(caseset), data = bresmack)

summary(conju_low.un)

summary(conju_low.adj)

#(d)

bresmack <- bresmack %>%
  mutate(dose = relevel(dose, ref = "none"))

conju_dur.un <- clogit(case ~ conjest:dur + strata(caseset), data = bresmack)

conju_dur.adj <- clogit(case ~ conjest:dur + gbdx + hyper + obese + nonest + strata(caseset), data = bresmack)

summary(conju_dur.un)

summary(conju_dur.adj)


