# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 1/20/2021
# Week 3 class materials (Hypothesis Testing)

library(tidyverse)
library(jtools)
library(car)
library(vtable)

data(txhousing)

vtable(txhousing)

m <- lm(listings ~ inventory, data = txhousing)

export_summs(m, digits = 3, error_format = '{statistic}, p = {p.value}')

beta1_hat <- coef(m)[[2]]

se_beta1 <- summary(m)$coefficients[2,2]

# z score is estimate
z <- (beta1_hat - 0) / se_beta1
z_100 <- (beta1_hat - 100) / se_beta1

# Only works for negative Z!
pnorm(z)
2 * pnorm(z)

# Positive Z
1 - pnorm(z)
2 * (1 - pnorm(z))

# critical value = 1.96
1 - pnorm(1.96)

beta1_hat - 1.96 * se_beta1
beta1_hat + 1.96 * se_beta1

# linear hypothesis function
# f-test
linearHypothesis(m, 'inventory = 0')
linearHypothesis(m, 'inventory = 100')
linearHypothesis(m, 'inventory = 100', white.adjust = TRUE)

# exercises in weekly slides: 
data(SLID, package = 'carData')
vtable(SLID)
model <- lm(wages ~ education, data = SLID)
summary(model)
export_summs(model, digits = 3, error_format = '(t = {statistic}, p = {p.value})')
plot_coefs(model)

# practice following steps above
beta1_hat_AN <- coef(model)[[2]]
se_beta1_hat_AN <- summary(model)$coefficients[2,2]
z_AN <- beta1_hat_AN - 0 / se_beta1_hat_AN
z_100_AN <- beta1_hat_AN - 100 / se_beta1_hat_AN

# end of lessons.
