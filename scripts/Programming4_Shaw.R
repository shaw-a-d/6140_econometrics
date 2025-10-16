# Programming 4
# 9-23-25
library(dplyr)
library(ggplot2)
# simulate data
set.seed(1935)
n = 10000
x2 = runif(n, min = 0, max = 10)
x1 = rnorm(n, mean = 8, 3) + 0.3 * x2
e = rnorm(n, mean = 0, sd = 1)
y = 0.1 + 0.08*x2 + 0.02*x1 + e

cov_xy <- cov(x1, y)
var_x1 <- var(x1)


df <- data.frame(x1, x2, y)

str(df)

df[1:10,]

cov_xy <- cov(x1,y)
var_x1 <- var(x1)
cov(x1, y)/var(x1)
cov_xy / var_x1

model_x1 <- lm(y ~ x1)
summary(model_x1)

cov_xy/var_x1


# Problem 1: Using the formula for single variate models (covariances and variances) estimate a model y that's based only on x1. # nolint


#Using univariate even though the true model features another variable upwardly biases the coeff. beta_x1 because it's incorporating some of the variation from thevariable x_2
beta_x1 <- cov_xy/var_x1
y_bar <- mean(y)

alpha <- y_bar - beta_x1*mean(x1)
alpha

cat("y=", alpha, "+", beta_x1, "x1")
model_x1


# 2 #Problem 2: Now estimate the covariance between x1 and x2 and the variance of x1.

cov_12 <- cov(x1, x2)
var_x1 <- var(x1)

beta_12 <- cov_12/var_x1

OVB <- 0.08*beta_12
OVB
model_2 <- lm(y~x1 + x2)
model_2
beta_x1 - OVB
cov(x2, y) / var(x2)

0.02 + 0.08*OVB
