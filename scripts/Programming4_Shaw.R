# Programming 4
# 9-23-25
library(dplyr)
library(glue)
library(ggplot2)
install.packages("kableExtra")
library(kableExtra)
# simulate data
set.seed(1935)
n = 10000
x2 = runif(n, min = 0, max = 10)
x1 = rnorm(n, mean = 8, 3) + 0.3 * x2
e = rnorm(n, mean = 0, sd = 1)
y = 0.1 + 0.08*x2 + 0.02*x1 + e

# Problem 1: Using the formula for single variate models (covariances and variances) estimate a model y that's based only on x1. # nolint

y_bar <- mean(y)
x1_bar <- mean(x1)
cov_1 <- cov(x1, y)
var_1 <- var(x1)
beta1_est <- cov_1 / var_1
beta1_est

# The covariance and variance of x_1 and y result in a beta coefficient upwardly biased given our true beta from the given model is 0.02 #nolint


# 2 #Problem 2: Now estimate the covariance between x1 and x2 and the variance of x1. #nolint

cov_2 <- cov(x1, x2)
beta2_est <- cov_2 / var_1
beta2_est


## Problem 3: Based on what you know from the way the data was created what is your best guess for omitted variable bias? #nolint
OVB <- 0.08* beta2_est
OVB
### this shows that univariate overestimates the effect of x1 on y

## Problem 4: imagine x1 is a treatment with some drug. Describe why randomizing x1 will give you the correct estimate of the effect of x1 on y? #nolint

# Randomizing will break the correlation between x1 and x2.


## Problem 5: run 10 regressions of x1 on y (using the lm(y~x1) function) within groups of x2. I.E. run a set of regressions of x1 on y where x2 equals 0 to 0.1, 0.1 to 0.2, 0.2 to 0.3, etc (don't include the same observations in any regressions. Take the weighted average of the estimates from each of these regressions where you weight by the number of observations in each of these regressions multiplied by the coefficients. What percent of the bias does this estimate have in comparison to your estimate from problem 1? # nolint

df <- data.frame(y, x1, x2)
betas <- c()
obs <- c()

for (i in 0:9) {
  model <- lm(y ~ x1, data = df, subset = (x2 >= i & x2 < (i + 1)))
  betas <- c(betas, model$coefficients[2])
  obs <- c(obs, length(model$residuals))
}


wtd_avg <- sum(obs * betas) / sum(obs)

bias_binned <- wtd_avg - 0.02
bias_naive <- beta1_est - 0.02


abs(bias_binned / bias_naive) * 100







# Using data from the birth records in 2017
rm(list = ls())
library(dplyr)
load("/Users/andrewshaw/Documents/GitHub/6140_econometrics/data/samplebirth2017.RData")
df <- samp


vars <- c("dbwt", "mager", "priorlive", "cig_1", "bmi", "previs", "oegest_comb")
df_small <- df[vars]

df_small
df_clean <- df_small |>
  filter(
    priorlive <= 14, cig_1 <= 98, dbwt < 9999,
    bmi != 99.9, previs != 99, oegest_comb != 99
  )




# Problem 6 Create a multivariate model that best predicts the effects of various characteristics on birth weight which is variable dbwt. Be sure to get rid of the missing variables (see the codebook on canvas).  You must use at least five variables (a set of dummy variables counts as one). Show your results in a table in RMarkdown. Describe the results. # nolint

model_natality <- lm(dbwt ~ mager + priorlive + cig_1 + bmi + previs + oegest_comb, data = df_clean)
model_natality



# Problem 7 Create a scatter-plot of your residuals on the y-axis against at least one continuous independent variable on the x-axis. Include this in your RMarkdown file along with a description of the results. # nolint
length(residuals(model_natality))
df_clean$residuals <- residuals(model_natality)
scatter <- ggplot(data = df_clean, aes(x = oegest_comb, y = residuals, color = dbwt)) +
  geom_point()
scatter
