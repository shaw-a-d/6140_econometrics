# Programming 4
# 9-23-25
library(dplyr)
# simulate data
set.seed(1935)
n = 10000
x2 = runif(n, min = 0, max = 10)
x1 = rnorm(n, mean = 8, 3) + 0.3 * x2
e = rnorm(n, mean = 0, sd = 1)
y = 0.1 + 0.08*x2 + 0.02*x1 + e

cov_xy <- cov(x1, y)
var_x1 <- var(x1)

b_1 <- cov_xy/var_x1

model_single <- lm(y ~ x1)
summary(model_single)


