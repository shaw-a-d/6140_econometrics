#clean code
library(dplyr)
library(survey)
library(tidyverse)
library(janitor)

rm(list = ls())

load(file = "data/nyts2019.rdata")
attach(nyts2019)

df <- nyts2019[c("StudentLoginID", "Q34", "Q37")]


## check for values and combinations
dplyr::count(df, Q34, Q37)

## drop combinations of .N and .Z
# 30(N,.N), 3(.N, 00), 1(.N,02), 12 (.Z,.Z)
## Total

df_new <- df |>
  mutate(days = if_else((Q34 == "02" & Q37 == ".S"), 0, as.numeric(Q37))) |>
  filter(Q34 == "01" | Q34 == "02")




# Check for any remaining invalid combinations before applying dummy
dplyr::count(df_new, Q34, days, Q37)
nrow(df_new)


## drop remaining invalid combinations
# 1(02,06), 2(02,.N), 1(01,99), 1(01,55), 1(01,.S), 59((01,.N))

df_dummy <- df_new |>
  filter(Q34 %in% c("01", "02") & days %in% 0:30) |>
  filter(!(Q34 == "02" & Q37 == "06")) |>
  mutate(
    dummy = if_else(days %in% 1:30, 1, 0)
  )
dplyr::count(df_dummy, Q34, dummy)



# Binomial Test at 5% significance hypothesized mean = 20%

x <- sum(df_dummy$dummy)
n <- nrow(df_dummy)

results <- binom.test(x, n, p = 0.20, alternative = "two.sided",
                      conf.level = 0.95)

prop.test(x, n, p = 0.20, alternative = "two.sided",
          conf.level = 0.95)
