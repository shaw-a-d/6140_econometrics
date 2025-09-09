#assignment 3 code
# 9-8-25

library(survey)
library(dplyr)
library(tidyverse)

load('data/nyts2019.rdata')
attach(nyts2019)


df <- nyts2019


# Create sexXsmoker populations

# 01 = Male, 02 = Female, e-cig 01 = Yes, 02 = No 
df_sex <- df[c("StudentLoginID", "Q2", "Q34")]

dplyr::count(df_sex, Q2, Q34)

df_sex <- df_sex %>%
    filter(!(Q2 == ".N")) |>
    filter(Q34 == "01")


# Create dummy where maleXsmoker = 1, femaleXsmoker = 0
df_dummy_sex <- df_sex |>
    mutate(dummy = ifelse(Q2 == "01", 1, 0))

View(df_dummy_sex)

p <- sum(df_dummy_sex$dummy)
n <- nrow(df_dummy_sex)

binom.test(p, n, p = .5, alternative = "two.sided", conf.level = 0.95)
