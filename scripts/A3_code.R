# assignment 3 code
# 9-8-25

library(survey)
library(dplyr)
library(tidyverse)
library(styler)
library(gtsummary)
library(arsenal)


load("data/nyts2019.rdata")
attach(nyts2019)


df <- nyts2019


# Create sexXsmoker populations
# Is there a difference in the proportion of male smokers and female smokers?

# 01 = Male, 02 = Female, e-cig 01 = Yes, 02 = No
df <- df[c("StudentLoginID", "Q2", "Q34")]

View(df)

dplyr::count(df, Q34)

df_clean <- df |>
  filter(Q34 %in% c("01", "02")) |>
  filter(Q2 %in% c("01", "02"))

View(df_clean)

df_group <- df_clean |>
  mutate(Q34 = ifelse(Q34 == "02", 0, 1)) |>
  mutate(Q34 = as.numeric(Q34), Q2 = as.numeric(Q2)) |>
  rename(sex = Q2, smoke = Q34)

View(df_group)

results <- t.test(smoke ~ sex,
  data = df_group, mu = 0,
  alternative = "two.sided"
)
View(df_group)
results



