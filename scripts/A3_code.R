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



# Is there a difference in the proportion of male smokers and female smokers?

# 01 = Male, 02 = Female, e-cig 01 = Yes, 02 = No
df <- df[c("StudentLoginID", "Q2", "Q34", "Q37")]

## Cleaned to 18907 students
df_s <- df |>
  mutate(days = if_else((Q34 == "02" & Q37 == ".S"), 0,
  as.numeric(Q37))) |> # nolint
  filter(Q34 == "01" | Q34 == "02") |>
  filter(!(Q34 == "02" & days == 6)) |>
  filter(days <= 30) |>
  filter(Q2 %in% c("01", "02"))

dplyr::count(df_s, Q34, Q37)

df_clean <- df_s |>
  mutate(current_user = ifelse(days > 0, 1, 0)) |>
  mutate(Q2 = ifelse(Q2 == "01", "Male", "Female")) |>
  rename(sex = Q2) |>
  select(sex, current_user, days)

#Check freq table
View(df_clean)
dplyr::count(df_clean, sex, current_user, days)
nrow(df_clean)


results_sex <- t.test(current_user ~ sex, data = df_clean,
  mu = 0, alternative = "two.sided")

results_sex

#Double Check pop proportion means
sum(df_clean$sex == "Female" & df_clean$current_user == 01)
sum(df_clean$sex == "Female")

results_sex
