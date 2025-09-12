# assignment 3 code
# 9-8-25

library(survey)
library(dplyr)
library(tidyverse)
library(styler)
library(gtsummary)
library(arsenal)
library(janitor)
rm(list = ls())

load("data/nyts2019.rdata")
attach(nyts2019)


df <- clean_names(nyts2019)



# Is there a difference in the proportion of male smokers and female smokers?

## Cleaned to 18907 students
df_filter <- df |>
  mutate(days_used = if_else((q34 == "02" & q37 == ".S"), 0, as.numeric(q37))) |>
  filter(q34 == "01" | q34 == "02") |>
  filter(!(q34 == "02" & days_used == 6)) |>
  filter(days_used <= 30) |>
  filter(q2 %in% c("01", "02"))

dplyr::count(df_filter, q34, q37, days_used)
nrow(df_filter)
# 01 = Male, 02 = Female, e-cig 01 = Yes, 02 = No
df_clean <- df_filter |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  mutate(Q2 = ifelse(Q2 == "01", "Male", "Female")) |>
  rename(sex = Q2) |>
  select(sex, current_user, days_used)

#Check freq table
View(df_clean)
dplyr::count(df_clean, sex, current_user, days_used)
nrow(df_clean)


results_sex <- t.test(ecig_use ~ sex, data = df_clean, mu = 0,
                      alternative = "two.sided")

#Double Check pop proportion means
sum(df_clean$sex == "Female" & df_clean$current_user == 01)
sum(df_clean$sex == "Female")

results_sex

dplyr::count(df_filter, days_used)

#Grade - clean table
df_grade <- df_filter |>
  filter(Q3 %in% c("07", "04")) |>
  rename(grade = Q3) |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  select(grade, ecig_use) |>
  mutate(grade = ifelse(grade == "07", "12th", "9th"))

# Test on grade
results_grade <- t.test(ecig_use ~ grade, data = df_grade)


dplyr::count(df_filter, q4a, q4b)
df_histest <- df_filter |>
  mutate(hispanic = case_when(
    q4a == "1" ~ "Other", q4a == "NA" ~ "Hispanic",
    TRUE ~ q4a
  ), ecig_use = case_when(days_used == 0 ~ 0, TRUE ~ 1)) |>
  filter(hispanic %in% c("Hispanic", "Other")) |>
  select(hispanic, ecig_use, days_used)

View(df_histest)
dplyr::count(df_histest, hispanic, ecig_use)

## Hispanic v. Other
df_hispanic <- df_filter |>
  filter(!(q4a == ".N")) |>
  mutate(hispanic = ifelse(q4a == "1", "Other", "Hispanic")) |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  select(hispanic, ecig_use)

View(df_hispanic)
dplyr::count(df_hispanic, hispanic, ecig_use)

results_hispanic <- t.test(ecig_use ~ hispanic,
  data = df_hispanic, alternative = "two.sided", mu = 0
)

results_hispanic


# Black/White Q5D, Q5E
df_black <- df_filter |>
  filter(!(Q5A == ".N")) |>
  mutate(black = ifelse(Q5C == "1", "Black", "Other")) |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  select(black, days_used, ecig_use)

dplyr::count(df_race, race, ecig_use)

results_black <- t.test(ecig_use ~ race,
  data = df_race,
  alternative = "two.sided", mu = 0
)


# White v. Other
df_white <- df_filter |>
  filter(!(Q5A == ".N")) |>
  mutate(white = ifelse(Q5E == "1", "White", "Other")) |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  select(white, ecig_use)

dplyr::count(df_white, white, ecig_use)

results_white <- t.test(ecig_use ~ white,
  data = df_white, alternative = "two.sided", mu = 0
)

results_white

#Smoker v. Non-smoker

dplyr::count(df_smoker, Q6, Q9)
df_smoker <- df_filter |>
  filter(!(Q6 == ".N" | Q9 == ".N")) |>
  filter(!(Q6 == "01" & Q9 == ".S")) |>
  mutate(Q9 = ifelse(Q9 == ".S", 0, as.numeric(Q9))) |>
  mutate(smoker = ifelse(Q9 > 0, "Smoker", "Non-Smoker")) |>
  mutate(ecig_use = ifelse(days_used > 0, 1, 0)) |>
  select(smoker, ecig_use, days_used, Q9)

dplyr::count(df_smoker, ecig_use, Q9)

results_smoker <- t.test(ecig_use ~ smoker,
  data = df_smoker,
  alternative = "two.sided", mu = 0
)

results_smoker

?coalesce()
