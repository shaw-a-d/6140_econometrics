# Andrew Shaw
# final clean code for A3
# 9.12

rm(list = ls())

library(dplyr)
library(tidyverse)
library(janitor)
library(survey)

load("data/nyts2019.rdata")
attach(nyts2019)


df <- clean_names(nyts2019)

# Data cleaned to 18907 respondents with valid responses wtih
# dummy variable for current affirmative e-cig use.

df_ecigusers <- df |>
  mutate(
    days_used = q37,
    days_used = case_when(
      days_used == ".S" & q34 == "02" ~ 0,
      TRUE ~ as.numeric(days_used)
    )
  ) |>
  filter(
    between(days_used, 0, 30) & q34 %in% c("01", "02"),
    (!(q34 == "02" & days_used > 0))
  ) |>
  mutate(current_user = case_when(days_used > 0 ~ 1, TRUE ~ 0))

nrow(df_ecigusers)
dplyr::count(df_sex, current_user, q2)


# Populations Male and Female (from q2)
# 108 non-responsive

df_sex <- df_ecigusers |>
  filter(q2 != ".N") |>
  mutate(sex = case_when(q2 == "01" ~ "Male", q2 == "02" ~ "Female")) |>
  select(sex, current_user)

results_sex <- t.test(current_user ~ sex, data = df_sex)


# Populations 12th and 9th Grade (from q3)

df_grade <- df_ecigusers |>
  filter(q3 %in% c("04", "07")) |>
  mutate(grade = case_when(q3 == "04" ~ "9th", q3 == "07" ~ "12th")) |>
  select(grade, current_user)

results_grade <- t.test(current_user ~ grade, data = df_grade)

# Populations Hispanic and Other (from q4a:q4e)

df_hispanic <- df_ecigusers |>
  filter(!(if_all(q4a:q4e, ~ .x == ".N"))) |>
  mutate(across(q4a:q4e, ~ na_if(.x, "NA")), hispanic = case_when(
    coalesce(q4b, q4c, q4d, q4e) == "1" ~ "Hispanic", TRUE ~ "Non-Hispanic"
  )) |>
  select(current_user, hispanic)

results_hispanic <- t.test(current_user ~ hispanic, data = df_hispanic)

# Populations Black and Non-black (from q5a:q5e)

df_black <- df_ecigusers |>
  filter(!(if_all(q5a:q5e, ~ .x == ".N"))) |>
  mutate(black = case_when(q5c == "1" ~ "Black", TRUE ~ "Non-Black")) |>
  select(current_user, black)

results_black <- t.test(current_user ~ black, data = df_black)

# Populations White and Non-White (from q5a:q5e)

df_white <- df_ecigusers |>
  filter(!(if_all(q5a:q5e, ~ .x == ".N"))) |>
  mutate(white = case_when(q5e == "1" ~ "White", TRUE ~ "Non-White")) |>
  select(current_user, white)

results_white <- t.test(current_user ~ white, data = df_white)

# Populations Smokers and Non-Smokers (from q9)


class(df_ecigusers$q9)

df_smoker <- df_ecigusers |>
  filter(!(q9 == ".N")) |>
  mutate(
    smoker = case_when(q9 == ".S" ~ "00", TRUE ~ q9),
    smoker = as.numeric(smoker),
    smoker = case_when(smoker == 0 ~ "Non-Smoker", .default = "Smoker")
  ) |>
  select(smoker, current_user)

results_smoker <- t.test(current_user ~ smoker, data = df_smoker)
