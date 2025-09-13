# Andrew Shaw
# final clean code for A3
# 9.12

rm(list = ls())

library(dplyr)
library(tidyverse)
library(janitor)
library(survey)
library(gtsummary)
library(gt)
library(broom)
library(survival)

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
  mutate(
    current_user = case_when(days_used > 0 ~ 1, TRUE ~ 0),
    as.numeric(current_user)
  )

# Populations Male and Female (from q2)
# 108 non-responsive

df_sex <- df_ecigusers |>
  filter(q2 != ".N") |>
  mutate(sex = case_when(q2 == "01" ~ "Male", q2 == "02" ~ "Female")) |>
  select(student_login_id, sex, current_user)

results_sex <- t.test(current_user ~ sex, data = df_sex)


# Populations 12th and 9th Grade (from q3)

df_grade <- df_ecigusers |>
  filter(q3 %in% c("04", "07")) |>
  mutate(grade = case_when(q3 == "04" ~ "9th", q3 == "07" ~ "12th")) |>
  select(student_login_id, grade, current_user)

results_grade <- t.test(current_user ~ grade, data = df_grade)

# Populations Hispanic and Other (from q4a:q4e)

df_hispanic <- df_ecigusers |>
  filter(!(if_all(q4a:q4e, ~ .x == ".N"))) |>
  mutate(across(q4a:q4e, ~ na_if(.x, "NA")), hispanic = case_when(
    coalesce(q4b, q4c, q4d, q4e) == "1" ~ "Hispanic", TRUE ~ "Non-Hispanic"
  )) |>
  select(student_login_id, current_user, hispanic)

results_hispanic <- t.test(current_user ~ hispanic, data = df_hispanic)

# Populations Black and Non-black (from q5a:q5e)

df_black <- df_ecigusers |>
  filter(!(if_all(q5a:q5e, ~ .x == ".N"))) |>
  mutate(black = case_when(q5c == "1" ~ "Black", TRUE ~ "Non-Black")) |>
  select(student_login_id, current_user, black)

results_black <- t.test(current_user ~ black, data = df_black)

# Populations White and Non-White (from q5a:q5e)

df_white <- df_ecigusers |>
  filter(!(if_all(q5a:q5e, ~ .x == ".N"))) |>
  mutate(white = case_when(q5e == "1" ~ "White", TRUE ~ "Non-White")) |>
  select(student_login_id, current_user, white)

results_white <- t.test(current_user ~ white, data = df_white)
results_white
# Populations Smokers and Non-Smokers (from q9)


df_smoker <- df_ecigusers |>
  filter(!(q9 == ".N")) |>
  mutate(
    smoker = case_when(q9 == ".S" ~ "00", TRUE ~ q9),
    smoker = as.numeric(smoker),
    smoker = case_when(smoker == 0 ~ "Non-Smoker", .default = "Smoker")
  ) |>
  select(student_login_id, smoker, current_user)

results_smoker <- t.test(current_user ~ smoker, data = df_smoker)


results_list <- list(
  Sex = results_sex,
  Grade = results_grade,
  Hispanic = results_hispanic,
  Black = results_black,
  White = results_white,
  Smoker = results_smoker
)

make_table <- function(result, group1, group2, title) {
  tibble(
    Group = c(group1, group2),
    "% E-cig users" = c(result$estimate[1] * 100, result$estimate[2] * 100)
  ) |>
    gt() |>
    tab_header(title = title) |>
    tab_source_note(paste(
      "p-value:", format(result$p.value, scientific = TRUE, digits = 3))) |>
    fmt_number(columns = "% E-cig users", decimals = 1)
}

table_sex <- make_table(results_list[[1]], "Female", "Male", "E-cigarette Use by Sex")
table_grade <- make_table(results_list[[2]], "9th", "12th", "E-cigarette Use by Grade")
table_hispanic <- make_table(results_list[[3]], "Hispanic", "Non-Hispanic", "E-cigarette Use by Hispanic & Non-Hispanic")
table_black <- make_table(results_list[[4]], "Black", "Non-Black", "E-cigarette Use by Black & Non-Black")
table_white <- make_table(results_list[[5]], "White", "Non-White", "E-cigarette Use by White & Non-White")
table_smoker <- make_table(results_list[[6]], "Smokers", "Non-Smokers", "E-cigarette Use by Smokers & Non-Smokers")

