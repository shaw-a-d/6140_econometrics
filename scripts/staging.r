
rm(list = ls())


load("data/nyts2019.rdata")
attach(nyts2019)
library(dplyr)
library(tidyverse)
library(janitor)

df <- clean_names(nyts2019)
View(df)


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
dplyr::count(df_ecigusers, current_user)

# mutate across to change "NA" to true NA
df_hispanic <- df_ecigusers |>
  filter(!(if_all(q4a:q4e, ~ .x == ".N"))) |>
  mutate(across(q4a:q4e, ~ na_if(.x, "NA")),
    hispanic = case_when(
      coalesce(q4b, q4c, q4d, q4e) == "1" ~ "Hispanic",
      TRUE ~ "Other"
    )
  )

results_hispanic <- t.test(current_user ~ hispanic, data = df_hispanic)