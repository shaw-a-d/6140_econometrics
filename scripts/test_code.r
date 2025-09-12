
rm(list = ls())


load("data/nyts2019.rdata")
attach(nyts2019)
library(dplyr)
library(tidyverse)
library(janitor)

df <- clean_names(nyts2019)
View(df)


df_clean <- df |>
  mutate(
    days_used = q37,
    days_used = case_when(
      days_used == ".S" & q34 == "02" ~ 0,
      TRUE ~ as.numeric(days_used)
    )
  ) |>
  filter(between(days_used, 0, 30) & q34 %in% c("01", "02")) |>
  filter(!(q34 == "02" & days_used > 0), (q4a != ".N")) 

dplyr::count(df_clean, q34, days_used, q4a)
nrow(df_clean)

df_hispanic <- df_clean |>
  mutate(hispanic = case_when(
    q4b == "1" | q4c == "1" | q4d == "1" | q4e == "1" ~ 1, TRUE ~ 0
  ))


df_hispanic <- df_hispanic |>
  mutate(across(q4a:q4e, ~na_if(.x, "NA"))) |>
  mutate(hispanic2 = coalesce(q4b, q4c, q4d, q4e))

dplyr::count(df_hispanic, hispanic, hispanic2, q4a)
unique(df_hispanic$q4e)