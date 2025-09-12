
rm(list = ls())


load("data/nyts2019.rdata")
attach(nyts2019)
library(dplyr)
library(tidyverse)
library(janitor)

df <- clean_names(nyts2019)
View(df)


df_clean <- df |>
  mutate(days_used = q37,
  days_used = case_when(
    days_used == ".S" & q34 == "02" ~ 0,
    TRUE ~ as.numeric(days_used)
  )) |>
  filter(between(days_used, 0, 30) & q34 %in% c("01", "02")) |>
  filter(!(q34 == "02" & days_used > 0))

dplyr::count(df_clean, q34, days_used)
nrow(df_clean)

# df_hispanic <- df_clean |>
#  filter(q4a != ".N") |>
#  mutate(hispanic = ifelse(q4a == "1", "Other", "Hispanic"))


# df_hispanic <- df_clean |>
# filter(q4a != ".N") |>
# mutate(hispanic = as.numeric(if_any(c(q4b, q4c, q4d, q4e), ~ .x == 1)))
#  mutate(hispanic = coalesce(q4b, q4c, q4d, q4e))


df_test <- df_clean |>
  mutate(
    count_ones = rowSums(across(c(q4b:q4e)) == 1, na.rm = TRUE),
    count_nas = rowSums(is.na(across(c(q4b:q4e)))),
    if_any_result = if_any(c(q4b:q4e), ~ .x == 1),
    coalesce_result = coalesce(q4b, q4c, q4d, q4e)
  ) |>
  select(q4b:q4e, count_ones, count_nas, if_any_result, coalesce_result)
dplyr::count(df_hispanic, hispanic, days_used)

View(df_test)
df_test$if_any_result

sum(df_test$count_ones)
sum(df_test$coalesce_result)

df_test |>
  filter(coalesce_result != "1")


sum(df_test$coalesce_result == 1, na.rm = TRUE)
