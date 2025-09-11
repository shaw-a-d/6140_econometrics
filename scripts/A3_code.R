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
df <- df[c("StudentLoginID", "Q2", "Q34", "Q37")]

## Cleaned to 18907 students
df_s <- df |>
  mutate(days = if_else((Q34 == "02" & Q37 == ".S"), 0,
  as.numeric(Q37))) |> # nolint
  filter(Q34 == "01" | Q34 == "02") |>
  filter(!(Q34 == "02" & days == 6)) |>
  filter(days <= 30)


# df_clean <- df |> # nolint
#   filter(Q34 %in% c("01", "02")) |> # nolint
#   filter(Q2 %in% c("01", "02")) # nolint
View(df_clean)

df_group <- # nolint df_clean |>
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

languageserver::languageserver_version()
