# assignment 3 code
# 9-8-25

library(survey)
library(dplyr)
library(tidyverse)
library(styler)

rm(list = ls())


load("data/nyts2019.rdata")
attach(nyts2019)


df <- nyts2019


# Create sexXsmoker populations
# Is there a difference in the proportion of male smokers and female smokers?

# 01 = Male, 02 = Female, e-cig 01 = Yes, 02 = No
df <- df[c("StudentLoginID", "Q2", "Q34")]
