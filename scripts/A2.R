# Andrew Shaw
# 9-2-2025
# A2 script

library(survey)
library(tidyverse)

rm(list = ls())

load(file ="data/nyts2019.rdata")
attach(nyts2019)



df <- nyts2019[c("psu", "StudentLoginID", "Q34", "Q37")]



##Q.34 ever smoked e-cigarette 
##  01 YES, 02 = NO .N = NotAnswered .Z = NotDisplayed
##Q.37 Last 30 days 
##  .S = Legitimate Skip

sum(df$Q34 == ".N" & df$Q37 == ".N")
# 30 students didn't answer either question
# DROP

sum(df$Q34 == "02" & df$Q37 == ".S")
# 12560

sum(df$Q37 == ".Z" & df$Q37 == ".Z")
## Same 12 observations 'Not Displayed' for both questions



#There is only 1 observation where the person did not answer but has smoked  
## an e-cig and that is StudentLoginID number 16582. No values are NA


filter(df, Q34 == "01" & Q37 == ".N")
sum(df$Q34 == "01" & df$Q37 == ".N")
# 59 students have said they have smoked an e-cig but have not answered 
# the past 30 days question. DROP 
str(df)




# 1 student put down he never has smoked an e-cig but then states 6 days 
## in past 30 days he has PSU no. 086869


# Conditions - if obs 34 == "03 & 37 == ".S" then dummy == 0,
# if obs 34 = 01 & 37 = ".N", drop
# drop 34 = 01 and 37 > "0" 
# if 34 = 01 and 37 >= 1 then dummy == 1


df_clean <- df |>
  filter(!(Q34 == "02" & Q37 == "06")) |>
  filter(!(Q34 == "01" & Q37 == ".N")) |>
  filter(!(Q34 == ".Z" & Q37 == ".Z")) |>
  filter(!Q37 > "30") |>
  filter(!(Q34 == ".N"))


df_dummy <- df_clean |>
  mutate(dummy = ifelse(Q34 == "01" & Q37 > "00", 1, 0))

View(df_dummy)

table(df_dummy$dummy)

mean(df_dummy$dummy)

se <- sd(df_dummy$dummy)/sqrt(length(df_dummy$dummy))

mean(df_dummy$dummy) - 1.96*se


result <- t.test(df_dummy$dummy, conf.level = 0.95)

print(result)

mean(df_dummy$dummy)
n <- length(df_dummy$dummy)
x <- sum(df_dummy$dummy)
?binom.test()

binom.test(x, n,  p = 0.15, alternative = "two.sided",
           conf.level = 0.95)

se <- sqrt(((x/n)*(1-(x/n)))/n)

1.96*se + (x/n)

prop.test(x, n, p=0.15, alternative = "two.sided",
          conf.level = 0.95)
