library(truncnorm) 
rm(list = ls())

set.seed(4289)


# step 1
x <- c(rtruncnorm(500, a=-Inf, b=Inf, mean=2, sd=0.001),
        rtruncnorm(500, a=-Inf, b=Inf, mean=6, sd=3))

# step 2

nn <- 100 
e <- rnorm(nn,0,3)

# step 3

y <- 2*x+e

model <- lm(y~x)
summary(model)
coef(model)[2]
# step 4
# Summary
mean(x)
sd(x)
hist(x, breaks = 18, freq = FALSE, xlim=c(-1,10))

# Problem 1-What happens to the probability density function as the standard 
# deviation of either of the peaks changes in the step 1? 
# What happens to the SD. of X as the means change? 
# Play around with the means and standard deviations to see.


nn <- 500

#step 2-Set the number of simulations
runs<-100

#step 3-program a function that creates random variables and returns the mean
# resets betas to empty vector as global vector

bimodal.run <- function(){
  x <<- c(rtruncnorm(nn, a=-Inf, b=Inf, mean=2, sd=10),  rtruncnorm(nn, a=-Inf, b=Inf, mean=8, sd=10))
  e <- rnorm(nn,0,3)
  y <<- 2*x+e
  #capture beta coefficient per run
  model <- lm(y~x)
  betas <- coef(model)[2]
  mean.x<-mean(x)
  return(data.frame(mean = mean.x, betas = betas))

}


#step 4-run simulations from the step 3
sims <- do.call(rbind, replicate(runs,bimodal.run(), simplify = FALSE))
print(sims)

hist(sims$mean)



hist(x)

cor(x,y)
plot(x,y,main="Scatterplot Example",xlab="x", ylab="y",pch=19)

lm(y~x)
