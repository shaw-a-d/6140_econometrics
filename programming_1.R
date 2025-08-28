library(truncnorm) 


set.seed(4289)


# step 1
x <- c(rtruncnorm(500, a=-Inf, b=Inf, mean=2, sd=0.001),
        rtruncnorm(500, a=-Inf, b=Inf, mean=6, sd=3))

# step 2

nn <- 0 
e <- rnorm(nn,0,3)

# step 3

y <- 2*x+e

# step 4
# Summary
mean(x)
sd(x)
hist(x, breaks = 18, freq = FALSE, xlim=c(-1,10))

# Problem 1-What happens to the probability density function as the standard 
# deviation of either of the peaks changes in the step 1? 
# What happens to the SD. of X as the means change? 
# Play around with the means and standard deviations to see.


nn <- 1000

#step 2-Set the number of simulations
runs<-3

#step 3-program a function that creates random variables and returns the mean
bimodal.run <- function(){
  x <- c(rtruncnorm(nn, a=-Inf, b=Inf, mean=2, sd=10),  rtruncnorm(nn, a=-Inf, b=Inf, mean=8, sd=10))
  e <- rnorm(nn,0,3)
  y <- 2*x+e
  data.frame(x = x, y = y)
  return(mean(x))
}



#step 4-run simulations from the step 3
sims <- replicate(runs,bimodal.run()) 
hist(sims)

hist(data$x)

cor(data$x,data$y)
plot(data$x,data$y,main="Scatterplot Example",xlab="x", ylab="y",pch=19)

lm(data$y~data$x)


rtruncnorm