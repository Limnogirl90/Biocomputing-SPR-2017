##Module 2 Exercise 1##
#1
# A) create data with 10 obs, 0-10
x = runif(10, min=0, max=10) #uniform distribution, continuous, every number has the same probability of being
#chosen. Probability density function unlike sample function, which draws from a pool
B0 = -4
B1 = 2.5

y=B0 + B1*x+rnorm(length(x),0,2) ##should use sqrt2 for SD
plot(y~x)
y.fit = lm(y~x)
summary(y.fit) ##variable holding linear model object; lm is still in frequentist realm
resid(y.fit)
plot(resid(y.fit))
plot(y.fit) #to see expected values for model estimated from simulated data
#residuals versus fitted, looks good. Want median of residuals to be close to zero
plot(x,y)

## B) write out model I simulated above##
#see exercise 1 handout that I wrote on

## C) simulate 100 data points
x1 = runif(100, min=0, max=10)
B0 = -4
B1 = 2.5

y=B0 + B1*x1+rnorm(length(x1),0,2)
plot(y~x1)
y.fit = lm(y~x1)
summary(y.fit)
#parameter estimates for B0 became 8.7 and for B1 became 0.1429. the value for B0 is much higher than the observed 
#value, and the variance decreased, which makes sense since we simulated 100 data points
#overall the st errors got smaller, which is good. But beware of false certainty because of 100 draws.
#false confidence

# D) create plots to display how point estimate and 95% CI change with N
##Lines 33 to 56 include the code I found on StackOverflow to make plots for N=10 with 95% CI
x = runif(10, min=0, max=10)
B0 = -4
B1 = 2.5
y=B0 + B1*x+rnorm(length(x),0,2)
df <- data.frame(x = x,
                 y = B0 + B1*x+rnorm(length(x),0,2))
plot(y ~ x, data = df)

# model
mod <- lm(y ~ x, data = df)

# predicts + interval
newx <- seq(min(df$x), max(df$x), length.out=10)
preds <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')

# plot
plot(y ~ x, data = df)
# model
abline(mod)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
##for this code above, generates a plot for 10 data points and the confidence interval is pretty tight

##plot of 95% CI for 100 points ##same code as above but for N=100
x1 = runif(100, min=0, max=10)
B0 = -4
B1 = 2.5

y=B0 + B1*x+rnorm(length(x1),0,2)
df <- data.frame(x1 = x1,
                 y = B0 + B1*x1+rnorm(length(x1),0,2))

plot(y ~ x1, data = df)

# model
mod <- lm(y ~ x1, data = df)

# predicts + interval
newx <- seq(min(df$x1), max(df$x1), length.out=100)
preds <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')

# plot
plot(y ~ x1, data = df)
error.bars(sd=T, alpha=.05)
# add fill
#polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
abline(mod)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
#these confidence intervals are really all over the place

##ANOTHER METHOD: FOR LOOP##


#2
library(MASS)
data(whiteside)
fitA=lm(whiteside$Gas~whiteside$Temp)
fitB=lm(whiteside$Gas~whiteside$Temp+whiteside$Insul)#interaction of temp and insulation
fitC=lm(whiteside$Gas~whiteside$Temp*whiteside$Insul)#cross of temp and insulation
summary(fitA) #estimate intercept 5.4862 whiteside$temp -.2902 res error .8606 r2 .457
summary(fitB) #estimate intercept 6.55 whiteside$temp -.33670 whiteside$insulafter -1.565 res error .3574
#r2 .9063
summary(fitC) # intercept 6.853 res error .323 r2 .9235

#I would choose the model that accounts for the most variance in the dependent variable. 
#I would also choose the model based on lowest residual error. In light of these standards, I think model C is
#best
#Both insulation and temperature have a significant negative effect on heating gas

#3
x = runif(10,0,10)
B0=-4
B1=2.5
sigma = 2
y = B0+B1*x+rnorm(length(x),0,sqrt(sigma)) ##first part is deterministic, rnorm part is stochastic part "noise"
#or residuals

#function for custom function for NLL
nll <- function(p,x,y){
  B0 =p[1]
  B1=p[2]
  sigma2 = exp(p[3]) #needs exponent to make it positive
  yhat = B0+B1*x
  nll = -sum(dnorm(y,mean=yhat,sd=sqrt(sigma2),log=T)) #dnorm is probability density function, and we use y because
  #our observations and yhat is expected values
  return(nll) #returns sum of vector of 10
}
#need initial values for optim to start working on
guess = c(B0=-2,B1=1,sigma2=0) #sd is harder to constrain in a linear model, more so than intercept

fit = optim(par=guess, fn=nll, x=x,y=y)
##value is the minimum value for the log likelihood
##counts, means it tried 138 combinations to minimize value returned by optim function. It turns B0, B1, sigma2
#to find best combination, person with knobs
##convergence = 0 is good, means it found a solution; convergence of 1 means it hit the limit of iterations 500
#default
#you can get same performance of model for different values of parameters



##Question 4##
biomass <- read.table("rainManipBiomass.txt", header =T, stringsAsFactors = F)
biomass
##create barplot of mean of each treatment level

treatment <- as.factor(biomass$V1)
plot(biomass$V2~treatment)

x1 = biomass$v2[which(biomass$V1=="wet")]
x1 =1
x2 = biomass$V2[which(biomass$v1=="drought")]
x2=2
cbind(x1,x2,biomass$V2)
means=tapply(biomass$V2,treatment, mean)
sds=tapply(biomass$V2,treatment, sd)
treat = c("ambient", "drought","wet")
data.biomass <- data.frame(means,sds)


qplot(treat,means)+geom_errorbar(aes(ymin=means-sds*2, ymax=means+sds*2), width=0.25)##worked!!
qplot(treat,means)+geom_errorbar(aes(ymin=means-sds*2, ymax=means+sds*2), width=0.25)
#4b)
#AGB ~ N(deterministic & stochastic)

  ##estimating differences between the means of treatments. effects of parameters are the difference
  #from the baseline intercept



