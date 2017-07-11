##MODULE 2 EXERCISE 2##
##From class 3/1/17
#going back to titer: VHS ~ gamma(shape=,shape'(comes from data), scale=)
#write the model first
#power law: write the equation: y = a*x^b this is expected value where the mean and params
#shape*scale = expected value
#scale = a*time^b/shape
#need to estimate shape because the deterministic is giving me expectation of mean 
#Beta distribution, mean = a/a+b
#y~beta(a=a, B=function(mean, a)) mean = a/a+b, isolate B: B= a/a+mean
#need to estimate an extra parameter to describe the variance
VhsNLL <- function(p,time,titer){#call to these args in optim){
  a=p[1]
  b=p[2]
  shape=p[3]
  expected = a*time^b #can't be named titer because that is arg in function
  NLL = -sum(dgamma(x=titer,shape=shape, scale=expected/shape, log=T)) #observation is first arg x))
  #titer in NLL matches up with titer in function arg, time matches up with time
             return(NLL)
}


#1.  avg proportion of time a white tailed deer spends foraging, 100 observations

#1a. plot histogram

deer <- read.table("propForaging.txt", header=F)
p=deer$V1
propforage <- deer$V1
plot(deer$V1)
hist(deer$V1, breaks = 7, xlab="Proportion of time spent foraging", main="")
##cannot decipher deterministic model, we only had one variable, NO DETERMINISTIC COMPONENT because no
#independent var
#propforaging ~ beta(alpha=alpha', beta=beta') # ' estimating parameter from data

ggplot
qplot(deer$V1, geom="histogram", xlab="Proportion forage time") 
ggplot(data=deer, stat_bin=0.1, aes(deer$V1)) + geom_histogram()
##good to plot just to see
alpha=2
beta=6
x1=seq(0,1,0.01)
plot(x1, dbeta(x1,shape1 = alpha,shape2=beta))
#putting a line through histogram is probability density fcn

#1b. Beta estimate params

DeerNLL<-function(p,x){
  a=p[1]
  b=p[2]
   NLL = -sum(dbeta(x=x,shape1=a,shape2=b,log=T))
 return(NLL)
}

##try a batch of 5 different guesses that covers a range and pick the one that gives the lowest value
guess = c(2,2)
deerfit <- optim(par=guess, fn=DeerNLL, x=deer$V1) # x=data!!!

#can calculate population sample mean by this
deerfit$par[1]/(deerfit$par[1]+deerfit$par[2]) ##from beta mean calculation wiki
mean(deer$V1)

#calculate confidence intervals (CI)
#graph alpha on x axis versus NLL on y axis, should look like upside down parabola
#NLL tend to be chi square distributed > can use to derive 95% CI
#95% CI is size of quantile from chi sq cum distr. 1.92 is constant

#likelihood profile, you hold one parameter fixed and let the other move around to fit the data 
#given the fixed parameter
#make vector of a's (as vector) to go into a forloop to consider each value of a that we are forcing to be
#fixed so only beta is turning and best estimate of beta determined to make U-shaped line
#alpha and beta are not independent of each other because they both influence the mean

#QUESTION 2. estimate params of model describing counts of oak trees based on mean annual soil

oak <- read.table("oakDensities.txt",header=T)
oak
moisture <- oak$soilMoisture
density <- oak$oakDensity
plot(moisture, density) ##parabolic

#od ~ poisson(lambda = a*(moisture)^2 + b*moisture +c)

oakfit <- function(p,oak,sm){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  expected = B0*sm^2 + B1*sm + B2
  NLL = -sum(dpois(oak,expected,log=T))
  return(NLL)
}
guess = c(2,2,-2)
oakfit = optim(par=guess, fn=oakfit, oak=oak$oakDensity, sm=oak$soilMoisture)