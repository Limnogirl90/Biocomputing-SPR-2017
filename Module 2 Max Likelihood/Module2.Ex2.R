##MODULE 2 EXERCISE 2##

#1.  avg proportion of time a white tailed deer spends foraging, 100 observations
#1a. plot histogram

deer <- read.table("propForaging.txt", header=F)
p=deer$V1
propforage <- deer$V1
plot(deer$V1)
hist(deer$V1, breaks = 7, xlab="Proportion of time spent foraging", main="")
lines(density(deer$V1),na.rm=TRUE)
order(deer$V1)

ggplot
qplot(deer$V1, geom="histogram", xlab="Proportion forage time") 
ggplot(data=deer, stat_bin=0.1, aes(deer$V1)) + geom_histogram()

#1b. Beta estimate params

DeerNLL<-function(params,p){
  a=params[1]
  b=params[2]
  shape1=params[3]
  shape2=params[4]
 meanforage <- a*p*exp(-b*p)
   NLL = -sum(dbeta(meanforage,shape1=shape1,shape2=shape2))
 return(NLL)
}

guess = c(1,1,5,5)
deerfit <- optim(par=guess, fn=DeerNLL, p=deer$V1)

#this worked but the model did not converge, think I got confused when setting up the shape parameters?

#1c. Plot likelihood profile for alpha parameter

plot(deer$V1,deerfit$par[1]*seq(0,100),lwd=4,lty=2)


#1d. create plot of likelihood landscape for alpha and Beta params


#QUESTION 2. estimate params of model describing counts of oak trees based on mean annual soil

oak <- read.table("oakDensities.txt",header=T)
oak
moisture <- oak$soilMoisture
density <- oak$oakDensity
plot(moisture, density) ##parabolic

#oaks ~ dnbinom(a*(moisture)^2+b*(moisture)+c)

oakfit <- function(params,density,N){
  a=params[1]
  b=params[2]
  c=params[3]
  oakdens = a*N^2 + b*N + c
  NLL = -sum(dnbinom(density,oakdens,N,log=T))
  return(NLL)
}
guess = c(2,2,140)
oakfit = optim(par=guess, fn=oakfit, N=moisture)