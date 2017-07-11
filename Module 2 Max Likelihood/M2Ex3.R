###M2 Exercise 3###

#Question 1 Logistic regression and bird watching###
#A) Use logistic regression to identify the southern
#boundary of bird's range. Define southern boundary
#as having less than 50% chance of sighting
#need to code the data into factors
birds = birds[which(birds$latitude1)] #birds$sighted < 50%

#import data
birds <- read.table("birdwatching.txt", header=T)
birds
birds=birds[order(birds[,1],decreasing=TRUE),]
birds
birds$latitude1<-cut(birds$latitude, c(49,48,47,46,45,44,43,42,41,40,39,38,37,36,35))
plot(birds$latitude,jitter(birds$sighted),lwd=2,cex=1.5,main="Birds")
#logistic regression
hist(birds$sighted)
mod0=glm(birds$sighted~birds$latitude,family=binomial)#family=binom is analogous to picking probability dist
summary(mod0) 

#plot line of best fit

ilogit <- function(x,beta){
  prob=exp(cbind(1,x)%*%beta)/(1+exp(cbind(1,x)%*%beta))
  return(prob)
}

plot(birds$latitude, birds$sighted, lwd=2,cex=1.5, xlab="Latitude", ylab= "Probability of sighting")
lines(seq(34,50,1.5),ilogit(seq(34,50,1.5),mod0$coefficients),lwd=2,col="red",lty=2)

sighting = numeric(length(birds$latitude))
  for(i in 1:length(sighting)){
    sighting[i] = (birds$latitude[i]*.18934)-7.70979
  }
plot(birds$latitude,sighting)
#stewart's answers
predlat = seq(35,49,0.01)
predlogitP = -7.70979+0.18934*predlat
predP = exp(predlogitP)/(1+exp(predlogitP))
lines(predlat,predP,lwd=2)
predlat[abs(predP-0.5)==min(abs(predP-0.5))] #find predicated latitude closest to 0.5, logic test, T or F

#import other bird data
hist.birds <- read.table("historical_birdwatching.txt", header=T)
hist.birds
hist.birds = hist.birds[order(hist.birds[,1],decreasing=T),]
hist.birds
hist.birds$latitude1 <- cut(hist.birds$latitude, c(48,47,46,45,44,43,42,41,40,39,38,37,36,35))
hist.birds
plot(hist.birds$latitude1,jitter(hist.birds$sighted,0.25),lwd=2,cex=1.5,main="Historical birds")
plot(hist.birds$latitude,hist.birds$sighted)
mod1 <- glm(hist.birds$sighted~hist.birds$latitude,family=binomial)
mod1

plot(hist.birds$latitude, hist.birds$sighted, lwd=2,cex=1.5, xlab="Latitude", ylab= "Probability of sighting")
lines(seq(34,50,1.5),ilogit(seq(34,50,1.5),mod1$coefficients),lwd=2,col="red",lty=2)

sighting.hist = numeric(length(hist.birds$latitude))
for(i in 1:length(sighting.hist)){
  sighting.hist[i] = (hist.birds$latitude[i]*.4251)-17.0890
}
plot(hist.birds$latitude,sighting.hist)


#QUESTION 2 right skewed distributions##
#A import data1.txt
#We only have observations

data1 <- read.table("data1.txt", header=F)
data1
plot(data1$V1)
hist(data1$V1, freq=FALSE, main="Histogram of Data1")
#gamma? 
#pdata1 ~ gamma(shape,scale)
scale=1
shape=5
x=seq(0,20,0.1)
lines(x,dgamma(x,shape=shape,scale=scale, log=F)*150, type='l',lwd=2,col='red')
#exp
rate=0.3
lines(x,dexp(x,rate=rate,log=F)*150, type='l',lwd=2,col='blue')

#lognormal
meanlog=0
sdlog=1
lines(x,dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=F)*45, lwd=2,type='l',col="black")
legend("topright", c("Lognormal","Gamma", "Exponential"), fill=c("blue","red","black"))

#MAX LIKELIHOOD FOR DISTRIBUTION
Gamma.nll<-function(p,x){
  shape=p[1]
  scale=p[2]
  
  -sum(dgamma(x=x,shape=shape,scale=scale,log=TRUE))
}

guess=c(5,1)
Gamma.fit=optim(guess,Gamma.nll,x=data1$V1)

lines(seq(0,1,0.01),dbeta(x=seq(0,1,0.01),pFfit$par[1],pFfit$par[2])*10,lwd=3)
pFfit$par[1]/(pFfit$par[1]+pFfit$par[2])

#EXP
Exp.nll<-function(p,x){
 rate=p[1]
  
  -sum(dexp(x=x,rate=rate,log=TRUE))
}

guess=c(0.3)
Exp.fit=optim(guess,Exp.nll,x=data1$V1)
Exp.fit

#LOGNORMAL
meanlog=0
sdlog=1
lines(x,dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=F)*45, lwd=2,type='l',col="black")
Logn.nll<-function(p,x){
  meanlog=p[1]
  sdlog=p[2]
  
  -sum(dlnorm(x=x,meanlog=meanlog,sdlog=sdlog,log=TRUE))
}

guess=c(0,1)
Log.fit=optim(guess,Logn.nll,x=data1$V1)
Log.fit

#AIC not really necessary b/c each dist has 2 params, could just use negLL as a guide
#closer AIC values mean it becomes more difficult to tell if one distribution fits the data better
#Gamma 
AIC.Gam= 2*Gamma.fit$value+2*length(Gamma.fit$par)
AIC.Gam #455.1513

AIC.exp = 2*Exp.fit$value+2*length(Exp.fit$par)
AIC.exp #469.6474

AIC.logn <- 2*Log.fit$value+2*length(Log.fit$par)
AIC.logn #460.3157
AIC.models <- c(AIC.exp,AIC.Gam,AIC.logn)
AIC.models
#histograms of these lines are not scaled, so not exactly probabilities
#Part B
#poisson, negative binomial, geometric
#Part C rgamma()

#Question 3 Plant litter decomp
#Consider multiple models to evaluate the impact of the inclusion of climatic variables abd distinct litter pools
#on ability to explain variation in litter decomp data
#neg exp decomp model
#load in data
litter <- read.csv("LIDETdata_Hobbs.csv", header=T, stringsAsFactors = F)
litter
plot(litter$Initial_percent_sugars,litter$M_t)
plot(litter$Initial_percent_cellulose,litter$M_t)
plot(litter$t, litter$M_t)
#Gamma because yeah continuous and positive
#Mass~ Gamma(e^-kt)
#k = ln(Mt)/-t
#expected = -(log(Mt)/t)
#expected/shape = scale
#b is k
#SIMPLE MODEL
Litter.nll <- function(p,x,y){
  k=p[1]
  shape =p[2]
  expected = exp(-k*y)
  NLL = -sum(dgamma(x=litter$M_t, shape=shape, scale=expected/shape,log=T))
}
guess = c(1,.25)
litter.fit <- optim(par=guess, fn=Litter.nll, x = litter$M_t, y = litter$t)

## MOST COMPLICATED##
Litter.nll <- function(p,x,y,M1,M2,M3,CDI){
  k1=p[1]
  k2=p[2]
  k3=p[3]
  shape =p[4]
  expected = (M1*exp(-k1*CDI*y)) + (M2*exp(-k2*CDI*y)) + (M3*exp(-k3*CDI*y))
  NLL = -sum(dgamma(x=litter$M_t, shape=shape, scale=expected/shape,log=T))
}
guess = c(0.5,0.5,0.5,1)
littercomplex.fit <- optim(par=guess, fn=Litter.nll, x = litter$M_t, y = litter$t, M1=litter$Initial_percent_sugars,M2=litter$Initial_percent_cellulose,M3=litter$Initial_percent_lignin, CDI=litter$CDI)

#MODEL comparison complex versus simple##

litterfit1 =litter.fit$value*2
litterfit2 = littercomplex.fit$value*2
diffD = litterfit1-litterfit2
pchisq(diffD,df=1,lower.tail=FALSE)
##we tried to do this method however didn't work, why?

#AIC
AIC.simple = 2*litter.fit$value+2*length(litter.fit$par)
AIC.complex = 2*littercomplex.fit$value+2*length(littercomplex.fit$par)
c(AIC.simple,AIC.complex)

##Simple model in class 03/29/17##
normalized = lapply(litter$M_t, FUN(i)=i/100)

simplest <- function(p,time,Mt,CDI){
  k=p[1]
  shape1=p[2]
  meanMt=exp(-k*time*CDI)
  NLL = -sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1),log=T))
}
plusclimatefit=optim(par=c(k=0.5,shape1=5),fn=simplest,time=litter$t,Mt=litter$M_t,CDI=litter$CDI)
#QUESTION 4 BIOFUEL
#import data
biofuel <- read.table("biofuel.txt",header=T)
biofuel
plot(biofuel$feed_concentration,biofuel$fuel_produced, xlab="Concentration", ylab="Fuel produced")
#model
# fuel_produced ~ B0 + B1*concentration + B2*treated_untreated
#distribution gamma
#deterministic monomolecular? y=a(1-e^-b*x)
unique(biofuel$feed_concentration)
bioNLL1 <- function(p,feed,fuel){
  m=p[1]
  b=p[2]
  shape=p[3]
  expected = m*feed+b
  NLL = -sum(dgamma(fuel,shape=shape, scale=expected/shape,log=T))
}
guess=c(1,1,1)
biofit <- optim(par=guess, fn=bioNLL, x=biofuel$feed_concentration,y=biofuel$fuel_produced,treat=biofuel$pretreated)

#model2 for treatment
bioNLL2 <- function(p,feed,fuel,treat){
  m=p[1]
  b=p[2]
  b1=p[3]
  shape=p[4]
  expected = m*feed+b+b1*treat
  NLL = -sum(dgamma(fuel,shape=shape, scale=expected/shape,log=T))
}
guess=c(1,2,2,1)
biofit <- optim(par=guess, fn=bioNLL2, feed=biofuel$feed_concentration,fuel=biofuel$fuel_produced,treat=biofuel$pretreated)
notreat = biofuel[which(biofuel$pretreated==0),]
treat = biofuel[which(biofuel$pretreated==1),]
modfuel <- glm(biofuel$fuel_produced~biofuel$feed_concentration+biofuel$pretreated)
modfuel
notreat
plot(notreat$feed_concentration,notreat$fuel_produced)
plot(treat$feed_concentration,treat$fuel_produced)
library(ggplot2)
ggplot(biofuel, aes(x = biofuel$feed_concentration, y = biofuel$fuel_produced, colour = biofuel$pretreated)) +
  geom_point() +
  facet_wrap( ~ biofuel$pretreated)
modtreat = glm(treat$fuel_produced~treat$feed_concentration)
modtreat
modnotreat =glm(notreat$fuel_produced~notreat$feed_concentration)
modnotreat

###M2 ExAM!!!###
y=rpois(50,lambda=4)

y = sample(seq(1:50),size=50,replace=F)
x = sample(seq(1:50),size=50, replace=F)
mod.y = glm(y~x,family=poisson)
mod.y
plot(x,y,xlab='X',ylab='Y')


mod.y$coefficients


predX=seq(1:50)
predLogitY=mod.y$coefficients*predX
predY=exp(predLogitY)/(1+exp(predLogitY))
lines(predX,predY,lwd=2,col='red')

#Q5
x = c(4.9,1.8,0.2,5.1,8.7,3.0,8.7,2.0,7.9,6.0)
y = c(17.6,1.2,0.2,10.3,10.0,14.0,12.0,4.8,15.3,12.1)
plot(x,y)

linearNLL <- function(p,x,y){
  B0=p[1]
  B1=p[2]
  sd=p[3]
  expected = B1*x+B0
 NLL = -sum(dnorm(y,mean=expected,sd=sd,log=T))
 return(NLL)
}

mmNLL <- function(p,x,y){
  a=p[1]
  b=p[2]
  sd=p[3]
  expected = a*x*exp(b+x)
  NLL = -sum(dnorm(y,mean=expected,sd=sd,log=T))
  return(NLL)
}

linearFit = optim(par=c(0,1,1),fn=linearNLL,x=x,y=y)
linearFit

guess=c(1,1,-1)
mmFit = optim(par=guess,fn=mmNLL,x=x,y=y)

AICs =c(linear=linearFit$value*2,mm=mmFit$value*2)
AICs
