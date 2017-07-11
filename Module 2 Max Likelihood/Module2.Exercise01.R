# Exercise 1 - Module 2
# BIOS 60576
# SEJ
rm(list=ls())
library(ggplot2)
setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module02-MaximumLikelihoodInference/Exercise01")

##### 1a) generate 10 observations of continuous x and y
B0=-4
B1=2.5
n=10

x=runif(n,0,10)
y=x*B1+B0+rnorm(n,0,sqrt(2))

xylm=lm(y~x)
summary(xylm)

xy=data.frame(x=x,y=y)
xyplot=ggplot(xy,aes(x,y))
xyplot+geom_point(shape=16,size=4)+theme_classic()+stat_smooth(method="lm")

#### 1b)
# y=B0+B1*x+epsilon; epsilon~N(0,sigma^2)

#### 1c)
n2=100
x2=runif(n2,0,10)
y2=x2*B1+B0+rnorm(n2,0,sqrt(2))
xylm2=lm(y2~x2)

summary(xylm)
summary(xylm2)

#### 1d)
ns=c(10,50,seq(100,1000,100))
summ=matrix(NA,length(ns),4)
for(i in 1:length(ns)){
	curx=runif(ns[i],0,10)
	cury=curx*B1+B0+rnorm(ns[i],0,sqrt(2))

	curfit=lm(cury~curx)
	summ[i,1]=summary(curfit)$coefficients[1,1]
	summ[i,2]=summary(curfit)$coefficients[1,2]
	summ[i,3]=summary(curfit)$coefficients[2,1]
	summ[i,4]=summary(curfit)$coefficients[2,2]
}
df=data.frame(N=ns,B0=summ[,1],B0se=summ[,2],B1=summ[,3],B1se=summ[,4])

B0plot=ggplot(data=df,aes(x=N,y=B0,ymin=B0-B0se,ymax=B0+B0se))
B0plot+geom_point(aes(N,B0),shape=16,size=3)+theme_classic()+geom_errorbar()+geom_abline(slope=0,intercept=B0)

B1plot=ggplot(data=df,aes(x=N,y=B1,ymin=B1-B1se,ymax=B1+B1se))
B1plot+geom_point(aes(N,B1),shape=16,size=3)+theme_classic()+geom_errorbar()+geom_abline(slope=0,intercept=B1)

#### 2a-c)
#A:  gas = B0+B1*temp+epsilon; epsilon~N(0,sigma^2)
#B:  gas = B0+B1*temp+B2*insul+epsilon; epsilon~N(0,sigma^2)
#C:  gas = B0+B1*temp+B2*insul+B3*temp*insul+epsilon; epsilon~N(0,sigma^2)

library(MASS)
data(whiteside)
fitA=lm(whiteside$Gas~whiteside$Temp)
fitB=lm(whiteside$Gas~whiteside$Temp+whiteside$Insul)
fitC=lm(whiteside$Gas~whiteside$Temp*whiteside$Insul)

summary(fitA)
summary(fitB)
summary(fitC)

#### 3)
## 1a with ML
# simulate data exactly the same way
B0=-4
B1=2.5
n=10

x=runif(n,0,10)
y=x*B1+B0+rnorm(n,0,sqrt(2))

xy=data.frame(x=x,y=y)
xyplot=ggplot(xy,aes(x,y))
xyplot+geom_point(point=16,size=3)+theme_classic()

# use custom likelihood function and optim
nllxy<-function(p,x,y){
	B0=p[1]
	B1=p[2]
	sigma=exp(p[3])
	
	yhat=B0+B1*x
	nll=-sum(dnorm(y,yhat,sd=sigma,log=TRUE))
	return(nll)
}

guess=c(-3.9,2,0)
nllfit=optim(guess,nllxy,x=x,y=y)
nllfit

guess2=c(0,0,0)
nllfit2=optim(guess2,nllxy,x=x,y=y)
nllfit2

## 1b)
# y~N(B0+B1x,sigma^2)

#### 4)
d=read.table("rainManipBiomass.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)

## 4a) barplot with 2*sd error bars
means=tapply(d$V2,d$V1,mean)
sds=tapply(d$V2,d$V1,sd)
dsumm=data.frame(trt=names(means),mean=means,sd=sds)

rainManip=ggplot(dsumm,mapping=aes(trt,mean,ymin=mean-2*sd,ymax=mean+2*sd))
rainManip+geom_bar(stat="identity",fill='white',color='black')+theme_classic()+geom_errorbar(width=0.2)

## 4b) 
# abg~N(B0+B1*x1+B2*x2,sigma2)

rmNLL<-function(p,x1,x2,abg){
	B0=p[1]
	B1=p[2]
	B2=p[3]
	sigma=exp(p[4])
	
	yhat=B0+B1*x1+B2*x2
	nll=-sum(dnorm(abg,yhat,sigma,log=TRUE))
	return(nll)
}

x1=numeric(nrow(d))
x1[d$V1=="drought"]=1
x2=numeric(nrow(d))
x2[d$V1=="wet"]=1
cbind(d$V1,x1,x2)

guess=c(400,-300,200,log(50))
rmFit=optim(guess,rmNLL,x1=x1,x2=x2,abg=d$V2)

rmFit

dsumm
dsumm[2,2]-dsumm[1,2]
dsumm[3,2]-dsumm[1,2]