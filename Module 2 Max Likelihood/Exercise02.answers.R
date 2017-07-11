rm(list=ls())
setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module02-MaximumLikelihoodInference/Exercise02")


# Question 1
# Whitetailed deer foraging

pForage=read.table("propForaging.txt",header=FALSE)
pForage=as.numeric(pForage[,1])

# a. Plot histogram of data
hist(pForage,xlim=c(0,1),breaks=seq(0,1,0.1),ylab="Frequency",xlab="proportion of time foraging",col='gray')

# b. write the model

# pForage ~ Beta(a,b)

alpha=2
beta=10
x=seq(0,1,0.01)
plot(x,dbeta(x,shape1=alpha,shape2=beta),ylab="FUN!",type='l',lwd=3)



# c. estimate a and b shape parameters of beta distribution describing pForage
# likelihood function
pForageNLL<-function(p,x){
	a=p[1]
	b=p[2]
	
	-sum(dbeta(x=x,shape1=a,shape2=b,log=TRUE))
}

guess=c(2,2)
pFfit=optim(guess,pForageNLL,x=pForage)

lines(seq(0,1,0.01),dbeta(x=seq(0,1,0.01),pFfit$par[1],pFfit$par[2])*10,lwd=3)

pFfit$par[1]/(pFfit$par[1]+pFfit$par[2])

# c. likelihood profile of shape parameter a
pForageNLLprofile_a<-function(b,a,x){
	-sum(dbeta(x,a,b,log=TRUE))
}

as=seq(0.1,5,0.1)
store=matrix(NA,2,length(as))
for(i in 1:length(as)){
	tempFit<-optimise(pForageNLLprofile_a,lower=0,upper=1500,a=as[i],x=pForage)
	store[1,i]=tempFit$minimum
	store[2,i]=tempFit$objective
}

colnames(store)=as
rownames(store)=c('shape_b','NLL')

plot(as,store[2,],type='l',lwd=2,xlab="shape_a",ylab="NLL")
points(pFfit$par[1],pFfit$value,pch=16,cex=2)

# 95% confidence interval
prof.lower=store[2,1:which.min(store[2,])]
prof.as=as[1:which.min(store[2,])]
lci=approx(prof.lower,prof.as,xout=(pFfit$value+qchisq(0.95,1)/2))

points(lci$y,lci$x,pch=15,cex=2)

prof.upper=store[2,which.min(store[2,]):ncol(store)]
prof.as=as[which.min(store[2,]):ncol(store)]
uci=approx(prof.upper,prof.as,xout=(pFfit$value+qchisq(0.95,1)/2))

points(uci$y,uci$x,pch=15,cex=2)

#### this confidence interval is based upon the chi squared cumulative distribution because
#### negative log likelihoods are approximately chi squared distributed
#### if we want a 95% confidence we get the quantile corresponding to that and assume the interval is symmetric about the point estimate of the parameter 
plot(qchisq(seq(0,1,0.01),1),seq(0,1,0.01),type='l',lwd=3,xlab="quantile",ylab="probability",main="Chi squared cumulative distribution")
abline(h=0.95,lty=2,col='red',lwd=2)


# d. likelihood landscape for shape parameters a and b
as=seq(0.1,5,0.1)
bs=seq(0.1,15,0.1)

store2=matrix(NA,length(as),length(bs))
for(i in 1:length(as)){
	for(j in 1:length(bs)){
		store2[i,j]=-sum(dbeta(pForage,as[i],bs[j],log=TRUE))
	}
}

rownames(store2)=as
colnames(store2)=bs

filled.contour(as,bs,store2,xlab=quote(italic(a)),ylab=quote(italic(b)),color.palette=colorRampPalette(c('purple','blue','green','yellow','orange','red')))



# Question 2
# oak density
od=read.table("oakDensities.txt",header=TRUE,sep="\t")

plot(od[,1],od[,2],pch=16,cex=2,xlab="soil moisture content",ylab="Oak Density")

# Discrete
# >0, poisson?
# hump shaped - quadratic deterministic model

# od ~ poisson(lambda=b0+b1*sm+b2*sm^2)

# likelihood function
odNLL<-function(p,od,sm){
	b0=p[1]
	b1=p[2]
	b2=p[3]
	
	expected=b0+b1*sm+b2*sm*sm
	NLL=-sum(dpois(od,expected,log=TRUE))
	return(NLL)
}

guess=c(-25,2.5,0)
odFit=optim(par=guess,fn=odNLL,od=od[,2],sm=od[,1])

odFit
predX=20:200
lines(predX,odFit$par[1]+odFit$par[2]*predX+odFit$par[3]*predX*predX,lwd=3,lty=2,col='red')


# Question 3
# lake primary production
pp=read.table("lakePrimProd.txt",header=TRUE,sep="\t")

plot(pp[,1],pp[,2],xlab="Total Phosphorus",ylab="Primary Production")

# Continuous
# >0, gamma?
# saturating deterministic component

# pp ~ gamma(shape=shape, scale=(a*tp/(b+tp))/shape)

ppNLL<-function(p,tp,pp){
	a=p[1]
	b=p[2]
	shape=p[3]
	
	expected=a*tp/(b+tp)
	NLL=-sum(dgamma(pp,shape=shape,scale=expected/shape,log=TRUE))
	return(NLL)
}

guess=c(75,70,40)

ppFit=optim(par=guess,fn=ppNLL,tp=pp[,1],pp=pp[,2])

xpred=0:250
lines(xpred,ppFit$par[1]*xpred/(xpred+ppFit$par[2]),lwd=3,col='red',lty=2)