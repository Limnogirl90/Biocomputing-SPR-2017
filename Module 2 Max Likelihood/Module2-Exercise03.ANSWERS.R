rm(list=ls())
setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module02-MaximumLikelihoodInference/Exercise03")

##### BIOS 60751
##### Module 2 - Exercise #3


# Question 1
# a
bw=read.table("birdwatching.txt",header=TRUE,sep="\t")
plot(bw$latitude,jitter(bw$sighted,0.25),cex=2,lwd=2)

bw_glm=glm(bw$sighted~bw$latitude,family=binomial)
summary(bw_glm)

predLat=seq(35,49,0.01)

predLogitP=-7.70979+0.18934*predLat

predP=exp(predLogitP)/(1+exp(predLogitP))

lines(predLat,predP,lwd=2,col='red')
abline(h=0.5,lwd=2,lty=2)

predLat[abs(predP-0.5)==min(abs(predP-0.5))]

abline(v=40.72,lwd=2,lty=2)




# b:  
hbw=read.table("historical_birdwatching.txt",header=TRUE,sep="\t")

plot(bw$latitude,jitter(bw$sighted,0.25),cex=2,lwd=2)
points(hbw$latitude,jitter(hbw$sighted,0.25),col='red',cex=2,lwd=2)

lines(predLat,predP,lwd=2)


bw$hVp=1
hbw$hVp=0

all_bw=rbind(bw,hbw)

histFit=glm(hbw$sighted~hbw$latitude,family=binomial)
predLat=seq(35,49,0.01)
predLogitPhist=-17.0890+0.4251*predLat
predPhist=exp(predLogitPhist)/(1+exp(predLogitPhist))
lines(predLat,predPhist,lwd=2,col='red',lty=2)


histVpresentFit=glm(all_bw$sighted~all_bw$latitude+all_bw$hVp,family=binomial)

allFit=glm(all_bw$sighted~all_bw$latitude,family=binomial)

# likelihood ratio test to determine if distribution has changed...
# NO
anova(allFit,histVpresentFit)



# Question 2
# a: What distribution is data1.txt from
data1=read.table("data1.txt",sep="\t")
data1=as.vector(data1[,1])
head(data1)	# --> continuous

# plot data
hist(data1,freq=FALSE,ylim=c(0,0.25),main=NA) # --> right skewed

# NLL functions
gammaNLL<-function(p,x){
	-sum(dgamma(x,shape=p[1],rate=p[2],log=TRUE))
}

lognormNLL<-function(p,x){
	-sum(dlnorm(x,meanlog=p[1],sdlog=p[2],log=TRUE))
}

normNLL<-function(p,x){
	-sum(dnorm(x,mean=p[1],sd=p[2],log=TRUE))
}

# fits to data
gammaFit1<-optim(c(1,1),gammaNLL,x=data1)
lognormFit1<-optim(c(1,1),lognormNLL,x=data1)
normFit1<-optim(c(1,1),normNLL,x=data1)

# calculate AICs
AIC1=c(gamma=2*gammaFit1$value+2*length(gammaFit1$par),lognormal=2*lognormFit1$value+2*length(lognormFit1$par),normal=2*normFit1$value+2*length(normFit1$par))
AIC1

# plot probability density functions on histogram
lines(seq(0,15,0.1),dgamma(seq(0,15,0.1),shape=gammaFit1$par[1],rate=gammaFit1$par[2]),lwd=3,col='red')
lines(seq(0,15,0.1),dlnorm(seq(0,15,0.1),meanlog=lognormFit1$par[1],sdlog=lognormFit1$par[2]),lwd=3,col='green')
lines(seq(0,15,0.1),dnorm(seq(0,15,0.1),mean=normFit1$par[1],sd=normFit1$par[2]),lwd=3,col='orange')
legend('topright',paste(c('gamma','log normal','normal'),round(c(gammaFit1$value,lognormFit1$value,normFit1$value),1),sep="; "),col=c('red','green','orange'),lty=1,box.lty=0,lwd=2)

# b
data2=read.table("data2.txt",sep="\t")
data2=as.vector(data2[,1])
head(data2) # --> discrete

quartz()
hist(data2,freq=FALSE,ylim=c(0,0.25),main=NA) # --> right skewed

# NLL functions
negbinomNLL<-function(p,x){
	-sum(dnbinom(x,size=p[1],prob=p[2],log=TRUE))
}
poisNLL<-function(p,x){
	-sum(dpois(x,lambda=p[1],log=TRUE))
}

# fits to data
negbinomFit2<-optim(c(15,0.8),negbinomNLL,x=data2)
poisFit2<-optim(3,poisNLL,x=data2)

# calculate AICs
AIC2=c(negBinom=2*negbinomFit2$value+2*length(negbinomFit2$par),poisson=2*poisFit2$value+2*length(poisFit2$par))
AIC2

lines(seq(0,15),dnbinom(seq(0,15),size=negbinomFit2$par[1],prob=negbinomFit2$par[2]),lwd=3,col='blue')
lines(seq(0,15),dpois(seq(0,15),lambda=poisFit2$par),lwd=3,col='orange')
legend('topright',paste(c('neg. binomial','poisson'),round(c(negbinomFit2$value,poisFit2$value),1),sep="; "),col=c('blue','orange'),lty=1,box.lty=0,lwd=2)


# c
# Does sample size improve ability to distinguish between right-skewed distributions?

lognormVgamma<-function(N){
	x=rgamma(N,shape=2,rate=0.5)
	
	gFit<-optim(c(1,1),gammaNLL,x=x)
	lnFit<-optim(c(1,1),lognormNLL,x=x)
	
	deltaAIC=(2*lnFit$value+2*length(lnFit$par))-(2*gFit$value+2*length(gFit$par))
}

Ns=seq(5,1000,length.out=40)

store=matrix(NA,10,length(Ns))
for(i in 1:length(Ns)){
	N=Ns[i]
	for(j in 1:10){
		store[j,i]=lognormVgamma(N)
	}
}

plot(rep(Ns[1],10),store[,1],xlim=c(0,1000),ylim=range(store),xlab="sample size",ylab="deltaAIC (lognormal - gamma)",cex=1.5,pch=16)
for(i in 2:length(Ns)){
	points(rep(Ns[i],10),store[,i],cex=1.5,pch=16)
}


# Question 3 Plant litter decomposition
lidet=read.csv("LIDETdata_Hobbs.csv",header=TRUE)
lidet[,c(1,3:5)]=lidet[,c(1,3:5)]/100

plot(lidet[,2],lidet[,1],lwd=2,cex=2,ylab="fraction mass remaining",xlab="time (yrs)",cex.axis=1.6,cex.lab=1.6,xlim=c(0,10),ylim=c(0,1))

# beta mean as a function of parameters
#mean=shape1/(shape1+shape2)
#mean*shape1+mean*shape2=shape1
#shape2=(shape1-mean*shape1)/mean

# simplest decay model
simplest<-function(p,time,Mt){
	k=p[1]
	shape1=p[2]
	meanMt=exp(-k*time)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
simplestfit=optim(par=c(k=0.5,shape1=5),fn=simplest,time=lidet[,2],Mt=lidet[,1])


# add climate index to simple model
plusClimate<-function(p,time,Mt,CDI){
	k=p[1]
	shape1=p[2]
	meanMt=exp(-k*CDI*time)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
plusClimateFit=optim(par=c(k=0.5,shape1=5),fn=plusClimate,time=lidet[,2],Mt=lidet[,1],CDI=lidet[,6])


# add a second carbon pool
twopool<-function(p,time,Mt,M1){
	k1=p[1]
	k2=p[2]
	shape1=p[3]
	meanMt=M1*exp(-k1*time)+(1-M1)*exp(-k2*time)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
twopoolFit=optim(par=c(k1=0.4,k2=0.2,shape=2.5),fn=twopool,time=lidet[,2],Mt=lidet[,1],M1=lidet[,3])


# add climate index to two carbon pool mode
twopoolClimate<-function(p,time,Mt,M1,CDI){
	k1=p[1]
	k2=p[2]
	shape1=p[3]
	meanMt=M1*exp(-k1*time*CDI)+(1-M1)*exp(-k2*time*CDI)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
twopoolClimateFit=optim(par=c(k1=0.4,k2=0.2,shape=2.5),fn=twopoolClimate,time=lidet[,2],Mt=lidet[,1],CDI=lidet[,6],M1=lidet[,3])


# add a third carbon pool
threepool<-function(p,time,Mt,M1,M2,M3){
	k1=p[1]
	k2=p[2]
	k3=p[3]
	shape1=p[4]
	meanMt=M1*exp(-k1*time)+M2*exp(-k2*time)+M3*exp(-k3*time)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
threepoolFit=optim(par=c(k1=0.4,k2=0.2,k3=0.1,shape=2.5),fn=threepool,time=lidet[,2],Mt=lidet[,1],M1=lidet[,3],M2=lidet[,4],M3=lidet[,5])


# add climate index to two carbon pool mode
threepoolClimate<-function(p,time,Mt,CDI,M1,M2,M3){
	k1=p[1]
	k2=p[2]
	k3=p[3]
	shape1=p[4]
	meanMt=M1*exp(-k1*time*CDI)+M2*exp(-k2*time*CDI)+M3*exp(-k3*time*CDI)
	-sum(dbeta(Mt,shape1=shape1,shape2=(shape1-meanMt*shape1)/meanMt,log=TRUE))
}
threepoolClimateFit=optim(par=c(k1=0.4,k2=0.2,k3=0.1,shape=2.5),fn=threepoolClimate,time=lidet[,2],Mt=lidet[,1],M1=lidet[,3],M2=lidet[,4],M3=lidet[,5],CDI=lidet[,6])


#AIC
lidetAIC=c(simplest=simplestfit$value*2+length(simplestfit$par)*2,twopool=twopoolFit$value*2+length(twopoolFit$par)*2,twopoolClimate=twopoolClimateFit$value*2+length(twopoolClimateFit$par)*2,threepool=threepoolFit$value*2+length(threepoolFit$par)*2,threepoolClimate=threepoolClimateFit$value*2+length(threepoolClimateFit$par)*2)

lidetAIC
sort(lidetAIC-min(lidetAIC))

pchisq(2*(twopoolFit$value-threepoolClimateFit$value),df=1,lower.tail=FALSE)

# Question 4
biofuel=read.table("biofuel.txt",header=TRUE,sep="\t")

biofuel$feed_concentration=biofuel$feed_concentration+5
biofuel$fuel_produced=biofuel$fuel_produced+2

pretreatColor=rep('black',nrow(biofuel))
pretreatColor[biofuel$pretreated==1]='red'

plot(biofuel$feed_concentration,biofuel$fuel_produced,col=pretreatColor,cex=2,lwd=2)

noPretreatNLL<-function(p,feed,fuel){
	a=p[1]
	b=p[2]	
	shape=p[3]
	expected=a*feed/(feed+b)
	
	return(-sum(dgamma(fuel,shape=shape,scale=expected/shape,log=TRUE)))
}

noPretreatFit=optim(par=c(1,20,1),noPretreatNLL,feed=biofuel$feed_concentration,fuel=biofuel$fuel_produced)
px=1:120
lines(px,noPretreatFit$par[1]*px/(noPretreatFit$par[2]+px),lwd=2,lty=2)

plusPretreatNLL<-function(p,feed,pretreat,fuel){
	a=p[1]
	b=p[2]
	c=p[3]	
	shape=p[4]
	expected=(a+c*pretreat)*feed/(feed+b)
	
	return(-sum(dgamma(fuel,shape=shape,scale=expected/shape,log=TRUE)))
}

plusPretreatFit=optim(par=c(1,20,0.2,1),plusPretreatNLL,feed=biofuel$feed_concentration,pretreat=biofuel$pretreated,fuel=biofuel$fuel_produced)
px=1:120

lines(px,plusPretreatFit$par[1]*px/(plusPretreatFit$par[2]+px),lwd=2,lty=2,col='red')
lines(px,(plusPretreatFit$par[1]+plusPretreatFit$par[3])*px/(plusPretreatFit$par[2]+px),lwd=2,col='red')


AICs=c(noPretreat=noPretreatFit$value*2,plusPretreat=plusPretreatFit$value*2)
AICs

# likelihood ratio test
deltaDev=AICs[1]-AICs[2]
pchisq(deltaDev,df=1,lower.tail=FALSE)