##CLASS 2/27/17##
##How many tadpoles are consumed##
#Binomial
tadpoles <- read.table("tadpoles.txt", header=T)
tadpoles
consumed = tadpoles[,1]-tadpoles[,2]
plot(tadpoles[,1],consumed)


##Type II functional response##
#a = 
#h = handling time
#N = #tadpoles in bucket

consumption ~ binomial(a/(1+a*h*N),N)

# Fit Type II functional response model for binomial probability of death

# consumption~binomial(a/(1+a*h*N),N)

# define likelihood function
typeIInll<-function(params,N,k){
  a=params[1]
  h=params[2]
  
  p=a/(1+a*h*N)
  
  -sum(dbinom(k,N,p,log=TRUE))
}

# initial guesses for parameters
guess=c(0.1,-0.1)

# minimization of negative log likelihood
fit=optim(par=guess,fn=typeIInll,N=tadpoles[,1],k=consumed)

# plot model fit
lines(seq(0,50),fit$par[1]/(1+fit$par[1]*fit$par[2]*seq(0,50))*seq(0,50),lwd=4,lty=2)

# likelihood profile for confidence intervals

avec=seq(4,12,length=50)
aprof=numeric(50)
for(i in 1:50){
  aprof[i]=optim(par=a,fn=typeIInll,N=tadpoles[,1], k=consumed, a=avec[i])$value
}

plot(avec,aprof,type='l',lwd=4,ylab="NLL",xlab="a",cex.axis=1.6,cex.lab=1.6)
points(mfit$par[1],mfit$value,pch=16,cex=3)


