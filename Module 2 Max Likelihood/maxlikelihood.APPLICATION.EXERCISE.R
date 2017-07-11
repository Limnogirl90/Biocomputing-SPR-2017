##MAX likelihood real applications###
#read-in simulated data:
aphid <- read.csv("app.exercise.csv", header = T)
aphid
#plot data
plot(aphid$cardenolide,aphid$Ant.visit)
plot(aphid$Ant.visit,aphid$Aphid.response)
ant = aphid$Ant.visit
plant = aphid$cardenolide
response = aphid$Aphid.response

##MODEL ANT VISITS##

##Modeling ants as a function of cardenolide##
# We are applying a maximum likelihood function to our own simulated data, based on Figure 1D in the text
#We estimated the data and used a hyperbolic deterministic function to fit a model to the data
plot(aphid$cardenolide,aphid$Ant.visit)
#ants ~ negbinom(a/b+cardenolides) plot looks hyperbolic, going with that
antNLL <- function(p,card,ant){
  a=p[1]
  b=p[2]
  expected = a/(b+card)
  NLL <- -sum(dnorm(ant,expected, log=T)) 
}

guess <- c(15750,450)
antFit <- optim(par=guess,fn=antNLL,card=ant.2$cardenolide, ant=ant.2$Ant.new)
ant.2 <- read.csv("app.exercise.csv", header=T)
plot(ant.2$cardenolide,ant.2$Ant.new, ylab="Ant counts per genotype", xlab="Cardenolide concentration")

lines(seq(0,1600),antFit$par[1]/(antFit$par[2]+seq(0,1600)),lwd=4,lty=1)

# c. likelihood profile of shape parameter a
antNLLprofile_a<-function(b,a,ant){
  -sum(dnorm(ant,a,b,log=TRUE))
}
as=seq(0,40)
store=matrix(NA,2,length(as))
for(i in 1:length(as)){
  tempFit<-optimise(antNLLprofile_a,lower=0,upper=500,a=as[i],ant=ant.2$Ant.new)
  store[1,i]=tempFit$minimum
  store[2,i]=tempFit$objective
}

colnames(store)=as
rownames(store)=c('shape_b','NLL')
plot(as,store[2,],type='l',lwd=2,xlab="shape_a",ylab="NLL")


# c. likelihood profile of shape parameter b
antNLLprofile_b<-function(a,b,ant){
  -sum(dnorm(ant,b,a,log=TRUE))
}
bs=seq(0,1500)
store=matrix(NA,2,length(bs))
for(i in 1:length(bs)){
  tempFit<-optimise(antNLLprofile_b,lower=0,upper=500,b=bs[i],ant=ant.2$Ant.new)
  store[1,i]=tempFit$minimum
  store[2,i]=tempFit$objective
}

colnames(store)=bs
rownames(store)=c('shape_b','NLL')
plot(bs,store[2,],type='l',lwd=2,xlab="shape_b",ylab="NLL")


