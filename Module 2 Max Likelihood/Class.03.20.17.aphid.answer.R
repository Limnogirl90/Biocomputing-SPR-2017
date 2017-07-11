##CLASS 3/20/17 APHID PAPER GOING OVER TOGETHER##

aphid <- read.csv("app.exercise.csv")
aphid
antNLL <- function(p,ants,card){
  a=p[1]
  b=p[2]
  shape=10^p[3]
  Eants = a/(card+b)
  NLL = -sum(dgamma(ants,shape=shape, scale=Eants/shape,log=T)) #log = T needs to be inside paren for dgamma
  return(NLL)
}

guess = c(1490,425,10) ##ran out of iterations based on these params, stopped because had 502 iterations
antFit = optim(par=guess, fn=antNLL, ants=aphid$Ant.visit,card=aphid$cardenolide)
antFit

#if params are orders of magnitude different, optim can have a hard time converging. might 
#have to transform, log10 or something
plot(aphid$cardenolide,aphid$Ant.visit)
xpred = 200:1200
lines(xpred,antFit$par[1]/(antFit$par[2]+xpred))

##Class 3/22/17 practice for likelihood ratio test###
m = 5
n =-.1
b=2
shape=12
x=runif(40,10,50)
x2=x*x
y=rgamma(length(x),shape=shape,scale=(n*x2+m*x+b/shape))

linfit=glm(y~x)
pchisq((10.245-5.4088),1,lower.tail=F) ##how you get a p-value

#example in class#
myxo <- read.table("Myxo.txt", header=T)
myxo ##titers virus
plot(myxo$Day,myxo$ViralTiter)

#possible models
#m1 = ax/b+x
#m2= ax^c/b+x^c
#m3=axe^-bx

M1NLL <- function(p,time,titer){
  a=p[1]
  b=p[2]
  shape=p[3]
  expected = a*time/(b+time)
  NLL = -sum(dgamma(titer,shape=shape, scale=expected/shape,log=T))
}
M1fit = optim(par=c(a=1,b=3,shape=50),fn=M1NLL, time=myxo$Day,titer=myxo$ViralTiter)
#warnings for optim in Gamma OK, just about the distribution being bounded in such a way
M1fit
