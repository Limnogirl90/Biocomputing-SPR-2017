




# oak tree density as a quadratic function of mean annual soil moisture
# make up soil moistures from uniform distribution
sm=runif(40,min=20,max=200)

# make up parameters for the quadratic deterministic component
B0=-20
B1=3
B2=-0.014

# expected values of oak density given the parametes above
lambda=B0+B1*sm+B2*sm^2

# add the stochastic component
oak=rpois(length(lambda),lambda)

data=data.frame(soilMoisture=sm,oakDensity=oak)

write.table(data,"oakDensities.txt",row.names=FALSE,sep="\t")


# primary production vs. lake total phosphorus concentration
# make up total phosphorus data from a uniform distribution
TP=runif(60,min=4,max=250)
# make up parameters of a michaelis-menten deterministic component
a=80
b=75
# expected values based on parameters above
Epp=a*TP/(b+TP)
# make up stochastic component parameter
shape=50
scale=Epp/shape
# simulate with deterministic and stochastic components
pp=rgamma(length(scale),shape=shape,scale=scale)

plot(TP,pp)

data2=data.frame(totalP=TP,primProd=pp)

write.table(data2,"lakePrimProd.txt",row.names=FALSE,sep="\t")