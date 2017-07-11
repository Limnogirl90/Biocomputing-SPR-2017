# ANOSIM, MRPP, and PERMANOVA example - revisiting diet data

npertype=c(8,8,8,8)

foodTypes=c('wholegrains','vegetables','fruits','sugar','dairy','meat')
means=matrix(c(30,20,20,2,1,0,4,4,4,25,25,25,20,20,10,10,0,20,20,15,15,10,15,30),nrow=4,byrow=TRUE)
for(i in 1:nrow(means)){
	means[i,]=means[i,]/sum(means[i,])
}

out=NULL
for(i in 1:length(npertype)){
	for(j in 1:npertype[i]){
		new=rnorm(length(foodTypes),mean=means[i,],sd=0.01)
		new[new<0]=0.001
		out=rbind(out,new/sum(new))
	}
}

library(vegan)
out.dist=vegdist(out) #32 by 32 matrix among people, Q mode
out.pcoa=cmdscale(out.dist)
plot(out.pcoa[,1],out.pcoa[,2],pch=rep(20:23,each=8),cex=3)

(out.anosim=anosim(out.dist,grouping=rep(1:4,each=8)))
out.mrpp=mrpp(out.dist,grouping=rep(1:4,each=8))
out.perm=adonis(out~as.factor(rep(1:4,each=8)))

# example of permutation test
outRand=out[sample(1:nrow(out),nrow(out)),]
(outRand.anosim=anosim(outRand,grouping=rep(1:4,each=8),distance="bray",permutations=0))
randRs=numeric(999)
for(i in 1:length(randRs)){
	outRand=out[sample(1:nrow(out),nrow(out)),]
	(outRand.anosim=anosim(outRand,grouping=rep(1:4,each=8),distance="bray",permutations=0))
	randRs[i]=outRand.anosim$statistic
}

hist(randRs,xlim=c(-1,1))
abline(v=out.anosim$statistic,lwd=3,col='red')




# RDA and CCA
library(ade4)

spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)
# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]

# Set aside the variable 'das' (distance from the source) for later use
das <- env[, 1]

# Remove the 'das' variable from the env dataset
env <- env[, -1]
# Recode the slope variable (pen) into a factor (qualitative) 
# variable (to show how these are handled in the ordinations)
pen2 <- rep("very_steep", nrow(env))
pen2[env$pen <= quantile(env$pen)[4]] = "steep"
pen2[env$pen <= quantile(env$pen)[3]] = "moderate"
pen2[env$pen <= quantile(env$pen)[2]] = "low"
pen2 <- factor(pen2, levels=c("low", "moderate", "steep", "very_steep"))
table(pen2)
# Create an env2 data frame with slope as a qualitative variable
env2 <- env
env2$pen <- pen2

# Create two subsets of explanatory variables
# Physiography (upstream-downstream gradient)
envtopo <- env[, c(1:3)]
names(envtopo)
# Water quality
envchem <- env[, c(4:10)]
names(envchem)

# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")


# REDUNDANCY ANALYSIS (RDA)
# *************************

# RDA of the Hellinger-transformed fish species data, constrained
# by all the environmental variables contained in env2

spe.rda <- rda(spe.hel~., env2) # Observe the shortcut formula

# How to obtain canonical coefficients from an rda() object
coef(spe.rda)


# Retrieval of the adjusted R^2
# *****************************

# Unadjusted R^2 retrieved from the rda result
(R2 <- RsquareAdj(spe.rda)$r.squared)

# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(spe.rda)$adj.r.squared)


# Triplots of the rda results
# ***************************
# Scaling 1: distance triplot
quartz(title="RDA scaling 1 + wa")
plot(spe.rda, scaling=1, main="Triplot RDA spe.hel ~ env2 - scaling 1 - wa scores")
spe.sc <- scores(spe.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

# Scaling 2 (default): correlation triplot
quartz(title="RDA scaling 2 + wa")
plot(spe.rda, main="Triplot RDA spe.hel ~ env2 - scaling 2 - wa scores")
spe2.sc <- scores(spe.rda, choices=1:2, display="sp")
arrows(0, 0, spe2.sc[, 1], spe2.sc[, 2], length=0, lty=1, col="red")

# Global test of the RDA result
anova.cca(spe.rda, step=1000)

# Tests of all canonical axes
anova.cca(spe.rda, by="axis", step=1000)



# variance partitioning
spe.part.all <- varpart(spe.hel, envchem, envtopo)
spe.part.all
quartz(title="Variation partitioning - all variables")
plot(spe.part.all, digits=2)  # Plot of the partitioning results
