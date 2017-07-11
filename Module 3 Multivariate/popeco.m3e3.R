###POP ECO EXAM 2###

#generate histograms of mean =2, sd =1; mean=5, sd =1

bird.1 = rnorm(50,mean=2, sd=1)
hist(bird.1)

bird.2 = rnorm(50, mean=5,sd=1)
hist(bird.2)

birds <- seq(1,10,0.1)
plot(birds, dnorm(birds,mean=2, sd=1), type="l", xlab="Mean seed size (mm)", ylab = "Frequency of use")
lines(birds, dnorm(birds,mean=5, sd=1), col="red")
lines(birds, dnorm(birds, mean = 3,sd=1),col="green")
lines(birds, dbeta(birds,2,5))
legend('topright', c("Species 1", "Species 3", "Species 2"),pch=15,col=c("black","green", "red") )

###BIOSTATS M3E3 MICROSATELLITES##
micro = read.table("islandMicrosats.txt")
micro = as.data.frame(micro)
micro
dim(micro)
island = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
island
micro$island = island
micro

#ordination PCoA
col.is = c(1:5)
pchs= c(15:19)
micro.dist = vegdist(micro[,1:24], 'bray', binary = T) #Q mode individual skinks
micro.pcoa = cmdscale(micro.dist)
plot(micro.pcoa, col = col.is[island])
legend('topright', c("1","2","3","4", "5"), pch = 1,col = col.is)

microfit = envfit(micro.pcoa, micro[,1:24],permutations=999)
plot(microfit,p.max=.05)
#hypothesis testing
micro.anosim = anosim(micro[,1:24], grouping = island)
#global R = 0.5674, p-value = 0.001
#phylogeography between the islands. Gene flow between islands 1 and 5, and 2,3,4, but not between those groups
#separate colonization events

##SIMPER
simper.micro = simper(micro, group = island, permutations = 999)
simper.micro

#probiotics and vegan diet
diet = read.table("probiotic_diet.txt",header=T)
diet1 = data.frame(diet[,1:120])
diet1 = decostand(diet1, 'total', MARGIN = 1)
diet1.dist = vegdist(diet1, 'bray')
diet.pcoa = cmdscale(diet1.dist)
plot(diet.pcoa)
dim(diet)
treatment = factor(rep(1:4, each = 8)) #1 = control, 2=non-veganprobiotic, 3=vegannoprobiotic, 4=veganprob
treatment
diet$treatment = treatment
treatment.matrix = model.matrix(~treatment,diet)
treatment.matrix
mod <- cca(diet1,treatment)
model.frame(mod)
model.matrix(mod)
adonis(diet1~treatment.matrix[,1]*treatment.matrix[,2], permutations=999)

##Centralia microbes and soil characteristics
OTU = read.table("Centralia_Crobes.txt")
OTU
pchs = c(15:17)
fire = c(2,2,2,2,2,3,2,1,3,3,3,3,3,3,3,3,1,2)
OTU$fire = fire
dim(OTU)
OTU.bray = vegdist(OTU[,1:1846], 'bray')
OTU.pcoa = cmdscale(OTU.bray)
plot(OTU.pcoa, xlab="Axis 1", ylab = "Axis 2", pch = pchs[fire])
legend('topright', c("Reference","Recovered","Fire affected"), pch=pchs)
soil.fit = envfit(OTU.pcoa, ENV1, permutations = 999)
plot(soil.fit, p.max=0.05)

##There appears to be distinct gradient in OTU composition from reference to fire affected site,
#with Fe and soil temperature significantly correlated with fire-affected sites
ENV = read.table("Centralia_Env.txt")
ENV$fire = fire
ENV1 = decostand(ENV[,2:11],'total',MARGIN = 1 )
ENV1.euc = vegdist(ENV1,'euclidean')
ENV1.pca = rda(ENV1)
biplot(ENV1.pca, pch=pchs[fire])
OTU.rda <- rda(OTU~., ENV1)

# Observe the shortcut formula
summary(rda)
adonis(OTU~ENV$fire)

# How to obtain canonical coefficients from an rda() object
coef(OTU.rda)


# Triplots of the rda results
# ***************************
# Scaling 1: distance triplot
quartz(title="RDA scaling 1 + wa")
plot(OTU.rda, scaling=1, main="Triplot RDA OTU ~ ENV1 - scaling 1 - scores")
spe.sc <- scores(OTU.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

# Scaling 2 (default): correlation triplot
quartz(title="RDA scaling 2 + wa")
plot(OTU.rda, main="Triplot RDA OTU ~ ENV1 - scaling 2 - scores")
spe2.sc <- scores(OTU.rda, choices=1:2, display="sites")
arrows(0, 0, spe2.sc[, 1], spe2.sc[, 2], length=0, lty=1, col="red")

###PCA vs RDA is you measure the right variables then there shouldn't be a meaningful difference-
#constrained is a warped picture of what is structuring the microbial community