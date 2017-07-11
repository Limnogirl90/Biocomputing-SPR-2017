##Exercise 1 Multivariate!!##
#1 exploring human diets
#1 A what metric would I choose for these data and why

#load in and view data diet composition
diet <- read.table("dietComposition.txt", header=T)
diet
colnames(diet)
dim(diet)
Subject <- c(1:61)
cbind(diet, Subject)
#Q-mode because we are comparing diets across objects (people)

#Euclidean?
library(vegan)
diet.dist <- vegdist(diet, method="euclidean")
diet.pcoa <- cmdscale(diet.dist)
plot(diet.pcoa[,1],diet.pcoa[,2], xlab="Axis 1", ylab="Axis 2")
min(diet.dist)
max(diet.dist)

#minimum and maximum similarities
library(car)
library(MASS)
head(diet)
scatterplotMatrix(diet)
plot(hclust(diet.dist,method='complete'))
#How many groups?
#3 groups
diet.rda <- rda(diet, scale=T)
plot(diet.rda,scaling=3)
pca.diet <- prcomp(diet)
biplot(pca.diet)
par(mfrow=c(1,1))
plot(hclust(diet.dist))
##use cascadeKM to optimize # groups
diet.cascade = cascadeKM(diet,inf.gr = 2,sup.gr=7,iter=1000,criterion="ssi")
plot(diet.cascade)
diet.cascade$size

library(dendextend) #do pretty things with dendograms
##4 Give each class a name
# a) sugar,dairy
# b) vegetables,fruits, whole grain
# c) meat
##If in R-mode, what diet categories tend to correlate or covary with each other


##2 insect###*******************
insect1 <- read.table('insectComposition1.txt', header=T)
insect2 <- read.table('insectComposition2.txt',header=T)
dim(insects)
LH <- c(rep(1,15),rep(2,15), rep(1,15), rep(2,15)) ##setting up the region and island columns
IslandAll <- c(rep(1,30),rep(2,30))
insectall <- rbind(insect1,insect2) ##I added the 2 region and island columns after rbind
insectall$LH = LH
insectall$IslandAll = IslandAll

insect.dist <- vegdist(insectall, method='bray')
insect.pcoa <- as.data.frame(cmdscale(insect.dist)) ##cmdscale does pcoa
insect.pcoa$Elevation = LH
insect.pcoa$Island = IslandAll
#binary data inflates impact of rare species
#can get differences between objects in 2 ways: descriptors there and descriptors not there
#incomplete competitive exclusion
#dispersal limitations
#one island could have been colonized a long time ago thereby exlcuding other species
cophenetic(hclust(insect.dist))

plot(insect.pcoa)
insect.rda <- rda(insect.pcoa)

biplot(insect.rda, type=c("text","points"), col=c("red","black"),display=c("sites","species"))
ordihull(insect.pcoa, groups = LH, col=c(3,4))
ordihull(insect.pcoa, groups = IslandAll, col=c(1,2))
legend("topright",
       col = c(3,4), 
       lty = 1,
       legend = region.names)

pca.insect<-prcomp(insectall)	#uses SVD for computations (mean-centered is default)
summary(pca.insect)
plot(pca.insect)

par(mfrow=c(1,1))

##Make an empty plot
plot(insect.pcoa[,1],insect.pcoa[,2],type='n')

##adding in points for every combination of island-altitude
points(insect.pcoa[insect.pcoa$Elevation=='1' & insect.pcoa$Island == '2',1],insect.pcoa[insect.pcoa$Elevation=='1' & insect.pcoa$Island == '2',2], pch=16,col=1)
points(insect.pcoa[insect.pcoa$Elevation=='1' & insect.pcoa$Island == '1',1],insect.pcoa[insect.pcoa$Elevation=='1' & insect.pcoa$Island == '1',2], pch=15,col=2)
points(insect.pcoa[insect.pcoa$Elevation=='2' & insect.pcoa$Island == '2',1],insect.pcoa[insect.pcoa$Elevation=='2' & insect.pcoa$Island == '2',2], pch=14,col=3)
points(insect.pcoa[insect.pcoa$Elevation=='2' & insect.pcoa$Island == '1',1],insect.pcoa[insect.pcoa$Elevation=='2' & insect.pcoa$Island == '1',2], pch=13,col=4)
legend('topright', legend = c("Elevation 1 Island 2","Elevation 1 Island 1","Elevation 2 Island 2", "Elevation 2 Island 1"), col=1:4, pch=1)

##what could contribute to observed changes in insect community between the 2 islands?
#island size, vegetation, competition, predation, vegetation could contribute to 
#types of secondary compounds, island proximity to each other-colonization and migration
#It looks like elevation did not have a major influence on the insect community composition of island 1, but elevation was 
#important for island 2. Interestingly, the insect communities of island 1 and elevation 1 of island 2 were very similar.
#Perhaps island 1 was smaller and less heterogeneous than island 2. Or elevation gradient may have been slight
#in island 1

##3 sources of methodological variation

##Ranking 1
#I think choice of quantitative vs. binary data is first importance because
#choosing either of these will take you down a path where you are analyzing
#strictly if something is there or not, or if you care about changes in relative abundance.

#Ranking 2
##choice of clustering algorithm is 3rd because you can always transform data to meet the assumptions, but 
#if you have a clear question, you will have collected the data needed to answer that question, and the data type
#will suit the multivariate method you wish to use

#Ranking 3
#Choice of association metric is important because this step treats high and low abundance species
#differently, for example euclidean is very sensitive to scale, and then there's the double-zero problem,
#leading to choice about symmetric or asymmetric metrics

##NOW HOWEVER I THINK CLUSTERING IS MOST IMPORTANT, FOLLOWED BY BINARY VS. QUANTITATIVE DATA

##BINARY IS MOST IMPORTANT!!####
#load in data
obs <- read.table("observations.txt",header=T)
obs
par(mfrow=c(3,3))
#euclidean for Q mode
obs.dist1 <- vegdist(obs, method = "euclidean")
obs.dist1
obs1.pcoa <- cmdscale(obs.dist1)
plot(obs1.pcoa[,1],obs1.pcoa[,2], main = "Euclidean")
obs1.clust <- hclust(obs.dist1, method = "single")
plot(obs1.clust, main = "Euclidean single") ##four group, more horizontal

obs1.complete <- hclust(obs.dist1,method="complete")
plot(obs1.complete, main = "Euclidean complete") ## 2 major groups, each with ~3 groups
obs1.avg <- hclust(obs.dist1,method="average")
plot(obs1.avg,main = "Euclidean average")
#bray-curtis
obs.dist2 <- vegdist(obs, method = "bray")
obs.dist2
obs2.pcoa <- cmdscale(obs.dist2)
#plot(obs2.pcoa[,1],obs2.pcoa[,2], main ="Bray-Curtis")
obs2.clust <- hclust(obs.dist2,method="single")
plot(obs2.clust, main= "BC single") #more vertical

obs2.complete = hclust(obs.dist2,method="complete")
plot(obs2.complete, main="BC complete") #very similar to complete for obs1

obs2.avg <- hclust(obs.dist2, method="average")
plot(obs2.avg,main="BC average")
#jaccard
obs.dist3 <- vegdist(obs, method = "jaccard", binary=T)
obs.dist3
obs3.pcoa <- cmdscale(obs.dist3)
#plot(obs3.pcoa[,1],obs3.pcoa[,2], main = "Jaccard")
obs3.clust <- hclust(obs.dist3, method = "single")
plot(obs3.clust, main = "Jaccard single") ## very similar to Bray, vertical

obs3.complete = hclust(obs.dist3,method="complete")
plot(obs3.complete, main = "Jaccard complete")

obs3.avg = hclust(obs.dist3,method="average")
plot(obs3.avg,main="Jaccard average")

##PCA method
##Euclidean
PCA.obs1 <- prcomp(obs)
PCA.obs1 <- as.data.frame(cmdscale(obs.dist1))
obs1.rda <- rda(PCA.obs1)
biplot(obs1.rda, main="Euclidean")

##BC
obs.dist2 <- vegdist(obs, method = "bray")
obs.dist2
PCA.obs2 <- prcomp(obs)
obs2.pcoa <- cmdscale(obs.dist2)
obs2.rda <- rda(obs2.pcoa)
biplot(obs2.rda, main="Bray")

#Jac
obs.dist3
PCA.obs3 <- prcomp(obs)
obs3.pca <- cmdscale(obs.dist3)
obs3.rda <- rda(obs3.pca)
biplot(obs3.rda,main="Jaccard")
obs3.pca
summary(PCA.obs3)
summary(obs3.pca)
