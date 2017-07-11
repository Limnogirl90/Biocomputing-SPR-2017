##M1 Class matrix species 4.3.17##

species <- matrix(c(2,5,12,2,8,9,10,5,16,3,7,9,32,8,8,3,61,7,3,1),5,4,byrow=T)
species
site = c(1,2,3,4,5)
library(vegan)

##M3 Class 4.5.17##

library(vegan)
library(FD)
library(ade4)
library(cluster)

vegdist(species,method="bray")
d.pcoa <- cmdscale(vegdist(species,method='bray'))
plot(d.pcoa[,1],d.pcoa[,2])

prac1 <- read.csv("DoubsSpe.csv",header=T)
prac1
spe.norm = decostand(prac1, 'normalize')
species.dist = vegdist(spe.norm,method="bray")
species.dist
spe.coa <- cmdscale(species.dist)
plot(spe.coa[,1],spe.coa[,2])
