# Examples from Lecture 3 - Indirect Ordination
rm(list=ls())

setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module03-MultavariateStatistics/Borcardetal/NumEcolR_Data")

# Load required libraries
#library(ade4)
library(vegan)
library(ape)

# Import the data from CSV files
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)

# Remove empty site 8
spe <- spe[-8,]
env <- env[-8,]

# A reminder of the content of the env dataset
summary(env)	# Descriptive statistics


# PCA on the full dataset (correlation matrix: scale=TRUE)
# ********************************************************

env.pca <- rda(env, scale=TRUE) # Argument scale=TRUE calls for a
                                # standardization of the variables
env.pca


# Examine and plot partial results from PCA output
# ************************************************

?cca.object  # Explains how an ordination object produced by vegan is
             # structured and how to extract its results.

# Eigenvalues
(ev <- env.pca$CA$eig)

# Apply Kaiser-Guttman criterion to select axes
ev[ev > mean(ev)]


# Plot eigenvalues and % of variance for each axis
dev.new(title="PCA eigenvalues")
barplot(ev, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
	


# Two PCA biplots: scaling 1 and scaling 2
# ****************************************

# Plots using biplot.rda
dev.new(title="PCA biplots - environment ", 12, 6)
par(mfrow=c(1,2))
plot(env.pca, scaling=1, main="PCA - scaling 1")
plot(env.pca, main="PCA - scaling 2")  # Default scaling = 2

# Calculating variance explained
varEx=ev/sum(ev)*100



# PCoA on a Bray-Curtis dissimilarity matrix of fish species
# **********************************************************
spe.bray <- vegdist(spe)
spe.b.pcoa <- cmdscale(spe.bray, k=(nrow(spe)-1), eig=TRUE)
# Plot of the sites and weighted average projection of species
dev.new(title="PCoA on fish species - Bray-Curtis")
ordiplot(scores(spe.b.pcoa)[,c(1,2)], type="t",display="sites", main="PCoA with species")
abline(h=0, lty=3)
abline(v=0, lty=3)
# Add species by considering associations between predictors and the object-based ordiation axes
spe.wa <- wascores(spe.b.pcoa$points[,1:2], spe)
text(spe.wa, rownames(spe.wa), cex=0.7, col="red")


# PCoA and projection of species vectors using function pcoa()
# ************************************************************
# Hellinger pre-transformation of the species data
spe.h <- decostand(spe, "hellinger")

spe.h.pcoa <- pcoa(dist(spe.h))
# Biplots
dev.new(title="PCoA with species vectors", 14, 8)

# First biplot: Hellinger-transformed species data
biplot.pcoa(spe.h.pcoa, spe.h, dir.axis2=-1) 
abline(h=0, lty=3)
abline(v=0, lty=3)


# Comparison of PCoA results with Euclidean and non-Euclidean
# dissimilarity matrices
# ***********************************************************

# PCoA on a Hellinger distance matrix
is.euclid(dist(spe.h))
summary(spe.h.pcoa) 
spe.h.pcoa$values

# PCoA on a Bray-Curtis dissimilarity matrix
is.euclid(spe.bray)
spe.bray.pcoa <- pcoa(spe.bray) 
spe.bray.pcoa$values    # Observe eigenvalues 18 and following

# PCoA on the square root of a Bray-Curtis dissimilarity matrix
is.euclid(sqrt(spe.bray))
spe.braysq.pcoa <- pcoa(sqrt(spe.bray))
spe.braysq.pcoa$values  # Observe the eigenvalues


# CA of the raw species dataset (original species abundances)
# ***********************************************************

# Compute CA
spe.ca <- cca(spe)
spe.ca

# Plot eigenvalues and % of variance for each axis
(ev2 <- spe.ca$CA$eig)
dev.new(title="CA eigenvalues")
barplot(ev2, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev2), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

# CA biplots
# **********
dev.new(title="CA biplots", 14, 7)
par(mfrow=c(1,2))
# Scaling 1: sites are centroids of species
plot(spe.ca, scaling=1, main="CA fish abundances - biplot scaling 1")
# Scaling 2 (default): species are centroids of sites
plot(spe.ca, main="CA fish abundances - biplot scaling 2")

# A posteriori projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active
spe.ca.env <- envfit(spe.ca, env)
plot(spe.ca.env)
# This has added the environmental variables to the last biplot drawn


# NMDS applied to the fish species - Bray-Curtis distance matrix
# **************************************************************

spe.nmds <- metaMDS(spe, distance="bray")
spe.nmds
spe.nmds$stress
dev.new(title="NMDS on fish species - Bray")
plot(spe.nmds, type="t", main=paste("NMDS/Bray - Stress =",round(spe.nmds$stress,3)))