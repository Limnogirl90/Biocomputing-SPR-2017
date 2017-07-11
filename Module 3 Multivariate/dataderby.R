###DATA DERBY###
##fish metals## prey fish
#ignore H20 percent and depth
fishmetals = read.csv("FishMetals_WC.csv",header = T)
fishmetals = fishmetals[,-6]
fishmetals
fishmetals = fishmetals[,-4]
X = fishmetals[,1:3]
X$Benthic_Pelagic = factor(X$Benthic_Pelagic)
dim(fishmetals)
data = fishmetals[,4:13]
data.stand = decostand(data[,5:10], 'standardize', MARGIN = 1)
dim(data)
dim(fishmetals)

# design matrix2 3
#benthic nearshore = 4
#benthic offshore = 1
#pelagic offshore = 2
#benthic/pel nearshore = 3

fish.design=matrix(0,nrow(X),1)

for(i in 1:nrow(fish.design)){
  if(X[i,3] == 'Benthic' & X[i,2] == 'Nearshore'){
    fish.design[i] = 4
  }else if(X[i,3] == 'Pelagic' & X[i,2] == 'Offshore'){
      fish.design[i] = 2
    }else if (X[i,3] == 'Benthic' & X[i,2] == "Offshore"){
      fish.design[i]= 1
    }else if(X[i,3]=='Benthic/Pelagic' & X[i,2] == 'Nearshore'){
  fish.design[i]=3
    }
}
fish.design
metals.category = cbind(fish.design, data.stand)
metals.category

col.cat = c(1:6)
metals.dist = vegdist(metals.category[,2:7], 'euclidean')
metal.clust1 = hclust(metals.dist, method = 'single')
metal.clust2 = hclust(metals.dist, method = 'complete')
cophenetic(metal.clust1)
cophenetic(metal.clust2)
cor(metal.clust1,metals.dist)
metals.pcoa = cmdscale(metals.dist)
plot(metals.pcoa, xlab= "Axis 1", ylab = "Axis 2", col = col.cat[X$Species])
legend('bottomright', c("Deep", "Alewife", "Rainbow", "Bloater","Slimy", "RG"), col = col.cat, pch=15)
metal.fit = envfit(metals.pcoa, data.stand, permutations = 999)
plot(metal.fit, p.max= 0.05)
metals.pca =  prcomp(data.stand)
biplot(metals.pca)
metal.adonis = adonis(data.stand~fish.design, data = metals.category)
metal.adonis
matrix = rep(c(1,2,3), each = 4)
matrix
#quantify percent variance of 1st 2 axes of PCoA
genes = matrix(sample(c(0:1),144,replace=T), 12,12)
genes
species = matrix
species
gene.dat = cbind(species, genes)
gene.dat = as.data.frame(gene.dat)
gene.dat
genes.dist = vegdist(genes, 'jaccard',binary=T)
genes.pcoa = cmdscale(genes.dist,eig=T, add=T)
plot(genes.pcoa$points[,2],genes.pcoa$points[,1])
var_exp = genes.pcoa$eig[1:2]
genes.anosim = anosim(gene.dat, grouping = gene.dat$species)
