#Class 2/15/2017

#ggplot

#Data and mapping to aesthetics (the physical lcations, shape, size, color)
#geoms (geometry) <- this is related to aesthetics
#coordinate system (pixels)

#qplot(x=  y= ,data=  )  # the q stands for quick plot

install.packages("ggplot2")
rm(list=ls())
getwd()
setwd("C:/Users/Brooke/Documents/Biocomputing in R/Excerise2")
Lake=read.table("LakeGPP.txt", header=TRUE)
Lake
# L=qplot(x="TP_mgm3", y="GPP_mmolm3d", data=Lake, geom="point")
# L
L<-ggplot(Lake, aes(x=TP_mgm3, y=GPP_mmolm3d, color=cyl))
L + geom_point()+ geom_smooth()
L

Tree=read.table("treegrowth.txt", header=TRUE)
Tree
T<-ggplot(Tree, aes(x=tree$treeNumber, y=tree$avgGrowth))
T + geom_point()+ geom_smooth()
T
