rm(list=ls())  #clean space
rm(species)

install.packages("vegan")
install.packages("gclus")
install.packages("FD")
install.packages("factoextra")
library(tidyverse)
library(caret)
library(dplyr)
library(vegan)
library(gclus)
library(cluster)

#task1
#Read in the doubs into your R and delete the site 8 which has no fishes. 
#Write code and answer the questions: Which site has the most species (and how many species)? 
#Which species is the most widespread (i.e., found in the most sites)?


data(doubs, package="ade4")      
fish <- doubs$fish
#delete the site 8 which has no fishes
fish <- fish[-8,]    
site <- data.matrix(rowSums(fish!=0))
#find the site 29 which has the most species 26
site[which.max(site),]     
species <- data.matrix(colSums(fish!=0))
#find species "Lece" is the most widespread, and the "Lece" is found in 25 sites
species[which.max(species),]      
spe <- doubs$species
#find the scientific name for "Lece"
spe[which.max(species),]


#task2


#Select a suitable association measure of species
spe.t<-t(spe)
spe.t.chi<-decostand(spe.t,"chi.square")
spe.t.D<-dist(spe.t.chi)
#Calculate ward & single minimum variance clustering
spe.t.chi.single<-hclust(spe.t.D,method = "single")
plot(spe.t.chi.single)
spe.t.chi.ward <- hclust(spe.t.D, method="ward.D2")
plot(spe.t.chi.ward)
source("coldiss.R")
coldiss(spe.t.D,byrank=FALSE,diag=FALSE)
#Q mode aggregated the locations
spe.norm<-decostand(spe,"normalize")
#Calculate the string distance matrix between quadrat
spe.ch<-vegdist(spe.norm,"euc")
#Calculate the Chord distance clustering
spe.dc<- vegdist(spe.norm)
spe.hel<-decostand(spe,"hel")
#Hellinger matrix
spe.dh<-vegdist(spe.hel)
#Calculate single & ward minimum variance clustering
spe.ch.single<-hclust(spe.ch,method = "single")
plot(spe.ch.single)
spe.ch.ward <- hclust(spe.ch, method="ward.D2")
plot(spe.ch.ward)


#task3


#choose the right analytic method
print(decorana(t(spe)))
#Axis lengths belongs to 3.0~4.0,so choose RDA analysis
RDA<-rda(spe,env,scale=T)

#extract statistics
spe_rda<-data.frame(RDA$CCA$u[,1:2],rownames(env))
colnames(spe_rda)=c("RDA1","RDA2","samples")
score
spe_rda_score<-data.frame(RDA$CCA$v[,1:2])
#Calculate axle label data
RDA1=round(RDA$CCA$eig[1]/sum(RDA$CCA$eig)*100,2)
RDA2=round(RDA$CCA$eig[2]/sum(RDA$CCA$eig)*100,2)
plot(RDA)

# Hellinger pre-transformation
spe_hel <- decostand(spe, method = 'hellinger')

#use all env data
rda_tb <- rda(spe_hel~., env, scale = FALSE)

#drawing
plot(rda_tb)#finish