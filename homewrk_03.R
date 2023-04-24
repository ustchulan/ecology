rm(list=ls())  #clean space

library(ade4)
library(tidyverse)
data(doubs)

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
# write code for clusters and answer: In terms of the fish community composition, which groups of species can you identify? Which groups of species are related to these groups of sites?
# Transpose matrix of species abundances

# Cluster analysis of environmental variables based on loci
env.z.hel<-decostand(env,method="hellinger") 
env.z.dhel<- vegdist(env.z.hel,method = "euclidean") 
env.z.dhel.complete<-hclust(env.z.dhel,method = "complete")  
plot(env.z.dhel.complete,hang=-1) 

# Cluster analysis of fish species based on loci
fish.hel<-decostand(fish,method="hellinger") 
fish.dhel<- vegdist(fish.hel,method = "euclidean")
fish.dhe1.complete<-hclust(fish.dhel,method = "complete") 
plot(fish.dhe1.complete,hang=-1) 

#Cluster analysis of different fish species
fish.t<-t(fish) 
fish.chi.t<-decostand(fish.t,method="chi.square") 
fish.chi.t.d<- vegdist(fish.chi.t,method = "euclidean") 
fish.chi.t.d.complete <- hclust(fish.chi.t.d,method = "complete") 
plot(fish.chi.t.d.complete,hang=-1) 


#task3
# Do RDA analysis, and then write code and answer: 
# Which environmental variables cause a community to vary across a landscape?

#means cluster analysis of site
fish.de <- vegdist(scale(fish), "euc")
fish.kmeans <- kmeans(fish.de, centers = 4, nstart = 100)
fish.kmeans.g <-fish.kmeans$cluster
env.de <- vegdist(scale(env), "euc")
env.kmeans <- kmeans(env.de, centers = 4, nstart = 100)
env.kmeans.g <- env.kmeans$cluster
fviz_cluster(fish.kmeans,data = fish)
fviz_cluster(env.kmeans,data = env)
table(fish.kmeans.g, env.kmeans.g)
fisher.test(table(fish.kmeans.g, env.kmeans.g))

#Do RDA analysis, and then write code and answer: 
#Which environmental variables cause a community to vary across a landscape?
fish.hel <- decostand(fish, "hellinger")  
fish.rda <- rda(fish.hel ~ ., env)
summary(fish.rda)
plot(fish.rda)
