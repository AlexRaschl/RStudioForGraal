library(tidyverse)


#General Filepath settings
rm(list=ls())

mainPath <- "/home/urzidil/Programming/CSV/"
folderPath <- "DacapoStatistics/"
namePrefix <- "HotSpotGraalRuntime"

globalSuffix <- "_GLOBAL.csv"
allocSiteSuffix <-"_ALLOC_SITES.csv"
opDistrSuffix <- "_OP_DISTR.csv"
typeUsageSuffix <- "_MAIN_TYPES.csv"

globalPath <- paste(mainPath, folderPath, namePrefix, globalSuffix, sep="")
allocPath <- paste(mainPath, folderPath, namePrefix, allocSiteSuffix, sep="")
opDistrPath <- paste(mainPath, folderPath, namePrefix, opDistrSuffix, sep="")
typeUsagePath <- paste(mainPath, folderPath, namePrefix, typeUsageSuffix, sep="")

#Fetch the dataFrames
globalInfo <- data.frame(read.csv(globalPath,header = TRUE, sep = ';'))
allocSites <- data.frame(read.csv(allocPath,header = TRUE, sep = ';'))
opDistrib <- data.frame(read.csv(opDistrPath,header = TRUE, sep = ';'))
typeUsage<- data.frame(read.csv(typeUsagePath, header = TRUE, sep = ';', stringsAsFactors = FALSE))


#Do the data transformations
allocSitesAndDistrib <- merge(allocSites, opDistrib)

#Aggregate Operation Occurrences based on Sites
opSumForSites <- aggregate(allocSitesAndDistrib$Occurrences ~ allocSitesAndDistrib$Allocation.Sites + allocSitesAndDistrib$Operation, allocSitesAndDistrib, sum)

#Fetch Tracker nums for each site
#trackNums <-table(unlist(allocSites$Allocation.Sites))
#table(unlist(opDistrib$Operation))

trackerOccurrences <- data.frame(count(allocSites, allocSites$Allocation.Sites))
names(trackerOccurrences)[1] <- "allocSitesAndDistrib$Allocation.Sites"
names(trackerOccurrences)[2]<- "Instances"



#Add Main Types for trackers
allocSitesAndTypes <- merge(allocSites, typeUsage, by= "Tracker")[, 2:3]
is.factor(allocSitesAndTypes$Allocation.Sites)

#lev = levels(allocSitesAndTypes$Main.Type)

#allocSitesAndTypes$Allocation.Sites[,1] <- as.character(as.numeric(allocSitesAndTypes$Allocation.Sites[,1]))
allocSitesFlattedTypes <- aggregate(allocSitesAndTypes$Main.Type ~ allocSitesAndTypes$Allocation.Sites, FUN = unique)

#is.factor(allocSitesFlattedTypes$`allocSitesAndTypes$Main.Type`[[1]])
#as.numeric(allocSitesFlattedTypes$`allocSitesAndTypes$Main.Type`[[2,1]])
names(allocSitesFlattedTypes)[1] <- "allocSitesAndDistrib$Allocation.Sites"

fullStats <- merge(opSumForSites, trackerOccurrences)
fullStats <- merge(fullStats, allocSitesFlattedTypes)
names(fullStats)[1] <-"AllocationSites"
names(fullStats)[2] <-"Operation"
names(fullStats)[3] <-"Occurrences"
names(fullStats)[4] <-"Instances"
names(fullStats)[5] <-"ContainedTypes"



