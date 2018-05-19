library(tidyverse)


#General Filepath settings
rm(list=ls())

mainPath <- "/home/urzidil/Programming/CSV/"
folderPath <- "DacapoStatistics/"
namePrefix <- "HotSpotGraalRuntime"

globalSuffix <- "_GLOBAL.csv"
allocSiteSuffix <-"_ALLOC_SITES.csv"
opDistrSuffix <- "_OP_DISTR.csv"

globalPath <- paste(mainPath, folderPath, namePrefix, globalSuffix, sep="");
allocPath <- paste(mainPath, folderPath, namePrefix, allocSiteSuffix, sep="");
opDistrPath <- paste(mainPath, folderPath, namePrefix, opDistrSuffix, sep="");

#Fetch the dataFrames
globalInfo <- data.frame(read.csv(globalPath,header = TRUE, sep = ';'))
allocSites <- data.frame(read.csv(allocPath,header = TRUE, sep = ';'))
opDistrib <- data.frame(read.csv(opDistrPath,header = TRUE, sep = ';'))


#Do the data transformations
merged <- merge(allocSites, opDistrib)

#Sort by allocation Site
allocSorted <- merged[order(allocSites$Allocation.Sites),2:4]
#allocSorted$Occurrences<- as.numeric(as.character(allocSorted$Occurrences))

#Aggregate Operation Occurrences based on Sites
#aggregated <- aggregate(allocSorted$Occurrences ~ allocSorted$Allocation.Sites + allocSorted$Operation, allocSorted, sum)
aggregated <- aggregate(merged$Occurrences ~ merged$Allocation.Sites + merged$Operation, merged, sum)

#aggregated <- aggregated[order(aggregated$`allocSorted$Allocation.Sites`),]

#Fetch Tracker nums for each site
#trackNums <-table(unlist(allocSites$Allocation.Sites))
#table(unlist(opDistrib$Operation))

trackerOccurrences <- data.frame(count(allocSites, allocSites$Allocation.Sites))
names(trackerOccurrences)[1] <- "merged$Allocation.Sites"
names(trackerOccurrences)[2]<- "Instances"

fullStats <- merge(aggregated, trackerOccurrences)


