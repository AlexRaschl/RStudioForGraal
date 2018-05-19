library(tidyverse)


#General Filepath settings
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

allocSorted <- merged[order(allocSorted$Allocation.Sites),2:4]
allocSorted$Occurrences<- as.numeric(as.character(allocSorted$Occurrences))
is.numeric(allocSorted$Occurrences)
is.factor(allocSorted$Occurrences)

aggregated <- aggregate(allocSorted$Occurrences ~ allocSorted$Allocation.Sites + allocSorted$Operation, allocSorted, sum)
aggregated <- aggregated[order(aggregated$`allocSorted$Allocation.Sites`),]
