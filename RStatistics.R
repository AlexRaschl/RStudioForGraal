library(tidyverse)


#General Filepath settings
rm(list=ls())

mainPath <- "/home/urzidil/Programming/CSV/"
folderPath <- "DacapoFopNew/"
namePrefix <- "HotSpotGraalRuntime"

globalSuffix <- "_GLOBAL.csv"
allocSiteSuffix <-"_ALLOC_SITES.csv"
opDistrSuffix <- "_OP_DISTR.csv"
typeUsageSuffix <- "_MAIN_TYPES.csv"
sizesSuffix <- "_SIZE_CAP.csv"

globalPath <- paste(mainPath, folderPath, namePrefix, globalSuffix, sep="")
allocPath <- paste(mainPath, folderPath, namePrefix, allocSiteSuffix, sep="")
opDistrPath <- paste(mainPath, folderPath, namePrefix, opDistrSuffix, sep="")
typeUsagePath <- paste(mainPath, folderPath, namePrefix, typeUsageSuffix, sep="")
sizesNCapsPath <- paste(mainPath,folderPath, namePrefix, sizesSuffix, sep="")

#Fetch the dataFrames
globalInfo <- data.frame(read.csv(globalPath,header = TRUE, sep = ';'))
allocSites <- data.frame(read.csv(allocPath,header = TRUE, sep = ';'))
opDistrib <- data.frame(read.csv(opDistrPath,header = TRUE, sep = ';'))
typeUsage<- data.frame(read.csv(typeUsagePath, header = TRUE, sep = ';', stringsAsFactors = FALSE))
sizesNCaps <- data.frame(read.csv(sizesNCapsPath, header = TRUE, sep=';'))



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

#allocSitesAndTypes$Allocation.Sites[,1] <- as.character(as.numeric(allocSitesAndTypes$Allocation.Sites[,1]))
allocSitesFlattedTypes <- aggregate(allocSitesAndTypes$Main.Type ~ allocSitesAndTypes$Allocation.Sites, FUN = unique)

names(allocSitesFlattedTypes)[1] <- "AllocationSites"

#Sizes and Capacities 
sizesAndSites <- merge(allocSites, sizesNCaps, by= "Tracker")
sizesAndSitesSum <- aggregate(sizesAndSites$Size ~sizesAndSites$Allocation.Sites, FUN = sum)
capsAndSitesSum <- aggregate(sizesAndSites$CAPACITY ~sizesAndSites$Allocation.Sites, FUN = sum)
names(sizesAndSitesSum)[1]<-"sizesAndSites$Allocation.Sites"
sizesAndSites <- merge(sizesAndSitesSum, capsAndSitesSum, by= "sizesAndSites$Allocation.Sites")
names(sizesAndSites)[1]<-"AllocationSites"
names(sizesAndSites)[2]<-"Total Size"
names(sizesAndSites)[3]<-"Total Capacity"

fullStats <- merge(opSumForSites, trackerOccurrences)
names(fullStats)[1] <-"AllocationSites"
names(fullStats)[2] <-"Operation"
names(fullStats)[3] <-"Occurrences"
names(fullStats)[4] <-"Instances"

fullStats <-merge(fullStats, sizesAndSites, by= "AllocationSites")
fullStats <- mutate(fullStats, Global_LF = fullStats$`Total Size` / fullStats$`Total Capacity`)
fullStats <- merge(fullStats, allocSitesFlattedTypes, by= "AllocationSites")
names(fullStats)[8] <-"ContainedTypes"


write.csv2(fullStats[1:7], file=paste(mainPath, "test.csv", sep = ""))



#fullStats<- mutate(fullStats, ContainedTypes = paste(unlist(fullStats$ContainedTypes), collapse = ','))
#fullStats<- mutate_each(fullStats, unlist(fullStats$ContainedTypes[]))

#l <- fullStats$ContainedTypes[[10]]
#l[2]
#paste(unlist(fullStats$ContainedTypes[[10]]), collapse=',')
