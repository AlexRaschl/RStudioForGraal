library(tidyverse)
library(gtools)


#General Filepath settings
rm(list=ls())

#mainPath <- "/home/urzidil/Programming/CSV/"
mainPath <- "./CSVData/"

subPath <- "Aggregated/Dacapo/"
benchPath = paste(mainPath, subPath, sep = "")
folderPath <- "lusearch/"
namePrefix <- "HotSpotGraalRuntime"

globalSuffix <- "_GLOBAL.csv"
allocSiteSuffix <-"_ALLOC_SITES.csv"
opDistrSuffix <- "_OP_DISTR.csv"
typeUsageSuffix <- "_MAIN_TYPES.csv"
sizesSuffix <- "_SIZE_CAP.csv"

globalPath <- paste(mainPath, subPath, folderPath, namePrefix, globalSuffix, sep="")
allocPath <- paste(mainPath, subPath,folderPath, namePrefix, allocSiteSuffix, sep="")
opDistrPath <- paste(mainPath, subPath,folderPath, namePrefix, opDistrSuffix, sep="")
typeUsagePath <- paste(mainPath, subPath,folderPath, namePrefix, typeUsageSuffix, sep="")
sizesNCapsPath <- paste(mainPath, subPath, folderPath, namePrefix, sizesSuffix, sep="")


createFullStats <- function(){
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
  
  #lev = levels(allocSitesAndTypes$Main.Type)
  
  #allocSitesAndTypes$Allocation.Sites[,1] <- as.character(as.numeric(allocSitesAndTypes$Allocation.Sites[,1]))
  allocSitesFlattedTypes <- aggregate(allocSitesAndTypes$Main.Type ~ allocSitesAndTypes$Allocation.Sites, FUN = unique)
  
  #is.factor(allocSitesFlattedTypes$`allocSitesAndTypes$Main.Type`[[1]])
  #as.numeric(allocSitesFlattedTypes$`allocSitesAndTypes$Main.Type`[[2,1]])
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
  
  #paste(fullStats$ContainedTypes[10], collapse=',')
  #fullStats<- mutate(fullStats, ContainedTypes = paste(fullStats$ContainedTypes, collapse = ', '))
  
  #write.csv2(fullStats[1:7], file=paste(mainPath, "test.csv", sep = ""))
  return(fullStats)
}



createMergableStat <- function(benchPath, suite){
  path = paste(benchPath, suite, namePrefix, sep="")
  
  allocSites <- data.frame(read.csv(paste(path, allocSiteSuffix, sep=""),header = TRUE, sep = ';'))
  opDistrib <- data.frame(read.csv(paste(path, opDistrSuffix, sep=""),header = TRUE, sep = ';'))
  sizesNCaps <- data.frame(read.csv(paste(path, sizesSuffix, sep=""), header = TRUE, sep=';'))
  
  allocSitesAndDistrib <- merge(allocSites, opDistrib)
  sizesNSites <- merge(allocSitesAndDistrib, sizesNCaps)
  
  result <- sizesNSites[,2:6]
  
  return(result)
}

createMergeOf <- function(data1, data2){
  
  result <- merge(data1, data2, by= c("Allocation.Sites", "Operation"))
  result <- mutate(result, Occurrences = result$Occurrences.x + result$Occurrences.y)
  result <- mutate(result, Size = result$Size.x + result$Size.y)
  result <- mutate(result, CAPACITY= result$CAPACITY.x + result$CAPACITY.y)
  result<- result[, !names(result) %in% c("Occurrences.x","Occurrences.y","Size.x","Size.y","CAPACITY.x","CAPACITY.y")]
}

mergeAllSuites <-function(){
  suites <- c("avrora/", "batik/", "fop/", "h2/", "jython/", "luindex/", "lusearch/", "pmd/", "sunflow/", "xalan/")
  result = createMergableStat(benchPath, suites[1])
  
  for(i in 2:10){
    result = createMergeOf(result, createMergableStat(benchPath, suites[i]))
  }
  result <- mutate(result, Global_LF = result$Size / result$CAPACITY)
  
 return(result)
}

result <- mergeAllSuites()

#fullStats <- createFullStats()






