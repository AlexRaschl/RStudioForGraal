#General Filepath settings
mainPath <- "/home/urzidil/Programming/CSV/StatisticsSimpleTest/"
nameHeader <-"StatisticsSimpleTest"

#Class to search for
allocationSite <- "org.graalvm.collections.test.list.statistics.StatisticsSimpleTest"

#Files to search in
allocSiteFP <- paste(mainPath, nameHeader, "AllocSites.csv", sep="")
opDistrFP <- paste(mainPath, nameHeader,"OpDistr.csv", sep="")
typeOpDistrFP <- paste(mainPath,nameHeader, "TypeOpDistr.csv", sep="")



#________________Function definitions______________________

#Get rows with searched allocationSite
filterAllocSites <- function(allocSites, toSearch){
  trackerList <- allocSites[grep(allocationSite, allocSites$Allocation.Sites),]
  return (trackerList)
}

#Get rows containing information about given trackers
filterOpDistr <- function(opDistr, trackerList){
  filteredTable <- opDistr[apply(opDistr, 1, function(r) any(r %in% trackerList)), ]
  return (filteredTable)
}


mainFunction <- function(){
  
  #Read File
  allocSites = read.csv(file=allocSiteFP, header=TRUE, sep=";")
  
  trackerList <- filterAllocSites(allocSites, allocationSite)
  trackerList <- trackerList[,1]
  
  
  opDistr <- read.csv(file=opDistrFP, header=TRUE, sep=";")
  filteredDistr <- filterOpDistr(opDistr,trackerList)
  
  str(filteredDistr)
  print(class(filteredDistr$Operation))
  print(summary(filteredDistr))
 
}

mainFunction()
rm(list=ls())
