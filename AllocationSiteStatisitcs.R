library(data.table)
library(ggplot2)


#General Filepath settings
mainPath <- "/home/urzidil/Programming/CSV/"
nameHeader <-"HotSpotGraalRuntime"


#Class to search for
allocationSite <- "org.graalvm.compiler.asm.Label"

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

groupByOps <- function(data){
  result <- data.table(data)
  result<-result[order(data[,2], data[,1]),]
  return(result)
}

createBPlotForOp <- function(data, op){
  dataSubset <- subset(data, Operation == op)
  dataSubset = data.table(dataSubset, stringsAsFactors = FALSE)
  #boxplot(Occurrences~as.character(Operation), dataSubset, "Operation", ylab="Occurrences", main = paste("BPlot of", op, sep=" "), col=("green"))
  p <- ggplot(dataSubset, aes(x = as.character(Operation),y=Occurrences)) + geom_boxplot(outlier.color="red", outlier.shape = 8, outlier.size = 4)
#  p <- p +geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 1/75)
  return (p)
}

mainFunction <- function(){
  
  #Read File
  allocSites = read.csv(file=allocSiteFP, header=TRUE, sep=";")
  
  #Get only trackers in allocSite
  trackerList <- filterAllocSites(allocSites, allocationSite)
  trackerList <- trackerList[,1]
  
  #Fetch OpDistribution
  opDistr <- read.csv(file=opDistrFP, header=TRUE, sep=";")
  
  #Get OpDistribution of selected Trackers
  filteredDistr <- filterOpDistr(opDistr,trackerList)
  
  #Order results by Operation
  #orderedOps <- groupByOps(filteredDistr)
  
# print(orderedOps)

  p <- createBPlotForOp(opDistr[20: 214000,], "ADD_OBJ") + scale_y_log10()
  print(p)
}

mainFunction()
#rm(list=ls())
