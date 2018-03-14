opDistr = read.csv(file="/home/urzidil/Programming/CSV/OpDistr.csv", header=TRUE, sep=";")

len <- dim(opDistr)[1]

curr <- 0
begin <- 1
currData <- 0
currLbls <- 0

print(curr)

for(i in 1:len){
  if(opDistr$Tracker[i] == curr){

  } else {
    currData = opDistr[begin:i-1,]
    
    print(currData)
    curr <- curr +1 
    begin <- i+1
  }
}
begin <-begin-1
currData = opDistr[begin:len,]
print(currData)

