library(ggplot2)

MTD <- vector("list", 25000)
MTD_T <- vector("list", 100)
MTDMeans <- vector ("list", 250)
totalMeans <- vector ("list", 9)
totalSD <- vector ("list", 9)
doAll <- 3

for (n in 1:doAll)
{
  #Choose proper beta directory
  if (n==3) betaString<-"High Beta"
  else if (n==1) betaString<-"Low Beta"
  else if (n==2) betaString<-"Normal Beta"
  doseString<-"Dose Level 3"
  setwd(paste0("/Users/jluu/Documents/Research Data/Current Run/", doseString, "/", betaString))
  
  for (k in 1:doAll)
  {
    #Choose proper P(N) directory 
    if (k==1) 
      setwd("./Low P(N)")
    else if (k==3)
      setwd("../High P(N)")
    else if (k==2)
      setwd("../Medium P(N)")
    
    fileList = list.files(path="Data/", pattern="*.csv")
    listSize<-length(fileList);
    currIteration <-1
    currScenario = 3*(n-1)+k
    
    for (i in 1:listSize)
    {
      #Current file being read
      currFile<-read.csv(paste0("Data/", fileList[i]))
      #Store all final MTD values - 25000 values
      for (j in 1:10)
      {
        MTD[[currIteration]] = currFile[j,2]
        currIteration <- currIteration + 1
      }
    }
    
    #Split up the vector of 25000 into 100 groups of size 250
    MTDSplit <- split(MTD, ceiling(seq_along(MTD)/250))

    for (j in 1:100)
    {
      #For each of the 100 groups, calculate the T value
      MTD_T[[j]] = unname(quantile(unlist(MTDSplit[[j]]), c(.90)) - quantile(unlist(MTDSplit[[j]]), c(.10)))
    }
    
    #Find total mean and standard deviation for this one scenario
    totalMeans[[currScenario]] <- mean(as.numeric(MTD_T))
    totalSD[[currScenario]] <- sd(as.numeric(MTD_T))
  }
}