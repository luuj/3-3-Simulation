##############RUN THIS CODE##############
##Set appropriate base file directory
baseDirectory <- c("/Users/jluu", "C:/Users/Jonathan")
folderDirectory <- paste(baseDirectory[1], "/Dropbox/Research/Groshen/R Code/Run", sep="")
doseDirectory <- list.files(folderDirectory)

#Function to retrieve full file path 
getFilePath <- function(doseLevel){
  c <- paste(folderDirectory, doseLevel, sep="/")
  fullPathList <- list()
  
  fullPathList <- lapply(list.files(c), function(x){
    fullPath = file.path(c, x)
  })
  return(fullPathList)
}
#########################################




##############Summary Statistics Section##############
#Set summary statistic file name and current outcomes of interest
fileName = paste(baseDirectory[2], "/Dropbox/Research/Groshen/R Code/SummaryStatistics.csv", sep="")
currOutcome = c("Final.MTD.Probability", "Dose.levels.tested", "DLT.count", "Total.patient.count", "Missing")

#Get descriptive statistics and output to fileName
write.csv(NULL, file=fileName)

for (curr_Outcome in currOutcome){
  lapply(doseDirectory, function(i){
    currDirectory <- getFilePath(i)
    r <- sapply(currDirectory, function(CD){
      currFile <- read.csv(as.character(CD), header=T)
      
      #Get count and proportion for missing categorical variable
      if (curr_Outcome=="Missing"){
        results = format(data.frame(count=sum(currFile["Final.MTD.Probability"]==0), proportion=sum(currFile["Final.MTD.Probability"]==0)/nrow(currFile["Final.MTD.Probability"])),digits=2)
        return(results)
      }
      
      #Get mean, sd, median, q1, q3, min, and max given a numeric vector
      outcome=currFile[[curr_Outcome]]
      results <- data.frame(mean=mean(outcome), sd=sd(outcome), median=median(outcome), q1=quantile(outcome, probs=0.25), q3=quantile(outcome, probs=0.75), min=min(outcome), max=max(outcome))
      format(results, digits=2)
    })

    colnames(r) = sub(".csv", "", basename(as.character(currDirectory)))
    write.table(paste(curr_Outcome, i), file=fileName, append=T, col.names = F, row.names = F)
    write.table(r, file=fileName, append=T, sep=",", col.names=NA)
    write.table(NULL, file=fileName, row.names=F, append=T, sep=",")
  })
}
########################################################




##############Graphing Section##############
#Get DLT probability graphs and histogram
library(ggplot2)
library(reshape2)

createGraph <- function(typeGraph){
  lapply(doseDirectory, function(DD){
    currDirectory <- getFilePath(DD)
    graphData = data.frame()
    
    #Combine data-sets with appropriate categorical variables
    for (CD in currDirectory){
      currFile <- read.csv(as.character(CD), header=T)
      categoryType = sub("_", ", ", sub(".csv", "", basename(CD)))
      
      #Select which graph you want to create
      if(typeGraph=="Scatter_FinalMTD"){
        graphMelt = melt(currFile[12:length(currFile)], measure.vars=colnames(currFile[12:length(currFile)]))
        graphMelt = cbind(graphMelt, categoryType)
        graphData = rbind(graphData, graphMelt)
      }
      else if (typeGraph == "Histogram_FinalMTD"){
        index <- apply(currFile, 1, function(x){
          if (x[["Final.MTD.Probability"]] == 0)
            return(0)
          i = match(x[["Final.MTD.Probability"]], x[12:length(currFile)])
        })
        
        graphMelt = cbind(index, categoryType)
        graphData = rbind(graphData, graphMelt)
      }
      else if (typeGraph == "Histogram_DL"){
        index <- apply(currFile, 1, function(x){
          if (x[["Final.MTD.Probability"]] == 0)
            return(0)
          i = match(x[["Final.MTD.Probability"]], x[12:length(currFile)])
        })
        
        graphMelt = do.call("cbind.data.frame", list(MTD.level=index, dlt=currFile[["Dose.levels.tested"]], categoryType=categoryType))
        graphData = rbind.data.frame(graphData, graphMelt)
      }
      else if (typeGraph == "Histogram_DLT"){
        index <- apply(currFile, 1, function(x){
          if (x[["Final.MTD.Probability"]] == 0)
            return(0)
          i = match(x[["Final.MTD.Probability"]], x[12:length(currFile)])
        })
        
        graphMelt = do.call("cbind.data.frame", list(MTD.level=index, dlt=currFile[["DLT.count"]], categoryType=categoryType))
        graphData = rbind.data.frame(graphData, graphMelt)
      }
      else if (typeGraph =="Box_Patient"){
        graphMelt = cbind.data.frame(ptCount=currFile[["Total.patient.count"]], categoryType=categoryType)
        graphData = rbind.data.frame(graphData, graphMelt)
      }
    }
    
    graphData$categoryType = factor(graphData$categoryType, levels(graphData$categoryType)[c(1,3,2,7,9,8,4,6,5)])

    #Plot the curves
    if (typeGraph=="Scatter_FinalMTD")
      combinedPlot = ggplot(graphData, aes(x=variable, y=value)) + geom_jitter(width=.5, shape=23) + labs(x="Dose Level", y="DLT Probability",  title=paste("DLT Probability Curve - ", DD, sep="")) + ylim(0,0.8)
    else if (typeGraph=="Histogram_FinalMTD")
      combinedPlot = ggplot(graphData, aes(x=index)) + geom_bar() + labs(x="Dose Level", y="Frequency", title=paste("Final MTD Histogram - ", DD, sep="")) 
    else if (typeGraph=="Histogram_DL"){
      graphData[["MTD.level"]] = as.factor(graphData[["MTD.level"]])
      combinedPlot = ggplot(graphData, aes(x=dlt)) + geom_bar(aes(fill=MTD.level), width = 0.5) + labs(x="Highest dose level tested", y="Frequency", title=paste("Highest Dose Level Tested Histogram - ", DD, sep="")) + scale_x_continuous(breaks=0:max(graphData[["dlt"]]))
    }
    else if (typeGraph=="Histogram_DLT"){
      graphData[["MTD.level"]] = as.factor(graphData[["MTD.level"]])
      combinedPlot = ggplot(graphData, aes(x=dlt)) + geom_bar(aes(fill=MTD.level), width = 0.5) + labs(x="Number of DLTs", y="Frequency", title=paste("Number of DLTs Histogram - ", DD, sep="")) + scale_x_continuous(breaks=0:max(graphData[["dlt"]]))
    }
    else if (typeGraph == "Box_Patient"){
      return(ggplot(graphData, aes(categoryType, ptCount)) + geom_boxplot(fill='#A4A4A4', color="black", fatten=3) + coord_flip() +labs(y="Total number of patients", x="Beta x P(D) category", title=paste("Total number of patients boxplot - ", DD, sep="")))
    }
    
    combinedPlot + facet_wrap(~categoryType, ncol=3)
  })
}

createGraph("Scatter_FinalMTD")
createGraph("Histogram_FinalMTD")
createGraph("Histogram_DL")
createGraph("Histogram_DLT")
createGraph("Box_Patient")

