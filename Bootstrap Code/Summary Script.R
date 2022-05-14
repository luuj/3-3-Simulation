library(ggplot2)

for (n in 1:3)
{
  #Choose proper beta directory
  if (n==3) betaString<-"High Beta"
  else if (n==1) betaString<-"Low Beta"
  else if (n==2) betaString<-"Normal Beta"
  doseString<-"Dose Level 8"
  setwd(paste0("/Users/jluu/Documents/Research Data/Current Run/", doseString, "/", betaString))
  
  for (k in 1:3)
  {
    #Choose proper P(N) directory and adjust limit for graphs
    if (k==1){setwd("./Low P(N)"); pString<-"Low P(N)"; upperLimit=0.3}
    else if (k==3){setwd("../High P(N)"); pString<-"High P(N)"; upperLimit=0.8}
    else if (k==2){setwd("../Medium P(N)"); pString<-"Medium P(N)"; upperLimit=0.5}
  
    #Create summary directory and set file paths
    dir.create("../../../SummaryData", showWarnings = FALSE)
    fileNameRaw<-paste0("Summary_Raw_",betaString, "_", pString, "_", doseString, ".csv")
    fileNameSummary<-paste0("../../../SummaryData/Summary_",betaString, "_", pString, "_", doseString, ".csv")
    fileList = list.files(path="Data/", pattern="*.csv")
    listSize<-length(fileList); decimalCount<-4
    
    #Variable declarations
    MTDStats = matrix(nrow=listSize+1, ncol=13, byrow=TRUE)
    DLTStats = TotalDLTStats = TPStats = PC1Stats = PC2Stats = PC3Stats = matrix(nrow=listSize+1, ncol=8, byrow=TRUE)
    OtherStats <- list(DLTStats, TotalDLTStats, TPStats, PC1Stats, PC2Stats, PC3Stats); totalOtherStats<-6;
    MTDRangeCount = matrix(nrow=listSize+2, ncol=6, byrow=TRUE)
    
    #Column name assignment
    colnames(MTDStats)<- c("Final MTD","Mean","Median","Min","Max","Q1","Q2","SD", "", "P(N)", "Beta", "Gamma", "File Name")
    colnames(OtherStats[[1]])<- c("Dose Levels Tested", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(OtherStats[[2]])<- c("Total DLTs", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(OtherStats[[3]])<- c("Total Patient Count", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(OtherStats[[4]])<- c("Patient count (Dose < .25)", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(OtherStats[[5]])<- c("Patient count (.25 <= Dose <= .35)", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(OtherStats[[6]])<- c("Patient count (> .35)", "Mean","Median","Min","Max","Q1","Q2","SD")
    colnames(MTDRangeCount)<- c("MTD Frequency", "MTD not found", "MTD < 0.15", "0.15 <= MTD < 0.25", "0.25 <= MTD <0.35", "MTD > 0.35")
    # png(paste0("../../../SummaryData/DLTGraph_",betaString, "_", pString, "_", doseString, ".png")) #DLT Graph initiation
    
    for (i in 1:listSize)
    {
      currFile<-read.csv(paste0("Data/", fileList[i]))

      #Calculate descriptive stats for all files
      MTDStats[i,]<-c("", round(mean(currFile[,2]),decimalCount), round(median(currFile[,2]),decimalCount), round(min(currFile[,2]),decimalCount), round(max(currFile[,2]),decimalCount), 
                      + round(quantile(currFile[,2], c(0.25)),decimalCount), round(quantile(currFile[,2], c(0.75)),decimalCount), round(sd(currFile[,2]),decimalCount), "", currFile[1,11], currFile[1,12], currFile[1,13], fileList[i])
      for (j in 1:totalOtherStats)
        OtherStats[[j]][i,]<-c("", round(mean(currFile[,j+2]),decimalCount), round(median(currFile[,j+2]),decimalCount), round(min(currFile[,j+2]),decimalCount), round(max(currFile[,j+2]),decimalCount), 
                      + round(quantile(currFile[,j+2], c(0.25)),decimalCount), round(quantile(currFile[,j+2], c(0.75)),decimalCount), round(sd(currFile[,j+2]),decimalCount))
      MTDRangeCount[i,]<- c("", sum(currFile[,2]==0), sum(currFile[,2]>0 & currFile[,2]<0.15), sum(currFile[,2]>=0.15 & currFile[,2]<0.25), sum(currFile[,2]>=0.25 & currFile[,2]<0.35), sum(currFile[,2]>=0.35))

      # DLT Probability Graph
      # if (i==1)
      #   plot(currFile[,9], currFile[,10], type="l", xlab="Dose Level", ylab="DLT Probability", ylim=c(0, 1), col='blue', main=paste0(betaString, ", ", pString))
      # else
      #   lines(currFile[,9], currFile[,10], col='blue')
    }

    # dev.off() #DLT Graph closed
    percentage <- function(x) { #Percentage function for MTDRangeCount
      (as.numeric(MTDRangeCount[listSize+1,x])/sum(as.numeric(MTDRangeCount[listSize+1,2:6]))) * 100
    }
    
    #Calculate overall stats
    MTDStats[listSize+1,]<-c("Overall", round(mean(as.numeric(MTDStats[,2]),na.rm=TRUE),decimalCount), round(mean(as.numeric(MTDStats[,3]),na.rm=TRUE),decimalCount), 
                             round(mean(as.numeric(MTDStats[,4]),na.rm=TRUE),decimalCount), round(mean(as.numeric(MTDStats[,5]),na.rm=TRUE),decimalCount), round(mean(as.numeric(MTDStats[,6]),na.rm=TRUE),decimalCount),
                             round(mean(as.numeric(MTDStats[,7]),na.rm=TRUE),decimalCount), round(mean(as.numeric(MTDStats[,8]),na.rm=TRUE),decimalCount), NA,NA,NA,NA,NA)
    MTDRangeCount[listSize+1,]<-c("Total", sum(as.numeric(MTDRangeCount[,2]), na.rm=TRUE), sum(as.numeric(MTDRangeCount[,3]), na.rm=TRUE), sum(as.numeric(MTDRangeCount[,4]), na.rm=TRUE), 
                     + sum(as.numeric(MTDRangeCount[,5]), na.rm=TRUE), sum(as.numeric(MTDRangeCount[,6]), na.rm=TRUE) )
    MTDRangeCount[listSize+2,]<-c("Percentage", percentage(2), percentage(3), percentage(4), percentage(5), percentage(6))
    for (j in 1:totalOtherStats)
      OtherStats[[j]][listSize+1,]<-c(paste0(betaString,", ",pString), mean(as.numeric(OtherStats[[j]][,2]),na.rm=TRUE), mean(as.numeric(OtherStats[[j]][,3]),na.rm=TRUE), mean(as.numeric(OtherStats[[j]][,4]),na.rm=TRUE), 
                     + mean(as.numeric(OtherStats[[j]][,5]),na.rm=TRUE), mean(as.numeric(OtherStats[[j]][,6]),na.rm=TRUE), mean(as.numeric(OtherStats[[j]][,7]),na.rm=TRUE),mean(as.numeric(OtherStats[[j]][,8]),na.rm=TRUE))
    
    #Save Stats
    currNum = 3*(n-1)+k
    assign(paste('SaveStats', currNum, sep=''), OtherStats)
    if (currNum==1)
    {
      combinedFinalMTD <- MTDStats[listSize+1,1:8]
      combinedRangeCount <- MTDRangeCount[listSize+2,]
    }
    else
    {
      combinedFinalMTD <- rbind(combinedFinalMTD, MTDStats[listSize+1,1:8])
      combinedRangeCount <- rbind(combinedRangeCount, MTDRangeCount[listSize+2,]) 
    }
    #assign(paste('FinalMTDStats', currNum, sep=''), MTDStats[listSize+1,1:8])
    
    # # Output to CSV files
    # # write.csv(MTDStats, file=fileNameRaw, row.names=FALSE, na="")
    # write.csv(rbind(MTDStats[listSize+1,1:8]), file=fileNameSummary, row.names=FALSE)
    # for (j in 1:totalOtherStats)
    # {
    #   # write.table(NULL, file=fileNameRaw, row.names=FALSE, na="", append=TRUE, sep=",")
    #   # write.table(OtherStats[[j]], file=fileNameRaw, row.names=FALSE, na="", append=TRUE, sep=",")
    #   write.table(NULL, file=fileNameSummary, na="", append=TRUE, sep=",")
    #   write.table(rbind(OtherStats[[j]][listSize+1,]), row.names=FALSE, file=fileNameSummary, na="", append=TRUE, sep=",")
    # }
    # # write.table(NULL, file=fileNameRaw, row.names=FALSE, na="", append=TRUE, sep=",")
    # write.table(NULL, file=fileNameSummary, na="", append=TRUE, sep=",")
    # # write.table(MTDRangeCount, file=fileNameRaw, row.names=FALSE, na="", append=TRUE, sep=",")
    # write.table(rbind(MTDRangeCount[listSize+1:2,]), row.names=FALSE, file=fileNameSummary, na="", append=TRUE, sep=",")

    #Output DLT Probability Graph - Gamma separated
    gammaLow<-read.csv(paste0("Data/", fileList[which.min(as.numeric(MTDStats[,10]))]))
    gammaHigh<-read.csv(paste0("Data/", fileList[which.max(as.numeric(MTDStats[,10]))]))
    png(paste0("../../../SummaryData/DLT_Graph_Gamma_",betaString, "_", pString, "_", doseString, ".png"))
    plot(gammaLow[,9], gammaLow[,10], type="l", xlab="Dose Level", ylab="DLT Probability", ylim=c(0, 1), col='blue', main=paste0(betaString, ", ", pString))
    lines(gammaHigh[,9], gammaHigh[,10], col='red')
    legend("topleft", legend=c("Gamma Low", "Gamma High"), col=c("blue", "red"), lty=1:1, cex=1.5)
    dev.off()

    # Output MTDRangeCount histogram
    # png(paste0("../../../SummaryData/MTD_Frequency_Histogram_",betaString, "_", pString, "_", doseString, ".png"))
    # barplot(as.numeric(MTDRangeCount[listSize+1,2:6]), ylim=c(0,150000), xlab="Ranges", ylab="Frequency", names.arg=c("NotFound", "<0.15", "0.15-0.25", "0.25-0.35", ">0.35"), main=paste0(betaString, ", ", pString, " MTD Frequency") )
    # dev.off()
    
    #boxplotlist[[k]]<-(OtherStats[[3]][,2])
    # if (k==1){
    #   png(paste0("../../../SummaryData/PatientBoxPlot_",betaString, "_", doseString, ".png"))
    #   boxplot(as.numeric(OtherStats[[3]][,2]), xlab="P(N) Level", ylab="Patient Count", at=k, xlim=c(0,4), ylim=c(8,20),main=paste0(betaString, ", ", doseString))
    # }
    # else if (k==2)
    #   boxplot(as.numeric(OtherStats[[3]][,2]), add=TRUE, at=k)
    # else{
    #   boxplot(as.numeric(OtherStats[[3]][,2]), add=TRUE, at=k)
    #   dev.off()
    # }
              
  }
}


# # Output patient data to CSV files

combinedRangeCount<- t(combinedRangeCount)
combinedFinalMTD <- t(combinedFinalMTD)

for (i in 1:6)
{
  statNum <- i
  
  Combined <- rbind (SaveStats1[[statNum]][listSize+1,],SaveStats2[[statNum]][listSize+1,], SaveStats3[[statNum]][listSize+1,], SaveStats4[[statNum]][listSize+1,], SaveStats5[[statNum]][listSize+1,],
         SaveStats6[[statNum]][listSize+1,],SaveStats7[[statNum]][listSize+1,], SaveStats8[[statNum]][listSize+1,], SaveStats9[[statNum]][listSize+1,])
  FinalArray <- t(Combined)

  if (i==1)
  {
    write.csv(rbind(FinalArray), file=fileNameSummary)
    write.table(NULL, file=fileNameSummary, na="", append=TRUE, sep=",")
    write.table(combinedFinalMTD, file=fileNameSummary, na="", append=TRUE, sep=",")
    write.table(NULL, file=fileNameSummary, na="", append=TRUE, sep=",")
    write.table(combinedRangeCount, file=fileNameSummary, na="", append=TRUE, sep=",")
  }
  else
  {
    write.table(NULL, file=fileNameSummary, na="", append=TRUE, sep=",")
    write.table(FinalArray, file=fileNameSummary, na="", append=TRUE, sep=",")
  }
}

