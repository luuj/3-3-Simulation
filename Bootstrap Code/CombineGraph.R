#Takes 9 category values forr the 7 different dose levels and graphs them

currStat <- "patient25"
excelFile <- read.table(paste0(currStat,".csv"), sep=",")
doseCount <- c(3:9)
png(paste0(currStat,".png"))

plot(doseCount, excelFile[1,], type="l", xlab="Max Dose Level", ylab="Patient Count where Final DLT Probability < 0.25", ylim=c(3,29), col="green")
lines(doseCount, excelFile[2,], type="l", col="blue")
lines(doseCount, excelFile[3,], type="l", col="red")

lines(doseCount, excelFile[4,], type="l", lty=2, col="green")
lines(doseCount, excelFile[5,], type="l", lty=2, col="blue")
lines(doseCount, excelFile[6,], type="l", lty=2, col="red")

lines(doseCount, excelFile[7,], type="l", lty=6, col="green")
lines(doseCount, excelFile[8,], type="l", lty=6, col="blue")
lines(doseCount, excelFile[9,], type="l", lty=6, col="red")

legend("topright", 
       legend=c("Low P(N)","Medium P(N)", "High P(N)", "Low Beta", "Medium Beta", "High Beta"), 
       col=c('green','blue', 'red', rep("black",3)),
       lty=c(1,1,1,1,2,6), ncol=2, cex=0.8 )

dev.off()