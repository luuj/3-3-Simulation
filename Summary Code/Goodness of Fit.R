#Goodness of Fit test
library(plyr)
fileName <- "C:\\Users\\jluu\\Dropbox\\Research\\Groshen\\Thesis\\R Code\\GoF\\GoF_Test_Input.csv"
inputData <- read.csv(fileName, header=T)

final = c(count(inputData, "Final.MTD.Probability")$freq, rep(0, 2))
comparison = c(0.0262, 0.0907, 0.1774, 0.2103, 0.2053, 0.148, 0.0884, 0.0362, 0.0142, 0.0026, 0.0006, 0.0001)

chisq.test(x=final, p=comparison)