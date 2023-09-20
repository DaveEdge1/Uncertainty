#	Take in a 3-column data set
##	Year
##	Reconstruction (Proxy)
##	Target
#	Build confidence intervals (using 2/3 of overlap interval: 
#split calibration-verification AND leave-one-out)
##	Strict Empirical
##	MEboot
##	Common Bootstrap
# Repeat the building of confidence intervals by the above methods using 
#simulations of the reconstruction (eg. 1000 times)
#Test the confidence intervals in the test interval (1/3 overlap remaining)
inputDat <- read.csv("D:/Lab Backup/Uncertainty/TreeNobRecon.csv")
head(inputDat)
inputDat <- inputDat[complete.cases(inputDat),]
numDataEntries <- length(inputDat[,1])
startYear <- inputDat[1,1]
endYear <- inputDat[numDataEntries,1]
reconSummary <- summary(inputDat[,2])
targetSummary <- summary(inputDat[,3])

cat("**Please review your data**
\nColumn 1 Year - Your Data:", names(inputDat)[1],
"\nColum 2 Reconstruction (proxy) - Your Data:", names(inputDat)[2], 
"\nColumn 3 Target - Your Data:", names(inputDat)[3], 
"\nTotal length of complete data: ", numDataEntries, 
"\nStart Year: ", startYear, 
"\nEnd Year: ", endYear)

errFidelity90 <- vector(length=10000)
errFidelity50 <- vector(length=10000)
for (i in 1:10000){
  #Split off 10 data points to reserve for testing of confidence intervals
  
  trainingIndex <- sort(sample(1:numDataEntries, (numDataEntries-10)))
  
  testIndex <- (1:numDataEntries)[!(1:numDataEntries %in% trainingIndex)]
  
  trainingSet <- inputDat[trainingIndex,]
  testSet <- inputDat[testIndex,] 
  
  #Emprical Confidence Intervals in training interval
  trainErr90 <- sort(sqrt((trainingSet[,2] - trainingSet[,3])^2))[round(0.90*length(trainingIndex))]
  trainErr50 <- sort(sqrt((trainingSet[,2] - trainingSet[,3])^2))[round(0.50*length(trainingIndex))]
  
  #Emprical Confidence Intervals in test interval
  testErr90 <- sort(sqrt((testSet[,2] - testSet[,3])^2))[round(0.90*length(testIndex))]
  testErr50 <- sort(sqrt((testSet[,2] - testSet[,3])^2))[round(0.50*length(testIndex))]
  
  #Difference between training and test intervals
  errFidelity90[i] <- abs(trainErr90-testErr90)/trainErr90
  errFidelity50[i] <- abs(trainErr50-testErr50)/trainErr50
}
hist(errFidelity90)
hist(errFidelity50)
summary(errFidelity50)
summary(errFidelity90)
