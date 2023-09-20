#clear workspace
rm(list=ls())
gc()
#load package(s)
require(dplR)
require(forecast)
devtools::load_all("C:/Users/dce72/Documents/GradSchool/Uncertainty")


#load Tree Nob detrended ring width data
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]
TNchron <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]
tnMVChron <- chron(TNchron)
length(climDat1$Year[climDat1$Year %in% as.numeric(rownames(tnMVChron))])
#average sample depth in chronology-target overlap period
mean(apply(TNchron[as.numeric(rownames(TNchron)) %in% climDat1$Year,], 1, function(x) sum(!is.na(x))))


#load ca646 Blue Oak detrended ring width data
climDat2 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/ca646PrecipTarget.csv")
climDat2 <- climDat2[,-1]
names(climDat2) <- c("Year", "Target")
chron646 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\ca646rwi.csv")
length(climDat2$Year[climDat2$Year %in% as.numeric(rownames(chron646))])
#average sample depth in chronology-target overlap period
mean(apply(chron646[as.numeric(rownames(chron646)) %in% climDat2$Year,], 1, function(x) sum(!is.na(x))))

#gather tree and core ids for rwi stats
ca646IDs <- data.frame(matrix(data=NA, ncol = 2, nrow = ncol(chron646)))
for(i in 1:ncol(chron646)){
  ca646IDs[i,1] <- substr(colnames(chron646)[i],start = 1, stop = 5)
  ca646IDs[i,2] <- substr(colnames(chron646)[i],start = 6, stop = 6)
}
uniqueTree <- unique(ca646IDs[,1])
for(i in 1:length(uniqueTree)){
  ca646IDs[ca646IDs[,1] %in% uniqueTree[i],1] <- i
}
uniqueCore <- unique(ca646IDs[,2])
for(i in 1:length(uniqueCore)){
  ca646IDs[ca646IDs[,2] %in% uniqueCore[i],2] <- i
}
ca646IDs <- apply(ca646IDs, 2, function(x) as.numeric(x))
colnames(ca646IDs) <- c("tree", "core")
ca646IDs <- as.data.frame(ca646IDs)
rwi.stats(chron646, ids = ca646IDs)

uniqueTree <- unique(substr(colnames(chron646),start = 1, stop = 5))
chron646[,which(substr(colnames(chron646),start = 1, stop = 5) %in% uniqueTree[seq(1,length(uniqueTree), by=2)])]



#load cana113 TSME MXD data
climDat4 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/canaTempTarget.csv")
head(climDat4)
climDat4 <- climDat4[,-1]
names(climDat4) <- c("Year", "Target")
chron113 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113.rwl")
length(climDat4$Year[climDat4$Year %in% as.numeric(rownames(chron113))])
mean(apply(chron113[as.numeric(rownames(chron113)) %in% climDat4$Year,], 1, function(x) sum(!is.na(x))))


TreeNobSimTest <- fullTestingIntervalsNew(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 100,
  bootstappingMethod = "both",
  bootIts = 100,
  predictionEnvelope = c(0.5,0.9),
  skipChron=TRUE,
  halfSampleDepth=0.25)
saveRDS(TreeNobSimTest, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobSimTest.RData")
rm(TreeNobSimTest)

TreeNobTest <- fullTestingIntervalsNew(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 0,
  bootstappingMethod = "both",
  bootIts = 100,
  predictionEnvelope = c(0.5,0.9),
  skipChron=FALSE,
  halfSampleDepth=0)
saveRDS(TreeNobTest, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\TreeNobTest.RData")
rm(TreeNobTest)

ca646SimTest <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 100,
  bootstappingMethod = "both",
  bootIts = 100,
  predictionEnvelope = c(0.5,0.9),
  skipChron=TRUE,
  halfSampleDepth=0.25)
saveRDS(ca646SimTest, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646SimTest.RData")
rm(ca646SimTest)

ca646Test <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 0,
  bootstappingMethod = "both",
  bootIts = 100,
  predictionEnvelope = c(0.5,0.9),
  skipChron=FALSE,
  halfSampleDepth=0)
saveRDS(ca646Test, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\ca646Test.RData")
rm(ca646Test)

cana113SimTest <- fullTestingIntervalsNew(
    chronology = chron113,
    climDat = climDat4,
    totalSims = 100,
    bootstappingMethod = "both",
    bootIts = 100,
    predictionEnvelope = c(0.5,0.9),
    skipChron=TRUE,
    halfSampleDepth=0.25)
saveRDS(cana113SimTest, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113SimTest.RData")
rm(cana113SimTest)

cana113test <- fullTestingIntervalsNew(
  chronology = chron113,
  climDat = climDat4,
  totalSims = 0,
  bootstappingMethod = "both",
  bootIts = 100,
  predictionEnvelope = c(0.5,0.9),
  skipChron=FALSE,
  halfSampleDepth=0)
saveRDS(cana113test, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\cana113test.RData")
rm(cana113test)



