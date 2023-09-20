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


# #load Rogen Blue Light RSFi data
# climDat3 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/RogenBluePlusTarget.csv")
# RogenBluechron <- read.rwl("C:/Users/dce72/Documents/GradSchool/Uncertainty/RogenBlueRWI.csv")
# climDat3 <- climDat3[complete.cases(climDat3),c(1,3)]
#
# RogenBluechron[!is.na(RogenBluechron)] <- (RogenBluechron[!is.na(RogenBluechron)] + 300)/100
#
# rwi.stats.running(RogenBluechron)

# rogenChron <- chron(RogenBluechron)
# cor(rogenChron$std[as.numeric(rownames(RogenBluechron)) %in% climDat3$Year], climDat3$JJA[climDat3$Year %in% as.numeric(rownames(RogenBluechron))])


#load cana113 TSME MXD data
climDat4 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/canaTempTarget.csv")
head(climDat4)
climDat4 <- climDat4[,-1]
names(climDat4) <- c("Year", "Target")
chron113 <- read.rwl("C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\SimpleBootTrials\\cana113.rwl")
length(climDat4$Year[climDat4$Year %in% as.numeric(rownames(chron113))])
mean(apply(chron113[as.numeric(rownames(chron113)) %in% climDat4$Year,], 1, function(x) sum(!is.na(x))))


#source("C:/Users/dce72/Documents/GradSchool/Uncertainty/R/fullTestingIntervalsNew.R")

test1slim <- fullTestingIntervalsNew(
    chronology = TNchron,
    climDat = climDat1,
    totalSims = 10,
    bootstappingMethod = "both",
    bootIts = 1000,
    predictionEnvelope = c(0.5, 0.9),
    corClim1=0,
    noiseFacor1=1,
    rbarADJ1=0,
    skipChron = FALSE,
    halfSampleDepth=TRUE)
saveRDS(test1slim, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test1slim.RData")
rm(test1slim)

test2slim <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1,
  rbarADJ1=0,
  skipChron = FALSE,
  halfSampleDepth=TRUE)
saveRDS(test2slim, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test2slim.RData")
rm(test2slim)

# test3 <- fullTestingIntervalsNew(
#   chronology = RogenBluechron,
#   climDat = climDat3,
#   totalSims = 10,
#   bootstappingMethod = "both",
#   bootIts = 1000,
#   predictionEnvelope = c(0.5, 0.9),
#   corClim1=0,
#   noiseFacor1=1,
#   rbarADJ1=0,
#   skipChron = FALSE)
# saveRDS(test3, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test3.RData")
# rm(test3)

test4slim <- fullTestingIntervalsNew(
  chronology = chron113,
  climDat = climDat4,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1,
  rbarADJ1=0,
  skipChron = TRUE,
  halfSampleDepth=FALSE)
saveRDS(test4slim, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test4slim.RData")
rm(test4slim)

test10 <- fullTestingIntervalsNew(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=-0.2,
  noiseFacor1=1,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test10, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test10.RData")
rm(test10)

test20 <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=-0.2,
  noiseFacor1=1,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test20, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test20.RData")
rm(test20)

# test30 <- fullTestingIntervalsNew(
#   chronology = RogenBluechron,
#   climDat = climDat3,
#   totalSims = 10,
#   bootstappingMethod = "both",
#   bootIts = 1000,
#   predictionEnvelope = c(0.5, 0.9),
#   corClim1=-0.2,
#   noiseFacor1=1,
#   rbarADJ1=0,
#   skipChron = TRUE)
# saveRDS(test30, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test30.RData")
# rm(test30)

test40 <- fullTestingIntervalsNew(
  chronology = chron113,
  climDat = climDat4,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=-0.2,
  noiseFacor1=1,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test40, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test40.RData")
rm(test40)

test11 <- fullTestingIntervalsNew(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1,
  rbarADJ1=1,
  skipChron = TRUE)
saveRDS(test11, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test11.RData")
rm(test11)

test21 <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1,
  rbarADJ1=1,
  skipChron = TRUE)
saveRDS(test21, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test21.RData")
rm(test21)

# test31 <- fullTestingIntervalsNew(
#   chronology = RogenBluechron,
#   climDat = climDat3,
#   totalSims = 10,
#   bootstappingMethod = "both",
#   bootIts = 1000,
#   predictionEnvelope = c(0.5, 0.9),
#   corClim1=0,
#   noiseFacor1=1,
#   rbarADJ1=1,
#   skipChron = TRUE)
# saveRDS(test31, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test31.RData")
# rm(test31)

test41 <- fullTestingIntervalsNew(
  chronology = chron113,
  climDat = climDat4,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1,
  rbarADJ1=1,
  skipChron = TRUE)
saveRDS(test41, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test41.RData")
rm(test41)

test12 <- fullTestingIntervalsNew(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1.5,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test12, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test12.RData")
rm(test12)

test22 <- fullTestingIntervalsNew(
  chronology = chron646,
  climDat = climDat2,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1.5,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test22, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test22.RData")
rm(test22)

# test32 <- fullTestingIntervalsNew(
#   chronology = RogenBluechron,
#   climDat = climDat3,
#   totalSims = 10,
#   bootstappingMethod = "both",
#   bootIts = 1000,
#   predictionEnvelope = c(0.5, 0.9),
#   corClim1=0,
#   noiseFacor1=1.5,
#   rbarADJ1=0,
#   skipChron = TRUE)
# saveRDS(test32, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test32.RData")
# rm(test32)

test42 <- fullTestingIntervalsNew(
  chronology = chron113,
  climDat = climDat4,
  totalSims = 10,
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5, 0.9),
  corClim1=0,
  noiseFacor1=1.5,
  rbarADJ1=0,
  skipChron = TRUE)
saveRDS(test42, file = "C:\\Users\\dce72\\Documents\\GradSchool\\Uncertainty\\FourChronTrialsSept4\\test42.RData")
rm(test42)
