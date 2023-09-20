#clear workspace
rm(list=ls())
gc()
#load package(s)
require(dplR)
require(forecast)

source("C:/Users/dce72/Documents/GradSchool/Uncertainty/fullTestingIntervals.R")

#load data
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]
TNchron <- read.rwl("D:/Lab Backup/Geoduck/Tree Nob/TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]

TreeNobTest <- fullTestingIntervals(
  chronology = TNchron,
  climDat = climDat1,
  totalSims = 50,
  simulationInterval = c(0.95,0.99),
  bootstappingMethod = "both",
  bootIts = 1000,
  predictionEnvelope = c(0.5,0.9)
)

#TreeNobTest[[24]]$runDetails

cT1 <- TreeNobTest[[1]]$CaptureSummary
cT2 <- TreeNobTest[[2]]$CaptureSummary
cT3 <- TreeNobTest[[23]]$CaptureSummary
cT4 <- TreeNobTest[[24]]$CaptureSummary

#lapply(TreeNobTest, function(x) x$runDetails)[seq(24,44,2)]
ME5 <- seq(3,22,2)
ME9 <- seq(4,22,2)
trad5 <- seq(25,44,2)
trad9 <- seq(26,44,2)

a1 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(TreeNobTest, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
           "ErrTheory" <- c(a1,b1,c1,d1),
           "ErrEmp" <- c(a2,b2,c2,d2),
           "ChronoConfEmp" <- c(a3,b3,c3,d3),
           "ChronoConfTheory" <- c(a4,b4,c4,d4))

summaryDat1 <- rbind.data.frame(summaryDat1,cT1,cT2,cT3,cT4)
colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
rownames(summaryDat1) <- c("simME50", "simME90", "simTrad50", "simTrad90","realME50", "realME90", "realTrad50", "realTrad90")
View(summaryDat1)

write.csv(summaryDat1,"C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/TreeNobResultsAug25.csv")

head(bootChrons1$confInt)
median(capVals$calibDat[,8,])
median(capVals$calibDat[,7,])

resids <- capVals$reconDat$reconstruction - capVals$reconDat$Target

residStruct <- auto.arima(resids)
shapiro.test(resids)
hist(resids)
