#clear workspace
rm(list=ls())
gc()
#load package(s)
require(dplR)
require(forecast)


#load data
climDat1 <- read.csv("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
climDat1 <- climDat1[climDat1$Year < 2002,]
TNchron <- read.rwl("D:/Lab Backup/Geoduck/Tree Nob/TreeNobRCStrunc1.csv")
TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron <- TNchron[,apply(TNchron, 2, function(x) sum(!is.na(x)))>0]


# chronology = TNchron
# climDat = climDat1
# totalSims = 5
# bootstappingMethod = "both"
# bootIts = 1000
# predictionEnvelope = c(0.5, 0.9)
# corClim1=0
# noiseFacor1=1
# rbarADJ1=0
# skipChron=FALSE



fullTestingIntervals <- function(
    chronology = TNchron,
    climDat = climDat1,
    totalSims = 5,
    bootstappingMethod = "both",
    bootIts = 1000,
    predictionEnvelope = c(0.5, 0.9),
    corClim1=0, 
    noiseFacor1=1, 
    rbarADJ1=0,
    skipChron=FALSE){
  
  
  
  #load special functions
  source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronSimul.R")
  source("C:/Users/dce72/Documents/GradSchool/Uncertainty/interpIndex.R")
  source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronBoot.R")
  source("C:/Users/dce72/Documents/GradSchool/Uncertainty/predIntTest.R")
  
  
  if (skipChron==FALSE){
    totalChrons <- c("chronology",rep("sim1",totalSims))
  }else{
    totalChrons <- rep("sim1",totalSims)
  }
  
  numStraps <- bootstappingMethod
  if (bootstappingMethod == "both"){
    numStraps <- c("ME","trad")
  }
  
  intervalsTesting <- list()
  
  numtotalRuns <- length(numStraps) * length(totalChrons) * length(predictionEnvelope)
  totalRuns <- 1
  
  for (i in numStraps){
    #i=numStraps[1]
    for (j in totalChrons){
      #j=totalChrons[1]
      for (k in predictionEnvelope){
        #k=predictionEnvelope[1]
        if (j == "sim1"){
          
          #build synthetic chronology based on the climate target

          simul1<- chronSim(rwiDat=chronology, climDat=climDat1, corClim=corClim1, noiseFacor=noiseFacor1, rbarADJ=rbarADJ1, maxAtts=10000)
          
          #bootstrap the chronogy
          bootChrons1 <- chronBoot(chron1=simul1$synChron, numIts = bootIts, boot = i, envelope=k)
        }else{
          #bootstrap the chronogy
          bootChrons1 <- chronBoot(chron1=eval(as.name(j)), numIts = bootIts, boot = i, envelope=k)
        }
        
        #format data for prediction interval testing
        start1 <- max(c(min(as.numeric(rownames(bootChrons1$confInt))),min(climDat1[,1])))
        end1 <- min(c(max(as.numeric(rownames(bootChrons1$confInt))),max(climDat1[,1])))
        
        reconDat1 <- data.frame("Year" = as.numeric(rownames(bootChrons1$confInt)),
                                "Target" = rep(NA, length(bootChrons1$confInt$X2)))
        reconDat1[reconDat1[,1] %in% climDat1[,1], 2] <- climDat1[climDat1[,1] %in% reconDat1[,1],2]
        reconDat1 <- reconDat1[reconDat1$Year %in% start1:end1,]
        
        reconDat2 <- cbind(reconDat1[reconDat1$Year %in% as.numeric(rownames(bootChrons1$confInt)),],
                           bootChrons1$confInt[as.numeric(rownames(bootChrons1$confInt)) %in% reconDat1$Year,])
        
        #Test regression assumptions
        regModel1 <- lm(reconDat2[,2]~reconDat2[,4])
        normTest <- shapiro.test(regModel1$residuals)$p.value
        residACF <- acf(regModel1$residuals, plot = FALSE)$acf[2]
        
        library(lmtest)
        varTrendTest <- gqtest(reconDat2[,2]~reconDat2[,4])$p.value
        
        regAssumpt <- list("residsNotNorm" = normTest,
                           "residAR1" = residACF,
                           "residsHeteroscedastic" = varTrendTest)
        
        #build reconstruction and associated prediction intervals, test those intervals
        capVals <- predIntTest(reconDat=reconDat2, envelope=k)
        
        #Look at the average capture 
        capDat1 <- unlist(lapply(capVals$Captures, function(x) mean(x)))
        
        runDetails <- list("BootstMethod" = i,
                           "Chronology" = j,
                           "Prediction Interval" = k)
        
        if (j == "sim1"){
          intervalsTesting[[totalRuns]] <- list("runDetails" = runDetails,
                                                "CaptureSummary" = capDat1,
                                                "FullData" = capVals,
                                                "synchron" = simul1,
                                                "regAssumpt" = regAssumpt)
        }else{
          intervalsTesting[[totalRuns]] <- list("runDetails" = runDetails,
                                                "CaptureSummary" = capDat1,
                                                "FullData" = capVals,
                                                "regAssumpt" = regAssumpt)
        }
        
        
        cat("\nRun ", totalRuns, " of ", numtotalRuns,  " complete.\n",
            "Run Settings: \n",
            "  Bootstrapping Method: ", i, "\n",
            "  Chronology: ", j, "\n",
            "  Prediction Interval: ", k, "\n",
            "Prediction interval capture: ", capDat1, "\n")
        
        totalRuns <- totalRuns + 1
      }
    }
  }
  return(intervalsTesting)
}

testRet <- fullTestingIntervals(
    chronology = TNchron,
    climDat = climDat1,
    totalSims = 1,
    bootstappingMethod = "ME",
    bootIts = 1000,
    predictionEnvelope = c(0.5),
    corClim1=-0.1, 
    noiseFacor1=1, 
    rbarADJ1=0,
    skipChron = FALSE)
testRet <- fullTestingIntervals(
  #chronology = NULL,
  climDat = climDat1,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 1000,
  predictionEnvelope = c(0.5),
  corClim1=0.1, 
  noiseFacor1=1, 
  rbarADJ1=0,
  skipChron = TRUE)
testRet <- fullTestingIntervals(
  #chronology = NULL,
  climDat = climDat1,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 1000,
  predictionEnvelope = c(0.5),
  corClim1=-0.1, 
  noiseFacor1=2, 
  rbarADJ1=0,
  skipChron = TRUE)
testRet <- fullTestingIntervals(
  #chronology = NULL,
  climDat = climDat1,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 1000,
  predictionEnvelope = c(0.5),
  corClim1=0.15, 
  noiseFacor1=1.2, 
  rbarADJ1=0,
  skipChron = TRUE)
testRet <- fullTestingIntervals(
  #chronology = NULL,
  climDat = climDat1,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 1000,
  predictionEnvelope = c(0.5),
  corClim1=-0.1, 
  noiseFacor1=1, 
  rbarADJ1=1,
  skipChron = TRUE)
testRet <- fullTestingIntervals(
  #chronology = NULL,
  climDat = climDat1,
  totalSims = 1,
  bootstappingMethod = "ME",
  bootIts = 1000,
  predictionEnvelope = c(0.5),
  corClim1=0.1, 
  noiseFacor1=1, 
  rbarADJ1=1,
  skipChron = TRUE)

cT1 <- testRet[[1]]$CaptureSummary
cT2 <- testRet[[2]]$CaptureSummary
cT3 <- testRet[[23]]$CaptureSummary
cT4 <- testRet[[24]]$CaptureSummary

#lapply(testRet, function(x) x$runDetails)[seq(24,44,2)]
ME5 <- seq(3,11,2)
ME9 <- seq(4,12,2)
trad5 <- seq(15,24,2)
trad9 <- seq(16,24,2)

a1 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[1])[ME5]))
a2 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[2])[ME5]))
a3 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[3])[ME5]))
a4 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[4])[ME5]))

b1 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[1])[ME9]))
b2 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[2])[ME9]))
b3 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[3])[ME9]))
b4 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[4])[ME9]))

c1 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[1])[trad5]))
c2 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[2])[trad5]))
c3 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[3])[trad5]))
c4 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[4])[trad5]))

d1 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[1])[trad9]))
d2 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[2])[trad9]))
d3 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[3])[trad9]))
d4 <- mean(unlist(lapply(testRet, function(x) x$CaptureSummary[4])[trad9]))

summaryDat1 <- data.frame(row.names = c("ME50", "ME90", "trad50", "trad90"),
                          "ErrTheory" <- c(a1,b1,c1,d1),
                          "ErrEmp" <- c(a2,b2,c2,d2),
                          "ChronoConfEmp" <- c(a3,b3,c3,d3),
                          "ChronoConfTheory" <- c(a4,b4,c4,d4))

summaryDat1 <- rbind.data.frame(summaryDat1,cT1,cT2,cT3,cT4)
colnames(summaryDat1) <- c("ErrTheory","ErrEmp","ChronoConfTheory", "ChronoConfEmp")
rownames(summaryDat1) <- c("simME50", "simME90", "simTrad50", "simTrad90","realME50", "realME90", "realTrad50", "realTrad90")
View(summaryDat1)
