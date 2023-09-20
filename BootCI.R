rm(list=ls())
gc()




boot="ME"
numIts <- 1000





require(meboot)
require(dplR)


###################################################################################################
###################################################################################################
## 1 ## Load chronology rwi and target data

#TNchron <- read.rwl("D:/Lab Backup/Geoduck/Tree Nob/TreeNobRCStrunc1.csv")
TNchron <- read.rwl("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimulChrons/TNSimul5.csv")
climDat <- read.csv("D:/Lab Backup/Geoduck/Tree Nob/PaperFigs/LangaraMonthlyData.csv")
climDat <- climDat[complete.cases(climDat),]
head(climDat)

TNchron <- TNchron[as.numeric(rownames(TNchron))>1725,]
TNchron2 <- chron(TNchron)
#plot(TNchron2)

reconDat <- data.frame("Year" = as.numeric(rownames(TNchron2)),
                       "Chronology" = TNchron2$xxxstd,
                       "SST" = rep(NA, dim(TNchron2)[1]))
for (i in unique(climDat$year)){
  reconDat$SST[reconDat$Year == i] <- mean(as.numeric(c(climDat$SST[climDat$month %in% 5:12 & climDat$year == i])))
}

###################################################################################################
###################################################################################################
## 2 ## Bootstrap chronology rwi values, building 1000 replicate chronologies

valueIndex <- !is.na(TNchron)

bootChrons <- array(data=NA, dim = c(dim(TNchron)[1], dim(TNchron)[2], numIts))


if (boot == "trad"){
  for (k in 1:numIts){
    for (i in 1:dim(TNchron)[1]){
      rowSpots <- !is.na(TNchron[i,])
      sampleSize <- sum(rowSpots)
      bootChrons[i,rowSpots,k] <- unlist(sample(TNchron[i,valueIndex[i,]],sampleSize, replace = TRUE))
    }
  }
}else if (boot == "ME"){
  MEbootSST <- meboot(reconDat$SST[complete.cases(reconDat$SST)], reps = numIts)$ensemble
  for (i in 1:dim(TNchron)[2]){
    givenYears <- valueIndex[,i]
    bootChrons[givenYears,i,1:numIts] <- meboot(TNchron[complete.cases(TNchron[,i]),i], reps = numIts)$ensemble
  }
}


###################################################################################################
###################################################################################################
## 3 ## Build mean-value chronologies from the replicate chronologies

bootChronsAvg <- data.frame(matrix(data=NA, nrow = dim(TNchron)[1], ncol = numIts))
for (k in 1:dim(bootChrons)[3]){
  bootChronsAvg[,k] <- chron(bootChrons[,,k])$xxxstd
}

###################################################################################################
###################################################################################################
## 4 ## Measure confidence interval of chronology by sorting mean value chronology data at each 
#       year and selecting corresponding percentile values

confInt <- data.frame(matrix(data=NA, ncol = 3, nrow = dim(TNchron)[1]))
for (i in 1:dim(TNchron)[1]){
  upperC <- interpIndex(bootChronsAvg[i,], 0.95)
  lowerC <- interpIndex(bootChronsAvg[i,], 0.05)
  medianC <- interpIndex(bootChronsAvg[i,], 0.5)
  confInt[i,] <- c(lowerC, medianC, upperC)
}
#head(confInt)

###################################################################################################
###################################################################################################
## 5 ## Find interval of proxy-target overlap
## 6 ##	Set aside 10 years of proxy-target overlap for testing of the prediction intervals



overlap1 <- reconDat[complete.cases(reconDat),]
possibleIts1 <- length(overlap1[,1])

#Track capture in SetAside interval
setAsideCapture <- rep(NA, possibleIts1)
setAsideCapture2 <- rep(NA, possibleIts1)
setAsideCapture3 <- rep(NA, possibleIts1)
# setAsideCapture4 <- rep(NA, possibleIts1)
setAsideCapture5 <- rep(NA, possibleIts1)

setAsideIndices <- c(1:possibleIts1, 1:possibleIts1)


setAdideLen <- 10

for (setAsideLoop in 1:possibleIts1){
  setAsideInterval <- setAsideIndices[setAsideLoop:(setAsideLoop+(setAdideLen-1))]
  
  setAside <- overlap1[setAsideInterval,]
  setAsideIndex <- as.numeric(rownames(setAside))
  overlapIndex <- as.numeric(rownames(overlap1))[!as.numeric(rownames(overlap1)) %in% as.numeric(rownames(setAside))]
  
  reconDat$reconstruction <- rep(NA, dim(reconDat)[1])
  reconDat2 <- reconDat
  
  possibleIts <- length(overlapIndex)-1
  indexWrap <- c(overlapIndex,overlapIndex)
  
  ###################################################################################################
  ###################################################################################################
  ## 7 ##	Perform reconstruction calibration by split calibration-verification
  ###### a.	Measure reconstruction confidence intervals in relation to the chronology confidence intervals
  ###### b.	Capture calibration coefficients and upper/lower calibration confidence intervals for a given calibration period
  
  ## 8 ##	Capture prediction intervals of calibration based on corresponding calibration coefficients, 
  #relative to chronology confidence intervals
  ######	Eg. 90th ranked-order value above the upper (and 90th ranked below lower) chronology confidence 
  #interval in the verification period
  
  verifLen <- floor(possibleIts/2)
  
  calibDat <- data.frame(matrix(nrow=(possibleIts+1), ncol=21, data=NA))
  for (i in 1:(possibleIts+1)){
    interestInterval <- indexWrap[i:(possibleIts+i)]
    verif1 <- interestInterval[1:verifLen]
    #define calibration and verification intervals
    calib1 <- interestInterval[!interestInterval %in% verif1]
    
    #calibrate regression
    # if (boot == "trad"){
    reconModel <- lm(SST ~ Chronology, data = reconDat[calib1,])
    # }else if (boot == "ME"){
    #   reconDat2$SST[complete.cases(reconDat2$SST)] <- MEbootSST[,sample(1:dim(MEbootSST)[2],1)]
    #   reconDat2$Chronology[complete.cases(reconDat2$Chronology)] <- bootChronsAvg[,sample(1:dim(bootChronsAvg)[2],1)]
    #   reconModel <- lm(SST ~ Chronology, data = reconDat2[calib1,])
    # }
    reconDat$reconstruction[interestInterval] <- reconModel$coefficients[1] + reconModel$coefficients[2] * reconDat$Chronology[interestInterval]
    #capture regression model and verification interval
    calibDat[i,1:10] <- verif1
    calibDat[i,11:12] <- c(reconModel$coefficients[1], reconModel$coefficients[2])
    #Reconstruct the target using upper and lower chronology confidence intervals
    #find the reconstruction confidence intervals based on the difference between the regression uncertainty and the target values
    upperCI <- (confInt$X3 * reconModel$coefficients[2]) + reconModel$coefficients[1]
    lowerCI <- (confInt$X1 * reconModel$coefficients[2]) + reconModel$coefficients[1]
    upperDif <- reconDat$SST[calib1] - upperCI[calib1]
    lowerDif <- lowerCI[calib1] - reconDat$SST[calib1]
    calibCI95 <- interpIndex(upperDif, 0.95)
    calibCI05 <- interpIndex(lowerDif, 0.05)
    calibDat[i,13:14] <- c(calibCI05,calibCI95)
    vUpperDif <- reconDat$SST[verif1] - upperCI[verif1]
    vLowerDif <- lowerCI[verif1] - reconDat$SST[verif1]
    verifPI95 <- interpIndex(vUpperDif, 0.95)
    verifPI05 <- interpIndex(vLowerDif, 0.05)
    errorSet <- abs(reconDat$SST[verif1] - reconDat$reconstruction[verif1])
    trueRMSE <- interpIndex(errorSet, 0.9)
    tValRMSE <- median(errorSet) + sd(errorSet)* qt(p=.05, df=length(errorSet), lower.tail = FALSE)
    calibDat[i,15:16] <- c(verifPI05,verifPI95)
    calibDat[i,17] <- tValRMSE
    calibDat[i,18] <- trueRMSE
    calibDat[i,19] <- median(abs(c(vUpperDif,vLowerDif)))*qt(p=.025, df=length(errorSet), lower.tail = FALSE)
    calibDat[i,20] <- median(vLowerDif) - sd(vLowerDif)*qt(p=.05, df=length(vLowerDif), lower.tail = FALSE)
    calibDat[i,21] <- median(vUpperDif) + sd(vUpperDif)*qt(p=.05, df=length(vUpperDif), lower.tail = FALSE)
    
  }
  
  
  ###################################################################################################
  ###################################################################################################
  ## 9 ##	Repeat steps 7-8 for all possible calibration intervals
  ## 10 ##	Test the prediction intervals in set-aside interval
  
  reconDat$reconstruction <- median(calibDat$X11) + median(calibDat$X12) * confInt$X2
  
  #Based on t-value * RMSE
  setAsideUpper2 <- reconDat$reconstruction[setAsideIndex] + median(calibDat$X17)
  setAsideLower2 <- reconDat$reconstruction[setAsideIndex] - median(calibDat$X17)
  SetAsideTarget2 <- reconDat$SST[setAsideIndex]
  
  setAsideCapture2[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget2 > setAsideUpper2 | SetAsideTarget2 < setAsideLower2))/setAdideLen
  
  #Based on simple epirical 90 percentile error
  setAsideUpper3 <- reconDat$reconstruction[setAsideIndex] + median(calibDat$X18)
  setAsideLower3 <- reconDat$reconstruction[setAsideIndex] - median(calibDat$X18)
  SetAsideTarget3 <- reconDat$SST[setAsideIndex]
  
  setAsideCapture3[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget3 > setAsideUpper3 | SetAsideTarget3 < setAsideLower3))/setAdideLen
  
  #use the average regression coefficeints from all possible calibration intervals and the average offsets (5 and 95% empirical) from the chronology 
  #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
  setAsideUpper <- median(calibDat$X11) + median(calibDat$X12) * confInt$X3[setAsideIndex] + median(calibDat$X16)
  setAsideLower <- median(calibDat$X11) + median(calibDat$X12) * confInt$X1[setAsideIndex] + median(calibDat$X15)
  SetAsideTarget <- reconDat$SST[setAsideIndex]
  
  setAsideCapture[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget > setAsideUpper | SetAsideTarget < setAsideLower))/setAdideLen
  
  # #use the average regression coefficeints from all possible calibration intervals and the average offsets (90% t-val) from the chronology 
  # #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
  # setAsideUpper4 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X3[setAsideIndex] + median(calibDat$X19)
  # setAsideLower4 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X1[setAsideIndex] - median(calibDat$X19)
  # SetAsideTarget4 <- reconDat$SST[setAsideIndex]
  # 
  # setAsideCapture4[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget4 > setAsideUpper4 | SetAsideTarget4 < setAsideLower4))/setAdideLen
  # 
  #use the average regression coefficeints from all possible calibration intervals and the average offsets (5 and 95% t-val) from the chronology 
  #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
  setAsideUpper5 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X3[setAsideIndex] + median(calibDat$X21)
  setAsideLower5 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X1[setAsideIndex] + median(calibDat$X20)
  SetAsideTarget5 <- reconDat$SST[setAsideIndex]
  
  setAsideCapture5[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget5 > setAsideUpper5 | SetAsideTarget5 < setAsideLower5))/setAdideLen
  
}
summary(setAsideCapture)
summary(setAsideCapture2)
summary(setAsideCapture3)
# summary(setAsideCapture4)
summary(setAsideCapture5)

par(mfrow=c(1,1))

hist(setAsideCapture)
hist(setAsideCapture2)
hist(setAsideCapture3)
# hist(setAsideCapture4)
hist(setAsideCapture5)

CaptureDat <- data.frame("MEbootEmpiricalTwoTail" = setAsideCapture,
           "RMSEthoery" = setAsideCapture2,
           "RMSEempirical" = setAsideCapture3,
           # "MEbootTheoryOneTail" = setAsideCapture4,
           "MEbootTheoryTwoTail" = setAsideCapture5)


#write.csv(CaptureDat, "C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/TreeNob15trad.csv")
