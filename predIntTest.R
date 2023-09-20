# reconDat=reconDat2
# envelope=0.5
#envelope: percent of values to capture with prediction intervals, min=0.5, max=0.99
#reconDat: data frame of chronology-target data in interval of continuous overlap

predIntTest <- function(reconDat, envelope=0.9){
  

  colnames(reconDat) <- c("Year", "Target", "ChronoLow", "Chronology", "ChronoHigh")
  
  ###################################################################################################
  ###################################################################################################
  ## 5 ## Find interval of proxy-target overlap
  ## 6 ##	Set aside 10 years of proxy-target overlap for testing of the prediction intervals
  

  envelopeLower <- (1-envelope)/2
  envelopeUpper <- 1-envelopeLower
  
  
  overlap1 <- reconDat[complete.cases(reconDat),1:3]
  possibleIts1 <- length(overlap1[,1])
  
  #Track capture in SetAside interval
  #setAsideCapture <- rep(NA, possibleIts1)
  setAsideCapture2 <- rep(NA, possibleIts1)
  setAsideCapture3 <- rep(NA, possibleIts1)
  # setAsideCapture4 <- rep(NA, possibleIts1)
  #setAsideCapture5 <- rep(NA, possibleIts1)
  setAsideCapture6 <- rep(NA, possibleIts1)
  setAsideCapture7 <- rep(NA, possibleIts1)
  
  setAsideIndices <- c(1:possibleIts1, 1:possibleIts1)
  
  
  setAdideLen <- 10
  
  calibDat <- array(dim=c((length(overlap1[,1]) - setAdideLen), 16, possibleIts1), data=NA)
  
  for (setAsideLoop in 1:possibleIts1){
    setAsideInterval <- setAsideIndices[setAsideLoop:(setAsideLoop+(setAdideLen-1))]
    
    setAside <- overlap1[setAsideInterval,]
    setAsideIndex <- as.numeric(rownames(setAside))
    
    
    overlapIndex <- which(!as.numeric(rownames(overlap1)) %in% as.numeric(rownames(setAside)))
    
    reconDat$reconstruction <- rep(NA, dim(reconDat)[1])
    reconDat$reconLow <- rep(NA, dim(reconDat)[1])
    reconDat$reconHigh <- rep(NA, dim(reconDat)[1])
    
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
    
    #calibDat <- data.frame(matrix(nrow=(possibleIts+1), ncol=12, data=NA))
    for (i in 1:(possibleIts+1)){
      interestInterval <- indexWrap[i:(possibleIts+i)]
      verif1 <- interestInterval[1:verifLen] 
      #define calibration and verification intervals
      calib1 <- interestInterval[!interestInterval %in% verif1]
      
      #calibrate regression
      # if (boot == "trad"){
      reconModel <- lm(Target ~ Chronology, data = reconDat[calib1,])
      reconModelLow <- lm(Target ~ ChronoLow, data = reconDat[calib1,])
      reconModelHigh <- lm(Target ~ ChronoHigh, data = reconDat[calib1,])
      # }else if (boot == "ME"){
      #   reconDat2$Target[complete.cases(reconDat2$Target)] <- MEbootTarget[,sample(1:dim(MEbootTarget)[2],1)]
      #   reconDat2$Chronology[complete.cases(reconDat2$Chronology)] <- bootChronsAvg[,sample(1:dim(bootChronsAvg)[2],1)]
      #   reconModel <- lm(Target ~ Chronology, data = reconDat2[calib1,])
      # }
      reconDat$reconstruction[interestInterval] <- reconModel$coefficients[1] + reconModel$coefficients[2] * reconDat$Chronology[interestInterval]
      reconDat$reconLow[interestInterval] <- reconModelLow$coefficients[1] + reconModelLow$coefficients[2] * reconDat$ChronoLow[interestInterval]
      reconDat$reconHigh[interestInterval] <- reconModelHigh$coefficients[1] + reconModelHigh$coefficients[2] * reconDat$ChronoHigh[interestInterval]
      #capture regression model and verification interval
      calibDat[i,1:2,setAsideLoop] <- c(verif1[1],verif1[length(verif1)])
      calibDat[i,3:4,setAsideLoop] <- c(reconModel$coefficients[1], reconModel$coefficients[2])
      calibDat[i,13:14,setAsideLoop] <- c(reconModelLow$coefficients[1], reconModelLow$coefficients[2])
      calibDat[i,15:16,setAsideLoop] <- c(reconModelHigh$coefficients[1], reconModelHigh$coefficients[2])
      #Reconstruct the target using upper and lower chronology confidence intervals
      #find the reconstruction confidence intervals based on the difference between the regression uncertainty and the target values
      upperCI <- (reconDat$ChronoHigh * reconModelHigh$coefficients[2]) + reconModelHigh$coefficients[1]
      lowerCI <- (reconDat$ChronoLow * reconModelLow$coefficients[2]) + reconModelLow$coefficients[1]
      upperDif <- reconDat$Target[calib1] - upperCI[calib1]
      lowerDif <- lowerCI[calib1] - reconDat$Target[calib1]
      calibCI95 <- interpIndex(upperDif, envelopeUpper)
      calibCI05 <- interpIndex(lowerDif, envelopeLower)
      calibDat[i,5:6,setAsideLoop] <- c(calibCI05,calibCI95)
      vUpperDif <- reconDat$Target[verif1] - upperCI[verif1]
      vLowerDif <- lowerCI[verif1] - reconDat$Target[verif1]
      verifPI95 <- interpIndex(vUpperDif, envelopeUpper)
      verifPI05 <- interpIndex(vLowerDif, envelopeLower)
      errorSet <- abs(reconDat$Target[verif1] - reconDat$reconstruction[verif1])
      trueRMSE <- interpIndex(errorSet, envelope)
      tValRMSE <- median(errorSet) + sd(errorSet)* qt(p=envelopeLower*2, df=length(errorSet), lower.tail = FALSE)
      calibDat[i,7:8,setAsideLoop] <- c(verifPI05,verifPI95)
      calibDat[i,9,setAsideLoop] <- tValRMSE
      calibDat[i,10,setAsideLoop] <- trueRMSE
      #calibDat[i,19] <- median(abs(c(vUpperDif,vLowerDif)))*qt(p=.025, df=length(errorSet), lower.tail = FALSE)
      calibDat[i,11,setAsideLoop] <- median(vLowerDif) - sd(vLowerDif)*qt(p=envelopeLower, df=length(vLowerDif), lower.tail = FALSE)
      calibDat[i,12,setAsideLoop] <- median(vUpperDif) + sd(vUpperDif)*qt(p=envelopeLower, df=length(vUpperDif), lower.tail = FALSE)
    }
    
    
    ###################################################################################################
    ###################################################################################################
    ## 9 ##	Repeat steps 7-8 for all possible calibration intervals
    ## 10 ##	Test the prediction intervals in set-aside interval
    setAsideIndex2 <- which(as.numeric(rownames(reconDat)) %in% setAsideIndex)
    
    reconDat$reconstruction <- median(calibDat[,3,setAsideLoop]) + median(calibDat[,4,setAsideLoop]) * reconDat$Chronology
    
    #Based on t-value * RMSE
    setAsideUpper2 <- reconDat$reconstruction[setAsideIndex2] + median(calibDat[,9,setAsideLoop])
    setAsideLower2 <- reconDat$reconstruction[setAsideIndex2] - median(calibDat[,9,setAsideLoop])
    SetAsideTarget2 <- reconDat$Target[setAsideIndex2]
    
    setAsideCapture2[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget2 > setAsideUpper2 | SetAsideTarget2 < setAsideLower2))/setAdideLen
    
    #Based on simple empirical 90 percentile error
    setAsideUpper3 <- reconDat$reconstruction[setAsideIndex2] + median(calibDat[,10,setAsideLoop])
    setAsideLower3 <- reconDat$reconstruction[setAsideIndex2] - median(calibDat[,10,setAsideLoop])
    SetAsideTarget3 <- reconDat$Target[setAsideIndex2]
    
    setAsideCapture3[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget3 > setAsideUpper3 | SetAsideTarget3 < setAsideLower3))/setAdideLen
    
    #use the average regression coefficeints from all possible calibration intervals and the average offsets (5 and 95% empirical) from the chronology 
    #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
    
    
    # setAsideUpper <- median(calibDat[,15,setAsideLoop]) + median(calibDat[,16,setAsideLoop]) * reconDat$ChronoHigh[setAsideIndex2] + median(calibDat[,8,setAsideLoop])
    # setAsideLower <- median(calibDat[,13,setAsideLoop]) + median(calibDat[,14,setAsideLoop]) * reconDat$ChronoLow[setAsideIndex2] + median(calibDat[,7,setAsideLoop])
    # SetAsideTarget <- reconDat$Target[setAsideIndex2]
    # 
    # setAsideCapture[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget > setAsideUpper | SetAsideTarget < setAsideLower))/setAdideLen
    
    
    # #use the average regression coefficeints from all possible calibration intervals and the average offsets (90% t-val) from the chronology 
    # #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
    # setAsideUpper4 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X3[setAsideIndex2] + median(calibDat$X19)
    # setAsideLower4 <- median(calibDat$X11) + median(calibDat$X12) * confInt$X1[setAsideIndex2] - median(calibDat$X19)
    # SetAsideTarget4 <- reconDat$Target[setAsideIndex2]
    # 
    # setAsideCapture4[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget4 > setAsideUpper4 | SetAsideTarget4 < setAsideLower4))/setAdideLen
    # 
    #use the average regression coefficeints from all possible calibration intervals and the average offsets (5 and 95% t-val) from the chronology 
    #confidence intervals to the target to define upper and lower prediction intervals in the "set Aside" interval 
   
    
    # setAsideUpper5 <- median(calibDat[,15,setAsideLoop]) + median(calibDat[,16,setAsideLoop]) * reconDat$ChronoHigh[setAsideIndex2] + median(calibDat[,12,setAsideLoop])
    # setAsideLower5 <- median(calibDat[,13,setAsideLoop]) + median(calibDat[,14,setAsideLoop]) * reconDat$ChronoLow[setAsideIndex2] + median(calibDat[,11,setAsideLoop])
    # SetAsideTarget5 <- reconDat$Target[setAsideIndex2]
    # 
    # setAsideCapture5[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget5 > setAsideUpper5 | SetAsideTarget5 < setAsideLower5))/setAdideLen
    # 
    
    #Based on t-value * RMSE + regression error
    
    setAsideUpper6 <- median(calibDat[,13,setAsideLoop]) + median(calibDat[,14,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] + median(calibDat[,9,setAsideLoop])
    setAsideLower6 <- median(calibDat[,15,setAsideLoop]) + median(calibDat[,16,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] - median(calibDat[,9,setAsideLoop])
    SetAsideTarget6 <- reconDat$Target[setAsideIndex2]
    
    setAsideCapture6[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget6 > setAsideUpper6 | SetAsideTarget6 < setAsideLower6))/setAdideLen
    
    #Based on simple empirical 90 percentile error + regression error
    
    setAsideUpper7 <- median(calibDat[,13,setAsideLoop]) + median(calibDat[,14,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] + median(calibDat[,10,setAsideLoop])
    setAsideLower7 <- median(calibDat[,15,setAsideLoop]) + median(calibDat[,16,setAsideLoop]) * reconDat$Chronology[setAsideIndex2] - median(calibDat[,10,setAsideLoop])
    SetAsideTarget7 <- reconDat$Target[setAsideIndex2]
    
    setAsideCapture7[setAsideLoop] <- (setAdideLen-sum(SetAsideTarget7 > setAsideUpper7 | SetAsideTarget7 < setAsideLower7))/setAdideLen
    
  }
  Captures <- list(#"Capture1"=setAsideCapture,
       "Capture1"=setAsideCapture2,
       "Capture2"=setAsideCapture3,
       #"Capture2"=setAsideCapture5,
       "Capture3"=setAsideCapture6,
       "Capture4"=setAsideCapture7)
  
  return1 <- list("Captures"=Captures, 
                  "calibDat"=calibDat,
                  "reconDat"=reconDat)
  return(return1)
}

