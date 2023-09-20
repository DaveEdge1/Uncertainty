#rwiDat - detrended ring widths from dplR
#climDat - two-column data.frame with columns (year, climate target)
#corClim - factor by which to increase/decrease chronology-climate correlation
#noiseFactor - adjust chronology AR1, values below 1 increase AR1, range (0.1,10)
#rbarADJ - reduce the rbar relative to the input chronology
#maxAtts - max number of attempts to create chronology with given parameters

# chronSim(rwiDat = RogenBluechron, climDat = climDat3, rbarADJ = 0)
chronSim <- function(rwiDat=NULL,
                     climDat=NULL,
                     corClim=runif(n=1, min=-0.5, max=0.3),
                     noiseFacor=runif(n=1, min=.1, max=3),
                     rbarADJ=runif(n=1, min = 0, max = 1),
                     maxAtts=1000,
                     alterSampleDepth=0.2){

  require(dplR)

  #calc vars
  chronYrsAll <- as.numeric(rownames(rwiDat))
  MVchron <- chron(rwiDat)
  chronLen <- length(chronYrsAll)
  climYrs <- climDat[,1]
  climLen <- length(climYrs)

  allCor2Chron <- rep(NA, dim(rwiDat)[2])
  for (www in 1:dim(rwiDat)[2]){
    TSindex <- !is.na(rwiDat[,www])
    chronAvg <- MVchron[TSindex,1]
    TS2 <- rwiDat[TSindex,www]
    allCor2Chron[www] <- cor(TS2, chronAvg)
  }
  medianCor2Chron <- median(allCor2Chron)


  ############################################################################
  #Build Chronology Surrogate
  ############################################################################
  climCor <- cor(MVchron[chronYrsAll %in% climYrs,1], climDat[climYrs %in% chronYrsAll,2])

  chronAR1 <- acf(MVchron[,1], plot = FALSE)
  #avgErr <- sd(MVchron[,1])*percErr

  SurClimCor <- 0

  corClimLow <- climCor + climCor*(corClim - 0.02)
  corClimHigh <- climCor + climCor*(corClim + 0.02)
  corClimAvg <- mean(c(corClimLow, corClimHigh))
  numTrys<-0

  cat("\nattempting to match specified climate correlation\n")
  cat("This may not be possible with all parameter settings\n")
  #cat("Attempts:\n")

  bestChronSur <- rep(NA, chronLen)
  closeBest <- 10
  while (SurClimCor > corClimHigh | SurClimCor < corClimLow){
    chronSur <- rep(NA, chronLen)

    spline2 <- detrend.series(MVchron[,1], method = "Spline", return.info = TRUE, nyrs = chronLen/runif(1,1,10), make.plot = FALSE)
    residsSD2 <- sd(spline2$series)

    origSplit <- runif(1, .3, .7)
    newsplit <- 1-origSplit

    for (iii in 1:chronLen){
      #chronSur[iii] <- rnorm(1, mean = MVchron[iii,1], sd=avgErr)
      chronSur[iii] <- spline2$curves[iii] + (rnorm(1, mean = spline2$series[iii], sd=residsSD2*noiseFacor) * newsplit) + (spline2$series[iii] * origSplit)
    }

    SurClimCor <- cor(chronSur[chronYrsAll %in% climYrs], climDat[climYrs %in% chronYrsAll,2])
    howClose <- abs(SurClimCor - corClimAvg)
    if(howClose < closeBest){
      closeBest <- howClose
      bestChronSur <- chronSur
    }
    numTrys <- numTrys + 1

    if (floor(numTrys/100) == numTrys/100){
      cat("Attempts: ", numTrys, "\r")
      if (maxAtts == numTrys){
        chronSur <- bestChronSur
        warning(crayon::bold("\nMax number of attempts reached without success.\nTry changing parameters closer to preset values.\n"))
        break
      }
    }
  }

  cat("Success on attempt number: ", numTrys, "\n")

  ############################################################################
  #Build time series surrogates
  ############################################################################

  trueDepthSampleSize <- alterSampleDepth * 100
  falseDepthSampleSize <- 100-trueDepthSampleSize
  tfSample <- c(rep(TRUE, trueDepthSampleSize), rep(FALSE, falseDepthSampleSize))
  message("Alter sample depth?\n")
  alterSampleDepth <- sample(tfSample, size = 1)
  if (alterSampleDepth == FALSE){

    message("Nope")
  }else{
    doubleHalf <- sample(c("double", "half"), size = 1)
    message(doubleHalf)
    if(doubleHalf == "half"){
      rwiDat <- rwiDat[,seq(1,ncol(rwiDat), by=2)]
    }else{
      rwiDat <- cbind(rwiDat, rwiDat)
    }
  }
  fullChronSur <- rwiDat
  message(rwi.stats(rwiDat))

  fullChronSur[!is.na(fullChronSur)] <- NA


  orig1 <- sqrt(rwi.stats(rwiDat)$rbar.eff)
  new1 <- 1-orig1
  jjj=1
  for (jjj in 1:dim(rwiDat)[2]){
    TSindex <- !is.na(rwiDat[,jjj])
    TS1 <- chronSur[TSindex]
    TSlen <- length(TS1)
    TSsur <- rep(NA, TSlen)
    noise1 <- rnorm(TSlen,mean=0,sd=sd(TS1)*rbarADJ)
    spline1 <- detrend.series(TS1+noise1, method = "Spline", return.info = TRUE, nyrs = TSlen, make.plot = FALSE)
    residsSD <- sd(spline1$series)

    avgErr2 <- sd(TS1)#*percErr

    for (qqq in 1:TSlen){
      TSsur[qqq] <- spline1$curves[qqq] + (rnorm(1, mean = spline1$series[qqq], sd=residsSD) * new1) + (spline1$series[qqq] * orig1)
    }
    fullChronSur[TSindex, jjj] <- TSsur

  }

  surChronMV <- chron(fullChronSur)

  #Correlation between real and surrogate chronologies
  chronsCor <- cor(surChronMV[,1], MVchron[,1])

  #correlation between synthetic chronology and climate target
  surChronTargetCor <- cor(surChronMV[chronYrsAll %in% climYrs,1], climDat[climYrs %in% chronYrsAll,2])

  #Surrogate Chron AR1
  surChronAR1 <- acf(surChronMV[,1], plot = FALSE)$acf[2]
  #Original Chron AR1
  origChronAR1 <- acf(MVchron[,1], plot = FALSE)$acf[2]
  #Climate series AR1
  climTargetAR1 <- acf(climDat[,2], plot = FALSE)$acf[2]

  #Relative change in AR1 (%)
  AR1change <- (acf(surChronMV[,1], plot = FALSE)$acf[2]/acf(MVchron[,1], plot = FALSE)$acf[2]-1)*100

  #Relative change in rbar (%)
  rbarChange <- (rwi.stats(fullChronSur)$rbar.eff/orig1-1)*100

  cat(crayon::bold("\nSynthetic chronology created: \n\n"))
  cat("Correlation to original chronology: ", chronsCor, "\n")
  cat("Correlation to climate target: ", surChronTargetCor, "\n\n")
  cat("First-Order Autocorrelation: \n")
  cat("Original Chronology: ", origChronAR1, "\n")
  cat("Synthetic Chronology: ", surChronAR1, "\n")
  cat("Climate Target: ", climTargetAR1, "\n\n")
  cat("change in rbar relative to Original Chronology: ", rbarChange,"%\n")

  newVars1 <- list("chronsCor" = chronsCor,
                   "surChronTargetCor" = surChronTargetCor,
                   "origChronAR1" = origChronAR1,
                   "surChronAR1" = surChronAR1,
                   "climTargetAR1" = climTargetAR1,
                   "rbarChange" = rbarChange)

  chronSimReturns <- list("synChron" = fullChronSur,
       "modelInput" = list("corClim"=corClim, "noiseFacor"=noiseFacor, "rbarADJ"=rbarADJ),
       "synChronData" = newVars1)

  return(chronSimReturns)

}



