

climChronSimul <- function(climDat, corPerc, numSurrogates=1000, numSeries=50){

  require(dplR)
  require(astrochron)
  
  
  simulChron <- data.frame(matrix(data=NA, ncol = numSeries, nrow = length(climDat[,1])))
  for (i in numSeries){
    simulChron[,i] <- rnorm(length(climDat[,1]))
  }
  simulChron <- as.rwl(simulChron)
  simulChron[1:length(climDat[,1]), 1:numSeries] <- NA
  rownames(simulChron) <- as.character(climDat[,1])
  
  seriesLengths <- round(runif(n=numSeries, min=30, max=dim(simulChron)[1]-1))
  startYears <- rep(NA, length(seriesLengths))
  
  zp=FALSE
  while (zp == FALSE){
    for (i in 1:length(seriesLengths)){
      startYears[i] <- round(runif(n=1, min = 1, max = (dim(simulChron)[1]-seriesLengths[i])))
    }
    if (max(startYears+seriesLengths) == dim(simulChron)[1]){
      if (min(startYears) == 1){
        zp <- TRUE
      }
    }
  }
  
  
  lowerPerc <- corPerc[1]*numSurrogates
  upperPerc <- corPerc[2]*numSurrogates
  
  numSurrogates2 <- 1000000
  
  Surrogates0 <- surrogates(climDat[,2], nsim = numSurrogates2,genplot=F,verbose=F)
  #Choose a surrogate based on correlation to the target
  surrogateIndex <- order(cor(climDat[,2], Surrogates0))[numSurrogates2]
  
  climDat[,3] <- Surrogates0[,surrogateIndex]
  
  for (i in 1:length(seriesLengths)){
    indices1 <- startYears[i]:(startYears[i]+seriesLengths[i])
    
    #create ebisuzaki surrogates in the interval of the simulated tree-ring index
    Surrogates1 <- surrogates(climDat[indices1,3], nsim = numSurrogates,genplot=F,verbose=F)
    #Choose a surrogate based on correlation to the target
    surrogateIndex <- sample(order(cor(climDat[indices1,3], Surrogates1))[lowerPerc:upperPerc], 1)
    simulChron[indices1,i] <- Surrogates1[,surrogateIndex]
  }
  
  MVsimulChron <- chron(simulChron)
  length(MVsimulChron[,1])
  cat("\nSimulated chronology - Target correlation: ", cor(MVsimulChron[,1], climDat[,2]), "\n")
  print(dplR::rwi.stats(simulChron))
  
  cor(climDat[,3], climDat[,2])
  
  return(simulChron)
}

#write.csv(simulChron, "C:/Users/dce72/Documents/GradSchool/Uncertainty/SimulChrons/TNSimul4.csv")
