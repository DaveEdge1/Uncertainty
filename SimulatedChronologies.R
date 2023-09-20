
chronSimul <- function(climDat2, corPerc, numSurrogates=1000){

  TNchron <- read.rwl("C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/TreeNobRCS.csv")
  
  
  
  # climDat <- read.csv("D:/Lab Backup/Geoduck/Tree Nob/PaperFigs/LangaraMonthlyData.csv")
  # climDat <- climDat[complete.cases(climDat),]
  # 
  # climDat2 <- data.frame("Year" = rep(NA, length(unique(climDat$year))),
  #                        "Target" = rep(NA, length(unique(climDat$year))))
  #   
  # for (i in 1:length(unique(climDat$year))){
  #   climDat2$Year[i] <- unique(climDat$year)[i]
  #   climDat2$Target[i] <- mean(as.numeric(c(climDat$SST[climDat$month %in% 5:12 & climDat$year == unique(climDat$year)[i]])))
  # }
  
  # write.csv(climDat2, "C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/LangaraSST.csv")
  # write.csv(TNchron, "C:/Users/dce72/Documents/GradSchool/Uncertainty/SimpleBootTrials/TreeNobRCS.csv")
  
  
  require(dplR)
  require(astrochron)
  
  
  OverlapDat <- climDat2[climDat2$Year %in% as.numeric(rownames(TNchron)),]
  
  
  simulChron <- TNchron[as.numeric(rownames(TNchron)) %in% climDat2$Year,]
  simulChron[1:dim(simulChron)[1],1:dim(simulChron)[2]] <- NA
  seriesLengths <- round(runif(n=50, min=30, max=dim(simulChron)[1]-1))
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
  
  for (i in 1:length(seriesLengths)){
    indices1 <- startYears[i]:(startYears[i]+seriesLengths[i])
    
    #create ebisuzaki surrogates in the interval of the simulated tree-ring index
    Surrogates1 <- surrogates(OverlapDat$Target[indices1], nsim = numSurrogates,genplot=F,verbose=F)
    #Choose a surrogate with a decent correlation to the target
    surrogateIndex <- sample(order(cor(OverlapDat$Target[indices1], Surrogates1))[lowerPerc:upperPerc], 1)
    simulChron[indices1,i] <- Surrogates1[,surrogateIndex]
  }
  
  MVsimulChron <- chron(simulChron)
  length(MVsimulChron$xxxstd)
  cat("\nSimulated chronology - Target correlation: ", cor(MVsimulChron$xxxstd, OverlapDat$Target), "\n")
  print(dplR::rwi.stats(simulChron))
  
  return(simulChron)
}

#write.csv(simulChron, "C:/Users/dce72/Documents/GradSchool/Uncertainty/SimulChrons/TNSimul4.csv")
