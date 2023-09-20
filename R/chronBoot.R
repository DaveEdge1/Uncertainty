# chron1=eval(as.name(j))
# numIts = bootIts
# boot = i
# envelope=k
#

chronBoot <- function(chron1, numIts = 1000, boot = "trad", envelope=0.9){

  envelopeLower <- (1-envelope)/2
  envelopeUpper <- 1-envelopeLower

  valueIndex <- !is.na(chron1)

  bootChrons <- array(data=NA, dim = c(dim(chron1)[1], dim(chron1)[2], numIts))


  if (boot == "trad"){
    for (k in 1:numIts){
      for (i in 1:dim(chron1)[1]){
        rowSpots <- !is.na(chron1[i,])
        sampleSize <- sum(rowSpots)
        bootChrons[i,rowSpots,k] <- unlist(sample(chron1[i,valueIndex[i,]],sampleSize, replace = TRUE))
      }
    }
  }else if (boot == "ME"){
    require(meboot)
    #MEbootSST <- meboot(reconDat$SST[complete.cases(reconDat$SST)], reps = numIts)$ensemble
    for (i in 1:dim(chron1)[2]){
      givenYears <- valueIndex[,i]
      bootChrons[givenYears,i,1:numIts] <- meboot(chron1[complete.cases(chron1[,i]),i], reps = numIts)$ensemble
    }
  }

  bootChronsAvg <- data.frame(matrix(data=NA, nrow = dim(chron1)[1], ncol = numIts))
  for (k in 1:dim(bootChrons)[3]){
    bootChronsAvg[,k] <- chron(bootChrons[,,k])[,1]
  }

  confInt <- data.frame(matrix(data=NA, ncol = 3, nrow = dim(chron1)[1]))
  for (i in 1:dim(chron1)[1]){
    upperC <- as.numeric(quantile(bootChronsAvg[i,], probs=envelopeUpper))
    lowerC <- as.numeric(quantile(bootChronsAvg[i,], probs=envelopeLower))
    medianC <- as.numeric(quantile(bootChronsAvg[i,], probs=0.5))
    confInt[i,] <- c(lowerC, medianC, upperC)
  }
  rownames(confInt) <- as.character(row.names(chron1))

  returns1 <- list("bootChronsAvg"=bootChronsAvg,
                   "confInt"=confInt)

  return(returns1)
}

