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


fullTestingIntervalsNew <- function(
    chronology = NULL,
    climDat = NULL,
    totalSims = 5,
    bootstappingMethod = "both",
    bootIts = 1000,
    predictionEnvelope = c(0.5, 0.9),
    skipChron=FALSE,
    alterSampleDepth=0.2){

  if(is.null(chronology)){
    stop("chronology required!\n")
  }

  if(is.null(climDat)){
    stop("climDat required!\n")
  }


  # #load special functions
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronSimul.R")
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronBoot.R")
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/predIntTest.R")


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

  for (j in totalChrons){
    #j=totalChrons[1]
    if (j == "sim1"){
      #build synthetic chronology based on the climate target
      simul1<- chronSim(rwiDat=chronology, climDat=climDat, alterSampleDepth=alterSampleDepth)
    }

    for (i in numStraps){
      #i=numStraps[1]


      for (k in predictionEnvelope){
        #k=predictionEnvelope[1]

        #bootstrap the chronogy
        if (j == "sim1"){
          bootChrons1 <- chronBoot(chron1=simul1$synChron, numIts = bootIts, boot = i, envelope=k)
        }else{
          #bootstrap the chronogy
          bootChrons1 <- chronBoot(chron1=eval(as.name(j)), numIts = bootIts, boot = i, envelope=k)
        }




        #format data for prediction interval testing
        start1 <- max(c(min(as.numeric(rownames(bootChrons1$confInt))),min(climDat[,1])))
        end1 <- min(c(max(as.numeric(rownames(bootChrons1$confInt))),max(climDat[,1])))

        reconDat1 <- data.frame("Year" = as.numeric(rownames(bootChrons1$confInt)),
                                "Target" = rep(NA, length(bootChrons1$confInt$X2)))
        reconDat1[reconDat1[,1] %in% climDat[,1], 2] <- climDat[climDat[,1] %in% reconDat1[,1],2]
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
