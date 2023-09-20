#' Title
#'
#' @param chronology
#' @param climDat
#' @param totalSims
#' @param simulationInterval
#' @param bootstappingMethod
#' @param bootIts
#' @param predictionEnvelope
#'
#' @return
#' @export
#'
#' @examples
fullTestingIntervals <- function(
    chronology = TNchron,
    climDat = climDat1,
    totalSims = 5,
    simulationInterval = c(0.95,0.99),
    bootstappingMethod = "both",
    bootIts = 1000,
    predictionEnvelope = c(0.5, 0.9)){



  # #load special functions
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronSimul.R")
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/interpIndex.R")
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/chronBoot.R")
  # source("C:/Users/dce72/Documents/GradSchool/Uncertainty/predIntTest.R")


  if (!is.null(chronology)){
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

    for (j in totalChrons){

      for (k in predictionEnvelope){
        if (j == "sim1"){
          #build synthetic chronology based on the climate target
          simul1<- chronSimul(climDat = climDat1, corPerc = simulationInterval, numSurrogates=1000, numSeries = 50)

          #bootstrap the chronogy
          bootChrons1 <- chronBoot(chron1=simul1, numIts = bootIts, boot = i, envelope=k)
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

        #build reconstruction and associated prediction intervals, test those intervals
        capVals <- predIntTest(reconDat=reconDat2, envelope=k)

        #Look at the average capture
        capDat1 <- unlist(lapply(capVals$Captures, function(x) mean(x)))

        runDetails <- list("BootstMethod" = i,
                           "Chronology" = j,
                           "Prediction Interval" = k)

        intervalsTesting[[totalRuns]] <- list("runDetails" = runDetails,
                                              "CaptureSummary" = capDat1,
                                              "FullData" = capVals)

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
