#measure the fidelity of cinfidence intervals
#what percentage of values are captured in the verfication interval?

reconData <- read.csv("D:/Lab Backup/Geoduck/Tree Nob/Comps Push/reconstructionVals.csv")

startYr <- min(reconData$Year[!is.na(reconData$SST)])
endYr <- max(reconData$Year[!is.na(reconData$SST)])

reconData$PI90up[reconData$Year %in% startYr:endYr] <- reconData$Recon[reconData$Year %in% startYr:endYr] + 0.585689299999999
reconData$PI90down[reconData$Year %in% startYr:endYr] <- reconData$Recon[reconData$Year %in% startYr:endYr] - 0.585689299999999

reconDataTrim <- reconData[reconData$Year %in% startYr:endYr,]

uncapturedYrs <- reconDataTrim$Year[reconDataTrim$SST < reconDataTrim$PI90down | reconDataTrim$SST > reconDataTrim$PI90up]

cat("The data points outside the confidence intervals make up ",
    round(length(uncapturedYrs)/length(startYr:endYr) * 100, 2), "% of the total.")

