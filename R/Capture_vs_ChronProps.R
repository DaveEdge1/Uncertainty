# #clear workspace
# rm(list=ls())
# gc()
# library(dplR)
# library(ggplot2)
# library(gridExtra)
# library(ggbeeswarm)
# library(reshape2)
#
#
# allChronProps <- readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/ChronologyProperties.RData")
# head(allChronProps)
# dim(allChronProps)
#
# allCapture <-readRDS("C:/Users/dce72/Documents/GradSchool/Uncertainty/CaptureDat.RData")
# head(allCapture)
# dim(allCapture)
#
# names(allChronProps)[3] <- "chronProp"
#
# allCapture$siteID <- NA
# allCapture$siteID[allCapture$site == "Tree_Nob"] <- "TN"
# allCapture$siteID[allCapture$site == "Rock_Springs_Ranch"] <- "RSR"
# allCapture$siteID[allCapture$site == "Arrowsmith_Mountain"] <- "AM"
#
#
# allCapture$chron[allCapture$chron == "Simulation"] <- "sim"
# allCapture$chron[allCapture$chron == "Original_Chron"] <- "chron"
#
# names(allCapture)[2] <- "PImethod"
#
#
# rbarDat <- allChronProps[allChronProps$chronProp == "rbar",]
# AR1Dat <- allChronProps[allChronProps$chronProp == "AR1",]
# corDat <- allChronProps[allChronProps$chronProp == "Cor",]
# sdDat <- allChronProps[allChronProps$chronProp == "SD",]
#
# rm(allChronProps)
#
# capturePropertyCors <- data.frame(matrix(nrow=108, ncol=5, data=NA))
# colnames(capturePropertyCors) <- c("Property", "Err_Method", "Site", "Boot", "Correlation")
#
# siteIDs <- unique(allCapture$siteID)
# Errs <- unique(allCapture$Err)
# boots <- unique(allCapture$boot)
# propVals <- list(rbarDat, AR1Dat, corDat, sdDat)
# propNames <- c("rbarDat", "AR1Dat", "corDat", "sdDat")
# rowCt <- 0
# for (m in 1:4){
#   for (j in 1:3){
#     for (k in 1:3){
#       for (i in 1:3){
#         rowCt <- rowCt + 1
#         TNME90E <- allCapture[allCapture$boot == as.character(boots)[j] &
#                                 allCapture$Err == as.character(Errs)[k] &
#                                 allCapture$siteID == as.character(siteIDs)[i],]
#
#         cor1 <- cor(propVals[[m]]$value[propVals[[m]]$siteID == as.character(siteIDs)[i]], TNME90E$Percent_Capture)
#
#         capturePropertyCors[rowCt,] <- c(propNames[m],Errs[k],siteIDs[i], boots[j], cor1)
#
#         cat("Chronology Property", propNames[m], "Error Method:",Errs[k],"siteID:",siteIDs[i], "Bootstrapping:", boots[j],"correlation:", cor1, "\n")
#
#
#       }
#     }
#   }
# }
#
# capturePropertyCors[order(as.numeric(capturePropertyCors$Correlation)),]
#
#
#
#
#
#
#
#
#
