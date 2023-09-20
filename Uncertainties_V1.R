###############################################
###Capturing chronology confidence intervals###
###############################################

##############
##Asumptions##
##############

#1 The chronology value at year t is an approximation of the population value 
#given a random(?!) subset

#2 The distribution of values at year t is a sample with identical distribution
#to the population

#3 The SD of the values may vary with the strength of the signal/limiting 
#factor (climate)

#################
##Error Sources##
#################

#1 MEboot captures error in an isolated time series. It does not take into account
#the range of values at time t for all series. Therefore, this technique captures
#series error, but not chronology error.

#2 Detrending introduces error!

#3 Chronology Mean Value error (CMVE) 

#4 Total chronology error (TCE) should account for all three of the above

#5 Reconstruction error is much more complex:

##1 The longer the periodicity of variability, the less opportunity for testing 
#in calibration-verification schemes

###a We could calculate DF for each frequency and perform a 2D calibration-verification

###b 

################################################################################
#Clean slate
rm(list=ls())
gc()
par(mfrow=c(1,1))

#Load data and packages

if(!require(dplR)){
  install.packages("dplR")
  library(dplR)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}

if(!require(meboot)){
  install.packages("meboot")
  library(meboot)
}

if(!require(plm)){
  install.packages("plm")
  library(plm)
}

if(!require(ringdater)){
  install.packages("ringdater")
  library(ringdater)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

data(ca533)
data(co021)

plot.crn(chron(dplR::detrend(ca533,method = "Spline")))
plot.crn(chron(dplR::detrend(co021,method = "Spline")))
################################################################################

#####################
########MEBoot#######
##Time Series Error##
#####################

#Look at a single series first
set.seed(345)
out <- meboot(x=ca533$CAM011[complete.cases(ca533$CAM011)], reps=10000, trim=0.25, elaps=TRUE)

seriesCI <- data.frame("time"=as.numeric(row.names(ca533)[complete.cases(ca533$CAM011)]),
           "series"=ca533$CAM011[complete.cases(ca533$CAM011)],
           "lower"=apply(out$ensemble, 1, function(x) sort(x)[51]),
           "upper"=apply(out$ensemble, 1, function(x) sort(x)[9950]))


plotData <- melt(seriesCI, id.vars = "time")

ggplot(data=plotData, mapping = aes(x=time, y = value)) + geom_line() +
  geom_ribbon(data=seriesCI, mapping = aes(x=time, ymin = lower, ymax=upper), inherit.aes = FALSE, 
              alpha=0.1,       #transparency
              linetype=1,      #solid, dashed or other line types
              colour="grey70", #border line color
              size=1,          #border line size
              fill="green")    #fill color)

#Now try with the whole rwi
ca533.detA <- detrend(ca533, method = "ModNegExp")
#Set number of ensemble members
ENSct <- 1000
#Set number of simulated chronologies
simChrons <- 1000
#Capture chronology simulations
captChron <- data.frame(matrix(data = NA, nrow = length(rownames(ca533.detA)), ncol = simChrons))
#Build ensemble for each series
bootDat <- apply(ca533.detA, 2, function(x) meboot(x=x[complete.cases(x)], reps=ENSct, trim=0.25, elaps=TRUE))
for (j in 1:simChrons){
  #Sample ensemble for each series
  simulatedSeries <- lapply(bootDat, function(x) x[[2]][,sample(1:ENSct,1)])
  #Build the simulated rwi
  rwiSim <- ca533.detA
  for (i in 1:dim(ca533)[2]){
    rwiSim[,i][complete.cases(rwiSim[,i])] <- simulatedSeries[[i]]
  }
  captChron[,j] <- chron(rwiSim)$xxxstd
}
pData <- cbind.data.frame("Year" = as.numeric(rownames(ca533.detA)),
                 captChron)
pDataCI <- cbind.data.frame("Year" = as.numeric(rownames(ca533.detA)),
                            t(apply(pData[,-1], 1, function(x) quantile(x, probs = c(.05,.95)))))
colnames(pDataCI) <- c("Year", "LowerB", "UpperB")
pData2 <- melt(pData, id.vars = "Year")
ggplot(data = pData2, mapping = aes(x=Year, y=value)) + geom_line() +
  geom_ribbon(data=pDataCI, mapping = aes(x=Year, ymin = LowerB, ymax=UpperB), inherit.aes = FALSE, 
              alpha=0.1,       #transparency
              linetype=1,      #solid, dashed or other line types
              colour="grey70", #border line color
              size=.3,          #border line size
              fill="green")    #fill color)
################################################################################

####################
#######EPS Adj######
##Detrending Error##
####################

#1 Detrend chronology for study as needed
ca533.detA <- detrend(ca533, method = "ModNegExp")

#2 Calculate running EPS on detrended indices
EXPstatsA <- rwi.stats.running(ca533.detA, window.length = 30, window.overlap = 29)

#3 Calculate running EPS on indices as detrended for Cofecha
ca533.detB <- detrend(ca533, method = "Spline", nyrs = 7)

EXPstatsB <- rwi.stats.running(ca533.detB, window.length = 30, window.overlap = 29)

#4 The reduced EPS (increased spread in any given year) is the error 
#due to detrending

plot(EXPstatsA$eps, type = "l")
lines(EXPstatsB$eps, col = "blue")
median(EXPstatsA$eps)
median(EXPstatsB$eps)
median(EXPstatsA$rbar.eff)
median(EXPstatsB$rbar.eff)

ca533.sdA <- apply(ca533.detA, 1, function(x) sd(x, na.rm = TRUE))
ca533.sdB <- apply(ca533.detB, 1, function(x) sd(x, na.rm = TRUE))
plot(ca533.sdA, type = "l")
lines(ca533.sdB, col = "blue")

source("D:/Lab Backup/R/EPSrcs.R")
EPSadj <- EPSrcs(rwl = ca533, rwi = ca533.detA, winLen = 30, winOv = 29, spLen = 7)
plot(EPSadj$eps, type = "l")
lines(EPSadj$EPSadj, col = "blue")
lines(x=1:length(EPSadj$eps), y=rep(0.85,length(EPSadj$eps)), lty=2)




